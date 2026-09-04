# =============================================================================
# R/core/analysis_core.R
# Fungsi inti untuk Extended Analysis dan Method Comparison
#
# BERISI:
#   - run_dominant_component()   — analisis komponen dominan per distrik
#   - run_component_profile()    — profil RC per kelas kerentanan
#   - run_moran_lisa()           — Global Moran's I + LISA
#   - run_sensitivity()          — Sensitivity analysis loading threshold
#   - run_cutter_comparison()    — Perbandingan dengan metode Cutter (2003)
#   - run_3way_comparison()      — Perbandingan 3 direction method
#
# DEPENDENSI:
#   - R/core/helpers.R    (normalize_id, normalize_01)
#   - R/core/sovi_core.R  (run_sovi_core)
#   - Package: dplyr, spdep, sf
# =============================================================================


# =============================================================================
# EXTENDED ANALYSIS 1 — DOMINANT COMPONENT
# Identifikasi komponen RC mana yang paling dominan per distrik
# =============================================================================

#' Tambahkan kolom komponen dominan ke sovi_df
#'
#' @param sovi_df  data.frame hasil SoVI (harus ada kolom RC*)
#' @param rc_cols  Vektor nama kolom RC (misal c("RC1","RC2","RC3"))
#'
#' @return sovi_df dengan tambahan kolom dom_comp (nama RC dominan)
run_dominant_component <- function(sovi_df, rc_cols) {
  
  # Ambil nilai absolut setiap RC per distrik
  rc_mat  <- as.matrix(sovi_df[, rc_cols, drop = FALSE])
  
  # Cari index RC dengan nilai absolut terbesar
  dom_idx  <- apply(abs(rc_mat), 1, which.max)
  
  # Simpan nama RC dominan (bukan index)
  sovi_df$dom_comp <- rc_cols[dom_idx]
  
  return(sovi_df)
}


# =============================================================================
# EXTENDED ANALYSIS 2 — COMPONENT PROFILE
# Rata-rata skor RC per kelas kerentanan (untuk heatmap dan radar chart)
# =============================================================================

#' Hitung profil komponen RC per kelas kerentanan
#'
#' @param sovi_df  data.frame hasil SoVI (harus ada kolom RC* dan vuln_class)
#' @param rc_cols  Vektor nama kolom RC
#'
#' @return data.frame profil: vuln_class × RC (nilai rata-rata ternormalisasi)
run_component_profile <- function(sovi_df, rc_cols) {
  
  # Normalisasi RC ke [0, 1] agar perbandingan antar komponen fair
  rc_norm            <- as.data.frame(
    lapply(sovi_df[, rc_cols, drop = FALSE], normalize_01)
  )
  rc_norm$vuln_class <- sovi_df$vuln_class
  
  # Rata-rata per kelas kerentanan
  profile <- rc_norm |>
    dplyr::group_by(vuln_class) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(rc_cols),
                    \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  return(profile)
}


# =============================================================================
# EXTENDED ANALYSIS 3 — MORAN'S I + LISA
# Mengukur autokorelasi spasial SoVI score
# =============================================================================

#' Hitung Global Moran's I dan Local Moran (LISA)
#'
#' @param sovi_df  data.frame hasil SoVI (harus ada kolom sovi_score)
#' @param shp      Shapefile sf object
#' @param join_shp Nama kolom ID di shapefile
#' @param join_df  Nama kolom ID di sovi_df
#'
#' @return List berisi:
#'   \item{peta}         sf object dengan kolom lisa_I, lisa_p, lisa_quad
#'   \item{moran_global} Hasil spdep::moran.test()
#'   \item{lisa_table}   Tabel frekuensi klasifikasi LISA
#'   \item{lisa_colors}  Named vector warna per kategori LISA
#'   \item{lw}           Spatial weights list (spdep)
#'   \item{n_islands}    Jumlah unit tanpa tetangga (fallback ke KNN)
run_moran_lisa <- function(sovi_df, shp, join_shp, join_df) {
  
  # ── Normalisasi ID & join ke shapefile ────────────────────────────────────
  sovi_df[[join_df]] <- normalize_id(sovi_df[[join_df]])
  shp[[join_shp]]    <- normalize_id(shp[[join_shp]])
  
  peta <- dplyr::left_join(shp, sovi_df,
                           by = setNames(join_df, join_shp))
  peta <- peta[!is.na(peta$sovi_score), ]
  
  # ── Validasi geometri ─────────────────────────────────────────────────────
  sf::sf_use_s2(FALSE)
  peta <- sf::st_make_valid(peta)
  sf::sf_use_s2(TRUE)
  
  # ── Bangun spatial weights (Queen contiguity) ─────────────────────────────
  nb_queen  <- spdep::poly2nb(peta, queen = TRUE, snap = 0.01)
  n_islands <- sum(spdep::card(nb_queen) == 0)
  
  # ── KNN Fallback untuk unit tanpa tetangga (pulau) ────────────────────────
  if (n_islands > 0) {
    sf::sf_use_s2(FALSE)
    peta_proj  <- sf::st_transform(peta, crs = 32749)
    coords_knn <- sf::st_coordinates(sf::st_centroid(peta_proj))
    sf::sf_use_s2(TRUE)
    
    nb_knn <- spdep::knn2nb(spdep::knearneigh(coords_knn, k = 3))
    nb     <- nb_queen
    
    # Ganti neighbors unit pulau dengan KNN
    for (i in which(spdep::card(nb_queen) == 0)) {
      nb[[i]] <- nb_knn[[i]]
    }
  } else {
    nb <- nb_queen
  }
  
  # ── Row-standardized spatial weights ─────────────────────────────────────
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # ── Global Moran's I ──────────────────────────────────────────────────────
  moran_global <- spdep::moran.test(
    peta$sovi_score,
    listw       = lw,
    zero.policy = TRUE
  )
  
  # ── Local Moran (LISA) ────────────────────────────────────────────────────
  lisa   <- spdep::localmoran(peta$sovi_score, listw = lw, zero.policy = TRUE)
  z_sc   <- scale(peta$sovi_score)[, 1]          # Z-score sovi_score
  lag_z  <- spdep::lag.listw(lw, z_sc, zero.policy = TRUE)  # spatial lag
  lisa_p <- lisa[, "Pr(z != E(Ii))"]
  
  peta$lisa_I <- lisa[, "Ii"]
  peta$lisa_p <- lisa_p
  
  # Klasifikasi kuadran LISA (Moran Scatterplot)
  peta$lisa_quad <- dplyr::case_when(
    lisa_p >= 0.05        ~ "Not Significant",
    z_sc > 0 & lag_z > 0 ~ "High-High (Hot Spot)",
    z_sc < 0 & lag_z < 0 ~ "Low-Low (Cold Spot)",
    z_sc > 0 & lag_z < 0 ~ "High-Low (Outlier)",
    z_sc < 0 & lag_z > 0 ~ "Low-High (Outlier)",
    TRUE                  ~ "Not Significant"
  )
  
  # ── Warna per kategori LISA ───────────────────────────────────────────────
  lisa_colors <- c(
    "High-High (Hot Spot)" = "#d7191c",
    "Low-Low (Cold Spot)"  = "#2c7bb6",
    "High-Low (Outlier)"   = "#fdae61",
    "Low-High (Outlier)"   = "#abd9e9",
    "Not Significant"      = "#f0f0f0"
  )
  
  peta$lisa_quad <- factor(peta$lisa_quad, levels = names(lisa_colors))
  
  # ── Tabel distribusi LISA ─────────────────────────────────────────────────
  lisa_table         <- as.data.frame(table(LISA = peta$lisa_quad))
  lisa_table$Percent <- round(lisa_table$Freq / nrow(peta) * 100, 1)
  
  return(list(
    peta         = peta,
    moran_global = moran_global,
    lisa_table   = lisa_table,
    lisa_colors  = lisa_colors,
    lw           = lw,
    n_islands    = n_islands
  ))
}


# =============================================================================
# EXTENDED ANALYSIS 4 — SENSITIVITY ANALYSIS
# Uji robustness SoVI terhadap perubahan loading threshold
# =============================================================================

#' Uji sensitivitas SoVI terhadap variasi loading threshold
#'
#' @param data             data.frame dataset asli
#' @param sovi_vars        Vektor variabel SoVI
#' @param neg_vars         Vektor variabel protektif
#' @param direction_method Metode direction yang digunakan
#' @param id_col           Nama kolom ID
#' @param name_col         Nama kolom nama wilayah
#' @param pca_rotation     Rotasi PCA (default "varimax")
#'
#' @return List berisi:
#'   \item{sens_scores} List skor SoVI per threshold
#'   \item{sens_info}   Info variabel masuk/keluar per threshold
#'   \item{corr_mat}    Matriks korelasi Spearman antar threshold
#'   \item{scores_df}   data.frame skor ketiga threshold berdampingan
run_sensitivity <- function(data,
                            sovi_vars,
                            neg_vars,
                            direction_method,
                            id_col,
                            name_col,
                            pca_rotation = "varimax") {
  
  # Tiga threshold yang dibandingkan
  thresholds  <- c(0.5, 0.6, 0.7)
  sens_scores <- list()
  sens_info   <- list()
  
  for (thr in thresholds) {
    
    res <- tryCatch(
      run_sovi_core(
        data              = data,
        sovi_vars         = sovi_vars,
        neg_vars          = neg_vars,
        direction_method  = direction_method,
        loading_threshold = thr,
        pca_rotation      = pca_rotation,
        id_col            = id_col,
        name_col          = name_col
      ),
      error = function(e) NULL
    )
    
    key <- paste0("thr_", thr)
    
    if (!is.null(res)) {
      sens_scores[[key]] <- res$sovi_df$sovi_score
      n_assigned         <- sum(!is.na(res$selection_out$assignment$component))
      n_unassigned       <- length(res$selection_out$unassigned_vars)
      sens_info[[key]]   <- list(
        threshold    = thr,
        n_assigned   = n_assigned,
        n_unassigned = n_unassigned,
        unassigned   = res$selection_out$unassigned_vars
      )
    } else {
      sens_scores[[key]] <- rep(NA, nrow(data))
      sens_info[[key]]   <- list(threshold = thr, error = TRUE)
    }
  }
  
  # ── Matriks korelasi Spearman antar threshold ─────────────────────────────
  scores_mat <- data.frame(
    thr_0.5 = sens_scores$thr_0.5,
    thr_0.6 = sens_scores$thr_0.6,
    thr_0.7 = sens_scores$thr_0.7
  )
  
  corr_mat <- tryCatch(
    cor(scores_mat, method = "spearman", use = "complete.obs"),
    error = function(e) matrix(NA, 3, 3)
  )
  
  return(list(
    sens_scores = sens_scores,
    sens_info   = sens_info,
    corr_mat    = corr_mat,
    scores_df   = scores_mat
  ))
}


# =============================================================================
# EXTENDED ANALYSIS 5 — CUTTER COMPARISON
# Bandingkan metode yang diusulkan dengan metode orisinal Cutter (2003)
# =============================================================================

#' Bandingkan SoVI score metode proposed vs Cutter (2003)
#'
#' @param sovi_df_proposed data.frame hasil SoVI metode yang diusulkan
#' @param pca_out          Output run_pca()
#' @param id_col           Nama kolom ID
#' @param name_col         Nama kolom nama wilayah
#'
#' @return List berisi:
#'   \item{comparison_df} data.frame perbandingan skor dan ranking
#'   \item{spearman_r}    Korelasi Spearman
#'   \item{spearman_p}    p-value korelasi Spearman
#'   \item{dir_cutter}    Direction per komponen metode Cutter
run_cutter_comparison <- function(sovi_df_proposed, pca_out,
                                  id_col, name_col) {
  
  load_mat   <- as.matrix(pca_out$loadings)
  scores_mat <- pca_out$scores
  n_factors  <- ncol(scores_mat)
  var_names  <- rownames(load_mat)
  
  # ── Tentukan direction Cutter: dari variabel dominan per komponen ─────────
  dir_cutter <- numeric(n_factors)
  for (k in seq_len(n_factors)) {
    dom_var        <- var_names[which.max(abs(load_mat[, k]))]
    dom_load       <- load_mat[dom_var, k]
    dir_cutter[k]  <- ifelse(dom_load > 0, 1, -1)
  }
  
  # ── Hitung SoVI Cutter: factor scores × direction ─────────────────────────
  directed         <- sweep(scores_mat, 2, dir_cutter, "*")
  sovi_raw_cutter  <- rowSums(directed)
  sovi_norm_cutter <- (sovi_raw_cutter - min(sovi_raw_cutter)) /
    (max(sovi_raw_cutter) - min(sovi_raw_cutter))
  
  # ── Korelasi Spearman ─────────────────────────────────────────────────────
  proposed <- sovi_df_proposed$sovi_score
  rho      <- cor(proposed, sovi_norm_cutter, method = "spearman")
  rho_test <- cor.test(proposed, sovi_norm_cutter, method = "spearman")
  
  # ── Tabel perbandingan ────────────────────────────────────────────────────
  comparison_df <- data.frame(
    ID            = sovi_df_proposed[[id_col]],
    Nama          = sovi_df_proposed[[name_col]],
    sovi_proposed = proposed,
    sovi_cutter   = sovi_norm_cutter,
    rank_proposed = rank(-proposed),
    rank_cutter   = rank(-sovi_norm_cutter),
    rank_diff     = rank(-proposed) - rank(-sovi_norm_cutter),
    stringsAsFactors = FALSE
  )
  
  return(list(
    comparison_df = comparison_df,
    spearman_r    = round(rho, 4),
    spearman_p    = rho_test$p.value,
    dir_cutter    = dir_cutter
  ))
}


# =============================================================================
# 3-WAY COMPARISON
# Bandingkan 3 direction method: theory, loading, cutter
# Metrik evaluasi yang dihitung:
#   1. Spearman ρ       — konsistensi ranking global
#   2. Kendall τ        — konsistensi ranking lokal (konfirmasi Spearman)
#   3. Top-20% Agreement— kesepakatan distrik paling rentan (relevan kebijakan)
#   4. Cohen's κ        — kesepakatan klasifikasi kelas (koreksi kebetulan)
#   5. MARD             — rata-rata pergeseran ranking (intuitif)
#   6. RMSD             — magnitude perbedaan skor
# =============================================================================

# ── Helper: Hitung semua metrik untuk satu pasangan method ─────────────────

#' Hitung semua metrik evaluasi untuk satu pasangan method
#'
#' @param score_a   Vektor skor SoVI method A
#' @param score_b   Vektor skor SoVI method B
#' @param class_a   Factor kelas kerentanan method A
#' @param class_b   Factor kelas kerentanan method B
#' @param top_pct   Persentase untuk top agreement (default 0.20 = 20%)
#'
#' @return List semua metrik
calc_comparison_metrics <- function(score_a, score_b,
                                    class_a, class_b,
                                    top_pct = 0.20) {
  n <- length(score_a)
  
  # ── 1. Spearman ρ ─────────────────────────────────────────────────────────
  spearman   <- cor.test(score_a, score_b, method = "spearman",
                         exact = FALSE)
  spearman_r <- round(spearman$estimate, 4)
  spearman_p <- spearman$p.value
  
  # ── 2. Kendall τ ──────────────────────────────────────────────────────────
  kendall   <- cor.test(score_a, score_b, method = "kendall",
                        exact = FALSE)
  kendall_r <- round(kendall$estimate, 4)
  kendall_p <- kendall$p.value
  
  # ── 3. Top-20% Agreement ──────────────────────────────────────────────────
  # Distrik mana yang masuk top 20% paling rentan di kedua method?
  k_top     <- max(1, round(n * top_pct))
  top_a     <- order(score_a, decreasing = TRUE)[seq_len(k_top)]
  top_b     <- order(score_b, decreasing = TRUE)[seq_len(k_top)]
  top_agree <- round(length(intersect(top_a, top_b)) / k_top * 100, 1)
  
  # Bottom 20% (paling tidak rentan)
  bot_a     <- order(score_a, decreasing = FALSE)[seq_len(k_top)]
  bot_b     <- order(score_b, decreasing = FALSE)[seq_len(k_top)]
  bot_agree <- round(length(intersect(bot_a, bot_b)) / k_top * 100, 1)
  
  # ── 4. Cohen's κ ──────────────────────────────────────────────────────────
  # Kesepakatan klasifikasi kelas kerentanan, koreksi kebetulan
  kappa_val <- tryCatch({
    tbl  <- table(class_a, class_b)
    n_   <- sum(tbl)
    po   <- sum(diag(tbl)) / n_               # observed agreement
    pe   <- sum(rowSums(tbl) * colSums(tbl)) / n_^2  # expected by chance
    round((po - pe) / (1 - pe), 4)
  }, error = function(e) NA)
  
  # ── 5. MARD (Mean Absolute Rank Difference) ────────────────────────────────
  rank_a <- rank(-score_a)   # rank 1 = paling rentan
  rank_b <- rank(-score_b)
  mard   <- round(mean(abs(rank_a - rank_b)), 2)
  
  # ── 6. RMSD (Root Mean Square Difference skor) ────────────────────────────
  rmsd <- round(sqrt(mean((score_a - score_b)^2)), 4)
  
  return(list(
    spearman_r = spearman_r,
    spearman_p = spearman_p,
    kendall_r  = kendall_r,
    kendall_p  = kendall_p,
    top_agree  = top_agree,   # % Top-20% sama
    bot_agree  = bot_agree,   # % Bottom-20% sama
    kappa      = kappa_val,
    mard       = mard,
    rmsd       = rmsd,
    n          = n,
    k_top      = k_top
  ))
}


#' Jalankan SoVI dengan 3 direction method dan bandingkan hasilnya
#'
#' @param data              data.frame dataset
#' @param sovi_vars         Vektor variabel SoVI
#' @param neg_vars          Vektor variabel protektif
#' @param loading_threshold Threshold loading
#' @param pca_rotation      Rotasi PCA
#' @param id_col            Nama kolom ID
#' @param name_col          Nama kolom nama wilayah
#'
#' @return List berisi:
#'   \item{results}    List hasil per method
#'   \item{corr_mat}   Matriks korelasi Spearman (kompatibilitas mundur)
#'   \item{scores_df}  data.frame skor ketiga method berdampingan
#'   \item{metrics}    List semua metrik evaluasi per pasangan
#'   \item{metrics_df} data.frame ringkasan semua metrik
run_3way_comparison <- function(data,
                                sovi_vars,
                                neg_vars,
                                loading_threshold = 0.5,
                                pca_rotation      = "varimax",
                                id_col,
                                name_col) {
  
  methods <- c("theory", "loading", "cutter")
  results <- list()
  
  # ── Langkah 1: Jalankan SoVI untuk setiap method ─────────────────────────
  for (m in methods) {
    res <- tryCatch(
      run_sovi_core(
        data              = data,
        sovi_vars         = sovi_vars,
        neg_vars          = neg_vars,
        direction_method  = m,
        loading_threshold = loading_threshold,
        pca_rotation      = pca_rotation,
        id_col            = id_col,
        name_col          = name_col
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    results[[m]] <- res
  }
  
  # ── Langkah 2: Ekstrak skor & kelas dari setiap method ───────────────────
  score_T <- results$theory$sovi_df$sovi_score
  score_L <- results$loading$sovi_df$sovi_score
  score_C <- results$cutter$sovi_df$sovi_score
  
  class_T <- results$theory$sovi_df$vuln_class
  class_L <- results$loading$sovi_df$vuln_class
  class_C <- results$cutter$sovi_df$vuln_class
  
  # ── Langkah 3: Hitung semua metrik per pasangan ───────────────────────────
  metrics <- list(
    theory_vs_loading = calc_comparison_metrics(score_T, score_L,
                                                class_T, class_L),
    theory_vs_cutter  = calc_comparison_metrics(score_T, score_C,
                                                class_T, class_C),
    loading_vs_cutter = calc_comparison_metrics(score_L, score_C,
                                                class_L, class_C)
  )
  
  # ── Langkah 4: Matriks Spearman (kompatibilitas mundur) ──────────────────
  scores_df <- data.frame(theory  = score_T,
                          loading = score_L,
                          cutter  = score_C)
  corr_mat  <- cor(scores_df, method = "spearman")
  
  # ── Langkah 5: Ringkasan semua metrik dalam satu tabel ───────────────────
  pair_labels <- c(
    "Theory-Based (PM) vs Loading Sign",
    "Theory-Based (PM) vs Cutter's Method",
    "Loading Sign vs Cutter's Method"
  )
  
  metrics_df <- data.frame(
    Pasangan        = pair_labels,
    Spearman_rho    = c(metrics$theory_vs_loading$spearman_r,
                        metrics$theory_vs_cutter$spearman_r,
                        metrics$loading_vs_cutter$spearman_r),
    Kendall_tau     = c(metrics$theory_vs_loading$kendall_r,
                        metrics$theory_vs_cutter$kendall_r,
                        metrics$loading_vs_cutter$kendall_r),
    Top20_Agreement = c(metrics$theory_vs_loading$top_agree,
                        metrics$theory_vs_cutter$top_agree,
                        metrics$loading_vs_cutter$top_agree),
    Bot20_Agreement = c(metrics$theory_vs_loading$bot_agree,
                        metrics$theory_vs_cutter$bot_agree,
                        metrics$loading_vs_cutter$bot_agree),
    Cohen_kappa     = c(metrics$theory_vs_loading$kappa,
                        metrics$theory_vs_cutter$kappa,
                        metrics$loading_vs_cutter$kappa),
    MARD            = c(metrics$theory_vs_loading$mard,
                        metrics$theory_vs_cutter$mard,
                        metrics$loading_vs_cutter$mard),
    RMSD            = c(metrics$theory_vs_loading$rmsd,
                        metrics$theory_vs_cutter$rmsd,
                        metrics$loading_vs_cutter$rmsd),
    stringsAsFactors = FALSE
  )
  
  return(list(
    results    = results,
    corr_mat   = corr_mat,
    scores_df  = data.frame(
      ID      = results$theory$sovi_df[[id_col]],
      Nama    = results$theory$sovi_df[[name_col]],
      theory  = score_T,
      loading = score_L,
      cutter  = score_C
    ),
    metrics    = metrics,
    metrics_df = metrics_df
  ))
}

message("[analysis_core.R] Fungsi Extended Analysis loaded.")