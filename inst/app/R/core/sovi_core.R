# =============================================================================
# R/core/sovi_core.R
# Pipeline inti perhitungan Social Vulnerability Index (SoVI)
#
# BERISI (urutan pipeline Fase 2 → Fase 6):
#   Fase 2 : standardize_data()              — Z-score standardisasi
#   Fase 3 : run_pca()                        — PCA + diagnostik KMO/Bartlett
#   Fase 4 : select_variables_per_component() — Seleksi variabel per komponen
#   Fase 5 : compute_sovi()                   — Agregasi skor SoVI
#   Fase 6 : classify_sovi()                  — Klasifikasi Jenks
#   UTAMA  : run_sovi_core()                  — Wrapper menjalankan Fase 2-6
#
# DEPENDENSI:
#   - R/core/helpers.R  (VULN_CLASSES harus sudah di-source)
#   - Package: psych, classInt, dplyr
# =============================================================================


# =============================================================================
# FASE 2 — STANDARDISASI Z-SCORE
# =============================================================================

#' Standardisasi data ke Z-score (mean=0, SD=1)
#'
#' @param data     data.frame dataset asli
#' @param sovi_vars Vektor nama kolom variabel SoVI yang dipilih
#' @param id_col   Nama kolom ID wilayah
#' @param name_col Nama kolom nama wilayah
#'
#' @return List berisi:
#'   \item{Z}{data.frame Z-score}
#'   \item{unit_ids}{Vektor ID wilayah}
#'   \item{kabupaten}{Vektor nama wilayah}
standardize_data <- function(data, sovi_vars, id_col, name_col) {
  
  unit_ids  <- data[[id_col]]
  kabupaten <- data[[name_col]]
  
  # Ambil hanya kolom variabel SoVI, pastikan numerik
  X    <- data[, sovi_vars, drop = FALSE]
  X[]  <- lapply(X, as.numeric)
  
  # Z-score: center=TRUE (kurangi mean), scale=TRUE (bagi SD)
  Z <- as.data.frame(scale(X, center = TRUE, scale = TRUE))
  
  return(list(
    Z         = Z,
    unit_ids  = unit_ids,
    kabupaten = kabupaten
  ))
}


# =============================================================================
# FASE 3 — DIAGNOSTIK PCA + PCA (Kaiser + Varimax)
# =============================================================================

#' Jalankan PCA dengan diagnostik KMO dan Bartlett
#'
#' @param Z              data.frame Z-score (output standardize_data)
#' @param rotation       Metode rotasi: "varimax" (default), "oblimin", dll
#' @param min_eigenvalue Ambang nilai eigen untuk Kaiser criterion (default 1)
#' @param comm_threshold Ambang communality minimum (default 0.4, info saja)
#'
#' @return List berisi semua hasil PCA dan diagnostik:
#'   \item{pca_result}   Objek psych::principal
#'   \item{n_factors}    Jumlah faktor terpilih (Kaiser)
#'   \item{eigen_vals}   Nilai eigen semua komponen
#'   \item{loadings}     Loading matrix (objek psych)
#'   \item{scores}       Factor scores matrix
#'   \item{var_expl}     Variansi yang dijelaskan per komponen
#'   \item{total_var}    Total variansi yang dijelaskan (%)
#'   \item{communalities}Communality per variabel
#'   \item{comm_df}      data.frame communality dengan status
#'   \item{kmo_overall}  Nilai KMO keseluruhan
#'   \item{kmo_label}    Label interpretasi KMO
#'   \item{kmo_per_var}  KMO per variabel
#'   \item{kmo_df}       data.frame KMO per variabel
#'   \item{bartlett_chi} Chi-square Bartlett
#'   \item{bartlett_df}  Degrees of freedom Bartlett
#'   \item{bartlett_p}   p-value Bartlett
#'   \item{rotation_used}Nama rotasi yang digunakan
run_pca <- function(Z,
                    rotation       = "varimax",
                    min_eigenvalue = 1,
                    comm_threshold = 0.4) {
  
  # ── Hitung matriks korelasi ────────────────────────────────────────────────
  R <- cor(Z, use = "complete.obs")
  
  # ── KMO (Kaiser-Meyer-Olkin) ──────────────────────────────────────────────
  kmo_result  <- psych::KMO(R)
  kmo_overall <- kmo_result$MSA
  kmo_per_var <- round(kmo_result$MSAi, 3)
  
  kmo_label <- dplyr::case_when(
    kmo_overall >= 0.90 ~ "Sangat Baik (Marvelous)",
    kmo_overall >= 0.80 ~ "Baik (Meritorious)",
    kmo_overall >= 0.70 ~ "Cukup (Middling)",
    kmo_overall >= 0.60 ~ "Moderate (Mediocre)",
    kmo_overall >= 0.50 ~ "Buruk (Miserable)",
    TRUE                ~ "Tidak Dapat Diterima"
  )
  
  # ── Bartlett's Test of Sphericity ─────────────────────────────────────────
  n_obs        <- nrow(Z)
  bartlett_res <- psych::cortest.bartlett(R, n = n_obs)
  
  # ── Kaiser Criterion: pilih komponen dengan eigenvalue >= 1 ───────────────
  eigen_vals <- eigen(R)$values
  n_factors  <- max(1, sum(eigen_vals >= min_eigenvalue))
  
  # ── PCA dengan rotasi ─────────────────────────────────────────────────────
  pca_result <- psych::principal(
    r        = Z,
    nfactors = n_factors,
    rotate   = rotation,
    scores   = TRUE
  )
  
  # ── Variansi yang dijelaskan ───────────────────────────────────────────────
  var_explained <- pca_result$Vaccounted
  total_var     <- sum(var_explained["Proportion Var", ]) * 100
  
  # ── Communality per variabel ──────────────────────────────────────────────
  communalities <- pca_result$communality
  comm_df <- data.frame(
    Variabel    = names(communalities),
    Communality = round(communalities, 3),
    Status      = dplyr::case_when(
      communalities >= 0.70 ~ "Sangat Baik",
      communalities >= 0.50 ~ "Cukup Baik",
      communalities >= 0.40 ~ "Batas Minimum",
      TRUE                  ~ "Low"
    ),
    stringsAsFactors = FALSE
  )
  
  # ── KMO per variabel dalam tabel ──────────────────────────────────────────
  kmo_df <- data.frame(
    Variabel = names(kmo_per_var),
    KMO      = kmo_per_var,
    Status   = ifelse(kmo_per_var >= 0.5, "OK", "Low"),
    stringsAsFactors = FALSE
  )
  
  return(list(
    pca_result    = pca_result,
    n_factors     = n_factors,
    eigen_vals    = eigen_vals,
    loadings      = pca_result$loadings,
    scores        = pca_result$scores,
    var_expl      = var_explained,
    total_var     = round(total_var, 2),
    communalities = communalities,
    comm_df       = comm_df,
    kmo_overall   = kmo_overall,
    kmo_label     = kmo_label,
    kmo_per_var   = kmo_per_var,
    kmo_df        = kmo_df,
    bartlett_chi  = round(bartlett_res$chisq, 3),
    bartlett_df   = bartlett_res$df,
    bartlett_p    = bartlett_res$p.value,
    rotation_used = rotation
  ))
}


# =============================================================================
# FASE 4 — SELEKSI VARIABEL PER KOMPONEN
# =============================================================================

#' Assign setiap variabel ke komponen dominannya dan tentukan direction
#'
#' @param pca_out         Output dari run_pca()
#' @param negative_vars   Vektor nama variabel yang bersifat protektif (-)
#' @param loading_threshold Ambang loading absolut minimum (default 0.5)
#' @param direction_method  Metode penentuan arah:
#'   "theory"  = dari teori (user tentukan via negative_vars)
#'   "loading" = dari tanda loading PCA
#'   "cutter"  = dari variabel dominan per komponen (Cutter 2003)
#'
#' @return List berisi:
#'   \item{assignment}       data.frame assignment variabel ke komponen
#'   \item{unassigned_vars}  Variabel yang loading-nya di bawah threshold
#'   \item{loadings_mat}     Loading matrix (as.matrix)
#'   \item{comp_names}       Nama komponen (RC1, RC2, ...)
#'   \item{comp_directions}  Direction per komponen (untuk metode cutter)
#'   \item{direction_method} Nama metode yang digunakan
select_variables_per_component <- function(pca_out,
                                           negative_vars     = character(0),
                                           loading_threshold = 0.5,
                                           direction_method  = "theory") {
  
  loadings_mat <- as.matrix(pca_out$loadings)
  n_vars       <- nrow(loadings_mat)
  n_factors    <- ncol(loadings_mat)
  var_names    <- rownames(loadings_mat)
  comp_names   <- colnames(loadings_mat)
  
  # ── Tentukan direction per komponen (khusus metode "cutter") ─────────────
  # Cutter (2003): direction komponen = tanda loading variabel dominan
  comp_directions <- rep(1L, n_factors)
  if (direction_method == "cutter") {
    for (k in seq_len(n_factors)) {
      dom_var  <- var_names[which.max(abs(loadings_mat[, k]))]
      dom_load <- loadings_mat[dom_var, k]
      if (dom_var %in% negative_vars) {
        comp_directions[k] <- ifelse(dom_load > 0, -1L, 1L)
      } else {
        comp_directions[k] <- ifelse(dom_load > 0, 1L, -1L)
      }
    }
  }
  
  # ── Cari komponen dominan untuk setiap variabel ───────────────────────────
  # Variabel diassign ke komponen dengan loading absolut tertinggi
  best_component <- apply(abs(loadings_mat), 1, which.max)
  
  # ── Inisialisasi tabel assignment ─────────────────────────────────────────
  assignment <- data.frame(
    variable  = var_names,
    component = NA_integer_,
    loading   = NA_real_,
    direction = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  unassigned_vars <- character(0)
  
  # ── Proses assignment per variabel ────────────────────────────────────────
  for (i in seq_len(n_vars)) {
    v       <- var_names[i]
    k       <- best_component[i]
    loading <- loadings_mat[v, k]
    
    if (abs(loading) >= loading_threshold) {
      
      # Tentukan direction sesuai metode
      direction <- switch(direction_method,
                          # Theory-based: user tentukan mana variabel negatif (protektif)
                          "theory" = ifelse(v %in% negative_vars, -1L, 1L),
                          
                          # Loading sign: ikuti tanda loading empiris
                          "loading" = {
                            if (v %in% negative_vars) ifelse(loading > 0, -1L, 1L)
                            else                      ifelse(loading > 0,  1L, -1L)
                          },
                          
                          # Cutter's method: ikuti direction komponen (sudah dihitung di atas)
                          "cutter" = comp_directions[k]
      )
      
      assignment[i, "component"] <- k
      assignment[i, "loading"]   <- loading
      assignment[i, "direction"] <- direction
      
    } else {
      # Loading di bawah threshold: variabel tidak digunakan
      unassigned_vars <- c(unassigned_vars, v)
    }
  }
  
  return(list(
    assignment       = assignment,
    unassigned_vars  = unassigned_vars,
    loadings_mat     = loadings_mat,
    comp_names       = comp_names,
    comp_directions  = comp_directions,
    direction_method = direction_method
  ))
}


# =============================================================================
# FASE 5 — AGREGASI SKOR SoVI
# =============================================================================

#' Hitung skor SoVI dengan bobot proporsional loading per komponen
#'
#' @param std_out       Output dari standardize_data()
#' @param selection_out Output dari select_variables_per_component()
#' @param pca_out       Output dari run_pca()
#'
#' @return data.frame berisi:
#'   DISTRICTCODE, KABUPATEN, sovi_raw, sovi_score [0-1], RC1, RC2, ...
#'
#' @details
#' Rumus skor komponen k:
#'   RC_k = Σ (w_ik × d_i × z_i)
#'   w_ik = |λ_ik| / Σ|λ_jk|  (bobot proporsional loading absolut)
#'   d_i  = direction variabel (+1 atau -1)
#'   z_i  = Z-score variabel i
#'
#' SoVI raw = Σ RC_k
#' SoVI score = (raw - min) / (max - min)  → [0, 1]
compute_sovi <- function(std_out, selection_out, pca_out) {
  
  Z          <- std_out$Z
  assignment <- selection_out$assignment
  comp_names <- selection_out$comp_names
  n_units    <- nrow(Z)
  n_factors  <- length(comp_names)
  
  # ── Hitung skor setiap komponen ───────────────────────────────────────────
  comp_scores <- matrix(
    0,
    nrow     = n_units,
    ncol     = n_factors,
    dimnames = list(NULL, comp_names)
  )
  
  for (k in seq_len(n_factors)) {
    
    # Ambil variabel yang diassign ke komponen k
    vars_k <- assignment[
      !is.na(assignment$component) & assignment$component == k,
    ]
    if (nrow(vars_k) == 0) next
    
    # Bobot proporsional: w = |loading| / Σ|loading|
    abs_loadings <- abs(vars_k$loading)
    weights      <- abs_loadings / sum(abs_loadings)
    
    # Akumulasi skor komponen: Σ (w × direction × Z-score)
    for (j in seq_len(nrow(vars_k))) {
      v   <- vars_k$variable[j]
      dir <- vars_k$direction[j]
      w   <- weights[j]
      comp_scores[, k] <- comp_scores[, k] + (Z[[v]] * dir * w)
    }
  }
  
  # ── Agregasi & normalisasi ────────────────────────────────────────────────
  sovi_raw  <- rowSums(comp_scores)
  sovi_min  <- min(sovi_raw, na.rm = TRUE)
  sovi_max  <- max(sovi_raw, na.rm = TRUE)
  sovi_norm <- (sovi_raw - sovi_min) / (sovi_max - sovi_min)
  
  # ── Susun hasil ───────────────────────────────────────────────────────────
  result <- data.frame(
    DISTRICTCODE = std_out$unit_ids,
    KABUPATEN    = std_out$kabupaten,
    sovi_raw     = sovi_raw,
    sovi_score   = sovi_norm,
    stringsAsFactors = FALSE
  )
  
  # Tambahkan kolom skor RC per komponen
  result <- cbind(result, as.data.frame(comp_scores))
  
  return(result)
}


# =============================================================================
# FASE 6 — KLASIFIKASI NATURAL BREAKS (JENKS)
# =============================================================================

#' Klasifikasi SoVI score ke dalam kelas kerentanan dengan Jenks Natural Breaks
#'
#' @param sovi_df   data.frame output compute_sovi() (harus ada kolom sovi_score)
#' @param n_classes Jumlah kelas (default 5)
#'
#' @return List berisi:
#'   \item{sovi_df}  data.frame dengan tambahan kolom vuln_class (ordered factor)
#'   \item{breaks}   Vektor break points yang dibulatkan 4 desimal
classify_sovi <- function(sovi_df, n_classes = 5) {
  
  labels <- VULN_CLASSES[seq_len(n_classes)]
  
  # Hitung Natural Breaks (Jenks)
  brks <- classInt::classIntervals(
    var   = sovi_df$sovi_score,
    n     = n_classes,
    style = "jenks"
  )
  
  # Assign kelas ke setiap unit
  sovi_df$vuln_class <- cut(
    sovi_df$sovi_score,
    breaks         = brks$brks,
    labels         = labels,
    include.lowest = TRUE
  )
  
  # Jadikan ordered factor agar sorting/plotting otomatis benar
  sovi_df$vuln_class <- factor(
    sovi_df$vuln_class,
    levels  = labels,
    ordered = TRUE
  )
  
  return(list(
    sovi_df = sovi_df,
    breaks  = round(brks$brks, 4)
  ))
}


# =============================================================================
# FUNGSI UTAMA: run_sovi_core()
# Wrapper yang menjalankan seluruh pipeline Fase 2 → Fase 6
# =============================================================================

#' Jalankan pipeline SoVI lengkap dari data mentah ke klasifikasi
#'
#' @param data              data.frame dataset (hasil upload user)
#' @param sovi_vars         Vektor nama variabel SoVI yang dipilih
#' @param neg_vars          Vektor variabel protektif/negatif (default kosong)
#' @param direction_method  "theory" | "loading" | "cutter"
#' @param loading_threshold Ambang |loading| minimum (default 0.5)
#' @param comm_threshold    Ambang communality (info saja, default 0.4)
#' @param pca_rotation      Rotasi PCA (default "varimax")
#' @param id_col            Nama kolom ID wilayah
#' @param name_col          Nama kolom nama wilayah
#' @param progress_fn       Callback function(pct, msg) untuk status progress
#'
#' @return List berisi:
#'   \item{sovi_df}       Hasil akhir dengan sovi_score dan vuln_class
#'   \item{pca_out}       Output run_pca()
#'   \item{std_out}       Output standardize_data()
#'   \item{selection_out} Output select_variables_per_component()
#'   \item{jenks_breaks}  Break points klasifikasi Jenks
run_sovi_core <- function(data,
                          sovi_vars,
                          neg_vars          = character(0),
                          direction_method  = "theory",
                          loading_threshold = 0.5,
                          comm_threshold    = 0.4,
                          pca_rotation      = "varimax",
                          id_col            = "DISTRICTCODE",
                          name_col          = "KABUPATEN",
                          progress_fn       = NULL) {

  # Helper: panggil progress_fn hanya jika tersedia
  # progress_fn(pct_absolut, pesan) — pct dalam skala 0..1
  .rp <- function(pct, msg = "") {
    if (is.function(progress_fn)) progress_fn(pct, msg)
  }

  # ── Fase 2: Z-score standardisasi ─────────────────────────────────────────
  .rp(0.05, "Phase 2: Z-score standardization...")
  std_out <- standardize_data(data, sovi_vars, id_col, name_col)

  # ── Fase 3: PCA + diagnostik ──────────────────────────────────────────────
  .rp(0.20, "Phase 3a: Computing correlation matrix & KMO...")
  # (KMO & Bartlett terjadi di dalam run_pca)
  .rp(0.35, "Phase 3b: Running PCA + varimax rotation...")
  pca_out <- run_pca(
    std_out$Z,
    rotation       = pca_rotation,
    min_eigenvalue = 1,
    comm_threshold = comm_threshold
  )
  
  # ── Fase 4: Seleksi variabel per komponen ─────────────────────────────────
  selection_out <- select_variables_per_component(
    pca_out,
    negative_vars     = neg_vars,
    loading_threshold = loading_threshold,
    direction_method  = direction_method
  )
  
  # ── Fase 5: Agregasi skor SoVI ────────────────────────────────────────────
  sovi_df <- compute_sovi(std_out, selection_out, pca_out)
  
  # ── Fase 6: Klasifikasi Jenks ─────────────────────────────────────────────
  classified   <- classify_sovi(sovi_df, n_classes = 5)
  sovi_df      <- classified$sovi_df
  jenks_breaks <- classified$breaks
  
  return(list(
    sovi_df       = sovi_df,
    pca_out       = pca_out,
    std_out       = std_out,
    selection_out = selection_out,
    jenks_breaks  = jenks_breaks
  ))
}

message("[sovi_core.R] Pipeline SoVI (Fase 2-6) dimuat.")