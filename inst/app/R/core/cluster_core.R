# =============================================================================
# R/core/cluster_core.R
# Fungsi inti untuk Cluster Analysis berbasis ClustGeo
#
# BERISI:
#   - build_feature_matrix()     — bangun matriks fitur dari berbagai sumber
#   - find_optimal_k()           — cari k optimal via silhouette
#   - find_optimal_alpha()       — cari alpha optimal via pseudo-inertia
#   - run_clustgeo()             — ClustGeo versi sederhana (Extended Analysis)
#   - run_clustgeo_advanced()    — ClustGeo Advanced (menu Cluster Analysis)
#   - build_leaflet_clustgeo_adv() — Leaflet untuk hasil ClustGeo Advanced
#
# DEPENDENSI:
#   - R/core/helpers.R  (normalize_id, normalize_01 harus sudah di-source)
#   - Package: ClustGeo, cluster, dplyr, sf, leaflet, RColorBrewer
# =============================================================================


# =============================================================================
# HELPER: BANGUN MATRIKS FITUR
# Mendukung 5 sumber data: raw, raw_norm, standardized, sovi, rc
# Dipakai oleh ClustGeo, FGWC, dan LFGWC
# =============================================================================

#' Bangun matriks fitur sesuai sumber data yang dipilih user
#'
#' @param data_source   "raw" | "raw_norm" | "standardized" | "sovi" | "rc"
#' @param raw_data      data.frame asli dari upload user (rv$data)
#' @param sovi_result   Hasil run_sovi_core() (rv$sovi_result), bisa NULL
#' @param selected_vars Vektor nama variabel (untuk raw/raw_norm/standardized)
#'
#' @return data.frame matriks fitur siap pakai
build_feature_matrix <- function(data_source,
                                 raw_data,
                                 sovi_result,
                                 selected_vars) {
  
  if (data_source == "raw") {
    # ── Data mentah tanpa transformasi ─────────────────────────────────────
    mat <- as.data.frame(raw_data[, selected_vars, drop = FALSE])
    mat <- as.data.frame(lapply(mat, as.numeric))
    
  } else if (data_source == "raw_norm") {
    # ── Data mentah dengan normalisasi min-max [0, 1] ──────────────────────
    mat <- as.data.frame(raw_data[, selected_vars, drop = FALSE])
    mat <- as.data.frame(lapply(mat, as.numeric))
    mat <- as.data.frame(lapply(mat, normalize_01))
    
  } else if (data_source == "standardized") {
    # ── Data Z-score dari proses standardisasi SoVI ────────────────────────
    if (is.null(sovi_result))
      stop("Jalankan SoVI Computation terlebih dahulu.")
    Z        <- sovi_result$std_out$Z
    use_vars <- intersect(selected_vars, names(Z))
    if (length(use_vars) == 0) use_vars <- names(Z)
    mat <- as.data.frame(Z[, use_vars, drop = FALSE])
    
  } else if (data_source == "sovi") {
    # ── SoVI Score tunggal [0, 1] ──────────────────────────────────────────
    if (is.null(sovi_result))
      stop("Jalankan SoVI Computation terlebih dahulu.")
    mat <- data.frame(sovi_score = sovi_result$sovi_df$sovi_score)
    
  } else if (data_source == "rc") {
    # ── Skor komponen RC dari PCA (ternormalisasi 0-1) ─────────────────────
    if (is.null(sovi_result))
      stop("Jalankan SoVI Computation terlebih dahulu.")
    rc_cols <- grep("^RC", names(sovi_result$sovi_df), value = TRUE)
    if (length(rc_cols) == 0)
      stop("Tidak ada kolom RC. Jalankan SoVI Computation terlebih dahulu.")
    mat <- as.data.frame(
      lapply(sovi_result$sovi_df[, rc_cols, drop = FALSE], normalize_01)
    )
    
  } else {
    stop(paste("data_source tidak dikenal:", data_source))
  }
  
  return(mat)
}


# =============================================================================
# HELPER: CARI K OPTIMAL VIA MEAN SILHOUETTE
# =============================================================================

#' Cari jumlah cluster optimal berdasarkan mean silhouette width
#'
#' @param D0      Matriks dissimilarity atribut (dist object)
#' @param D1      Matriks dissimilarity spasial (dist object)
#' @param alpha   Nilai alpha ClustGeo yang digunakan
#' @param k_min   k minimum yang dicoba (default 2)
#' @param k_max   k maksimum yang dicoba (default 8)
#'
#' @return List berisi:
#'   \item{k_opt}  k dengan mean silhouette tertinggi
#'   \item{sil_df} data.frame (k, mean_silhouette)
find_optimal_k <- function(D0, D1, alpha, k_min = 2, k_max = 8) {
  
  # Bangun satu dendrogram dengan alpha yang diberikan
  tree    <- ClustGeo::hclustgeo(D0, D1, alpha = alpha)
  sil_vec <- numeric(k_max - k_min + 1)
  
  # Hitung mean silhouette untuk setiap k
  for (i in seq_along(sil_vec)) {
    k_try      <- k_min + i - 1
    C_try      <- cutree(tree, k = k_try)
    sil_try    <- cluster::silhouette(C_try, D0)
    sil_vec[i] <- mean(sil_try[, 3])
  }
  
  # k optimal = k dengan silhouette tertinggi
  k_opt <- k_min + which.max(sil_vec) - 1
  
  return(list(
    k_opt  = k_opt,
    sil_df = data.frame(
      k               = k_min:k_max,
      mean_silhouette = round(sil_vec, 4)
    )
  ))
}


# =============================================================================
# HELPER: CARI ALPHA OPTIMAL VIA PSEUDO-INERTIA
# Menggantikan ClustGeo::chooseAlpha() yang tidak tersedia semua versi.
# =============================================================================

#' Cari alpha optimal via trade-off homogenitas atribut vs spasial
#'
#' @param D0          Matriks dissimilarity atribut
#' @param D1          Matriks dissimilarity spasial
#' @param k           Jumlah cluster yang digunakan
#' @param range_alpha Vektor nilai alpha yang dicoba (default seq(0, 1, 0.1))
#'
#' @return List berisi:
#'   \item{alpha_opt} Alpha optimal (|Q1 - Q2| minimum)
#'   \item{q_df}      data.frame (alpha, Q1_atribut, Q2_spasial, diff_abs)
#'
#' @details
#' Q = 1 - (within_inertia / total_inertia), nilai mendekati 1 = homogen.
#' Alpha optimal = titik di mana |Q1 - Q2| minimum (trade-off terbaik).
find_optimal_alpha <- function(D0, D1, k,
                               range_alpha = seq(0, 1, 0.1)) {
  
  # ── Fungsi hitung normalized within-cluster inertia ───────────────────────
  calc_Q <- function(D, C) {
    D_mat          <- as.matrix(D)
    k_vals         <- unique(C)
    total_inertia  <- sum(D_mat^2) / (2 * nrow(D_mat))
    within_inertia <- 0
    for (cl in k_vals) {
      idx <- which(C == cl)
      if (length(idx) < 2) next
      sub            <- D_mat[idx, idx, drop = FALSE]
      within_inertia <- within_inertia + sum(sub^2) / (2 * length(idx))
    }
    Q <- 1 - within_inertia / max(total_inertia, 1e-10)
    return(max(0, min(1, Q)))
  }
  
  n_alpha <- length(range_alpha)
  Q1_vec  <- numeric(n_alpha)   # homogenitas atribut
  Q2_vec  <- numeric(n_alpha)   # homogenitas spasial
  
  for (i in seq_along(range_alpha)) {
    a         <- range_alpha[i]
    tree      <- ClustGeo::hclustgeo(D0, D1, alpha = a)
    C         <- cutree(tree, k = k)
    Q1_vec[i] <- calc_Q(D0, C)
    Q2_vec[i] <- calc_Q(D1, C)
  }
  
  q_df <- data.frame(
    alpha      = range_alpha,
    Q1_atribut = round(Q1_vec, 4),
    Q2_spasial = round(Q2_vec, 4),
    diff_abs   = round(abs(Q1_vec - Q2_vec), 4)
  )
  
  # Alpha optimal: |Q1 - Q2| minimum
  alpha_opt <- q_df$alpha[which.min(q_df$diff_abs)]
  
  return(list(
    alpha_opt = alpha_opt,
    q_df      = q_df
  ))
}


# =============================================================================
# ClustGeo SEDERHANA
# Dipakai oleh menu Extended Analysis (tab ClustGeo lama)
# =============================================================================

#' Jalankan ClustGeo sederhana berbasis RC scores
#'
#' @param sovi_df   data.frame hasil SoVI (harus ada kolom RC*)
#' @param shp       Shapefile sf object
#' @param rc_cols   Vektor nama kolom RC
#' @param join_shp  Nama kolom ID di shapefile
#' @param join_df   Nama kolom ID di sovi_df
#' @param k         Jumlah cluster
#' @param alpha     Bobot spasial [0, 1]
#'
#' @return List berisi sovi_df+cluster, profile, sil_mean, k, alpha
run_clustgeo <- function(sovi_df, shp, rc_cols,
                         join_shp, join_df,
                         k = 4, alpha = 0.2) {
  
  # ── D0: dissimilarity atribut (RC scores ternormalisasi) ──────────────────
  rc_norm <- as.data.frame(lapply(sovi_df[, rc_cols, drop = FALSE], normalize_01))
  D0      <- dist(rc_norm, method = "euclidean")
  
  # ── D1: dissimilarity spasial (centroid UTM Zone 49S) ─────────────────────
  sf::sf_use_s2(FALSE)
  shp_proj  <- sf::st_transform(shp, crs = 32749)
  centroids <- sf::st_centroid(sf::st_geometry(shp_proj))
  sf::sf_use_s2(TRUE)
  coords <- sf::st_coordinates(centroids)
  D1     <- dist(coords, method = "euclidean")
  
  # ── Clustering ────────────────────────────────────────────────────────────
  tree            <- ClustGeo::hclustgeo(D0, D1, alpha = alpha)
  C               <- cutree(tree, k = k)
  sovi_df$cluster <- as.factor(C)
  
  # ── Silhouette ────────────────────────────────────────────────────────────
  sil      <- cluster::silhouette(C, D0)
  sil_mean <- round(mean(sil[, 3]), 3)
  
  # ── Profil cluster ────────────────────────────────────────────────────────
  rc_norm$cluster    <- sovi_df$cluster
  rc_norm$sovi_score <- sovi_df$sovi_score
  
  profile <- rc_norm |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      n         = dplyr::n(),
      dplyr::across(dplyr::all_of(rc_cols),
                    \(x) round(mean(x, na.rm = TRUE), 3)),
      mean_sovi = round(mean(sovi_score, na.rm = TRUE), 3),
      .groups   = "drop"
    )
  
  return(list(
    sovi_df  = sovi_df,
    profile  = profile,
    sil_mean = sil_mean,
    k        = k,
    alpha    = alpha
  ))
}


# =============================================================================
# ClustGeo ADVANCED
# Dipakai oleh menu Cluster Analysis > ClustGeo
# Lebih fleksibel: pilihan sumber data, k otomatis, alpha otomatis
# =============================================================================

#' Jalankan ClustGeo Advanced dengan opsi lengkap
#'
#' @param data_source   Sumber data fitur (lihat build_feature_matrix)
#' @param raw_data      data.frame asli
#' @param sovi_result   Hasil run_sovi_core()
#' @param shp           Shapefile sf object
#' @param selected_vars Variabel yang dipilih (untuk raw/raw_norm/standardized)
#' @param k_mode        "manual" | "auto" (auto = cari via silhouette)
#' @param k             Jumlah cluster jika k_mode = "manual"
#' @param k_max         k maksimum jika k_mode = "auto"
#' @param alpha_mode    "manual" | "auto" (auto = cari via pseudo-inertia)
#' @param alpha         Nilai alpha jika alpha_mode = "manual"
#' @param id_col        Nama kolom ID wilayah
#' @param name_col      Nama kolom nama wilayah
#'
#' @return List lengkap hasil clustering
run_clustgeo_advanced <- function(data_source,
                                  raw_data,
                                  sovi_result,
                                  shp,
                                  selected_vars = NULL,
                                  k_mode        = "manual",
                                  k             = 4,
                                  k_max         = 8,
                                  alpha_mode    = "manual",
                                  alpha         = 0.2,
                                  id_col        = "DISTRICTCODE",
                                  name_col      = "KABUPATEN") {
  
  # ── Langkah 1: Bangun matriks fitur ───────────────────────────────────────
  feat_mat <- build_feature_matrix(data_source, raw_data, sovi_result,
                                   selected_vars)
  
  # ── Langkah 2: Dissimilarity atribut ─────────────────────────────────────
  D0 <- dist(feat_mat, method = "euclidean")
  
  # ── Langkah 3: Dissimilarity spasial (centroid UTM) ──────────────────────
  sf::sf_use_s2(FALSE)
  shp_proj  <- sf::st_transform(shp, crs = 32749)
  centroids <- sf::st_centroid(sf::st_geometry(shp_proj))
  sf::sf_use_s2(TRUE)
  coords <- sf::st_coordinates(centroids)
  D1     <- dist(coords, method = "euclidean")
  
  # ── Langkah 4: Tentukan alpha ─────────────────────────────────────────────
  alpha_info <- NULL
  if (alpha_mode == "auto") {
    alpha_info <- find_optimal_alpha(
      D0, D1,
      k = if (k_mode == "manual") k else 4
    )
    alpha <- alpha_info$alpha_opt
  }
  
  # ── Langkah 5: Tentukan k ─────────────────────────────────────────────────
  k_info <- NULL
  if (k_mode == "auto") {
    k_info <- find_optimal_k(D0, D1, alpha = alpha, k_min = 2, k_max = k_max)
    k      <- k_info$k_opt
  }
  
  # ── Langkah 6: Clustering final ───────────────────────────────────────────
  tree <- ClustGeo::hclustgeo(D0, D1, alpha = alpha)
  C    <- cutree(tree, k = k)
  
  # ── Langkah 7: Silhouette ─────────────────────────────────────────────────
  sil      <- cluster::silhouette(C, D0)
  sil_mean <- round(mean(sil[, 3]), 3)
  sil_df   <- as.data.frame(summary(sil)$clus.avg.widths)
  colnames(sil_df) <- "avg_sil_width"
  sil_df$cluster   <- factor(seq_len(nrow(sil_df)))
  
  # ── Langkah 8: Susun result_df ────────────────────────────────────────────
  if (data_source == "raw") {
    result_df <- data.frame(
      id_col   = raw_data[[id_col]],
      name_col = raw_data[[name_col]],
      cluster  = as.factor(C),
      stringsAsFactors = FALSE
    )
    names(result_df)[1:2] <- c(id_col, name_col)
    result_df <- cbind(result_df, feat_mat)
  } else {
    result_df          <- sovi_result$sovi_df
    result_df$cluster  <- as.factor(C)
  }
  
  # ── Langkah 9: Profil cluster ─────────────────────────────────────────────
  feat_cols          <- names(feat_mat)
  feat_mat2          <- feat_mat
  feat_mat2$cluster  <- as.factor(C)
  
  profile <- feat_mat2 |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(dplyr::all_of(feat_cols),
                    \(x) round(mean(x, na.rm = TRUE), 3)),
      .groups = "drop"
    )
  
  # Tambahkan mean SoVI ke profil jika tersedia
  if (!is.null(sovi_result) && "sovi_score" %in% names(result_df)) {
    sovi_means <- result_df |>
      dplyr::group_by(cluster) |>
      dplyr::summarise(
        mean_sovi = round(mean(sovi_score, na.rm = TRUE), 3),
        .groups   = "drop"
      )
    profile <- dplyr::left_join(profile, sovi_means, by = "cluster")
  }
  
  # ── Langkah 10: Data dendrogram ───────────────────────────────────────────
  dend_data <- list(tree = tree, k = k, alpha = alpha)
  
  return(list(
    result_df   = result_df,
    feat_mat    = feat_mat,
    profile     = profile,
    sil_obj     = sil,
    sil_df      = sil_df,
    sil_mean    = sil_mean,
    k           = k,
    alpha       = alpha,
    alpha_mode  = alpha_mode,
    k_mode      = k_mode,
    k_info      = k_info,       # NULL jika manual
    alpha_info  = alpha_info,   # NULL jika manual
    feat_cols   = feat_cols,
    data_source = data_source,
    dend_data   = dend_data,
    D0          = D0,
    id_col      = id_col,
    name_col    = name_col
  ))
}


# =============================================================================
# LEAFLET BUILDER — ClustGeo Advanced
# =============================================================================

#' Bangun peta Leaflet choropleth untuk hasil ClustGeo Advanced
#'
#' @param result_df data.frame hasil run_clustgeo_advanced()
#' @param shp       Shapefile sf object
#' @param join_shp  Nama kolom ID di shapefile
#' @param join_df   Nama kolom ID di result_df
#' @param name_col  Nama kolom nama wilayah
#' @param k         Jumlah cluster
#'
#' @return Objek leaflet map
build_leaflet_clustgeo_adv <- function(result_df, shp, join_shp, join_df,
                                       name_col = "KABUPATEN", k = 4) {
  
  # Normalisasi ID agar join tidak case-sensitive
  result_df[[join_df]] <- normalize_id(result_df[[join_df]])
  shp[[join_shp]]      <- normalize_id(shp[[join_shp]])
  
  # Join data cluster ke shapefile
  peta <- dplyr::left_join(shp, result_df,
                           by = setNames(join_df, join_shp))
  
  # Palet warna cluster
  n_col     <- min(max(k, 3), 8)
  pal_c     <- RColorBrewer::brewer.pal(n_col, "Set2")[seq_len(k)]
  pal_clust <- leaflet::colorFactor(
    palette  = pal_c,
    domain   = as.character(seq_len(k)),
    na.color = "#D3D3D3"
  )
  
  # Popup teks
  has_sovi   <- "sovi_score" %in% names(peta)
  popup_text <- paste0(
    "<b>", peta[[name_col]], "</b><br>",
    "Cluster: ", peta$cluster,
    if (has_sovi) paste0("<br>SoVI Score: ", round(peta$sovi_score, 4)) else ""
  )
  
  leaflet::leaflet(peta) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      fillColor        = ~pal_clust(as.character(cluster)),
      fillOpacity      = 0.75,
      color            = "#555555",
      weight           = 0.5,
      popup            = popup_text,
      label            = ~get(name_col),
      highlightOptions = leaflet::highlightOptions(
        weight = 2, color = "#333333",
        fillOpacity = 0.9, bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal      = pal_clust,
      values   = as.character(seq_len(k)),
      title    = "Cluster",
      opacity  = 0.9
    )
}

message("[cluster_core.R] Fungsi ClustGeo dimuat.")