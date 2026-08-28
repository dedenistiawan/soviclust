# =============================================================================
# R/FGWC/fgwc_wrapper.R
# Helper functions dan fungsi utama FGWC untuk Shiny SoVI App
#
# BERISI:
#   - Operator %||%               — null-coalescing
#   - read_uploaded_file()        — baca Excel/CSV dari upload Shiny
#   - parse_distance_matrix()     — konversi df ke matriks jarak n×n
#   - parse_population()          — ekstrak vektor populasi dari df
#   - haversine_matrix()          — hitung jarak Haversine dari lon/lat
#   - build_fgwc_feature_matrix() — bangun matriks fitur dari berbagai sumber
#   - build_opt_param()           — bangun parameter algoritma optimasi
#   - run_fgwc_shiny()            — fungsi utama menjalankan FGWC
#   - build_leaflet_fgwc()        — leaflet map hasil FGWC
#
# CATATAN:
#   File algoritma (fgwc.R, abcfgwc.R, dll) sudah di-source dari
#   R/shared/Function/ oleh global.R — tidak perlu di-source lagi di sini.
#
# DEPENDENSI:
#   - R/core/helpers.R  (normalize_id, normalize_01)
#   - R/shared/Function/ (semua algoritma FGWC)
#   - Package: dplyr, cluster, leaflet, RColorBrewer, readxl
# =============================================================================


# =============================================================================
# OPERATOR: Null-coalescing
# Mengembalikan x jika tidak NULL/NA, selain itu mengembalikan y
# Dipakai di build_opt_param() dan run_fgwc_shiny()
# =============================================================================
`%||%` <- function(x, y) if (!is.null(x) && !is.na(x)) x else y


# =============================================================================
# HELPER: Baca file upload (Excel/CSV)
# Dipakai di fgwc_server.R dan lfgwc_server.R
# =============================================================================

#' Baca file upload Shiny (Excel atau CSV) menjadi data.frame
#'
#' @param file_info List dari input$file (name, datapath)
#' @return data.frame
read_uploaded_file <- function(file_info) {
  ext <- tolower(tools::file_ext(file_info$name))
  if (ext == "xlsx") {
    as.data.frame(readxl::read_excel(file_info$datapath))
  } else {
    read.csv(file_info$datapath, stringsAsFactors = FALSE)
  }
}


# =============================================================================
# HELPER: Konversi data.frame ke matriks jarak n×n
# =============================================================================

#' Parse data.frame menjadi matriks jarak numerik n×n
#'
#' @param df data.frame matriks jarak (kolom label pertama diabaikan otomatis)
#' @return matrix numerik n×n
parse_distance_matrix <- function(df) {
  
  # Buang kolom label jika kolom pertama bukan numerik
  if (!is.numeric(df[, 1])) {
    rownames(df) <- df[, 1]
    df           <- df[, -1]
  }
  
  mat <- data.matrix(df)
  
  if (nrow(mat) != ncol(mat))
    stop(paste("Matriks jarak tidak persegi:", nrow(mat), "x", ncol(mat)))
  
  return(mat)
}


# =============================================================================
# HELPER: Ekstrak vektor populasi dari data.frame
# =============================================================================

#' Ambil vektor populasi dari kolom numerik pertama data.frame
#'
#' @param df data.frame dengan minimal 1 kolom numerik
#' @return Vektor numerik populasi
parse_population <- function(df) {
  num_cols <- sapply(df, is.numeric)
  if (!any(num_cols))
    stop("Tidak ada kolom numerik di file populasi.")
  as.numeric(unlist(df[, which(num_cols)[1], drop = FALSE]))
}


# =============================================================================
# HELPER: Hitung Haversine Distance Matrix dari lon/lat
# Satuan hasil: kilometer
# =============================================================================

#' Hitung matriks jarak Haversine antar koordinat lon/lat
#'
#' @param lon Vektor longitude (desimal)
#' @param lat Vektor latitude (desimal)
#' @return Matrix jarak (km) n×n
haversine_matrix <- function(lon, lat) {
  n   <- length(lon)
  mat <- matrix(0, nrow = n, ncol = n)
  R   <- 6371  # Radius bumi (km)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      
      dlat      <- (lat[j] - lat[i]) * pi / 180
      dlon      <- (lon[j] - lon[i]) * pi / 180
      a         <- sin(dlat / 2)^2 +
        cos(lat[i] * pi / 180) * cos(lat[j] * pi / 180) * sin(dlon / 2)^2
      mat[i, j] <- R * 2 * asin(sqrt(a))
    }
  }
  
  return(mat)
}


# =============================================================================
# HELPER: Bangun matriks fitur dari sumber data yang dipilih
# Versi FGWC — sama dengan build_feature_matrix di cluster_core.R
# tetapi dipertahankan agar FGWC tidak bergantung pada cluster_core
# =============================================================================

#' Bangun matriks fitur untuk FGWC dari berbagai sumber data
#'
#' @param data_source   "raw"|"raw_norm"|"standardized"|"sovi"|"rc"
#' @param raw_data      data.frame asli dari upload
#' @param sovi_result   Hasil run_sovi_core() (bisa NULL)
#' @param selected_vars Vektor nama variabel yang dipilih
#' @return data.frame matriks fitur
build_fgwc_feature_matrix <- function(data_source, raw_data,
                                      sovi_result, selected_vars) {
  
  if (data_source == "raw") {
    # Data mentah tanpa transformasi
    mat <- as.data.frame(raw_data[, selected_vars, drop = FALSE])
    return(as.data.frame(lapply(mat, as.numeric)))
    
  } else if (data_source == "raw_norm") {
    # Data mentah dengan normalisasi min-max [0, 1]
    mat <- as.data.frame(raw_data[, selected_vars, drop = FALSE])
    mat <- as.data.frame(lapply(mat, as.numeric))
    return(as.data.frame(lapply(mat, normalize_01)))
    
  } else if (data_source == "standardized") {
    # Z-score dari proses SoVI
    if (is.null(sovi_result))
      stop("Jalankan SoVI Computation terlebih dahulu.")
    Z        <- sovi_result$std_out$Z
    use_vars <- intersect(selected_vars, names(Z))
    if (length(use_vars) == 0) use_vars <- names(Z)
    return(as.data.frame(Z[, use_vars, drop = FALSE]))
    
  } else if (data_source == "sovi") {
    # SoVI Score tunggal
    if (is.null(sovi_result))
      stop("Jalankan SoVI Computation terlebih dahulu.")
    return(data.frame(sovi_score = sovi_result$sovi_df$sovi_score))
    
  } else if (data_source == "rc") {
    # Skor komponen RC dari PCA (ternormalisasi)
    if (is.null(sovi_result))
      stop("Jalankan SoVI Computation terlebih dahulu.")
    rc_cols <- grep("^RC", names(sovi_result$sovi_df), value = TRUE)
    if (length(rc_cols) == 0)
      stop("Tidak ada kolom RC. Jalankan SoVI Computation terlebih dahulu.")
    return(as.data.frame(
      lapply(sovi_result$sovi_df[, rc_cols, drop = FALSE], normalize_01)
    ))
    
  } else {
    stop(paste("data_source tidak dikenal:", data_source))
  }
}


# =============================================================================
# HELPER: Bangun opt_param berdasarkan algoritma dan input user
# =============================================================================

#' Bangun vektor parameter algoritma optimasi untuk fgwc()
#'
#' @param algorithm Nama algoritma: "classic"|"abc"|"fpa"|"gsa"|"gwo"|
#'                  "hho"|"ifa"|"pso"|"tlbo"|"woa"
#' @param opts      List parameter dari input Shiny
#' @return Named character vector parameter algoritma
build_opt_param <- function(algorithm, opts) {
  
  # Classic tidak perlu parameter optimasi
  if (algorithm == "classic") return(1)
  
  # Parameter universal (semua algoritma non-classic)
  base <- c(
    vi.dist = opts$vi_dist %||% "uniform",
    npar    = opts$npar    %||% 10,
    same    = opts$same    %||% 10
  )
  
  # Parameter spesifik per algoritma
  algo_extra <- switch(algorithm,
                       
                       "abc" = c(
                         pso        = as.character(opts$pso        %||% FALSE),
                         n.onlooker = opts$n_onlooker %||% 5,
                         limit      = opts$limit      %||% 5
                       ),
                       
                       "fpa" = c(
                         p        = opts$p        %||% 0.7,
                         gamma    = opts$gamma    %||% 1.2,
                         lambda   = opts$lambda   %||% 1.5,
                         ei.distr = opts$ei_distr %||% "logchaotic",
                         chaos    = opts$chaos    %||% 3
                       ),
                       
                       "gsa" = c(
                         G    = opts$G    %||% 1,
                         vmax = opts$vmax %||% 0.7,
                         new  = as.character(opts$new %||% FALSE)
                       ),
                       
                       # GWO hanya butuh parameter universal
                       "gwo" = c(),
                       
                       "hho" = c(
                         algo = opts$hho_algo %||% "bairathi",
                         a1   = opts$a1       %||% 3,
                         a2   = opts$a2       %||% 1,
                         a3   = opts$a3       %||% 0.4
                       ),
                       
                       "ifa" = c(
                         par.no      = opts$par_no      %||% 3,
                         par.dist    = opts$par_dist    %||% "minkowski",
                         par.order   = opts$par_order   %||% 4,
                         gamma       = opts$gamma       %||% 1,
                         beta        = opts$beta        %||% 1,
                         alpha       = opts$alpha_ifa   %||% 1,
                         ei.distr    = opts$ei_distr    %||% "logchaotic",
                         chaos       = opts$chaos       %||% 4,
                         update_type = opts$update_type %||% 4
                       ),
                       
                       "pso" = c(
                         vmax = opts$vmax %||% 0.8,
                         c1   = opts$c1   %||% 0.7,
                         c2   = opts$c2   %||% 0.6,
                         type = opts$type %||% "chaotic",
                         wmax = opts$wmax %||% 0.8,
                         wmin = opts$wmin %||% 0.3,
                         map  = opts$map  %||% 0.3
                       ),
                       
                       "tlbo" = c(
                         nselection = opts$nselection %||% 10,
                         elitism    = as.character(opts$elitism %||% FALSE),
                         n.elite    = opts$n_elite %||% 2
                       ),
                       
                       "woa" = c(
                         woa.b = opts$woa_b %||% 1
                       ),
                       
                       c()  # fallback kosong
  )
  
  return(c(base, algo_extra))
}


# =============================================================================
# FUNGSI UTAMA: run_fgwc_shiny()
# Entry point dari fgwc_server.R
# =============================================================================

#' Jalankan FGWC lengkap dari pemilihan data hingga profil cluster
#'
#' @param data_source   Sumber data fitur
#' @param raw_data      data.frame asli dari upload
#' @param sovi_result   Hasil run_sovi_core()
#' @param selected_vars Variabel yang dipilih
#' @param pop_vec       Vektor populasi (n)
#' @param dist_mat      Matriks jarak n×n
#' @param algorithm     Algoritma optimasi
#' @param ncluster      Jumlah cluster
#' @param fgwc_params   List parameter FGWC
#' @param opt_params    List parameter algoritma
#' @param id_col        Nama kolom ID wilayah
#' @param name_col      Nama kolom nama wilayah
#'
#' @return List hasil lengkap FGWC
run_fgwc_shiny <- function(data_source,
                           raw_data,
                           sovi_result,
                           selected_vars,
                           pop_vec,
                           dist_mat,
                           algorithm    = "classic",
                           ncluster     = 4,
                           fgwc_params  = list(),
                           opt_params   = list(),
                           id_col       = "DISTRICTCODE",
                           name_col     = "KABUPATEN") {
  
  # ── 1. Bangun matriks fitur ─────────────────────────────────────────────
  feat_df <- build_fgwc_feature_matrix(data_source, raw_data,
                                       sovi_result, selected_vars)
  n <- nrow(feat_df)
  
  # ── 2. Validasi dimensi ─────────────────────────────────────────────────
  if (length(pop_vec) != n)
    stop(sprintf("Panjang vektor populasi (%d) tidak sesuai jumlah unit (%d).",
                 length(pop_vec), n))
  if (nrow(dist_mat) != n || ncol(dist_mat) != n)
    stop(sprintf("Dimensi matriks jarak (%dx%d) tidak sesuai jumlah unit (%d).",
                 nrow(dist_mat), ncol(dist_mat), n))
  
  # ── 3. Susun parameter FGWC ─────────────────────────────────────────────
  param_fgwc <- c(
    kind     = "v",
    ncluster = ncluster,
    m        = fgwc_params$m        %||% 2,
    distance = fgwc_params$distance %||% "euclidean",
    order    = fgwc_params$order    %||% 3,
    alpha    = fgwc_params$alpha    %||% 0.5,
    a        = fgwc_params$a        %||% 1.2,
    b        = fgwc_params$b        %||% 1.2,
    max.iter = fgwc_params$max.iter %||% 500,
    error    = fgwc_params$error    %||% 1e-6,
    randomN  = fgwc_params$randomN  %||% 0
  )
  
  # ── 4. Susun parameter algoritma ────────────────────────────────────────
  opt_param <- build_opt_param(algorithm, opt_params)
  
  # ── 5. Jalankan FGWC ────────────────────────────────────────────────────
  result <- fgwc(
    data       = feat_df,
    pop        = pop_vec,
    distmat    = dist_mat,
    algorithm  = algorithm,
    fgwc_param = param_fgwc,
    opt_param  = opt_param
  )
  
  # ── 6. Susun result_df ──────────────────────────────────────────────────
  if (!is.null(sovi_result)) {
    result_df <- sovi_result$sovi_df
  } else {
    result_df <- data.frame(
      id   = raw_data[[id_col]],
      nama = raw_data[[name_col]],
      stringsAsFactors = FALSE
    )
    names(result_df)[1:2] <- c(id_col, name_col)
  }
  result_df$fgwc_cluster <- as.factor(result$cluster)
  
  # ── 7. Hitung silhouette ────────────────────────────────────────────────
  D0      <- dist(feat_df, method = "euclidean")
  sil_obj <- tryCatch(
    cluster::silhouette(result$cluster, D0),
    error = function(e) NULL
  )
  sil_mean <- if (!is.null(sil_obj)) round(mean(sil_obj[, 3]), 3) else NA
  
  # ── 8. Profil cluster ───────────────────────────────────────────────────
  feat_df2         <- feat_df
  feat_df2$cluster <- as.factor(result$cluster)
  feat_cols        <- names(feat_df)
  
  profile <- feat_df2 |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(dplyr::all_of(feat_cols),
                    \(x) round(mean(x, na.rm = TRUE), 3)),
      .groups = "drop"
    )
  
  # Tambahkan mean SoVI jika tersedia
  if ("sovi_score" %in% names(result_df)) {
    sovi_means <- result_df |>
      dplyr::mutate(cluster = as.factor(fgwc_cluster)) |>
      dplyr::group_by(cluster) |>
      dplyr::summarise(
        mean_sovi = round(mean(sovi_score, na.rm = TRUE), 3),
        .groups   = "drop"
      )
    profile <- dplyr::left_join(profile, sovi_means, by = "cluster")
  }
  
  # ── 9. Tabel indeks validasi ────────────────────────────────────────────
  val_df <- data.frame(
    Indeks     = c("PC (max)", "CE (min)", "SC (min)",
                   "SI (min)", "XB (min)", "IFV (max)", "Kwon (min)"),
    Nilai      = round(unlist(result$validation), 6),
    Keterangan = c("Partition Coefficient",
                   "Classification Entropy",
                   "SC Index",
                   "Separation Index",
                   "Xie-Beni Index",
                   "IFV Index",
                   "Kwon Index"),
    stringsAsFactors = FALSE
  )
  
  return(list(
    result_df   = result_df,
    fgwc_raw    = result,
    profile     = profile,
    val_df      = val_df,
    sil_obj     = sil_obj,
    sil_mean    = sil_mean,
    feat_cols   = feat_cols,
    feat_df     = feat_df,
    k           = ncluster,
    algorithm   = algorithm,
    data_source = data_source,
    id_col      = id_col,
    name_col    = name_col,
    conv        = result$converg,
    f_obj       = result$f_obj,
    iteration   = result$iteration
  ))
}


# =============================================================================
# HELPER: Leaflet map untuk hasil FGWC
# =============================================================================

#' Bangun peta Leaflet choropleth untuk hasil FGWC
#'
#' @param result_df   data.frame hasil run_fgwc_shiny()
#' @param shp         Shapefile sf object
#' @param join_shp    Nama kolom ID di shapefile
#' @param join_df     Nama kolom ID di result_df
#' @param name_col    Nama kolom nama wilayah
#' @param k           Jumlah cluster
#' @param cluster_col Nama kolom cluster di result_df
#' @return Objek leaflet map
build_leaflet_fgwc <- function(result_df, shp, join_shp, join_df,
                               name_col    = "KABUPATEN",
                               k           = 4,
                               cluster_col = "fgwc_cluster") {
  
  result_df[[join_df]] <- normalize_id(result_df[[join_df]])
  shp[[join_shp]]      <- normalize_id(shp[[join_shp]])
  
  peta <- dplyr::left_join(shp, result_df,
                           by = setNames(join_df, join_shp))
  
  n_col     <- min(max(k, 3), 8)
  pal_c     <- RColorBrewer::brewer.pal(n_col, "Set1")[seq_len(k)]
  pal_clust <- leaflet::colorFactor(
    palette  = pal_c,
    domain   = as.character(seq_len(k)),
    na.color = "#D3D3D3"
  )
  
  has_sovi   <- "sovi_score" %in% names(peta)
  popup_text <- paste0(
    "<b>", peta[[name_col]], "</b><br>",
    "Cluster: ", peta[[cluster_col]],
    if (has_sovi) paste0("<br>SoVI Score: ",
                         round(peta$sovi_score, 4)) else ""
  )
  
  leaflet::leaflet(peta) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
    leaflet::addPolygons(
      fillColor        = ~pal_clust(as.character(get(cluster_col))),
      fillOpacity      = 0.75,
      color            = "#555555",
      weight           = 0.5,
      popup            = popup_text,
      label            = ~get(name_col),
      highlightOptions = leaflet::highlightOptions(
        weight      = 2,
        color       = "#333333",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal      = pal_clust,
      values   = as.character(seq_len(k)),
      title    = "FGWC Cluster",
      opacity  = 0.9
    )
}

message("[fgwc_wrapper.R] FGWC helper functions dimuat.")