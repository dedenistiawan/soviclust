# =============================================================================
# alfgwc_wrapper.R — Adaptive Local Fuzzy Geographically Weighted Clustering
# Implementasi berdasarkan: Unified Spatial Fuzzy Clustering Framework
# =============================================================================
#
# Mendukung:
# - Adaptive-LFGWC-QC (TN = QUEEN, TW = SPATIAL_INTERACTION, A = TRUE)
# - Adaptive-DLFGWC-QC (TN = QUEEN, TW = DISTANCE_DECAY, A = TRUE)
# =============================================================================

message("[alfgwc_wrapper] Loading ALFGWC...")

# =============================================================================
# FASE 0 — BANGUN SPATIAL WEIGHTS MATRIX W (Adaptive ALFGWC)
# =============================================================================

#' Bangun spatial weights matrix W untuk ALFGWC
#'
#' @param dist_mat  Matriks jarak antar unit spasial (n x n)
#' @param pop_vec   Vektor populasi (n x 1)
#' @param nb_list   Daftar ketetanggaan (misal dari spdep::poly2nb)
#' @param tw        Tipe pembobotan: "SPATIAL_INTERACTION" atau "DISTANCE_DECAY"
#' @param gamma     Eksponen jarak untuk DISTANCE_DECAY
#'
#' @return W_std    Spatial weights matrix row-standardized (n x n)
build_alfgwc_weights <- function(dist_mat, pop_vec, nb_list,
                                 tw = "SPATIAL_INTERACTION",
                                 gamma = 2) {
  n <- nrow(dist_mat)
  W_std <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    # Ni: daftar index tetangga untuk unit i
    Ni <- nb_list[[i]]
    
    # Jika unit tidak memiliki tetangga (pulau), W_std baris i tetap 0
    # Namun ini biasanya diatasi dengan KNN fallback di analysis_core
    if (length(Ni) == 1 && Ni[1] == 0) {
      next
    }
    
    if (tw == "SPATIAL_INTERACTION") {
      # φij <- (Pj/dij) / Σj∈Ni(Pj/dij)
      P_j <- pop_vec[Ni]
      d_ij <- dist_mat[i, Ni]
      
      # Hindari pembagian dengan 0 jika d_ij = 0
      d_ij[d_ij == 0] <- .Machine$double.eps
      
      phi_raw <- P_j / d_ij
      sum_phi <- sum(phi_raw)
      
      if (sum_phi > 0) {
        W_std[i, Ni] <- phi_raw / sum_phi
      }
      
    } else if (tw == "DISTANCE_DECAY") {
      # fij <- 1/(dij^γ)
      # wij <- fij / Σj∈Ni fij
      d_ij <- dist_mat[i, Ni]
      d_ij[d_ij == 0] <- .Machine$double.eps
      
      f_ij <- 1 / (d_ij^gamma)
      sum_f <- sum(f_ij)
      
      if (sum_f > 0) {
        W_std[i, Ni] <- f_ij / sum_f
      }
    }
  }
  
  return(W_std)
}

# =============================================================================
# FASE 1 — INISIALISASI CENTROID
# =============================================================================

#' Generate centroid awal untuk ALFGWC
alfgwc_init_centroid <- function(data, ncluster, randomN = 0) {
  d <- ncol(data)
  centroid <- matrix(0, nrow = ncluster, ncol = d)
  for (j in seq_len(d)) {
    set.seed(randomN + j)
    centroid[, j] <- runif(ncluster, min = min(data[, j]), max = max(data[, j]))
  }
  return(centroid)
}

# =============================================================================
# FASE 2 — HITUNG MEMBERSHIP MATRIX (FCM Standard)
# =============================================================================

#' Hitung membership matrix U dari centroid V
alfgwc_compute_membership <- function(data, centroid, m) {
  n <- nrow(data)
  c <- nrow(centroid)
  
  D <- matrix(0, nrow = n, ncol = c)
  for (i in seq_len(c)) {
    diff <- sweep(data, 2, centroid[i, ], "-")
    D[, i] <- sqrt(rowSums(diff^2))
  }
  
  D_safe <- D
  D_safe[D_safe == 0] <- .Machine$double.eps
  
  exp_val <- 2 / (m - 1)
  tmp <- D_safe^(-exp_val)
  U <- tmp / rowSums(tmp)
  
  return(list(U = U, D = D))
}

# =============================================================================
# FASE 3 — ADAPTIVE GEOGRAPHIC MODIFICATION
# =============================================================================

#' Modifikasi geografis lokal dengan mekanisme adaptif
#'
#' @param U       Membership matrix (n x c)
#' @param W_std   Spatial weights matrix (n x n)
#' @param alpha_v Vektor alpha untuk tiap unit i (berdasarkan Local Moran's I)
alfgwc_geographic_modify <- function(U, W_std, alpha_v) {
  # G_ik <- Σj∈Ni W_std[i,j] * U[j,k]
  G <- W_std %*% U  # (n x n) %*% (n x c) = (n x c)
  
  # Update membership
  # u_ik <- (1 - α_i)*u_ik + α_i * G_ik
  # Perhatikan bahwa vektor alpha_v panjangnya n
  U_new <- sweep(U, 1, 1 - alpha_v, "*") + sweep(G, 1, alpha_v, "*")
  
  # Normalisasi baris
  row_sums <- rowSums(U_new)
  row_sums[row_sums == 0] <- 1
  U_new <- U_new / row_sums
  
  return(U_new)
}

# =============================================================================
# FASE 4 — UPDATE CLUSTER CENTERS
# =============================================================================

#' Update cluster centers dari membership matrix
alfgwc_update_centers <- function(data, U, m) {
  data <- as.matrix(data)
  Um <- U^m
  num <- t(Um) %*% data
  den <- colSums(Um)
  den[den == 0] <- .Machine$double.eps
  V <- sweep(num, 1, den, "/")
  return(V)
}

# =============================================================================
# FASE 5 — OBJECTIVE FUNCTION & VALIDITY
# =============================================================================

alfgwc_objective <- function(U, D, m) {
  sum((U^m) * (D^2))
}

alfgwc_validity <- function(U, V, D, m) {
  # Implementasi validasi sama dengan LFGWC
  n <- nrow(U)
  c <- ncol(U)
  
  PC <- (1/n) * sum(U^2)
  
  U_safe <- U
  U_safe[U_safe <= 0] <- .Machine$double.eps
  CE <- -(1/n) * sum(U_safe * log(U_safe))
  
  ni <- colSums(U)
  si <- colSums((U^m) * (D^2))
  
  dist_vv <- as.matrix(dist(V))^2
  sep <- rowSums(dist_vv)
  sep[sep == 0] <- .Machine$double.eps
  SC <- sum((si / ni) / sep)
  
  diag(dist_vv) <- 0
  SD_max <- max(dist_vv)
  if (SD_max == 0) SD_max <- .Machine$double.eps
  
  sigma_D <- (1/c) * sum((1/n) * colSums(D^2))
  if (sigma_D == 0) sigma_D <- .Machine$double.eps
  
  part1 <- (1/n) * colSums(log2(U_safe))
  part2 <- (log2(c) - part1)^2
  part3 <- (1/n) * colSums(U_safe^2)
  part4 <- part3 * part2
  IFV <- (1/c) * sum(part4) * (SD_max / sigma_D)
  

  dist_vv_min <- dist_vv
  diag(dist_vv_min) <- Inf
  min_sep <- min(dist_vv_min)
  if (min_sep == 0) min_sep <- .Machine$double.eps
  
  XB <- sum(si) / (n * min_sep)
  
  v_mean <- colMeans(V)
  v_var <- sum(sweep(V, 2, v_mean, "-")^2)
  Kwon <- (sum(si) + (v_var / c)) / min_sep
  
  return(list(PC = round(PC, 6), CE = round(CE, 6), SC = round(SC, 6), IFV = round(IFV, 6), XB = round(XB, 6), Kwon = round(Kwon, 6)))

}

# =============================================================================
# FUNGSI INTI: alfgwc_classic()
# =============================================================================

#' ALFGWC Klasik
alfgwc_classic <- function(data, W_std, ncluster, alpha_v,
                           m = 2, max_iter = 100, error = 0.001, randomN = 0) {
  ptm <- proc.time()
  data <- as.matrix(data)
  
  V <- alfgwc_init_centroid(data, ncluster, randomN)
  md <- alfgwc_compute_membership(data, V, m)
  U <- md$U
  D <- md$D
  
  conv <- numeric(0)
  iter <- 0
  
  repeat {
    U <- alfgwc_geographic_modify(U, W_std, alpha_v)
    V <- alfgwc_update_centers(data, U, m)
    
    md <- alfgwc_compute_membership(data, V, m)
    U <- md$U
    D <- md$D
    
    J <- alfgwc_objective(U, D, m)
    conv <- c(conv, J)
    iter <- iter + 1
    
    if (iter > 1 && abs(conv[iter] - conv[iter - 1]) < error) break
    if (iter >= max_iter) break
  }
  
  cluster <- apply(U, 1, which.max)
  finaldata <- cbind(as.data.frame(data), cluster = cluster)
  validity <- alfgwc_validity(U, V, D, m)
  
  return(list(
    converg = conv, f_obj = J, membership = U, centroid = V,
    validation = validity, cluster = cluster, finaldata = finaldata,
    iteration = iter, time = proc.time() - ptm
  ))
}

# =============================================================================
# FUNGSI INTI: alfgwc_with_optimizer()
# =============================================================================

#' ALFGWC dengan optimasi centroid via swarm intelligence
alfgwc_with_optimizer <- function(data, pop_vec, dist_mat, W_std, ncluster, alpha_v,
                                  m = 2, max_iter = 100, error = 0.001, randomN = 0,
                                  algorithm = "pso", opt_params = list()) {
  ptm <- proc.time()
  data <- as.matrix(data)
  
  param_fgwc <- c(
    kind     = "v",
    ncluster = ncluster,
    m        = m,
    distance = "euclidean",
    order    = 2,
    alpha    = mean(alpha_v),
    a        = 1,
    b        = 1,
    max.iter = max_iter,
    error    = error,
    randomN  = randomN
  )
  
  opt_param <- build_opt_param(algorithm, opt_params)
  message(sprintf("[ALFGWC] Running %s for centroid initialization...", toupper(algorithm)))
  
  opt_result <- tryCatch({
    fgwc(
      data       = as.data.frame(data),
      pop        = pop_vec,
      distmat    = dist_mat,
      algorithm  = algorithm,
      fgwc_param = param_fgwc,
      opt_param  = opt_param
    )
  }, error = function(e) {
    message(sprintf("[ALFGWC] Optimasi %s gagal: %s. Fallback ke random init.", toupper(algorithm), e$message))
    NULL
  })
  
  if (!is.null(opt_result)) {
    V <- opt_result$centroid
    message(sprintf("[ALFGWC] Centroid dari %s: J_init = %.4f", toupper(algorithm), opt_result$f_obj))
  } else {
    V <- alfgwc_init_centroid(data, ncluster, randomN)
    message("[ALFGWC] Menggunakan random initialization.")
  }
  
  md <- alfgwc_compute_membership(data, V, m)
  U <- md$U
  D <- md$D
  
  conv <- numeric(0)
  iter <- 0
  
  repeat {
    U <- alfgwc_geographic_modify(U, W_std, alpha_v)
    V <- alfgwc_update_centers(data, U, m)
    
    md <- alfgwc_compute_membership(data, V, m)
    U <- md$U
    D <- md$D
    
    J <- alfgwc_objective(U, D, m)
    conv <- c(conv, J)
    iter <- iter + 1
    
    if (iter > 1 && abs(conv[iter] - conv[iter - 1]) < error) break
    if (iter >= max_iter) break
  }
  
  cluster <- apply(U, 1, which.max)
  finaldata <- cbind(as.data.frame(data), cluster = cluster)
  validity <- alfgwc_validity(U, V, D, m)
  
  return(list(
    converg = conv, f_obj = J, membership = U, centroid = V,
    validation = validity, cluster = cluster, finaldata = finaldata,
    iteration = iter, time = proc.time() - ptm
  ))
}

# =============================================================================
# WRAPPER SHINY: run_alfgwc_shiny()
# =============================================================================

run_alfgwc_shiny <- function(data_source, raw_data, sovi_result, selected_vars,
                             pop_vec, dist_mat, nb_list, lisa_p, lisa_I, 
                             algorithm = "classic", opt_params = list(),
                             ncluster = 4,
                             tw = "SPATIAL_INTERACTION",
                             gamma = 2, alpha_high = 0.8, alpha_mid = 0.5, alpha_low = 0.2,
                             m = 2, max_iter = 100, error = 0.001, randomN = 0,
                             id_col = "DISTRICTCODE", name_col = "KABUPATEN") {
  
  # Gunakan fungsi dari LFGWC untuk standarisasi matrix fitur
  # Jika memilih 'sovi' atau 'rc', sovi_result harus ada. Jika tidak, akan diambil dari raw_data.
  if (data_source %in% c("sovi", "rc") && is.null(sovi_result)) {
    stop("Data source 'SoVI Score' or 'RC Scores' selected, but SoVI results are not available.")
  }
  feat_df <- build_lfgwc_feature_matrix(data_source, raw_data, sovi_result, selected_vars)
  n <- nrow(feat_df)
  
  # Bangun Spatial Weights Matrix
  if (is.null(nb_list)) stop("Neighborhood list (nb_list) is not valid.")
  W_std <- build_alfgwc_weights(dist_mat, pop_vec, nb_list, tw, gamma)
  
  # Tentukan nilai Alpha secara adaptif menggunakan Local Moran's I
  if (is.null(lisa_p) || is.null(lisa_I)) {
    stop("Local Moran's I (lisa_p, lisa_I) not found.")
  }
  
  alpha_v <- numeric(n)
  for (i in 1:n) {
    if (!is.na(lisa_p[i]) && lisa_p[i] < 0.05 && lisa_I[i] > 0) {
      alpha_v[i] <- alpha_high
    } else if (!is.na(lisa_p[i]) && lisa_p[i] < 0.05 && lisa_I[i] < 0) {
      alpha_v[i] <- alpha_low
    } else {
      alpha_v[i] <- alpha_mid
    }
  }
  
  # Jalankan algoritma
  if (algorithm == "classic") {
    result <- alfgwc_classic(feat_df, W_std, ncluster, alpha_v, m, max_iter, error, randomN)
  } else {
    result <- alfgwc_with_optimizer(feat_df, pop_vec, dist_mat, W_std, ncluster, alpha_v, 
                                    m, max_iter, error, randomN, algorithm, opt_params)
  }
  
  # Gabungkan hasil dengan data identitas
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
  result_df$alfgwc_cluster <- as.factor(result$cluster)
  
  # Hitung silhouette
  D0      <- dist(feat_df, method = "euclidean")
  sil_obj <- tryCatch(
    cluster::silhouette(result$cluster, D0),
    error = function(e) NULL
  )
  sil_mean <- if (!is.null(sil_obj)) round(mean(sil_obj[, 3]), 3) else NA
  
  # Profil cluster
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
  
  if ("sovi_score" %in% names(result_df)) {
    sovi_means <- result_df |>
      dplyr::mutate(cluster = as.factor(alfgwc_cluster)) |>
      dplyr::group_by(cluster) |>
      dplyr::summarise(
        mean_sovi = round(mean(sovi_score, na.rm = TRUE), 3),
        .groups   = "drop"
      )
    profile <- dplyr::left_join(profile, sovi_means, by = "cluster")
  }
  
  # Membership matrix per unit (untuk GIS output)
  memb_df      <- as.data.frame(result$membership)
  memb_cols    <- paste0("memb_c", seq_len(ncluster))
  names(memb_df) <- memb_cols
  memb_df$max_membership <- apply(result$membership, 1, max)
  
  # Tabel indeks validasi
  val      <- result$validation
  val_df   <- data.frame(
    Index      = c("PC (max)", "CE (min)", "SC (min)", "SI (max)", "XB (min)", "IFV (max)", "Kwon (min)"),
    Value      = round(c(val$PC, val$CE, val$SC, sil_mean, val$XB, val$IFV, val$Kwon), 6),
    Description = c(
      "Partition Coefficient \u2014 higher is better",
      "Classification Entropy \u2014 lower is better",
      "Partition Index \u2014 lower is better",
      "Silhouette Index \u2014 higher is better",
      "Xie-Beni Index \u2014 lower is better",
      "IFV Spatial Index \u2014 higher is better",
      "Kwon Index \u2014 lower is better"
    ),
    stringsAsFactors = FALSE
  )
  
  return(list(
    result_df  = result_df,
    memb_df    = memb_df,
    feat_df    = feat_df,
    feat_cols  = feat_cols,
    id_col     = id_col,
    name_col   = name_col,
    k          = ncluster,
    algorithm  = algorithm,
    tw         = tw,
    gamma      = gamma,
    data_source= data_source,
    mode_label = tw,
    f_obj      = result$f_obj,
    iteration  = result$iteration,
    conv       = result$converg,
    val_df     = val_df,
    sil_obj    = sil_obj,
    sil_mean   = sil_mean,
    profile    = profile,
    raw_result = result,
    lisa_p     = lisa_p,
    lisa_I     = lisa_I,
    alpha_v    = alpha_v
  ))
}


build_leaflet_alfgwc <- function(result_df, shp, join_shp, join_df,
                                name_col    = "KABUPATEN",
                                k           = 4,
                                cluster_col = "alfgwc_cluster",
                                palette_name = "Dark2") {
  
  result_df[[join_df]] <- normalize_id(result_df[[join_df]])
  shp[[join_shp]]      <- normalize_id(shp[[join_shp]])
  
  peta <- dplyr::left_join(shp, result_df,
                           by = setNames(join_df, join_shp))
  
  n_col     <- min(max(k, 3), 8)
  pal_c     <- RColorBrewer::brewer.pal(n_col, palette_name)[seq_len(k)]
  
  pal_clust <- leaflet::colorFactor(
    palette  = pal_c,
    domain   = as.character(seq_len(k)),
    na.color = "#D3D3D3"
  )
  
  has_sovi <- "sovi_score" %in% names(peta)
  has_memb <- "max_membership" %in% names(peta)
  
  popup_text <- paste0(
    "<b>", peta[[name_col]], "</b><br>",
    "Cluster: ", peta[[cluster_col]],
    if (has_memb) paste0("<br>Max Membership: ",
                         round(peta$max_membership, 4)) else "",
    if (has_sovi) paste0("<br>SoVI Score: ",
                         round(peta$sovi_score, 4)) else ""
  )
  
  leaflet::leaflet(peta) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
    leaflet::addPolygons(
      fillColor        = ~pal_clust(as.character(get(cluster_col))),
      fillOpacity      = 0.75,
      color            = "#444444",
      weight           = 0.5,
      popup            = popup_text,
      label            = ~get(name_col),
      highlightOptions = leaflet::highlightOptions(
        weight      = 2,
        color       = "#222222",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal      = pal_clust,
      values   = as.character(seq_len(k)),
      title    = "ALFGWC Cluster",
      opacity  = 0.9
    )
}

# =============================================================================
# HELPER: Leaflet map untuk max membership value (Figure 5b di paper)
# =============================================================================

#' Leaflet choropleth max membership value per unit
build_leaflet_alfgwc_membership <- function(result_df, shp, join_shp, join_df,
                                           name_col    = "KABUPATEN",
                                           cluster_col = "alfgwc_cluster") {
  
  result_df[[join_df]] <- normalize_id(result_df[[join_df]])
  shp[[join_shp]]      <- normalize_id(shp[[join_shp]])
  
  peta <- dplyr::left_join(shp, result_df,
                           by = setNames(join_df, join_shp))
  
  # Tambahkan max_membership jika ada di result_df
  if (!"max_membership" %in% names(peta)) {
    return(leaflet::leaflet() |>
             leaflet::addTiles() |>
             leaflet::addControl("<b>max_membership tidak tersedia</b>",
                                 position = "topright"))
  }
  
  pal_memb <- leaflet::colorNumeric(
    palette  = "YlOrRd",
    domain   = c(0, 1),
    na.color = "#D3D3D3"
  )
  
  popup_text <- paste0(
    "<b>", peta[[name_col]], "</b><br>",
    "Cluster: ", peta[[cluster_col]], "<br>",
    "Max Membership: ", round(peta$max_membership, 4)
  )
  
  leaflet::leaflet(peta) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
    leaflet::addPolygons(
      fillColor        = ~pal_memb(max_membership),
      fillOpacity      = 0.8,
      color            = "#444444",
      weight           = 0.4,
      popup            = popup_text,
      label            = ~get(name_col),
      highlightOptions = leaflet::highlightOptions(
        weight      = 2,
        color       = "#222222",
        fillOpacity = 0.95,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position  = "bottomright",
      pal       = pal_memb,
      values    = ~max_membership,
      title     = "Max Membership",
      opacity   = 0.9,
      labFormat = leaflet::labelFormat(digits = 2)
    )
}

message("[alfgwc_wrapper] ALFGWC successfully loaded. Ready for use in Shiny.")