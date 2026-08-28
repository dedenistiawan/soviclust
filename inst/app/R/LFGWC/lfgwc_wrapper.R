# =============================================================================
# lfgwc_wrapper.R — Local Fuzzy Geographically Weighted Clustering
# Implementasi berdasarkan: Grekousis (2020)
# "Local fuzzy geographically weighted clustering: a new method for
#  geodemographic segmentation"
# International Journal of Geographical Information Science
# DOI: 10.1080/13658816.2020.1808221
#
# Letakkan file ini di: R/LFGWC/lfgwc_wrapper.R
# Di-source dari global.R
# =============================================================================
#
# PERBEDAAN UTAMA LFGWC vs FGWC:
#   FGWC (Global): u'_i = α*u_i + β*(1/A)*Σ_ALL_j w_ij*u_j
#                  → Σ mencakup SEMUA n unit
#                  → w_ij = (Pi*Pj)^b / d_ij^a (SIM-PF, butuh populasi)
#
#   LFGWC (Local): u'_i = α*u_i + β*Σ_NEIGHBOR_j W_std[i,j]*u_j   [Eq.12]
#                  → Σ hanya unit j dengan d_ij ≤ dthr (neighborhood)
#                  → W_std sudah row-standardized (tidak perlu A)
#                  → Mode 1: W[i,j] = (Pi*Pj)^b / d_ij^a  (SIM-PF)
#                  → Mode 0: W[i,j] = 1 / d_ij^exp        (Distance Decay)
#
# PARAMETER BARU vs FGWC:
#   dthr  = distance threshold (radius neighborhood)
#   si    = mode weighting: TRUE = SIM-PF, FALSE = Distance Decay
#   exp_d = eksponent distance decay (hanya jika si = FALSE, default = 2)
#
# VARIAN YANG DIIMPLEMENTASIKAN (sesuai paper Section 3.2):
#   "global"      → FGWC Global (dthr = -99, si = TRUE)
#   "lfgwc"       → LFGWC lokal + SIM-PF (dthr > 0, si = TRUE)
#   "dlfgwc"      → DLFGWC lokal + Distance Decay (dthr > 0, si = FALSE)
#   "dlfgwc_pso"  → DLFGWC + Distance Decay + PSO (dthr > 0, si = FALSE,
#                    algorithm = "pso")
# =============================================================================

message("[lfgwc_wrapper] Memuat LFGWC...")

# =============================================================================
# FASE 0 — BANGUN SPATIAL WEIGHTS MATRIX W
# Persamaan (10) & (11) paper Grekousis (2020)
# =============================================================================

#' Bangun spatial weights matrix W untuk LFGWC
#'
#' @param dist_mat  Matriks jarak antar unit spasial (n x n)
#' @param pop_vec   Vektor populasi (n x 1), digunakan jika si = TRUE
#' @param dthr      Distance threshold. -99 = mode global (semua unit)
#' @param si        TRUE = SIM-PF (Mason 2007), FALSE = Distance Decay
#' @param a         Parameter magnitude jarak untuk SIM-PF (default 1)
#' @param b         Parameter magnitude populasi untuk SIM-PF (default 1)
#' @param exp_d     Eksponent distance decay jika si = FALSE (default 2)
#'
#' @return List berisi:
#'   W_std     = Spatial weights matrix yang sudah row-standardized (n x n)
#'   n_isolated = Jumlah unit tanpa tetangga yang di-fallback ke KNN
#'   max_dist   = Jarak minimum agar setiap unit punya minimal 1 tetangga
#'   mode       = Label mode yang digunakan
build_lfgwc_weights <- function(dist_mat, pop_vec,
                                dthr  = 600,
                                si    = FALSE,
                                a     = 1,
                                b     = 1,
                                exp_d = 2) {
  
  n <- nrow(dist_mat)
  
  # ── Hitung jarak minimum agar tiap unit punya ≥1 tetangga ────────────────
  dist_aux          <- dist_mat
  diag(dist_aux)    <- NA
  col_mins          <- apply(dist_aux, 2, min, na.rm = TRUE)
  max_dist          <- max(col_mins)    # threshold minimum yang aman
  
  # ── FASE 0A: Bangun matriks jarak dengan pembatasan threshold ─────────────
  # dist_thr[i,j] = d_ij  jika d_ij ≤ dthr
  # dist_thr[i,j] = Inf   jika d_ij > dthr  (di luar neighborhood)
  # dist_thr[i,i] = Inf   (diagonal, no self-weight)
  
  if (dthr == -99) {
    # Mode Global: semua unit masuk sebagai tetangga
    dist_thr          <- dist_mat
    diag(dist_thr)    <- Inf
    n_isolated        <- 0
    mode_label        <- if (si) "Global FGWC (SIM-PF)" else
      "Global FGWC (Distance Decay)"
  } else {
    # Mode Lokal: hanya unit dalam radius dthr
    dist_thr          <- dist_mat
    dist_thr[dist_mat > dthr] <- Inf
    diag(dist_thr)    <- Inf
    
    # ── FASE 0B: KNN Fallback untuk unit tanpa tetangga ──────────────────
    # (Paper Section 3.4: assign at least the first nearest neighbor)
    isolated_idx <- which(apply(is.finite(dist_thr), 1, sum) == 0)
    n_isolated   <- length(isolated_idx)
    
    if (n_isolated > 0) {
      message(sprintf(
        "[LFGWC] %d unit tidak punya tetangga dalam dthr=%.1f. Fallback ke KNN.",
        n_isolated, dthr))
      for (i in isolated_idx) {
        # Cari tetangga terdekat (exclude diri sendiri)
        d_row         <- dist_aux[, i]
        nn_idx        <- which.min(d_row)
        # Assign jarak asli (bukan Inf)
        dist_thr[nn_idx, i] <- dist_mat[nn_idx, i]
        dist_thr[i, nn_idx] <- dist_mat[i, nn_idx]
      }
    }
    
    mode_label <- if (si) "LFGWC (SIM-PF)" else "DLFGWC (Distance Decay)"
  }
  
  # ── FASE 0C: Hitung nilai weight W[i,j] ──────────────────────────────────
  if (si) {
    # Mode SIM-PF — Persamaan (7) paper: w_ij = (Pi*Pj)^b / d_ij^a
    # (sama dengan renew_uij FGWC tapi dengan pembatasan neighborhood)
    pop_mat <- matrix(pop_vec, ncol = 1)
    mi_mj   <- pop_mat %*% t(pop_mat)     # outer product populasi (n x n)
    W_raw   <- (mi_mj ^ b) / (dist_thr ^ a)
  } else {
    # Mode Distance Decay — Persamaan (11) paper: f_ij = 1 / d_ij^exp
    W_raw   <- 1 / (dist_thr ^ exp_d)
  }
  
  # Ganti Inf dan NaN dengan 0
  W_raw[!is.finite(W_raw)] <- 0
  
  # ── FASE 0D: Row Standardization ─────────────────────────────────────────
  # Paper Section 2.4: "Weights are row-standardized for scaling in [0,1]"
  # W_std[i,j] = W_raw[i,j] / Σ_j W_raw[i,j]
  # Memastikan: Σ_j W_std[i,j] = 1 per baris (Constraint Eq.1 terpenuhi)
  row_sums <- rowSums(W_raw)
  row_sums[row_sums == 0] <- 1    # hindari pembagian nol
  W_std    <- W_raw / row_sums    # row-standardized (n x n)
  
  return(list(
    W_std      = W_std,
    n_isolated = n_isolated,
    max_dist   = max_dist,
    mode       = mode_label
  ))
}

# =============================================================================
# FASE 1 — INISIALISASI CENTROID
# Mendukung: random uniform, random normal, PSO via algoritma optimasi
# =============================================================================

#' Generate centroid awal untuk LFGWC
#'
#' @param data      Matriks data ternormalisasi (n x d)
#' @param ncluster  Jumlah cluster c
#' @param vi_dist   "uniform" atau "normal"
#' @param randomN   Random seed
#'
#' @return Matriks centroid (c x d)
lfgwc_init_centroid <- function(data, ncluster, vi_dist = "uniform",
                                randomN = 0) {
  d       <- ncol(data)
  centroid <- matrix(0, nrow = ncluster, ncol = d)
  for (j in seq_len(d)) {
    set.seed(randomN + j)
    if (vi_dist == "normal") {
      centroid[, j] <- rnorm(ncluster,
                             mean = mean(data[, j]),
                             sd   = sd(data[, j]))
    } else {
      centroid[, j] <- runif(ncluster,
                             min = min(data[, j]),
                             max = max(data[, j]))
    }
  }
  return(centroid)
}

# =============================================================================
# FASE 2 — HITUNG MEMBERSHIP MATRIX (FCM Standard)
# Persamaan (4) paper Grekousis (2020)
# u_ik = 1 / Σ_j (||v_i - x_k|| / ||v_j - x_k||)^(2/(m-1))
# =============================================================================

#' Hitung membership matrix U dari centroid V (FCM, sebelum modif geografis)
#'
#' @param data      Matriks data (n x d)
#' @param centroid  Matriks centroid (c x d)
#' @param m         Fuzzifier
#'
#' @return List: U = membership matrix (n x c), D = distance matrix (n x c)
lfgwc_compute_membership <- function(data, centroid, m) {
  n <- nrow(data)
  c <- nrow(centroid)
  
  # Hitung jarak Euclidean data ke tiap centroid: D[k,i] = ||x_k - v_i||
  D <- matrix(0, nrow = n, ncol = c)
  for (i in seq_len(c)) {
    diff    <- sweep(data, 2, centroid[i, ], "-")
    D[, i]  <- sqrt(rowSums(diff^2))
  }
  
  # Hindari pembagian nol: jika d = 0, unit tepat di centroid
  D_safe <- D
  D_safe[D_safe == 0] <- .Machine$double.eps
  
  # Hitung membership — Persamaan (4)
  exp_val <- 2 / (m - 1)
  tmp     <- D_safe^(-exp_val)           # (n x c)
  U       <- tmp / rowSums(tmp)          # row-normalize → Σ_i U[k,i] = 1
  
  return(list(U = U, D = D))
}

# =============================================================================
# FASE 3 — GEOGRAPHIC MODIFICATION (Inti LFGWC)
# Persamaan (12) paper Grekousis (2020)
# u'_i = α*u_i + β*Σ_j W_std[i,j]*u_j
#
# Perbedaan vs FGWC renew_uij():
#   FGWC: u'_i = α*u_i + (β/A)*Σ_ALL_j w_ij*u_j
#         → semua unit, normalisasi via A
#   LFGWC: u'_i = α*u_i + β*Σ_NEIGHBOR_j W_std[i,j]*u_j
#          → hanya tetangga, W sudah row-std, tidak perlu A
# =============================================================================

#' Modifikasi geografis lokal pada membership matrix
#'
#' @param U       Membership matrix sebelum modifikasi (n x c)
#' @param W_std   Spatial weights matrix row-standardized (n x n)
#' @param alpha   Bobot membership lama [0,1]
#'
#' @return U_new  Membership matrix setelah modifikasi (n x c)
lfgwc_geographic_modify <- function(U, W_std, alpha) {
  beta <- 1 - alpha
  
  # Pengaruh tetangga: weighted_sum[i,] = Σ_j W_std[i,j] * U[j,]
  # W_std (n x n) sudah row-standardized: Σ_j W_std[i,j] = 1 per baris
  # Dengan row-standardization, tidak perlu parameter A
  weighted_sum <- W_std %*% U      # (n x n) × (n x c) = (n x c)
  
  # Update membership — Persamaan (12)
  U_new <- alpha * U + beta * weighted_sum
  
  # Re-normalisasi agar Σ_i U_new[k,i] = 1 (Constraint Eq.1)
  row_sums               <- rowSums(U_new)
  row_sums[row_sums == 0] <- 1
  U_new                  <- U_new / row_sums
  
  return(U_new)
}

# =============================================================================
# FASE 4 — UPDATE CLUSTER CENTERS
# Persamaan (5) paper Grekousis (2020)
# v_i = Σ_k u_ik^m * x_k / Σ_k u_ik^m
# =============================================================================

#' Update cluster centers dari membership matrix
#'
#' @param data  Matriks data (n x d)
#' @param U     Membership matrix (n x c)
#' @param m     Fuzzifier
#'
#' @return Matriks centroid baru (c x d)
lfgwc_update_centers <- function(data, U, m) {
  data <- as.matrix(data)
  Um   <- U^m                              # (n x c)
  # t(Um) %*% data = (c x n) × (n x d) = (c x d)
  # colSums(Um) = panjang c — jumlah fuzzy cardinality per cluster
  num  <- t(Um) %*% data                  # (c x d)
  den  <- colSums(Um)                     # (c) — bukan rowSums!
  den[den == 0] <- .Machine$double.eps    # hindari pembagian nol
  V    <- sweep(num, 1, den, "/")         # (c x d) dibagi per baris
  return(V)
}

# =============================================================================
# FASE 5 — OBJECTIVE FUNCTION
# Persamaan (3) paper Grekousis (2020)
# J = Σ_i Σ_k u_ik^m * ||v_i - x_k||^2
# =============================================================================

#' Hitung objective function LFGWC
#'
#' @param U  Membership matrix (n x c)
#' @param D  Distance matrix (n x c)
#' @param m  Fuzzifier
#'
#' @return Nilai objective function J (scalar)
lfgwc_objective <- function(U, D, m) {
  sum((U^m) * (D^2))
}

# =============================================================================
# FASE 6 — INDEKS VALIDASI LFGWC
# Sesuai paper Grekousis (2020) Section 2.5:
#   PC  (Eq.13) — Partition Coefficient        → MAKSIMUM lebih baik
#   CE  (Eq.14) — Classification Entropy       → MINIMUM lebih baik
#   SC  (Eq.15) — Partition Index              → MINIMUM lebih baik
#   IFV (Eq.16) — Index for spatial data       → MAKSIMUM lebih baik
# =============================================================================

#' Hitung indeks validasi LFGWC
#'
#' @param U  Membership matrix final (n x c)
#' @param V  Centroid matrix final (c x d)
#' @param D  Distance matrix (n x c) — jarak data ke centroid
#' @param m  Fuzzifier
#'
#' @return List berisi PC, CE, SC, IFV
lfgwc_validity <- function(U, V, D, m) {
  n <- nrow(U)
  c <- ncol(U)
  
  # ── PC — Partition Coefficient — Persamaan (13) ───────────────────────────
  # PC = (1/n) * Σ_i Σ_k u_ik^2
  # Nilai lebih tinggi = partisi lebih baik
  PC <- (1/n) * sum(U^2)
  
  # ── CE — Classification Entropy — Persamaan (14) ─────────────────────────
  # CE = -(1/n) * Σ_i Σ_k u_ik * log(u_ik)
  # Nilai lebih rendah = partisi lebih baik
  U_safe    <- U
  U_safe[U_safe <= 0] <- .Machine$double.eps
  CE        <- -(1/n) * sum(U_safe * log(U_safe))
  
  # ── SC — Partition Index — Persamaan (15) ────────────────────────────────
  # SC = Σ_i [ Σ_k u_ik^m * ||x_k - v_i||^2 / n_i * Σ_j ||v_i - v_j||^2 ]
  # n_i = fuzzy cardinality = Σ_k u_ik
  # Nilai lebih rendah = partisi lebih baik
  ni      <- colSums(U)                     # fuzzy cardinality (c x 1)
  si      <- colSums((U^m) * (D^2))        # fuzzy variation (c x 1)
  
  # Jarak antar centroid: dist_vv[i,j] = ||v_i - v_j||^2
  dist_vv <- as.matrix(dist(V))^2          # (c x c)
  sep     <- rowSums(dist_vv)              # separasi per cluster (c x 1)
  sep[sep == 0] <- .Machine$double.eps     # hindari pembagian nol
  
  SC      <- sum((si / ni) / sep)
  
  # ── IFV — Index for Spatial Data — Persamaan (16-18) ─────────────────────
  # SD_max = max_{k≠j} ||v_k - v_j||^2     [Eq.17]
  # σ_D    = (1/c) * Σ_i (1/n * Σ_k ||x_k - v_j||^2)  [Eq.18]
  # IFV    = (1/c) * Σ_j {(1/n)*Σ_i u_ij^2 *
  #           [log2(c) - (1/n)*Σ_i log2(u_ij)]^2} * SD_max/σ_D  [Eq.16]
  # Nilai lebih tinggi = partisi lebih baik
  
  # Persamaan (17): SD_max
  diag(dist_vv) <- 0
  SD_max        <- max(dist_vv)
  if (SD_max == 0) SD_max <- .Machine$double.eps
  
  # Persamaan (18): σ_D
  sigma_D       <- (1/c) * sum((1/n) * colSums(D^2))
  if (sigma_D == 0) sigma_D <- .Machine$double.eps
  
  # Persamaan (16): IFV per cluster j
  U_safe2       <- U
  U_safe2[U_safe2 <= 0] <- .Machine$double.eps
  
  part1 <- (1/n) * colSums(log2(U_safe2))           # (c x 1)
  part2 <- (log2(c) - part1)^2                       # (c x 1)
  part3 <- (1/n) * colSums(U_safe2^2)               # (c x 1)
  part4 <- part3 * part2                             # (c x 1)
  IFV   <- (1/c) * sum(part4) * (SD_max / sigma_D)
  
  # ── XB — Xie-Beni Index ──────────────────────────────────────────────────
  dist_vv_min <- dist_vv
  diag(dist_vv_min) <- Inf
  min_sep <- min(dist_vv_min)
  if (min_sep == 0) min_sep <- .Machine$double.eps
  XB <- sum(si) / (n * min_sep)
  
  # ── Kwon — Kwon Index ────────────────────────────────────────────────────
  v_mean <- colMeans(V)
  v_var <- sum(sweep(V, 2, v_mean, "-")^2)
  Kwon <- (sum(si) + (v_var / c)) / min_sep
  
  return(list(
    PC   = round(PC,  6),
    CE   = round(CE,  6),
    SC   = round(SC,  6),
    IFV  = round(IFV, 6),
    XB   = round(XB,  6),
    Kwon = round(Kwon, 6)
  ))
}

# =============================================================================
# FUNGSI INTI: lfgwc_classic()
# Implementasi LFGWC tanpa algoritma optimasi (klasik / iteratif)
# Mengikuti algoritma di paper Section 2.4:
#   (1) Init V
#   (2) Hitung U dari V  [Eq.4]
#   (3) Update U secara geografis lokal  [Eq.12]
#   (4) Update V dari U termodifikasi  [Eq.5]
#   (5) Hitung J  [Eq.3]
#   (6) Cek konvergensi → ulangi atau stop
# =============================================================================

#' LFGWC klasik tanpa optimasi metaheuristik
#'
#' @param data      Matriks data (n x d), sudah ternormalisasi
#' @param W_std     Spatial weights matrix row-standardized (n x n)
#' @param ncluster  Jumlah cluster c
#' @param m         Fuzzifier (default 2)
#' @param alpha     Bobot membership lama (default 0.8)
#' @param max_iter  Maksimum iterasi (default 100)
#' @param error     Toleransi konvergensi (default 0.001)
#' @param vi_dist   Distribusi inisialisasi centroid
#' @param randomN   Random seed
#'
#' @return List hasil LFGWC (membership, centroid, cluster, konvergensi, validasi)
lfgwc_classic <- function(data, W_std, ncluster,
                          m        = 2,
                          alpha    = 0.8,
                          max_iter = 100,
                          error    = 0.001,
                          vi_dist  = "uniform",
                          randomN  = 0) {
  
  ptm  <- proc.time()
  data <- as.matrix(data)
  n    <- nrow(data)
  
  # ── FASE 1: Inisialisasi centroid ─────────────────────────────────────────
  V <- lfgwc_init_centroid(data, ncluster, vi_dist, randomN)
  
  # ── Hitung membership awal sebelum loop ──────────────────────────────────
  md  <- lfgwc_compute_membership(data, V, m)
  U   <- md$U
  D   <- md$D
  
  conv <- numeric(0)
  iter <- 0
  
  # ── Loop utama LFGWC ─────────────────────────────────────────────────────
  repeat {
    
    # FASE 3: Geographic modification (Local) — Persamaan (12)
    U <- lfgwc_geographic_modify(U, W_std, alpha)
    
    # FASE 4: Update cluster centers — Persamaan (5)
    V <- lfgwc_update_centers(data, U, m)
    
    # FASE 2: Hitung membership baru dari V baru — Persamaan (4)
    md  <- lfgwc_compute_membership(data, V, m)
    U   <- md$U
    D   <- md$D
    
    # FASE 5: Hitung objective function — Persamaan (3)
    J    <- lfgwc_objective(U, D, m)
    conv <- c(conv, J)
    iter <- iter + 1
    
    message(sprintf("[LFGWC] Iter %d | J = %.6f", iter, J))
    
    # Cek konvergensi
    if (iter > 1 && abs(conv[iter] - conv[iter - 1]) < error) break
    if (iter >= max_iter) break
  }
  
  # ── FASE 6: Assign cluster & validasi ────────────────────────────────────
  cluster   <- apply(U, 1, which.max)
  finaldata <- cbind(as.data.frame(data), cluster = cluster)
  validity  <- lfgwc_validity(U, V, D, m)
  
  return(list(
    converg    = conv,
    f_obj      = J,
    membership = U,
    centroid   = V,
    validation = validity,
    cluster    = cluster,
    finaldata  = finaldata,
    iteration  = iter,
    time       = proc.time() - ptm
  ))
}

# =============================================================================
# FUNGSI INTI: lfgwc_with_optimizer()
# LFGWC dengan algoritma optimasi metaheuristik untuk inisialisasi centroid
# PSO digunakan sebagai DLFGWC-PSO sesuai paper Section 3.2(d)
#
# Strategi: algoritma optimasi (abc/fpa/gsa/hho/ifa/pso/tlbo/woa) digunakan
# untuk mencari centroid awal optimal, kemudian LFGWC iteratif dijalankan
# dengan centroid hasil optimasi tersebut.
# =============================================================================

#' LFGWC dengan optimasi centroid via swarm intelligence
#'
#' @param data        Matriks data (n x d), sudah ternormalisasi
#' @param pop_vec     Vektor populasi (n), untuk objective function FGWC
#' @param dist_mat    Matriks jarak antar unit (n x n), untuk objective FGWC
#' @param W_std       Spatial weights matrix row-standardized (n x n)
#' @param ncluster    Jumlah cluster c
#' @param m           Fuzzifier
#' @param alpha       Bobot membership lama
#' @param a           Parameter SIM-PF (jarak)
#' @param b           Parameter SIM-PF (populasi)
#' @param max_iter    Maksimum iterasi LFGWC
#' @param error       Toleransi konvergensi
#' @param randomN     Random seed
#' @param algorithm   Algoritma optimasi: "pso"|"abc"|"fpa"|"gsa"|"hho"|"ifa"|
#'                    "tlbo"|"woa"
#' @param opt_params  List parameter algoritma optimasi
#'
#' @return List hasil LFGWC (sama dengan lfgwc_classic)
lfgwc_with_optimizer <- function(data, pop_vec, dist_mat, W_std,
                                 ncluster,
                                 m        = 2,
                                 alpha    = 0.8,
                                 a        = 1,
                                 b        = 1,
                                 max_iter = 100,
                                 error    = 0.001,
                                 randomN  = 0,
                                 algorithm  = "pso",
                                 opt_params = list()) {
  
  ptm  <- proc.time()
  data <- as.matrix(data)
  
  # ── FASE 1: Cari centroid optimal via algoritma optimasi ──────────────────
  # Gunakan fgwc() dari naspaclust (yang sudah di-source dari FGWC_OPT)
  # sebagai engine untuk menemukan centroid awal yang baik.
  # Catatan: fgwc() menggunakan FGWC global untuk optimasi centroid,
  # hasilnya (centroid) kemudian dipakai sebagai inisialisasi LFGWC.
  
  param_fgwc <- c(
    kind     = "v",
    ncluster = ncluster,
    m        = m,
    distance = "euclidean",
    order    = 2,
    alpha    = alpha,
    a        = a,
    b        = b,
    max.iter = max_iter,
    error    = error,
    randomN  = randomN
  )
  
  opt_param <- build_opt_param(algorithm, opt_params)
  
  message(sprintf("[LFGWC] Menjalankan %s untuk inisialisasi centroid...",
                  toupper(algorithm)))
  
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
    message(sprintf("[LFGWC] Optimasi %s gagal: %s. Fallback ke random init.",
                    toupper(algorithm), e$message))
    NULL
  })
  
  # ── Ambil centroid hasil optimasi, atau fallback ke random ────────────────
  if (!is.null(opt_result)) {
    V_init <- opt_result$centroid
    message(sprintf("[LFGWC] Centroid dari %s: J_init = %.4f",
                    toupper(algorithm), opt_result$f_obj))
  } else {
    V_init <- lfgwc_init_centroid(data, ncluster, "uniform", randomN)
    message("[LFGWC] Menggunakan random initialization.")
  }
  
  # ── FASE 2: Hitung membership awal dari centroid hasil optimasi ───────────
  md  <- lfgwc_compute_membership(data, V_init, m)
  U   <- md$U
  D   <- md$D
  V   <- V_init
  
  conv <- numeric(0)
  iter <- 0
  
  # ── Loop utama LFGWC dengan centroid teroptimasi ──────────────────────────
  repeat {
    
    # FASE 3: Geographic modification (Local) — Persamaan (12)
    U <- lfgwc_geographic_modify(U, W_std, alpha)
    
    # FASE 4: Update cluster centers — Persamaan (5)
    V <- lfgwc_update_centers(data, U, m)
    
    # FASE 2: Hitung membership baru — Persamaan (4)
    md  <- lfgwc_compute_membership(data, V, m)
    U   <- md$U
    D   <- md$D
    
    # FASE 5: Hitung objective function — Persamaan (3)
    J    <- lfgwc_objective(U, D, m)
    conv <- c(conv, J)
    iter <- iter + 1
    
    message(sprintf("[LFGWC-%s] Iter %d | J = %.6f",
                    toupper(algorithm), iter, J))
    
    # Cek konvergensi
    if (iter > 1 && abs(conv[iter] - conv[iter - 1]) < error) break
    if (iter >= max_iter) break
  }
  
  # ── FASE 6: Assign cluster & validasi ────────────────────────────────────
  cluster   <- apply(U, 1, which.max)
  finaldata <- cbind(as.data.frame(data), cluster = cluster)
  validity  <- lfgwc_validity(U, V, D, m)
  
  return(list(
    converg    = conv,
    f_obj      = J,
    membership = U,
    centroid   = V,
    validation = validity,
    cluster    = cluster,
    finaldata  = finaldata,
    iteration  = iter,
    time       = proc.time() - ptm
  ))
}

# =============================================================================
# HELPER: Bangun matriks fitur untuk LFGWC
# Sama persis dengan build_fgwc_feature_matrix di fgwc_wrapper.R
# tetapi dengan normalisasi min-max [0,1] wajib (sesuai paper LFGWC)
# =============================================================================

#' Bangun matriks fitur dan normalisasi untuk LFGWC
#'
#' @param data_source  "raw"|"raw_norm"|"standardized"|"sovi"|"rc"
#' @param raw_data     data.frame asli dari upload user
#' @param sovi_result  hasil run_sovi_core (bisa NULL)
#' @param selected_vars vektor nama variabel yang dipilih
#'
#' @return data.frame matriks fitur (n x d), ternormalisasi min-max [0,1]
build_lfgwc_feature_matrix <- function(data_source, raw_data, sovi_result,
                                       selected_vars) {
  
  # ── Ambil matriks fitur sesuai sumber ─────────────────────────────────────
  if (data_source == "raw") {
    mat <- as.data.frame(raw_data[, selected_vars, drop = FALSE])
    mat <- as.data.frame(lapply(mat, as.numeric))
    
  } else if (data_source == "raw_norm") {
    mat <- as.data.frame(raw_data[, selected_vars, drop = FALSE])
    mat <- as.data.frame(lapply(mat, as.numeric))
    mat <- as.data.frame(lapply(mat, normalize_01))   # dari global.R
    
  } else if (data_source == "standardized") {
    if (is.null(sovi_result))
      stop("Jalankan SoVI Computation terlebih dahulu.")
    Z        <- sovi_result$std_out$Z
    use_vars <- intersect(selected_vars, names(Z))
    if (length(use_vars) == 0) use_vars <- names(Z)
    mat      <- as.data.frame(Z[, use_vars, drop = FALSE])
    
  } else if (data_source == "sovi") {
    if (is.null(sovi_result))
      stop("Jalankan SoVI Computation terlebih dahulu.")
    mat <- data.frame(sovi_score = sovi_result$sovi_df$sovi_score)
    
  } else if (data_source == "rc") {
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
  
  # ── Normalisasi min-max [0,1] wajib untuk LFGWC ──────────────────────────
  # Paper LFGWC_Call.m: "Normalize Data (range 0 to 1)"
  # Namun sesuai permintaan, data "raw" dibiarkan benar-benar mentah.
  # Hanya apply jika data_source == "standardized" (karena Z-score bisa ada nilai negatif yang bermasalah untuk fuzzy).
  if (data_source %in% c("standardized")) {
    mat <- as.data.frame(lapply(mat, normalize_01))
  }
  
  return(mat)
}

# =============================================================================
# FUNGSI UTAMA SHINY: run_lfgwc_shiny()
# Entry point dari server.R — menggabungkan semua fase
#
# Parameter:
#   data_source   - "raw"|"raw_norm"|"standardized"|"sovi"|"rc"
#   raw_data      - rv$data
#   sovi_result   - rv$sovi_result
#   selected_vars - variabel yang dipilih (untuk raw/raw_norm/standardized)
#   pop_vec       - vektor populasi (n)
#   dist_mat      - matriks jarak (n x n)
#   algorithm     - "classic"|"pso"|"abc"|"fpa"|"gsa"|"hho"|"ifa"|"tlbo"|"woa"
#   ncluster      - jumlah cluster
#   lfgwc_params  - list parameter LFGWC
#   opt_params    - list parameter algoritma optimasi
#   id_col        - nama kolom ID wilayah
#   name_col      - nama kolom nama wilayah
# =============================================================================

run_lfgwc_shiny <- function(data_source,
                            raw_data,
                            sovi_result,
                            selected_vars,
                            pop_vec,
                            dist_mat,
                            algorithm    = "classic",
                            ncluster     = 4,
                            lfgwc_params = list(),
                            opt_params   = list(),
                            id_col       = "DISTRICTCODE",
                            name_col     = "KABUPATEN") {
  
  # ── 1. Bangun matriks fitur (ternormalisasi) ───────────────────────────────
  feat_df <- build_lfgwc_feature_matrix(data_source, raw_data, sovi_result,
                                        selected_vars)
  n       <- nrow(feat_df)
  
  # ── 2. Validasi dimensi input ─────────────────────────────────────────────
  if (length(pop_vec) != n)
    stop(sprintf("Panjang vektor populasi (%d) tidak sesuai jumlah unit (%d).",
                 length(pop_vec), n))
  if (nrow(dist_mat) != n || ncol(dist_mat) != n)
    stop(sprintf("Dimensi matriks jarak (%dx%d) tidak sesuai jumlah unit (%d).",
                 nrow(dist_mat), ncol(dist_mat), n))
  
  # ── 3. Ambil parameter LFGWC ──────────────────────────────────────────────
  m        <- lfgwc_params$m        %||% 2
  alpha    <- lfgwc_params$alpha    %||% 0.8
  a        <- lfgwc_params$a        %||% 1
  b        <- lfgwc_params$b        %||% 1
  max_iter <- lfgwc_params$max_iter %||% 100
  error    <- lfgwc_params$error    %||% 0.001
  randomN  <- lfgwc_params$randomN  %||% 0
  dthr     <- lfgwc_params$dthr     %||% -99     # -99 = global mode
  si       <- lfgwc_params$si       %||% FALSE   # FALSE = distance decay
  exp_d    <- lfgwc_params$exp_d    %||% 2       # eksponent distance decay
  
  # ── 4. FASE 0: Bangun spatial weights matrix W ────────────────────────────
  message("[LFGWC] Membangun spatial weights matrix...")
  w_result <- build_lfgwc_weights(
    dist_mat = dist_mat,
    pop_vec  = pop_vec,
    dthr     = dthr,
    si       = si,
    a        = a,
    b        = b,
    exp_d    = exp_d
  )
  W_std      <- w_result$W_std
  n_isolated <- w_result$n_isolated
  max_dist   <- w_result$max_dist
  mode_label <- w_result$mode
  
  message(sprintf("[LFGWC] Mode: %s | dthr=%.1f | Isolated (KNN)=%d | MaxDist=%.2f",
                  mode_label, dthr, n_isolated, max_dist))
  
  # ── 5. Jalankan LFGWC (classic atau dengan optimizer) ─────────────────────
  if (algorithm == "classic") {
    # LFGWC klasik tanpa optimasi
    vi_dist <- opt_params$vi_dist %||% "uniform"
    result  <- lfgwc_classic(
      data     = feat_df,
      W_std    = W_std,
      ncluster = ncluster,
      m        = m,
      alpha    = alpha,
      max_iter = max_iter,
      error    = error,
      vi_dist  = vi_dist,
      randomN  = randomN
    )
  } else {
    # LFGWC dengan metaheuristik optimizer
    result <- lfgwc_with_optimizer(
      data       = feat_df,
      pop_vec    = pop_vec,
      dist_mat   = dist_mat,
      W_std      = W_std,
      ncluster   = ncluster,
      m          = m,
      alpha      = alpha,
      a          = a,
      b          = b,
      max_iter   = max_iter,
      error      = error,
      randomN    = randomN,
      algorithm  = algorithm,
      opt_params = opt_params
    )
  }
  
  # ── 6. Susun result_df (gabung cluster ke data identitas) ─────────────────
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
  result_df$lfgwc_cluster <- as.factor(result$cluster)
  
  # ── 7. Hitung silhouette ──────────────────────────────────────────────────
  D0      <- dist(feat_df, method = "euclidean")
  sil_obj <- tryCatch(
    cluster::silhouette(result$cluster, D0),
    error = function(e) NULL
  )
  sil_mean <- if (!is.null(sil_obj)) round(mean(sil_obj[, 3]), 3) else NA
  
  # ── 8. Profil cluster ─────────────────────────────────────────────────────
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
      dplyr::mutate(cluster = as.factor(lfgwc_cluster)) |>
      dplyr::group_by(cluster) |>
      dplyr::summarise(
        mean_sovi = round(mean(sovi_score, na.rm = TRUE), 3),
        .groups   = "drop"
      )
    profile <- dplyr::left_join(profile, sovi_means, by = "cluster")
  }
  
  # ── 9. Membership matrix per unit (untuk GIS output) ─────────────────────
  # Kolom = cluster, Nilai = derajat keanggotaan [0,1]
  memb_df      <- as.data.frame(result$membership)
  memb_cols    <- paste0("memb_c", seq_len(ncluster))
  names(memb_df) <- memb_cols
  
  # Max membership per unit
  memb_df$max_membership <- apply(result$membership, 1, max)
  
  # ── 10. Tabel indeks validasi ─────────────────────────────────────────────
  val      <- result$validation
  val_df   <- data.frame(
    Indeks     = c("PC (max)", "CE (min)", "SC (min)", "SI (max)", "XB (min)", "IFV (max)", "Kwon (min)"),
    Nilai      = round(c(val$PC, val$CE, val$SC, sil_mean, val$XB, val$IFV, val$Kwon), 6),
    Keterangan = c(
      "Partition Coefficient — lebih tinggi lebih baik",
      "Classification Entropy — lebih rendah lebih baik",
      "Partition Index — lebih rendah lebih baik",
      "Silhouette Index — lebih tinggi lebih baik",
      "Xie-Beni Index — lebih rendah lebih baik",
      "Index for Spatial Fuzzy Validity — lebih tinggi lebih baik",
      "Kwon Index — lebih rendah lebih baik"
    ),
    stringsAsFactors = FALSE
  )
  
  # ── 11. Return hasil lengkap ──────────────────────────────────────────────
  return(list(
    result_df    = result_df,
    lfgwc_raw    = result,
    profile      = profile,
    val_df       = val_df,
    memb_df      = memb_df,
    memb_cols    = memb_cols,
    sil_obj      = sil_obj,
    sil_mean     = sil_mean,
    feat_cols    = feat_cols,
    feat_df      = feat_df,
    k            = ncluster,
    algorithm    = algorithm,
    data_source  = data_source,
    id_col       = id_col,
    name_col     = name_col,
    conv         = result$converg,
    f_obj        = result$f_obj,
    iteration    = result$iteration,
    # Info LFGWC spesifik
    dthr         = dthr,
    si           = si,
    exp_d        = exp_d,
    n_isolated   = n_isolated,
    max_dist     = max_dist,
    mode_label   = mode_label,
    W_std        = W_std
  ))
}

# =============================================================================
# HELPER: Leaflet map untuk hasil LFGWC
# Sama pola dengan build_leaflet_fgwc di fgwc_wrapper.R
# =============================================================================

#' Bangun leaflet choropleth untuk hasil LFGWC
#'
#' @param result_df   data.frame hasil run_lfgwc_shiny
#' @param shp         Shapefile sf object
#' @param join_shp    Nama kolom ID di shapefile
#' @param join_df     Nama kolom ID di result_df
#' @param name_col    Nama kolom nama wilayah
#' @param k           Jumlah cluster
#' @param cluster_col Nama kolom cluster di result_df
build_leaflet_lfgwc <- function(result_df, shp, join_shp, join_df,
                                name_col    = "KABUPATEN",
                                k           = 4,
                                cluster_col = "lfgwc_cluster",
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
      title    = "LFGWC Cluster",
      opacity  = 0.9
    )
}

# =============================================================================
# HELPER: Leaflet map untuk max membership value (Figure 5b di paper)
# =============================================================================

#' Leaflet choropleth max membership value per unit
build_leaflet_lfgwc_membership <- function(result_df, shp, join_shp, join_df,
                                           name_col    = "KABUPATEN",
                                           cluster_col = "lfgwc_cluster") {
  
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

message("[lfgwc_wrapper] LFGWC berhasil dimuat. Siap digunakan di Shiny.")