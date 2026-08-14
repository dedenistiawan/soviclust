#' Fuzzy Geographically Weighted Clustering with Grey Wolf Optimizer
#' @description Fuzzy clustering with addition of spatial configuration of
#' membership matrix with centroid optimization using Grey Wolf Optimizer (GWO).
#' @param data an object of data with d>1. Can be \code{matrix} or \code{data.frame}.
#' If your data is univariate, bind it with \code{1} to get a 2 columns.
#' @param pop an n*1 vector contains population.
#' @param distmat an n*n distance matrix between regions.
#' @param ncluster an integer. The number of clusters.
#' @param m degree of fuzziness or fuzzifier. Default is 2.
#' @param distance the distance metric between data and centroid, the default is
#' euclidean, see \code{\link{cdist}} for details.
#' @param order minkowski order. Default is 2.
#' @param alpha the old membership effect with [0,1], if \code{alpha} equals 1,
#' it will be same as fuzzy C-Means, if 0, it equals to neighborhood effect.
#' @param a spatial magnitude of distance. Default is 1.
#' @param b spatial magnitude of population. Default is 1.
#' @param max.iter maximum iteration. Default is 500.
#' @param error error tolerance. Default is 1e-5.
#' @param randomN random seed for initialisation. Default is 0.
#' @param vi.dist a string of centroid population distribution between
#' \code{"uniform"} (default) and \code{"normal"}.
#' @param nwolf number of wolf population. Can be defined as \code{npar=} in
#' \code{opt_param}. Default is 10.
#' @param wolf.same number of consecutive unchanged iterations to stop.
#' Can be defined as \code{same=} in \code{opt_param}. Default is 10.
#'
#' @return an object of class \code{"fgwc"}.\cr
#' An \code{"fgwc"} object contains as follows:
#' \itemize{
#' \item \code{converg} - the process convergence of objective function
#' \item \code{f_obj} - objective function value
#' \item \code{membership} - membership matrix
#' \item \code{centroid} - centroid matrix
#' \item \code{validation} - validation indices
#' \item \code{max.iter} - Maximum iteration
#' \item \code{cluster} - the cluster of the data
#' \item \code{finaldata} - The final data (with the cluster)
#' \item \code{call} - the syntax called previously
#' \item \code{time} - computational time.
#' }
#'
#' @details
#' Fuzzy Geographically Weighted Clustering (FGWC) was developed by adding
#' neighborhood effects and population to configure the membership matrix in
#' Fuzzy C-Means. The Grey Wolf Optimizer (GWO) was developed by
#' Mirjalili et al. (2014) inspired by the leadership hierarchy and hunting
#' mechanism of grey wolves. GWO uses four types of wolves:
#' Alpha (best solution), Beta (second best), Delta (third best),
#' and Omega (remaining wolves). The positions are updated based on the
#' positions of Alpha, Beta, and Delta wolves.
#'
#' @references
#' Mirjalili, S., Mirjalili, S. M., & Lewis, A. (2014).
#' Grey Wolf Optimizer. Advances in Engineering Software, 69, 46-61.
#' https://doi.org/10.1016/j.advengsoft.2013.12.007
#'
#' @export
#' @import rdist
#' @import stats

gwofgwc <- function(data, pop = NA, distmat = NA, ncluster = 2, m = 2,
                    distance = 'euclidean', order = 2, alpha = 0.7,
                    a = 1, b = 1, error = 1e-5, max.iter = 100,
                    randomN = 0, vi.dist = "uniform", nwolf = 10,
                    wolf.same = 10) {
  
  randomnn <- randomN
  ptm      <- proc.time()
  n        <- nrow(data)
  d        <- ncol(data)
  iter     <- 0
  beta     <- 1 - alpha
  same     <- 0
  data     <- as.matrix(data)
  
  # Jika alpha = 1 (FCM murni), populasi dan jarak tidak digunakan
  if (alpha == 1) {
    pop     <- rep(1, n)
    distmat <- matrix(1, n, n)
  }
  
  datax  <- data
  pop    <- matrix(pop, ncol = 1)
  mi.mj  <- pop %*% t(pop)
  
  # ── FASE 1: Inisialisasi kawanan serigala ─────────────────────────────────
  wolf <- init.swarm(data, mi.mj, distmat, distance, order, vi.dist,
                     ncluster, m, alpha, a, b, randomN, nwolf)
  
  wolf.swarm <- wolf$centroid    # list posisi centroid tiap serigala
  wolf.other <- wolf$membership  # list membership matrix tiap serigala
  wolf.fit   <- wolf$I           # vector nilai objective function tiap serigala
  
  # ── FASE 2: Tentukan Alpha, Beta, Delta (3 serigala terbaik) ─────────────
  sorted_idx <- order(wolf.fit)
  alpha_pos  <- wolf.swarm[[ sorted_idx[1] ]]
  beta_pos   <- wolf.swarm[[ sorted_idx[2] ]]
  delta_pos  <- wolf.swarm[[ sorted_idx[min(3, nwolf)] ]]
  
  alpha_fit   <- wolf.fit[ sorted_idx[1] ]
  alpha_other <- wolf.other[[ sorted_idx[1] ]]  # inisialisasi sebelum loop
  
  conv <- c(alpha_fit)
  
  # ── Loop utama GWO ────────────────────────────────────────────────────────
  repeat {
    
    iter <- iter + 1
    
    # Koefisien a menurun linear dari 2 ke 0 seiring iterasi
    # Persamaan (3) paper Mirjalili 2014: a = 2 - 2*(iter/max.iter)
    a_coef <- 2 - 2 * (iter / max.iter)
    
    # Update posisi tiap serigala omega (Persamaan 5-7 paper)
    wolf.swarm <- lapply(seq_len(nwolf), function(i) {
      set.seed(randomN + iter * nwolf + i)
      
      dd <- dim(alpha_pos)   # (ncluster x d)
      
      # ── Komponen dari Alpha ──────────────────────────────────────────
      r1_a <- matrix(runif(dd[1] * dd[2]), nrow = dd[1])
      r2_a <- matrix(runif(dd[1] * dd[2]), nrow = dd[1])
      A1   <- 2 * a_coef * r1_a - a_coef   # koefisien A
      C1   <- 2 * r2_a                       # koefisien C
      D_alpha <- abs(C1 * alpha_pos - wolf.swarm[[i]])
      X1   <- alpha_pos - A1 * D_alpha
      
      # ── Komponen dari Beta ───────────────────────────────────────────
      r1_b <- matrix(runif(dd[1] * dd[2]), nrow = dd[1])
      r2_b <- matrix(runif(dd[1] * dd[2]), nrow = dd[1])
      A2   <- 2 * a_coef * r1_b - a_coef
      C2   <- 2 * r2_b
      D_beta <- abs(C2 * beta_pos - wolf.swarm[[i]])
      X2   <- beta_pos - A2 * D_beta
      
      # ── Komponen dari Delta ──────────────────────────────────────────
      r1_d <- matrix(runif(dd[1] * dd[2]), nrow = dd[1])
      r2_d <- matrix(runif(dd[1] * dd[2]), nrow = dd[1])
      A3   <- 2 * a_coef * r1_d - a_coef
      C3   <- 2 * r2_d
      D_delta <- abs(C3 * delta_pos - wolf.swarm[[i]])
      X3   <- delta_pos - A3 * D_delta
      
      # Posisi baru = rata-rata dari ketiga komponen (Persamaan 7)
      (X1 + X2 + X3) / 3
    })
    
    # Update membership dan hitung objective function
    wolf.other <- lapply(seq_len(nwolf), function(x)
      uij(data, wolf.swarm[[x]], m, distance, order))
    wolf.other <- lapply(seq_len(nwolf), function(x)
      renew_uij(data, wolf.other[[x]]$u, mi.mj, distmat, alpha, beta, a, b))
    wolf.swarm <- lapply(seq_len(nwolf), function(x)
      vi(data, wolf.other[[x]], m))
    wolf.fit   <- sapply(seq_len(nwolf), function(x)
      jfgwcv(data, wolf.swarm[[x]], m, distance, order))
    
    # Update Alpha, Beta, Delta dari generasi baru
    sorted_idx <- order(wolf.fit)
    cur_alpha_pos  <- wolf.swarm[[ sorted_idx[1] ]]
    cur_alpha_other <- wolf.other[[ sorted_idx[1] ]]
    cur_alpha_fit  <- wolf.fit[ sorted_idx[1] ]
    
    conv <- c(conv, alpha_fit)
    
    # Cek konvergensi
    if (abs(conv[iter + 1] - conv[iter]) < error) same <- same + 1
    else same <- 0
    
    # Update posisi Alpha, Beta, Delta jika ditemukan yang lebih baik
    if (cur_alpha_fit <= alpha_fit) {
      alpha_pos  <- cur_alpha_pos
      alpha_fit  <- cur_alpha_fit
      alpha_other <- cur_alpha_other
    }
    beta_pos  <- wolf.swarm[[ sorted_idx[2] ]]
    delta_pos <- wolf.swarm[[ sorted_idx[min(3, nwolf)] ]]
    
    randomN <- randomN + nwolf
    
    if (iter == max.iter || same == wolf.same) break
  }
  
  # ── Hasil final ───────────────────────────────────────────────────────────
  finaldata <- determine_cluster(datax, alpha_other)
  cluster   <- finaldata[, ncol(finaldata)]
  
  print(c(order, ncluster, m, randomN))
  
  gwo <- list(
    "converg"    = conv,
    "f_obj"      = jfgwcv(data, alpha_pos, m, distance, order),
    "membership" = alpha_other,
    "centroid"   = alpha_pos,
    "validation" = index_fgwc(data, cluster, alpha_other, alpha_pos,
                              m, exp(1)),
    "cluster"    = cluster,
    "finaldata"  = finaldata,
    "call"       = match.call(),
    "iteration"  = iter,
    "same"       = same,
    "time"       = proc.time() - ptm
  )
  class(gwo) <- 'fgwc'
  return(gwo)
}