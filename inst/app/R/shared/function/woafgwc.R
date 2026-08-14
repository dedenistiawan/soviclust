#' Fuzzy Geographically Weighted Clustering with Whale Optimization Algorithm
#' @description Fuzzy clustering with addition of spatial configuration of membership matrix
#'   with centroid optimization using Whale Optimization Algorithm (WOA).
#' @param data an object of data with d>1. Can be \code{matrix} or \code{data.frame}.
#'   If your data is univariate, bind it with \code{1} to get a 2 columns.
#' @param pop an n*1 vector contains population.
#' @param distmat an n*n distance matrix between regions.
#' @param ncluster an integer. The number of clusters.
#' @param m degree of fuzziness or fuzzifier. Default is 2.
#' @param distance the distance metric between data and centroid, the default is euclidean,
#'   see \code{\link{cdist}} for details.
#' @param order minkowski order. Default is 2.
#' @param alpha the old membership effect with [0,1], if \code{alpha} equals 1, it will be
#'   same as fuzzy C-Means, if 0, it equals to neighborhood effect.
#' @param a spatial magnitude of distance. Default is 1.
#' @param b spatial magnitude of population. Default is 1.
#' @param max.iter maximum iteration. Default is 500.
#' @param error error tolerance. Default is 1e-5.
#' @param randomN random seed for initialisation. Default is 0.
#' @param vi.dist a string of centroid population distribution between \code{'uniform'}
#'   (default) and \code{'normal'}. Can be defined as \code{vi.dist=} in \code{opt_param}.
#' @param nwhale number of whales (search agents). Can be defined as \code{npar=} in
#'   \code{opt_param}. Default is 10.
#' @param woa.b spiral shape constant. Controls the shape of the logarithmic spiral.
#'   Can be defined as \code{b=} in \code{opt_param}. Default is 1.
#' @param woa.same number of consecutive unchanged iterations to stop. Can be defined
#'   as \code{same=} in \code{opt_param}. Default is 10.
#'
#' @return an object of class \code{'fgwc'}.\cr
#' An \code{'fgwc'} object contains as follows:
#' \itemize{
#' \item \code{converg}    - the process convergence of objective function
#' \item \code{f_obj}      - objective function value
#' \item \code{membership} - membership matrix
#' \item \code{centroid}   - centroid matrix
#' \item \code{validation} - validation indices (PC, CE, SC, SI, XB, IFV, Kwon)
#' \item \code{cluster}    - the cluster of each data point
#' \item \code{finaldata}  - the final data (with cluster column)
#' \item \code{call}       - the syntax called previously
#' \item \code{iteration}  - number of iterations executed
#' \item \code{same}       - number of consecutive unchanged iterations at stop
#' \item \code{time}       - computational time
#' }
#'
#' @details
#' Fuzzy Geographically Weighted Clustering (FGWC) was developed by Mason and Jacobson
#' (2007) by adding neighborhood effects and population to configure the membership matrix
#' in Fuzzy C-Means.
#'
#' The Whale Optimization Algorithm (WOA) was introduced by Mirjalili and Lewis (2016).
#' It mimics the bubble-net hunting strategy of humpback whales. The algorithm has three
#' main phases:
#' \itemize{
#'   \item \strong{Encircling prey} - whales identify the best current solution and update
#'     positions relative to it using a shrinking coefficient \code{A}.
#'   \item \strong{Bubble-net attack} - a logarithmic spiral movement around the best
#'     solution, controlled by constant \code{b} and random parameter \code{l}.
#'   \item \strong{Search for prey} - when |A| >= 1, whales move toward a randomly chosen
#'     whale instead of the best, enabling global exploration.
#' }
#' The balance between exploration and exploitation is governed by parameter \code{a},
#' which linearly decreases from 2 to 0 over the iterations.
#'
#' @references
#' Mirjalili, S., & Lewis, A. (2016). The whale optimization algorithm.
#' \emph{Advances in Engineering Software}, 95, 51-67.
#'
#' @seealso \code{\link{psofgwc}} \code{\link{gsafgwc}} \code{\link{fpafgwc}}
#'
#' @examples
#' data('census2010')
#' data('census2010dist')
#' data('census2010pop')
#' # First way - direct call
#' res1 <- woafgwc(census2010, census2010pop, census2010dist, 3, 2,
#'                 'euclidean', 4, nwhale=10)
#' # Second way - via fgwc() wrapper
#' param_fgwc <- c(kind='v', ncluster=3, m=2, distance='minkowski', order=3,
#'                 alpha=0.5, a=1.2, b=1.2, max.iter=1000, error=1e-6, randomN=0)
#' woa_param  <- c(vi.dist='normal', npar=10, same=15, woa.b=1)
#' res2 <- fgwc(census2010, census2010pop, census2010dist, 'woa', param_fgwc, woa_param)
#'
#' @export
woafgwc <- function(data, pop = NA, distmat = NA,
                    ncluster = 2, m = 2,
                    distance = 'euclidean', order = 2,
                    alpha = 0.7, a = 1, b = 1,
                    error = 1e-5, max.iter = 100,
                    randomN = 0, vi.dist = "uniform",
                    nwhale = 10, woa.b = 1, woa.same = 10) {
  
  ptm  <- proc.time()
  n    <- nrow(data)
  d    <- ncol(data)
  iter <- 0
  beta <- 1 - alpha
  same <- 0
  data <- as.matrix(data)
  
  # Jika alpha = 1, bobot spasial diabaikan (sama dengan FCM biasa)
  if (alpha == 1) {
    pop     <- rep(1, n)
    distmat <- matrix(1, n, n)
  }
  
  datax  <- data
  pop    <- matrix(pop, ncol = 1)
  mi.mj  <- pop %*% t(pop)
  
  # ── Inisialisasi swarm ───────────────────────────────────────────────────────
  whale       <- init.swarm(data, mi.mj, distmat, distance, order,
                            vi.dist, ncluster, m, alpha, a, b, randomN, nwhale)
  wh.swarm    <- whale$centroid    # list of nwhale centroid matrices
  wh.other    <- whale$membership  # list of nwhale membership matrices
  wh.fit      <- whale$I           # numeric vector of objective values
  
  best.idx            <- which.min(wh.fit)
  wh.finalpos         <- wh.swarm[[best.idx]]
  wh.finalpos.other   <- wh.other[[best.idx]]
  wh.fit.finalbest    <- wh.fit[[best.idx]]
  
  conv <- c(wh.fit.finalbest)
  
  # ── Loop utama WOA ───────────────────────────────────────────────────────────
  repeat {
    
    # Koefisien a menurun linier dari 2 ke 0
    a_coef <- 2 * (1 - iter / max.iter)
    
    wh.swarm <- woa.move(wh.swarm, wh.finalpos, a_coef, woa.b,
                         nwhale, randomN, iter)
    
    # Update membership, bobot spasial, dan centroid
    wh.other <- lapply(seq_len(nwhale), function(x)
      uij(data, wh.swarm[[x]], m, distance, order))
    wh.other <- lapply(seq_len(nwhale), function(x)
      renew_uij(data, wh.other[[x]]$u, mi.mj, distmat, alpha, beta, a, b))
    wh.swarm <- lapply(seq_len(nwhale), function(x)
      vi(data, wh.other[[x]], m))
    
    # Hitung objective function semua paus
    wh.fit <- sapply(seq_len(nwhale), function(x)
      jfgwcv(data, wh.swarm[[x]], m, distance, order))
    
    # Perbarui solusi terbaik global
    best.idx       <- which.min(wh.fit)
    wh.curbest     <- wh.swarm[[best.idx]]
    wh.curbest.oth <- wh.other[[best.idx]]
    wh.fit.curbest <- wh.fit[[best.idx]]
    
    conv <- c(conv, wh.fit.finalbest)
    iter <- iter + 1
    
    if (abs(conv[iter + 1] - conv[iter]) < error) same <- same + 1
    else same <- 0
    
    if (wh.fit.curbest <= wh.fit.finalbest) {
      wh.finalpos       <- wh.curbest
      wh.finalpos.other <- wh.curbest.oth
      wh.fit.finalbest  <- wh.fit.curbest
    }
    
    randomN <- randomN + nwhale
    if (iter == max.iter || same == woa.same) break
  }
  
  # ── Output ───────────────────────────────────────────────────────────────────
  finaldata <- determine_cluster(datax, wh.finalpos.other)
  cluster   <- finaldata[, ncol(finaldata)]
  
  print(c(order, ncluster, m, randomN))
  
  woa <- list(
    "converg"    = conv,
    "f_obj"      = jfgwcv(data, wh.finalpos, m, distance, order),
    "membership" = wh.finalpos.other,
    "centroid"   = wh.finalpos,
    "validation" = index_fgwc(data, cluster, wh.finalpos.other,
                              wh.finalpos, m, exp(1)),
    "cluster"    = cluster,
    "finaldata"  = finaldata,
    "call"       = match.call(),
    "iteration"  = iter,
    "same"       = same,
    "time"       = proc.time() - ptm
  )
  class(woa) <- 'fgwc'
  return(woa)
}


# ── Fungsi pembaruan posisi paus (inti mekanisme WOA) ──────────────────────────
#
# Setiap paus memilih satu dari tiga perilaku:
#   (1) Encircling prey   : |A| < 1  AND  p < 0.5
#   (2) Bubble-net spiral : |A| < 1  AND  p >= 0.5
#   (3) Search for prey   : |A| >= 1  (eksplorasi acak)
#
# Parameter:
#   swarm   - list posisi paus saat ini (list of matrices ncluster x d)
#   prey    - posisi terbaik global (matrix ncluster x d)
#   a_coef  - nilai koefisien a saat ini (menurun 2 -> 0)
#   b       - konstanta spiral logaritmik
#   nwhale  - jumlah paus
#   seed    - random seed
#   iter    - iterasi saat ini (untuk reprodusibilitas seed)
#
woa.move <- function(swarm, prey, a_coef, b, nwhale, seed, iter) {
  dd <- dim(prey)   # c(ncluster, d)
  
  lapply(seq_len(nwhale), function(i) {
    set.seed(seed + i + iter * nwhale)
    r1 <- matrix(runif(dd[1] * dd[2]), ncol = dd[2])   # [0,1]
    r2 <- matrix(runif(dd[1] * dd[2]), ncol = dd[2])   # [0,1]
    p  <- runif(1)   # probabilitas pemilihan fase
    
    # Koefisien A dan C
    A <- 2 * a_coef * r1 - a_coef   # rentang [-a, a]
    C <- 2 * r2                       # rentang [0, 2]
    
    if (abs(mean(A)) < 1) {
      # ── Fase eksploitasi ──────────────────────────────────────────────────
      if (p < 0.5) {
        # (1) Encircling prey: gerak lurus menuju prey terbaik
        D <- abs(C * prey - swarm[[i]])
        swarm[[i]] - A * D
      } else {
        # (2) Bubble-net spiral: gerakan spiral logaritmik
        set.seed(seed + i + iter * nwhale + 1)
        l  <- matrix(runif(dd[1] * dd[2], -1, 1), ncol = dd[2])
        D_prey <- abs(prey - swarm[[i]])
        prey + D_prey * exp(b * l) * cos(2 * pi * l)
      }
    } else {
      # ── Fase eksplorasi ───────────────────────────────────────────────────
      # (3) Search for prey: pilih paus acak sebagai referensi
      rand_idx  <- sample(seq_len(nwhale)[-i], 1)
      rand_wh   <- swarm[[rand_idx]]
      D_rand    <- abs(C * rand_wh - swarm[[i]])
      rand_wh - A * D_rand
    }
  })
}
