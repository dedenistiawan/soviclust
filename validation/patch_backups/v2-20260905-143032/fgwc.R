# =============================================================================
# Classical Fuzzy Geographically Weighted Clustering (FGWC)
#
# Original FGWC-related source adapted from the GPL-3-licensed naspaclust
# package. Numerical corrections and robustness improvements were added in
# soviclust.
# =============================================================================

#' Classical Fuzzy Geographically Weighted Clustering
#'
#' @description
#' Fuzzy clustering with spatial configuration of the membership matrix.
#'
#' @param data Numeric matrix/data.frame.
#' @param pop Population vector.
#' @param distmat Inter-region distance matrix.
#' @param kind `"u"` for membership approach or `"v"` for centroid approach.
#' @param ncluster Number of clusters.
#' @param m Fuzzifier; must be greater than 1.
#' @param distance Distance metric.
#' @param order Minkowski order.
#' @param alpha Weight of the original membership in classical FGWC.
#' @param a Spatial distance exponent.
#' @param b Population exponent.
#' @param max.iter Maximum number of iterations.
#' @param error Convergence tolerance.
#' @param randomN Random seed.
#' @param uij Optional initial membership matrix.
#' @param vi Optional initial centroid matrix.
#'
#' @return An object of class `"fgwc"`.
#' @export
fgwcuv <- function(data, pop, distmat, kind = NA, ncluster = 2, m = 2,
                   distance = "euclidean", order = 2, alpha = 0.7,
                   a = 1, b = 1, max.iter = 500, error = 1e-5,
                   randomN = 0, uij = NA, vi = NA) {

  ptm <- proc.time()
  data <- as.matrix(data)

  if (!is.numeric(data) || any(!is.finite(data))) {
    stop("`data` must be a finite numeric matrix/data.frame.", call. = FALSE)
  }
  if (nrow(data) < 2L || ncol(data) < 1L) {
    stop("`data` must contain at least two rows and one column.", call. = FALSE)
  }
  if (!is.numeric(m) || length(m) != 1L || !is.finite(m) || m <= 1) {
    stop("`m` must be a finite number greater than 1.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L ||
      !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("`alpha` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(ncluster) || length(ncluster) != 1L ||
      ncluster < 2L || ncluster >= nrow(data)) {
    stop("`ncluster` must be >= 2 and smaller than the number of observations.",
         call. = FALSE)
  }

  if (is.na(kind)[1]) {
    kind <- "u"
  }
  if (!kind %in% c("u", "v")) {
    stop("`kind` must be either 'u' or 'v'.", call. = FALSE)
  }

  n <- nrow(data)
  beta <- 1 - alpha
  iter <- 0L
  conv <- numeric(0)

  if (alpha == 1) {
    pop <- rep(1, n)
    distmat <- matrix(1, n, n)
  }

  if (length(pop) != n || any(!is.finite(pop))) {
    stop("`pop` must contain one finite value per observation.", call. = FALSE)
  }

  distmat <- as.matrix(distmat)
  if (!all(dim(distmat) == c(n, n)) || any(is.na(distmat))) {
    stop("`distmat` must be an n x n matrix without missing values.",
         call. = FALSE)
  }

  pop <- matrix(pop, ncol = 1)
  mi.mj <- pop %*% t(pop)

  is_missing_init <- function(x) {
    length(x) == 1L && is.na(x)
  }

  if (kind == "u") {
    if (is_missing_init(uij)) {
      new_uij <- gen_uij(data, ncluster, n, randomN)
    } else {
      new_uij <- as.matrix(uij)
      if (!all(dim(new_uij) == c(n, ncluster))) {
        stop("Initial `uij` has incompatible dimensions.", call. = FALSE)
      }
      new_uij <- normalize_membership(new_uij)
    }

    old_uij <- new_uij + 1

    while (max(abs(new_uij - old_uij)) > error && iter < max.iter) {
      old_uij <- new_uij

      centers_old <- centroid_from_membership(data, old_uij, m)
      base_u <- membership_from_centroids(
        data, centers_old, m, distance, order
      )

      new_uij <- renew_uij(
        data, base_u$u, mi.mj, distmat, alpha, beta, a, b
      )

      centers_new <- centroid_from_membership(data, new_uij, m)

      conv <- c(
        conv,
        fgwc_objective(data, new_uij, centers_new, m, distance, order)
      )

      iter <- iter + 1L
    }

    centers <- centroid_from_membership(data, new_uij, m)

  } else {
    if (is_missing_init(vi)) {
      centers <- gen_vi(data, ncluster, "uniform", randomN)
    } else {
      centers <- as.matrix(vi)
      if (!all(dim(centers) == c(ncluster, ncol(data)))) {
        stop("Initial `vi` has incompatible dimensions.", call. = FALSE)
      }
    }

    centers <- clamp_centroids(centers, data)
    centers_old <- centers + 1

    while (centroid_change(centers, centers_old) > error &&
           iter < max.iter) {

      centers_old <- centers

      base_u <- membership_from_centroids(
        data, centers_old, m, distance, order
      )
      new_uij <- renew_uij(
        data, base_u$u, mi.mj, distmat, alpha, beta, a, b
      )
      centers <- centroid_from_membership(data, new_uij, m)

      conv <- c(
        conv,
        fgwc_objective(data, new_uij, centers, m, distance, order)
      )

      iter <- iter + 1L
    }
  }

  fgwc_obj <- fgwc_objective(
    data, new_uij, centers, m, distance, order
  )

  finaldata <- determine_cluster(data, new_uij)
  cluster <- finaldata[, ncol(finaldata)]

  result <- list(
    converg = conv,
    f_obj = fgwc_obj,
    membership = new_uij,
    centroid = centers,
    validation = index_fgwc(
      data, cluster, new_uij, centers, m, exp(1)
    ),
    iteration = iter,
    cluster = cluster,
    finaldata = finaldata,
    call = match.call(),
    time = proc.time() - ptm
  )

  class(result) <- "fgwc"
  result
}


# -----------------------------------------------------------------------------
# Numerical helpers
# -----------------------------------------------------------------------------

normalize_membership <- function(u) {
  u <- as.matrix(u)
  u[!is.finite(u) | u < 0] <- 0

  rs <- rowSums(u)
  bad <- !is.finite(rs) | rs <= 0

  if (any(bad)) {
    u[bad, ] <- 1 / ncol(u)
    rs <- rowSums(u)
  }

  u / rs
}


centroid_from_membership <- function(data, uij, m) {
  data <- as.matrix(data)
  uij <- normalize_membership(uij)

  weights <- uij^m
  den <- colSums(weights)

  if (any(!is.finite(den)) || any(den <= 0)) {
    stop("Cannot compute centroid because a cluster has zero fuzzy mass.",
         call. = FALSE)
  }

  t(weights) %*% data / den
}


# Backward-compatible helper name used by the optimizer source files.
vi <- function(data, uij, m) {
  centroid_from_membership(data, uij, m)
}


membership_from_centroids <- function(data, centers, m,
                                      distance = "euclidean", order = 2) {
  data <- as.matrix(data)
  centers <- as.matrix(centers)

  if (m <= 1) {
    stop("`m` must be greater than 1.", call. = FALSE)
  }

  d <- rdist::cdist(data, centers, distance, order)^2
  u <- matrix(0, nrow(data), nrow(centers))

  tol <- sqrt(.Machine$double.eps)

  for (i in seq_len(nrow(d))) {
    zero_idx <- which(d[i, ] <= tol)

    if (length(zero_idx) > 0L) {
      # If an observation coincides with multiple identical centroids,
      # split membership equally among those centroids.
      u[i, zero_idx] <- 1 / length(zero_idx)
    } else {
      w <- d[i, ]^(-1 / (m - 1))
      u[i, ] <- w / sum(w)
    }
  }

  list(d = d, u = normalize_membership(u))
}


# Backward-compatible helper name used throughout the existing code.
uij <- function(data, vi, m, distance, order = 2) {
  membership_from_centroids(data, vi, m, distance, order)
}


determine_cluster <- function(data, uij) {
  clust <- apply(uij, 1, which.max)
  cbind.data.frame(data, cluster = clust)
}


renew_uij <- function(data, old_uij, mi.mj, dist,
                      alpha, beta, a, b) {
  old_uij <- normalize_membership(old_uij)
  dist <- as.matrix(dist)
  mi.mj <- as.matrix(mi.mj)

  diag(dist) <- Inf

  wij <- (mi.mj^b) / (dist^a)
  wij[!is.finite(wij)] <- 0
  diag(wij) <- 0

  wijmuj <- wij %*% old_uij
  A <- rowSums(wijmuj)

  neighbor_u <- matrix(0, nrow(old_uij), ncol(old_uij))
  valid <- is.finite(A) & A > 0

  if (any(valid)) {
    neighbor_u[valid, ] <- wijmuj[valid, , drop = FALSE] / A[valid]
  }

  # If a row has no usable spatial interaction, retain its own membership.
  if (any(!valid)) {
    neighbor_u[!valid, ] <- old_uij[!valid, , drop = FALSE]
  }

  new_uij <- alpha * old_uij + beta * neighbor_u
  normalize_membership(new_uij)
}


gen_uij <- function(data, ncluster, n, randomN) {
  set.seed(randomN)
  u <- matrix(runif(ncluster * n, 0, 1), n, ncluster)
  normalize_membership(u)
}


gen_vi <- function(data, ncluster, gendist, randomN) {
  data <- as.matrix(data)
  p <- ncol(data)
  centers <- matrix(0, ncluster, p)

  set.seed(randomN)

  for (i in seq_len(p)) {
    if (gendist == "normal") {
      s <- stats::sd(data[, i])
      if (!is.finite(s) || s == 0) s <- 1e-8
      centers[, i] <- stats::rnorm(
        ncluster, mean(data[, i]), s
      )
    } else if (gendist == "uniform") {
      centers[, i] <- stats::runif(
        ncluster, min(data[, i]), max(data[, i])
      )
    } else {
      stop("`gendist` must be 'uniform' or 'normal'.", call. = FALSE)
    }
  }

  clamp_centroids(centers, data)
}


clamp_centroids <- function(centers, data) {
  centers <- as.matrix(centers)
  data <- as.matrix(data)

  lower <- apply(data, 2, min)
  upper <- apply(data, 2, max)

  for (j in seq_len(ncol(centers))) {
    centers[, j] <- pmin(pmax(centers[, j], lower[j]), upper[j])
  }

  centers
}


centroid_change <- function(new_centers, old_centers) {
  sqrt(sum((new_centers - old_centers)^2))
}


fgwc_objective <- function(data, uij, centers, m,
                           distance = "euclidean", order = 2) {
  data <- as.matrix(data)
  uij <- as.matrix(uij)
  centers <- as.matrix(centers)

  d <- rdist::cdist(data, centers, distance, order)^2
  value <- sum((uij^m) * d)

  if (!is.finite(value)) Inf else value
}


# -----------------------------------------------------------------------------
# Objective helpers retained for backward compatibility
# -----------------------------------------------------------------------------

jfgwcu <- function(data, uij, m, distance, order) {
  centers <- centroid_from_membership(data, uij, m)
  fgwc_objective(data, uij, centers, m, distance, order)
}


jfgwcu2 <- function(data, uij, m, distance, order,
                    mi.mj, dist, alpha, beta, a, b) {
  spatial_u <- renew_uij(
    data, uij, mi.mj, dist, alpha, beta, a, b
  )
  centers <- centroid_from_membership(data, spatial_u, m)
  fgwc_objective(
    data, spatial_u, centers, m, distance, order
  )
}


# Non-spatial FCM objective for a supplied centroid matrix.
jfgwcv <- function(data, vi, m, distance, order) {
  base_u <- membership_from_centroids(data, vi, m, distance, order)
  fgwc_objective(data, base_u$u, vi, m, distance, order)
}


# Spatial FGWC objective for a supplied candidate centroid matrix.
jfgwcv2 <- function(data, vi, m, distance, order,
                    mi.mj, dist, alpha, beta, a, b) {
  base_u <- membership_from_centroids(data, vi, m, distance, order)

  spatial_u <- renew_uij(
    data, base_u$u, mi.mj, dist, alpha, beta, a, b
  )

  centers <- centroid_from_membership(data, spatial_u, m)

  fgwc_objective(
    data, spatial_u, centers, m, distance, order
  )
}
