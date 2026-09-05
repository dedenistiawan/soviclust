# =============================================================================
# Cluster validity indices for FGWC-family algorithms
#
# Portions of the original implementation were adapted from the GPL-3-licensed
# naspaclust package and subsequently corrected/extended in soviclust.
# =============================================================================

index_fgwc <- function(data, cluster, uij, vi, m, a = exp(1)) {
  list(
    PC = PC1(uij),
    CE = CE1(uij, a),
    SC = SC1(data, cluster, uij, vi, m),
    SI = SI1(data, uij, vi),
    XB = XB1(data, uij, vi, m),
    IFV = IFV1(data, uij, vi, m),
    Kwon = Kwon1(data, uij, vi, m)
  )
}


# Partition Coefficient: larger is better.
PC1 <- function(uij) {
  uij <- as.matrix(uij)
  sum(uij^2) / nrow(uij)
}


# Classification Entropy: smaller is better.
# Zero memberships contribute zero to u * log(u).
CE1 <- function(uij, a = exp(1)) {
  uij <- as.matrix(uij)

  if (!is.numeric(a) || length(a) != 1L || !is.finite(a) ||
      a <= 0 || a == 1) {
    stop("`a` must be a finite logarithm base > 0 and != 1.", call. = FALSE)
  }

  positive <- uij > 0
  if (!any(positive)) {
    return(0)
  }

  -sum(uij[positive] * log(uij[positive], base = a)) / nrow(uij)
}


# Separation Coefficient / Partition Index: smaller is better.
SC1 <- function(data, cluster, uij, vi, m) {
  data <- as.matrix(data)
  uij <- as.matrix(uij)
  vi <- as.matrix(vi)

  d <- matrix(0, nrow(data), nrow(vi))
  for (i in seq_len(nrow(data))) {
    for (j in seq_len(nrow(vi))) {
      d[i, j] <- sum((vi[j, ] - data[i, ])^2)
    }
  }

  pt1 <- colSums((uij^m) * d)

  vkvi <- matrix(0, nrow(vi), nrow(vi))
  for (i in seq_len(nrow(vi))) {
    for (k in seq_len(nrow(vi))) {
      vkvi[i, k] <- sum((vi[i, ] - vi[k, ])^2)
    }

    Ni <- sum(cluster == i)
    vkvi[i, ] <- Ni * vkvi[i, ]
  }

  pt2 <- colSums(vkvi)

  # Undefined separation (e.g., empty/duplicated clusters) is penalized.
  if (any(!is.finite(pt2)) || any(pt2 <= 0)) {
    return(Inf)
  }

  sum(pt1 / pt2)
}


# Separation Index: smaller is better.
SI1 <- function(data, uij, vi) {
  data <- as.matrix(data)
  uij <- as.matrix(uij)
  vi <- as.matrix(vi)

  d <- matrix(0, nrow(data), nrow(vi))
  for (i in seq_len(nrow(data))) {
    for (j in seq_len(nrow(vi))) {
      d[i, j] <- sum((vi[j, ] - data[i, ])^2)
    }
  }

  vkvi <- as.matrix(stats::dist(vi))^2
  diag(vkvi) <- Inf

  min_sep <- min(vkvi)
  if (!is.finite(min_sep) || min_sep <= 0) {
    return(Inf)
  }

  sum((uij^2) * d) / (nrow(data) * min_sep)
}


# Xie-Beni Index: smaller is better.
#
# XB = sum_i sum_k u_ik^m ||x_i-v_k||^2 /
#      (n * min_{k != h} ||v_k-v_h||^2)
XB1 <- function(data, uij, vi, m) {
  data <- as.matrix(data)
  uij <- as.matrix(uij)
  vi <- as.matrix(vi)

  d <- matrix(0, nrow(data), nrow(vi))
  for (i in seq_len(nrow(data))) {
    for (j in seq_len(nrow(vi))) {
      d[i, j] <- sum((vi[j, ] - data[i, ])^2)
    }
  }

  centroid_dist <- as.matrix(stats::dist(vi))^2
  diag(centroid_dist) <- Inf

  min_sep <- min(centroid_dist)

  if (!is.finite(min_sep) || min_sep <= 0) {
    return(Inf)
  }

  numerator <- sum((uij^m) * d)
  numerator / (nrow(data) * min_sep)
}


# IFV: larger is better.
#
# A very small positive floor is used only inside log() so exact zero
# memberships do not produce -Inf/NaN.
IFV1 <- function(data, uij, vi, m) {
  data <- as.matrix(data)
  uij <- as.matrix(uij)
  vi <- as.matrix(vi)

  vkvi <- as.matrix(stats::dist(vi))^2

  d <- matrix(0, nrow(data), nrow(vi))
  for (i in seq_len(nrow(data))) {
    for (j in seq_len(nrow(vi))) {
      d[i, j] <- sum((vi[j, ] - data[i, ])^2)
    }
  }

  sigmaD <- mean(d)
  SDmax <- max(vkvi)

  if (!is.finite(sigmaD) || sigmaD <= 0 ||
      !is.finite(SDmax) || SDmax <= 0) {
    return(NA_real_)
  }

  u_safe <- pmax(uij, .Machine$double.eps)
  log2u <- colSums(log(u_safe, base = 2)) / nrow(data)
  u2ij <- colSums(uij^2)

  sum(
    u2ij *
      ((log(nrow(vi), base = 2) - log2u)^2) /
      nrow(data) *
      (SDmax / sigmaD)
  )
}


# Kwon Index: smaller is better.
#
# Kwon = [sum_i sum_k u_ik^m ||x_i-v_k||^2
#         + (1/c) sum_k ||v_k - x_bar||^2] /
#        min_{k != h} ||v_k-v_h||^2
Kwon1 <- function(data, uij, vi, m) {
  data <- as.matrix(data)
  uij <- as.matrix(uij)
  vi <- as.matrix(vi)

  d <- matrix(0, nrow(data), nrow(vi))
  for (i in seq_len(nrow(data))) {
    for (j in seq_len(nrow(vi))) {
      d[i, j] <- sum((vi[j, ] - data[i, ])^2)
    }
  }

  global_mean <- colMeans(data)
  centroid_penalty <- vapply(
    seq_len(nrow(vi)),
    function(j) sum((vi[j, ] - global_mean)^2),
    numeric(1)
  )

  centroid_dist <- as.matrix(stats::dist(vi))^2
  diag(centroid_dist) <- Inf
  min_sep <- min(centroid_dist)

  if (!is.finite(min_sep) || min_sep <= 0) {
    return(Inf)
  }

  compactness <- sum((uij^m) * d)
  penalty <- sum(centroid_penalty) / nrow(vi)

  (compactness + penalty) / min_sep
}
