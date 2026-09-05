# =============================================================================
# FGWC with Grey Wolf Optimizer (GWO)
#
# GWO: Mirjalili, Mirjalili & Lewis (2014)
# Numerical corrections in soviclust:
#   - global-best convergence is recorded AFTER best-solution update;
#   - fitness uses the spatial FGWC membership/centroid pair;
#   - centroid positions are constrained to observed feature bounds;
#   - canonical Alpha/Beta/Delta hierarchy is retained across iterations.
# =============================================================================

#' Fuzzy Geographically Weighted Clustering with Grey Wolf Optimizer
#'
#' @export
gwofgwc <- function(data, pop = NA, distmat = NA, ncluster = 2, m = 2,
                    distance = "euclidean", order = 2, alpha = 0.7,
                    a = 1, b = 1, error = 1e-5, max.iter = 100,
                    randomN = 0, vi.dist = "uniform", nwolf = 10,
                    wolf.same = 10) {

  ptm <- proc.time()
  data <- as.matrix(data)
  n <- nrow(data)

  if (m <= 1) {
    stop("`m` must be greater than 1.", call. = FALSE)
  }
  if (ncluster < 2L || ncluster >= n) {
    stop("`ncluster` must be >= 2 and smaller than n.", call. = FALSE)
  }
  if (nwolf < 3L) {
    stop("GWO requires at least 3 wolves.", call. = FALSE)
  }
  if (max.iter < 1L) {
    stop("`max.iter` must be >= 1.", call. = FALSE)
  }

  beta <- 1 - alpha
  same <- 0L

  if (alpha == 1) {
    pop <- rep(1, n)
    distmat <- matrix(1, n, n)
  }

  pop <- matrix(pop, ncol = 1)
  mi.mj <- pop %*% t(pop)

  # Initial population. init.swarm() is retained for compatibility with
  # existing optimizer infrastructure, but fitness is recalculated using the
  # spatial membership/centroid pair.
  wolf <- init.swarm(
    data, mi.mj, distmat, distance, order, vi.dist,
    ncluster, m, alpha, a, b, randomN, nwolf
  )

  wolf.swarm <- lapply(wolf$centroid, clamp_centroids, data = data)
  wolf.other <- wolf$membership

  wolf.fit <- vapply(
    seq_len(nwolf),
    function(i) {
      fgwc_objective(
        data, wolf.other[[i]], wolf.swarm[[i]], m, distance, order
      )
    },
    numeric(1)
  )

  sorted_idx <- order(wolf.fit)

  alpha_pos <- wolf.swarm[[sorted_idx[1]]]
  alpha_other <- wolf.other[[sorted_idx[1]]]
  alpha_fit <- wolf.fit[sorted_idx[1]]

  beta_pos <- wolf.swarm[[sorted_idx[2]]]
  beta_fit <- wolf.fit[sorted_idx[2]]

  delta_pos <- wolf.swarm[[sorted_idx[3]]]
  delta_fit <- wolf.fit[sorted_idx[3]]

  conv <- alpha_fit
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    # Linear schedule from 2 at the first iteration to 0 at the final one.
    if (max.iter == 1L) {
      a_coef <- 0
    } else {
      a_coef <- 2 - 2 * ((iter - 1) / (max.iter - 1))
    }

    current_swarm <- wolf.swarm

    wolf.swarm <- lapply(seq_len(nwolf), function(i) {
      set.seed(randomN + iter * 10000L + i)

      dd <- dim(alpha_pos)

      r1_a <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      r2_a <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      A1 <- 2 * a_coef * r1_a - a_coef
      C1 <- 2 * r2_a
      D_alpha <- abs(C1 * alpha_pos - current_swarm[[i]])
      X1 <- alpha_pos - A1 * D_alpha

      r1_b <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      r2_b <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      A2 <- 2 * a_coef * r1_b - a_coef
      C2 <- 2 * r2_b
      D_beta <- abs(C2 * beta_pos - current_swarm[[i]])
      X2 <- beta_pos - A2 * D_beta

      r1_d <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      r2_d <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      A3 <- 2 * a_coef * r1_d - a_coef
      C3 <- 2 * r2_d
      D_delta <- abs(C3 * delta_pos - current_swarm[[i]])
      X3 <- delta_pos - A3 * D_delta

      clamp_centroids((X1 + X2 + X3) / 3, data)
    })

    # FGWC projection: candidate centroid -> base membership -> spatial
    # membership -> spatially informed centroid.
    base_membership <- lapply(
      seq_len(nwolf),
      function(i) {
        membership_from_centroids(
          data, wolf.swarm[[i]], m, distance, order
        )$u
      }
    )

    wolf.other <- lapply(
      seq_len(nwolf),
      function(i) {
        renew_uij(
          data, base_membership[[i]], mi.mj, distmat,
          alpha, beta, a, b
        )
      }
    )

    wolf.swarm <- lapply(
      seq_len(nwolf),
      function(i) centroid_from_membership(data, wolf.other[[i]], m)
    )

    wolf.fit <- vapply(
      seq_len(nwolf),
      function(i) {
        fgwc_objective(
          data, wolf.other[[i]], wolf.swarm[[i]], m, distance, order
        )
      },
      numeric(1)
    )

    # Canonical GWO leader hierarchy. Leaders persist as best-so-far
    # solutions instead of being overwritten by worse candidates.
    for (i in order(wolf.fit)) {
      score <- wolf.fit[i]
      pos <- wolf.swarm[[i]]

      if (score < alpha_fit) {
        delta_fit <- beta_fit
        delta_pos <- beta_pos

        beta_fit <- alpha_fit
        beta_pos <- alpha_pos

        alpha_fit <- score
        alpha_pos <- pos
        alpha_other <- wolf.other[[i]]

      } else if (score < beta_fit && score >= alpha_fit) {
        delta_fit <- beta_fit
        delta_pos <- beta_pos

        beta_fit <- score
        beta_pos <- pos

      } else if (score < delta_fit && score >= beta_fit) {
        delta_fit <- score
        delta_pos <- pos
      }
    }

    # Record the UPDATED global best, then evaluate stagnation.
    conv <- c(conv, alpha_fit)

    if (abs(conv[length(conv)] - conv[length(conv) - 1L]) < error) {
      same <- same + 1L
    } else {
      same <- 0L
    }

    if (same >= wolf.same) {
      break
    }
  }

  finaldata <- determine_cluster(data, alpha_other)
  cluster <- finaldata[, ncol(finaldata)]

  result <- list(
    converg = conv,
    f_obj = alpha_fit,
    membership = alpha_other,
    centroid = alpha_pos,
    validation = index_fgwc(
      data, cluster, alpha_other, alpha_pos, m, exp(1)
    ),
    cluster = cluster,
    finaldata = finaldata,
    call = match.call(),
    iteration = iter_done,
    same = same,
    time = proc.time() - ptm
  )

  class(result) <- "fgwc"
  result
}
