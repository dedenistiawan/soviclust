# =============================================================================
# FGWC with Whale Optimization Algorithm (WOA)
#
# WOA: Mirjalili & Lewis (2016)
#
# Canonical phase logic implemented here:
#   p < 0.5 and |A| < 1 -> encircle best prey
#   p < 0.5 and |A| >= 1 -> search around a random whale
#   p >= 0.5            -> logarithmic spiral around best prey
#
# Numerical corrections in soviclust:
#   - encircling update is based on the prey/best position;
#   - phase selection follows the canonical WOA pseudocode;
#   - scalar A/C per search agent avoids ambiguous mean(A) branching;
#   - global-best convergence is recorded AFTER best-solution update;
#   - fitness uses the spatial FGWC membership/centroid pair;
#   - candidate centroids are constrained to observed feature bounds.
# =============================================================================

#' Fuzzy Geographically Weighted Clustering with Whale Optimization Algorithm
#'
#' @export
woafgwc <- function(data, pop = NA, distmat = NA,
                    ncluster = 2, m = 2,
                    distance = "euclidean", order = 2,
                    alpha = 0.7, a = 1, b = 1,
                    error = 1e-5, max.iter = 100,
                    randomN = 0, vi.dist = "uniform",
                    nwhale = 10, woa.b = 1, woa.same = 10) {

  ptm <- proc.time()
  data <- as.matrix(data)
  n <- nrow(data)

  if (m <= 1) {
    stop("`m` must be greater than 1.", call. = FALSE)
  }
  if (ncluster < 2L || ncluster >= n) {
    stop("`ncluster` must be >= 2 and smaller than n.", call. = FALSE)
  }
  if (nwhale < 2L) {
    stop("WOA requires at least 2 whales.", call. = FALSE)
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

  whale <- init.swarm(
    data, mi.mj, distmat, distance, order, vi.dist,
    ncluster, m, alpha, a, b, randomN, nwhale
  )

  wh.swarm <- lapply(whale$centroid, clamp_centroids, data = data)
  wh.other <- whale$membership

  wh.fit <- vapply(
    seq_len(nwhale),
    function(i) {
      fgwc_objective(
        data, wh.other[[i]], wh.swarm[[i]], m, distance, order
      )
    },
    numeric(1)
  )

  best.idx <- which.min(wh.fit)
  wh.finalpos <- wh.swarm[[best.idx]]
  wh.finalpos.other <- wh.other[[best.idx]]
  wh.fit.finalbest <- wh.fit[best.idx]

  conv <- wh.fit.finalbest
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    if (max.iter == 1L) {
      a_coef <- 0
    } else {
      a_coef <- 2 - 2 * ((iter - 1) / (max.iter - 1))
    }

    wh.swarm <- woa.move(
      swarm = wh.swarm,
      prey = wh.finalpos,
      a_coef = a_coef,
      b = woa.b,
      nwhale = nwhale,
      seed = randomN,
      iter = iter,
      data = data
    )

    base_membership <- lapply(
      seq_len(nwhale),
      function(i) {
        membership_from_centroids(
          data, wh.swarm[[i]], m, distance, order
        )$u
      }
    )

    wh.other <- lapply(
      seq_len(nwhale),
      function(i) {
        renew_uij(
          data, base_membership[[i]], mi.mj, distmat,
          alpha, beta, a, b
        )
      }
    )

    wh.swarm <- lapply(
      seq_len(nwhale),
      function(i) centroid_from_membership(data, wh.other[[i]], m)
    )

    wh.fit <- vapply(
      seq_len(nwhale),
      function(i) {
        fgwc_objective(
          data, wh.other[[i]], wh.swarm[[i]], m, distance, order
        )
      },
      numeric(1)
    )

    best.idx <- which.min(wh.fit)
    wh.curbest <- wh.swarm[[best.idx]]
    wh.curbest.oth <- wh.other[[best.idx]]
    wh.fit.curbest <- wh.fit[best.idx]

    # Update global best BEFORE recording convergence.
    if (wh.fit.curbest < wh.fit.finalbest) {
      wh.finalpos <- wh.curbest
      wh.finalpos.other <- wh.curbest.oth
      wh.fit.finalbest <- wh.fit.curbest
    }

    conv <- c(conv, wh.fit.finalbest)

    if (abs(conv[length(conv)] - conv[length(conv) - 1L]) < error) {
      same <- same + 1L
    } else {
      same <- 0L
    }

    if (same >= woa.same) {
      break
    }
  }

  finaldata <- determine_cluster(data, wh.finalpos.other)
  cluster <- finaldata[, ncol(finaldata)]

  result <- list(
    converg = conv,
    f_obj = wh.fit.finalbest,
    membership = wh.finalpos.other,
    centroid = wh.finalpos,
    validation = index_fgwc(
      data, cluster, wh.finalpos.other, wh.finalpos, m, exp(1)
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


woa.move <- function(swarm, prey, a_coef, b, nwhale, seed, iter,
                     data = NULL) {

  if (length(swarm) != nwhale) {
    stop("`nwhale` must match the length of `swarm`.", call. = FALSE)
  }

  lapply(seq_len(nwhale), function(i) {
    set.seed(seed + iter * 10000L + i)

    # Canonical WOA uses random coefficient vectors. Here scalar coefficients
    # are intentionally used for each search agent so the |A| phase decision
    # is unambiguous and the same coefficient applies across centroid
    # dimensions for that agent.
    r1 <- runif(1)
    r2 <- runif(1)
    p <- runif(1)

    A <- 2 * a_coef * r1 - a_coef
    C <- 2 * r2

    current <- swarm[[i]]

    if (p < 0.5) {
      if (abs(A) < 1) {
        # Encircling prey:
        # D = |C * X* - X|
        # X(t+1) = X* - A * D
        D <- abs(C * prey - current)
        new_pos <- prey - A * D

      } else {
        # Search for prey using a random whale.
        candidates <- setdiff(seq_len(nwhale), i)
        rand_idx <- sample(candidates, 1)
        rand_whale <- swarm[[rand_idx]]

        D <- abs(C * rand_whale - current)
        new_pos <- rand_whale - A * D
      }

    } else {
      # Bubble-net logarithmic spiral:
      # X(t+1) = D' * exp(b*l) * cos(2*pi*l) + X*
      l <- runif(1, -1, 1)
      D_prey <- abs(prey - current)

      new_pos <- D_prey * exp(b * l) * cos(2 * pi * l) + prey
    }

    if (!is.null(data)) {
      new_pos <- clamp_centroids(new_pos, data)
    }

    new_pos
  })
}
