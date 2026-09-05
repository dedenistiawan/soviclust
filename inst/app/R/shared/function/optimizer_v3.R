# =============================================================================
# soviclust — Optimizer Engine v3
# Canonical Optimizer & Spatial Fitness Architecture
#
# Design principles
# -----------------
# 1. Optimizer state is the RAW centroid position proposed by the metaheuristic.
# 2. FGWC spatial projection is used only to EVALUATE a raw search position.
# 3. Candidate fitness is separation-aware spatial Xie-Beni (XB).
# 4. A candidate is feasible only if all requested hard clusters are occupied.
# 5. Reported membership/centroid are the spatial FGWC solution, while
#    `search_centroid` stores the optimizer state that produced that solution.
#
# This file is sourced AFTER the legacy optimizer files and intentionally
# overrides only the public optimizer entry points. Legacy source remains in the
# repository for provenance, attribution, and rollback.
# =============================================================================


# -----------------------------------------------------------------------------
# Shared validation/context helpers
# -----------------------------------------------------------------------------

.soviclust_v3_validate_common <- function(data, pop, distmat, ncluster, m,
                                          alpha) {
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
    stop(
      "`ncluster` must be >= 2 and smaller than the number of observations.",
      call. = FALSE
    )
  }

  n <- nrow(data)
  beta <- 1 - alpha

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

  popmat <- matrix(as.numeric(pop), ncol = 1)

  list(
    data = data,
    n = n,
    p = ncol(data),
    beta = beta,
    mi.mj = popmat %*% t(popmat),
    distmat = distmat
  )
}


# -----------------------------------------------------------------------------
# Unified FGWC candidate evaluator
# -----------------------------------------------------------------------------

evaluate_optimizer_candidate_v3 <- function(
    data,
    search_centers,
    mi.mj,
    distmat,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    beta = 1 - alpha,
    a = 1,
    b = 1,
    require_all_clusters = TRUE) {

  data <- as.matrix(data)
  search_centers <- clamp_centroids(as.matrix(search_centers), data)

  base_u <- membership_from_centroids(
    data = data,
    centers = search_centers,
    m = m,
    distance = distance,
    order = order
  )$u

  spatial_u <- renew_uij(
    data = data,
    old_uij = base_u,
    mi.mj = mi.mj,
    dist = distmat,
    alpha = alpha,
    beta = beta,
    a = a,
    b = b
  )

  spatial_centers <- centroid_from_membership(
    data = data,
    uij = spatial_u,
    m = m
  )

  hard_cluster <- apply(spatial_u, 1, which.max)
  occupied <- length(unique(hard_cluster))
  requested <- nrow(search_centers)

  xb <- XB1(
    data = data,
    uij = spatial_u,
    vi = spatial_centers,
    m = m
  )

  spatial_j <- fgwc_objective(
    data = data,
    uij = spatial_u,
    centers = spatial_centers,
    m = m,
    distance = distance,
    order = order
  )

  feasible <- is.finite(xb) &&
    (!require_all_clusters || occupied == requested)

  list(
    search_centers = search_centers,
    membership = spatial_u,
    centroid = spatial_centers,
    cluster = hard_cluster,
    occupied_clusters = occupied,
    feasible = feasible,
    fitness = if (feasible) as.numeric(xb) else Inf,
    xb = as.numeric(xb),
    spatial_obj = as.numeric(spatial_j)
  )
}


.soviclust_v3_eval <- function(ctx, search_centers, m, distance, order,
                               alpha, a, b,
                               require_all_clusters = TRUE) {
  evaluate_optimizer_candidate_v3(
    data = ctx$data,
    search_centers = search_centers,
    mi.mj = ctx$mi.mj,
    distmat = ctx$distmat,
    m = m,
    distance = distance,
    order = order,
    alpha = alpha,
    beta = ctx$beta,
    a = a,
    b = b,
    require_all_clusters = require_all_clusters
  )
}


.soviclust_v3_new_position <- function(data, ncluster, vi.dist, seed) {
  gen_vi(
    data = data,
    ncluster = ncluster,
    gendist = vi.dist,
    randomN = seed
  )
}


.soviclust_v3_init_population <- function(
    ctx, n_agents, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b,
    max_retry = 50L) {

  if (n_agents < 2L) {
    stop("Optimizer population must contain at least 2 agents.", call. = FALSE)
  }

  search <- vector("list", n_agents)
  evals <- vector("list", n_agents)

  for (i in seq_len(n_agents)) {
    accepted <- FALSE

    for (attempt in 0:max_retry) {
      seed <- as.integer(randomN + i * 1009L + attempt * 100003L)

      pos <- .soviclust_v3_new_position(
        ctx$data, ncluster, vi.dist, seed
      )

      ev <- .soviclust_v3_eval(
        ctx, pos, m, distance, order, alpha, a, b,
        require_all_clusters = TRUE
      )

      if (ev$feasible) {
        search[[i]] <- ev$search_centers
        evals[[i]] <- ev
        accepted <- TRUE
        break
      }
    }

    if (!accepted) {
      # Keep the last candidate for diagnostics; the population-level check
      # below will fail only if no feasible candidate exists at all.
      search[[i]] <- ev$search_centers
      evals[[i]] <- ev
    }
  }

  fitness <- vapply(evals, `[[`, numeric(1), "fitness")

  if (!any(is.finite(fitness))) {
    stop(
      "Unable to initialize a feasible optimizer population in which all ",
      "requested hard clusters are occupied.",
      call. = FALSE
    )
  }

  list(search = search, evals = evals, fitness = fitness)
}


.soviclust_v3_replace_if_better <- function(search, evals, fitness,
                                             i, candidate, candidate_eval) {
  if (candidate_eval$fitness < fitness[i]) {
    search[[i]] <- candidate_eval$search_centers
    evals[[i]] <- candidate_eval
    fitness[i] <- candidate_eval$fitness
    improved <- TRUE
  } else {
    improved <- FALSE
  }

  list(
    search = search,
    evals = evals,
    fitness = fitness,
    improved = improved
  )
}


.soviclust_v3_result <- function(
    data,
    best_search,
    best_eval,
    converg,
    iteration,
    same,
    call,
    ptm) {

  membership <- best_eval$membership
  centers <- best_eval$centroid

  finaldata <- determine_cluster(data, membership)
  cluster <- finaldata[, ncol(finaldata)]

  out <- list(
    converg = as.numeric(converg),
    f_obj = as.numeric(best_eval$fitness),
    fitness_type = "spatial_XB_feasible",
    spatial_obj = as.numeric(best_eval$spatial_obj),
    membership = membership,
    centroid = centers,
    search_centroid = best_search,
    occupied_clusters = best_eval$occupied_clusters,
    validation = index_fgwc(
      data, cluster, membership, centers, m = 2, e = exp(1)
    ),
    cluster = cluster,
    finaldata = finaldata,
    call = call,
    iteration = as.integer(iteration),
    same = as.integer(same),
    time = proc.time() - ptm
  )

  # index_fgwc() needs the actual fuzzifier. It is overwritten by caller below
  # through a private attribute to preserve the public list layout.
  class(out) <- "fgwc"
  out
}


.soviclust_v3_result_m <- function(
    data,
    best_search,
    best_eval,
    converg,
    iteration,
    same,
    call,
    ptm,
    m) {

  membership <- best_eval$membership
  centers <- best_eval$centroid

  finaldata <- determine_cluster(data, membership)
  cluster <- finaldata[, ncol(finaldata)]

  out <- list(
    converg = as.numeric(converg),
    f_obj = as.numeric(best_eval$fitness),
    fitness_type = "spatial_XB_feasible",
    spatial_obj = as.numeric(best_eval$spatial_obj),
    membership = membership,
    centroid = centers,
    search_centroid = best_search,
    occupied_clusters = best_eval$occupied_clusters,
    validation = index_fgwc(
      data, cluster, membership, centers, m, exp(1)
    ),
    cluster = cluster,
    finaldata = finaldata,
    call = call,
    iteration = as.integer(iteration),
    same = as.integer(same),
    time = proc.time() - ptm
  )

  class(out) <- "fgwc"
  out
}


.soviclust_v3_stagnation <- function(conv, same, error) {
  if (length(conv) < 2L) return(0L)

  if (abs(conv[length(conv)] - conv[length(conv) - 1L]) < error) {
    same + 1L
  } else {
    0L
  }
}


# =============================================================================
# Canonical helper functions used by tests and optimizer implementations
# =============================================================================


# -----------------------------------------------------------------------------
# ABC canonical single-dimension perturbation
# -----------------------------------------------------------------------------

.soviclust_v3_abc_neighbor <- function(
    source,
    partner,
    dimension,
    phi,
    gbest = NULL,
    psi = 0) {

  source <- as.matrix(source)
  partner <- as.matrix(partner)

  if (!identical(dim(source), dim(partner))) {
    stop("`source` and `partner` must have identical dimensions.", call. = FALSE)
  }

  d <- length(source)
  if (dimension < 1L || dimension > d) {
    stop("`dimension` is outside the flattened centroid vector.", call. = FALSE)
  }

  src <- as.vector(source)
  prt <- as.vector(partner)

  cand <- src
  cand[dimension] <- src[dimension] +
    phi * (src[dimension] - prt[dimension])

  if (!is.null(gbest) && psi != 0) {
    gb <- as.vector(as.matrix(gbest))
    cand[dimension] <- cand[dimension] +
      psi * (gb[dimension] - src[dimension])
  }

  matrix(
    cand,
    nrow = nrow(source),
    ncol = ncol(source)
  )
}


.soviclust_v3_abc_probabilities <- function(fitness) {
  fitness <- as.numeric(fitness)
  finite <- is.finite(fitness)

  if (!any(finite)) {
    return(rep(1 / length(fitness), length(fitness)))
  }

  worst_finite <- max(fitness[finite])
  safe <- fitness
  safe[!finite] <- worst_finite + max(1, abs(worst_finite))

  # Positive monotone transformation for minimization.
  # Avoid the near-degenerate 1/epsilon probability that results from
  # subtracting the best fitness all the way to machine epsilon.
  offset <- if (min(safe) <= 0) abs(min(safe)) + 1 else 0
  quality <- 1 / (1 + safe + offset)

  quality / sum(quality)
}


.soviclust_v3_abc_onlooker_indices <- function(fitness, n, seed) {
  probs <- .soviclust_v3_abc_probabilities(fitness)
  set.seed(seed)
  sample(
    seq_along(fitness),
    size = n,
    replace = TRUE,
    prob = probs
  )
}


# -----------------------------------------------------------------------------
# GSA canonical full-agent distance and mass helpers
# -----------------------------------------------------------------------------

.soviclust_v3_gsa_distance <- function(xi, xj) {
  xi <- as.matrix(xi)
  xj <- as.matrix(xj)

  if (!identical(dim(xi), dim(xj))) {
    stop("GSA agents must have identical dimensions.", call. = FALSE)
  }

  sqrt(sum((xi - xj)^2))
}


.soviclust_v3_gsa_masses <- function(fitness) {
  fitness <- as.numeric(fitness)

  if (any(!is.finite(fitness))) {
    finite <- is.finite(fitness)
    if (!any(finite)) {
      return(rep(1 / length(fitness), length(fitness)))
    }
    replacement <- max(fitness[finite]) + max(1, abs(max(fitness[finite])))
    fitness[!finite] <- replacement
  }

  best <- min(fitness)
  worst <- max(fitness)
  span <- worst - best

  if (!is.finite(span) || span <= .Machine$double.eps) {
    return(rep(1 / length(fitness), length(fitness)))
  }

  raw <- (worst - fitness) / span
  raw <- pmax(raw, .Machine$double.eps)

  raw / sum(raw)
}


.soviclust_v3_gsa_velocity_step <- function(
    search,
    fitness,
    velocity,
    G0,
    gsa.alpha,
    iter,
    max.iter,
    kbest,
    vmax,
    seed) {

  n <- length(search)
  masses <- .soviclust_v3_gsa_masses(fitness)

  Gt <- G0 * exp(
    -gsa.alpha * ((iter - 1) / max(1, max.iter))
  )

  elite_idx <- order(fitness)[
    seq_len(min(max(1L, as.integer(kbest)), n))
  ]

  out <- vector("list", n)

  for (i in seq_len(n)) {
    force <- matrix(0, nrow(search[[i]]), ncol(search[[i]]))

    for (j in elite_idx) {
      if (j == i) next

      Rij <- .soviclust_v3_gsa_distance(
        search[[i]], search[[j]]
      ) + .Machine$double.eps

      set.seed(seed + iter * 100000L + i * 1000L + j)

      rand_force <- matrix(
        runif(length(search[[i]])),
        nrow = nrow(search[[i]]),
        ncol = ncol(search[[i]])
      )

      force <- force +
        rand_force *
        Gt *
        masses[i] *
        masses[j] *
        (search[[j]] - search[[i]]) / Rij
    }

    acceleration <- force / max(masses[i], .Machine$double.eps)

    set.seed(seed + iter * 100000L + i * 1000L + 777L)

    rand_velocity <- matrix(
      runif(length(search[[i]])),
      nrow = nrow(search[[i]]),
      ncol = ncol(search[[i]])
    )

    vnew <- rand_velocity * velocity[[i]] + acceleration

    if (is.finite(vmax) && vmax > 0) {
      vnew <- pmin(pmax(vnew, -vmax), vmax)
    }

    out[[i]] <- vnew
  }

  out
}


# -----------------------------------------------------------------------------
# TLBO canonical teacher / learner helpers
# -----------------------------------------------------------------------------

.soviclust_v3_tlbo_teacher_candidate <- function(
    student,
    teacher,
    population_mean,
    seed) {

  student <- as.matrix(student)
  teacher <- as.matrix(teacher)
  population_mean <- as.matrix(population_mean)

  set.seed(seed)
  TF <- round(1 + runif(1))

  set.seed(seed + 1L)
  r <- matrix(
    runif(length(student)),
    nrow = nrow(student),
    ncol = ncol(student)
  )

  candidate <- student +
    r * (teacher - TF * population_mean)

  list(candidate = candidate, TF = TF, r = r)
}


.soviclust_v3_tlbo_learner_candidate <- function(
    student,
    partner,
    student_fit,
    partner_fit,
    seed) {

  student <- as.matrix(student)
  partner <- as.matrix(partner)

  set.seed(seed)
  r <- matrix(
    runif(length(student)),
    nrow = nrow(student),
    ncol = ncol(student)
  )

  if (student_fit < partner_fit) {
    candidate <- student + r * (student - partner)
  } else {
    candidate <- student + r * (partner - student)
  }

  list(candidate = candidate, r = r)
}


# -----------------------------------------------------------------------------
# GWO canonical movement helper
# -----------------------------------------------------------------------------

.soviclust_v3_gwo_move <- function(
    swarm,
    alpha_pos,
    beta_pos,
    delta_pos,
    a_coef,
    seed,
    iter,
    data = NULL) {

  lapply(seq_along(swarm), function(i) {
    set.seed(seed + iter * 10000L + i)

    dd <- dim(alpha_pos)

    r1_a <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    r2_a <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    A1 <- 2 * a_coef * r1_a - a_coef
    C1 <- 2 * r2_a
    X1 <- alpha_pos - A1 * abs(C1 * alpha_pos - swarm[[i]])

    r1_b <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    r2_b <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    A2 <- 2 * a_coef * r1_b - a_coef
    C2 <- 2 * r2_b
    X2 <- beta_pos - A2 * abs(C2 * beta_pos - swarm[[i]])

    r1_d <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    r2_d <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    A3 <- 2 * a_coef * r1_d - a_coef
    C3 <- 2 * r2_d
    X3 <- delta_pos - A3 * abs(C3 * delta_pos - swarm[[i]])

    out <- (X1 + X2 + X3) / 3

    if (!is.null(data)) {
      out <- clamp_centroids(out, data)
    }

    out
  })
}


# -----------------------------------------------------------------------------
# WOA canonical movement helper
# -----------------------------------------------------------------------------

.soviclust_v3_woa_move <- function(
    swarm,
    prey,
    a_coef,
    b,
    seed,
    iter,
    data = NULL) {

  nwhale <- length(swarm)

  lapply(seq_len(nwhale), function(i) {
    set.seed(seed + iter * 10000L + i)

    r1 <- runif(1)
    r2 <- runif(1)
    p <- runif(1)

    A <- 2 * a_coef * r1 - a_coef
    C <- 2 * r2

    current <- swarm[[i]]

    if (p < 0.5) {
      if (abs(A) < 1) {
        D <- abs(C * prey - current)
        new_pos <- prey - A * D
      } else {
        candidates <- setdiff(seq_len(nwhale), i)
        rand_idx <- sample(candidates, 1)
        rand_whale <- swarm[[rand_idx]]

        D <- abs(C * rand_whale - current)
        new_pos <- rand_whale - A * D
      }
    } else {
      l <- runif(1, -1, 1)
      D_prey <- abs(prey - current)

      new_pos <- D_prey *
        exp(b * l) *
        cos(2 * pi * l) +
        prey
    }

    if (!is.null(data)) {
      new_pos <- clamp_centroids(new_pos, data)
    }

    new_pos
  })
}


# =============================================================================
# ABC-FGWC v3
# =============================================================================

abcfgwc <- function(
    data, pop = NA, distmat = NA, ncluster = 2, m = 2,
    distance = "euclidean", order = 2, alpha = 0.7, a = 1, b = 1,
    error = 1e-5, max.iter = 100, randomN = 0, vi.dist = "uniform",
    nfood = 10, n.onlooker = 5, limit = 4, pso = FALSE,
    abc.same = 10) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (nfood < 2L) stop("ABC requires at least 2 food sources.", call. = FALSE)
  if (n.onlooker < 1L) stop("`n.onlooker` must be >= 1.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, nfood, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness
  trials <- integer(nfood)

  best_idx <- which.min(fit)
  best_search <- search[[best_idx]]
  best_eval <- evals[[best_idx]]
  best_fit <- fit[best_idx]

  conv <- best_fit
  same <- 0L
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    # Employed-bee phase: canonical one-dimension perturbation.
    for (i in seq_len(nfood)) {
      set.seed(randomN + iter * 100000L + i * 100L + 1L)
      partner <- sample(setdiff(seq_len(nfood), i), 1)

      set.seed(randomN + iter * 100000L + i * 100L + 2L)
      dimension <- sample(seq_len(length(search[[i]])), 1)

      set.seed(randomN + iter * 100000L + i * 100L + 3L)
      phi <- runif(1, -1, 1)

      psi <- 0
      if (isTRUE(pso)) {
        set.seed(randomN + iter * 100000L + i * 100L + 4L)
        psi <- runif(1, 0, 1.5)
      }

      cand <- .soviclust_v3_abc_neighbor(
        source = search[[i]],
        partner = search[[partner]],
        dimension = dimension,
        phi = phi,
        gbest = best_search,
        psi = psi
      )

      cand <- clamp_centroids(cand, ctx$data)

      ev <- .soviclust_v3_eval(
        ctx, cand, m, distance, order, alpha, a, b
      )

      if (ev$fitness < fit[i]) {
        search[[i]] <- ev$search_centers
        evals[[i]] <- ev
        fit[i] <- ev$fitness
        trials[i] <- 0L
      } else {
        trials[i] <- trials[i] + 1L
      }
    }

    # Onlooker phase: probabilistic food-source selection.
    selected <- .soviclust_v3_abc_onlooker_indices(
      fit, n.onlooker,
      seed = randomN + iter * 100000L + 50000L
    )

    for (q in seq_along(selected)) {
      i <- selected[q]

      set.seed(randomN + iter * 100000L + q * 100L + 50001L)
      partner <- sample(setdiff(seq_len(nfood), i), 1)

      set.seed(randomN + iter * 100000L + q * 100L + 50002L)
      dimension <- sample(seq_len(length(search[[i]])), 1)

      set.seed(randomN + iter * 100000L + q * 100L + 50003L)
      phi <- runif(1, -1, 1)

      psi <- 0
      if (isTRUE(pso)) {
        set.seed(randomN + iter * 100000L + q * 100L + 50004L)
        psi <- runif(1, 0, 1.5)
      }

      cand <- .soviclust_v3_abc_neighbor(
        search[[i]],
        search[[partner]],
        dimension,
        phi,
        best_search,
        psi
      )

      cand <- clamp_centroids(cand, ctx$data)

      ev <- .soviclust_v3_eval(
        ctx, cand, m, distance, order, alpha, a, b
      )

      if (ev$fitness < fit[i]) {
        search[[i]] <- ev$search_centers
        evals[[i]] <- ev
        fit[i] <- ev$fitness
        trials[i] <- 0L
      } else {
        trials[i] <- trials[i] + 1L
      }
    }

    # Scout phase.
    scout_idx <- which(trials >= limit)

    for (i in scout_idx) {
      accepted <- FALSE

      for (attempt in 0:50) {
        cand <- .soviclust_v3_new_position(
          ctx$data,
          ncluster,
          vi.dist,
          randomN + iter * 100000L + i * 1000L + 70000L + attempt
        )

        ev <- .soviclust_v3_eval(
          ctx, cand, m, distance, order, alpha, a, b
        )

        if (ev$feasible) {
          search[[i]] <- ev$search_centers
          evals[[i]] <- ev
          fit[i] <- ev$fitness
          trials[i] <- 0L
          accepted <- TRUE
          break
        }
      }

      if (!accepted) {
        trials[i] <- 0L
      }
    }

    cur_idx <- which.min(fit)

    if (fit[cur_idx] < best_fit) {
      best_fit <- fit[cur_idx]
      best_search <- search[[cur_idx]]
      best_eval <- evals[[cur_idx]]
    }

    conv <- c(conv, best_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= abc.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, best_search, best_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}



# -----------------------------------------------------------------------------
# Namespace-safe random generators for FPA / IFA
# -----------------------------------------------------------------------------

.soviclust_v3_stable_random <- function(
    n,
    alpha,
    beta = 0,
    gamma = 1,
    delta = 0) {

  stabledist::rstable(
    n = n,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    delta = delta
  )
}


.soviclust_v3_ei_dist <- function(
    distr = "normal",
    n,
    randomN = 40,
    r = 4,
    m = 0.7,
    ind = 1,
    skew = 0,
    sca = 1) {

  set.seed(randomN)

  if (distr == "uniform") {
    return(stats::runif(n, -1, 1))
  }

  if (distr == "normal") {
    return(stats::rnorm(n, 0, 1))
  }

  if (distr == "levy") {
    return(
      .soviclust_v3_stable_random(
        n = n,
        alpha = ind,
        beta = skew,
        gamma = sca,
        delta = 0
      )
    )
  }

  if (distr == "logchaotic") {
    return(logchaotic(n, r, randomN))
  }

  if (distr == "kentchaotic") {
    return(kentchaotic(n, m, randomN))
  }

  if (distr == "sinechaotic") {
    return(sinechaotic(n, m, randomN))
  }

  if (distr == "dyadchaotic") {
    return(dyadchaotic(n, randomN))
  }

  if (distr == "chebychaotic") {
    return(chebychaotic(n, randomN))
  }

  if (distr == "circhaotic") {
    return(circhaotic(n, m, randomN))
  }

  stop(
    "Unsupported random distribution: ",
    distr,
    call. = FALSE
  )
}


.soviclust_v3_pollination <- function(
    flow,
    p,
    pollen,
    gamma,
    lambda,
    delta,
    seed,
    ei.distr,
    r,
    m,
    skew,
    sca) {

  set.seed(seed + 10L)
  rand <- stats::runif(length(flow))

  dd <- dim(pollen)
  npar <- dd[1L] * dd[2L]

  lapply(
    seq_along(flow),
    function(x) {

      if (rand[x] < p) {
        step <- matrix(
          .soviclust_v3_stable_random(
            n = npar,
            alpha = lambda,
            beta = skew,
            gamma = sca,
            delta = delta
          ),
          nrow = dd[1L],
          ncol = dd[2L]
        )

        flow[[x]] +
          gamma *
          step *
          (pollen - flow[[x]])

      } else {
        set.seed(seed + x * 1000L + 1L)
        candidates <- setdiff(seq_along(flow), x)

        if (length(candidates) < 2L) {
          stop(
            "FPA local pollination requires at least 3 flowers.",
            call. = FALSE
          )
        }

        pair <- sample(candidates, 2L, replace = FALSE)

        ei <- matrix(
          .soviclust_v3_ei_dist(
            distr = ei.distr,
            n = npar,
            randomN = seed + x * 1000L + 2L,
            r = r,
            m = m,
            ind = lambda,
            skew = skew,
            sca = sca
          ),
          nrow = dd[1L],
          ncol = dd[2L]
        )

        flow[[x]] +
          ei *
          (flow[[pair[1L]]] - flow[[pair[2L]]])
      }
    }
  )
}



# =============================================================================
# FPA-FGWC v3
# =============================================================================

fpafgwc <- function(
    data, pop = NA, distmat = NA, ncluster = 2, m = 2,
    distance = "euclidean", order = 2, alpha = 0.7, a = 1, b = 1,
    error = 1e-5, max.iter = 100, randomN = 0, vi.dist = "uniform",
    nflow = 10, p = 0.8, gamma = 1, lambda = 1.5, delta = 0,
    ei.distr = "normal", flow.same = 10, r = 4, m.chaotic = 0.7,
    skew = 0, sca = 1) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (nflow < 3L) stop("FPA requires at least 3 flowers.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, nflow, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness

  best_idx <- which.min(fit)
  best_search <- search[[best_idx]]
  best_eval <- evals[[best_idx]]
  best_fit <- fit[best_idx]

  conv <- best_fit
  same <- 0L
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    candidates <- .soviclust_v3_pollination(
      flow = search,
      p = p,
      pollen = best_search,
      gamma = gamma,
      lambda = lambda,
      delta = delta,
      seed = randomN + iter * 10000L,
      ei.distr = ei.distr,
      r = r,
      m = m.chaotic,
      skew = skew,
      sca = sca
    )

    for (i in seq_len(nflow)) {
      cand <- clamp_centroids(candidates[[i]], ctx$data)

      ev <- .soviclust_v3_eval(
        ctx, cand, m, distance, order, alpha, a, b
      )

      if (ev$fitness < fit[i]) {
        search[[i]] <- ev$search_centers
        evals[[i]] <- ev
        fit[i] <- ev$fitness
      }
    }

    cur_idx <- which.min(fit)

    if (fit[cur_idx] < best_fit) {
      best_fit <- fit[cur_idx]
      best_search <- search[[cur_idx]]
      best_eval <- evals[[cur_idx]]
    }

    conv <- c(conv, best_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= flow.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, best_search, best_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}


# =============================================================================
# GSA-FGWC v3
# =============================================================================

gsafgwc <- function(
    data, pop = NA, distmat = NA, ncluster = 2, m = 2,
    distance = "euclidean", order = 2, alpha = 0.7, a = 1, b = 1,
    error = 1e-5, max.iter = 100, randomN = 0, vi.dist = "uniform",
    npar = 10, par.no = 2, par.dist = "euclidean", par.order = 2,
    gsa.same = 10, G = 1, vmax = 0.7, new = FALSE,
    gsa.alpha = 20) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (npar < 2L) stop("GSA requires at least 2 particles.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, npar, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness

  velocity <- lapply(
    seq_len(npar),
    function(i) matrix(0, ncluster, ctx$p)
  )

  pbest <- search
  pbest_eval <- evals
  pbest_fit <- fit

  best_idx <- which.min(fit)
  best_search <- search[[best_idx]]
  best_eval <- evals[[best_idx]]
  best_fit <- fit[best_idx]

  conv <- best_fit
  same <- 0L
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    velocity <- .soviclust_v3_gsa_velocity_step(
      search = search,
      fitness = fit,
      velocity = velocity,
      G0 = G,
      gsa.alpha = gsa.alpha,
      iter = iter,
      max.iter = max.iter,
      kbest = par.no,
      vmax = vmax,
      seed = randomN
    )

    candidates <- lapply(
      seq_len(npar),
      function(i) clamp_centroids(search[[i]] + velocity[[i]], ctx$data)
    )

    if (isTRUE(new) && exists("new.move", mode = "function")) {
      candidates <- lapply(
        seq_len(npar),
        function(i) {
          clamp_centroids(
            new.move(
              candidates[[i]],
              pbest[[i]],
              best_search,
              randomN + iter * 1000L + i
            ),
            ctx$data
          )
        }
      )
    }

    new_evals <- lapply(
      candidates,
      function(cand) {
        .soviclust_v3_eval(
          ctx, cand, m, distance, order, alpha, a, b
        )
      }
    )

    search <- lapply(new_evals, `[[`, "search_centers")
    evals <- new_evals
    fit <- vapply(evals, `[[`, numeric(1), "fitness")

    improved <- which(fit < pbest_fit)

    for (i in improved) {
      pbest[[i]] <- search[[i]]
      pbest_eval[[i]] <- evals[[i]]
      pbest_fit[i] <- fit[i]
    }

    cur_idx <- which.min(fit)

    if (fit[cur_idx] < best_fit) {
      best_fit <- fit[cur_idx]
      best_search <- search[[cur_idx]]
      best_eval <- evals[[cur_idx]]
    }

    conv <- c(conv, best_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= gsa.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, best_search, best_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}


# =============================================================================
# GWO-FGWC v3
# =============================================================================

gwofgwc <- function(
    data, pop = NA, distmat = NA, ncluster = 2, m = 2,
    distance = "euclidean", order = 2, alpha = 0.7,
    a = 1, b = 1, error = 1e-5, max.iter = 100,
    randomN = 0, vi.dist = "uniform", nwolf = 10,
    wolf.same = 10) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (nwolf < 3L) stop("GWO requires at least 3 wolves.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, nwolf, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness

  ord <- order(fit)

  alpha_pos <- search[[ord[1L]]]
  beta_pos <- search[[ord[2L]]]
  delta_pos <- search[[ord[3L]]]

  alpha_fit <- fit[ord[1L]]
  beta_fit <- fit[ord[2L]]
  delta_fit <- fit[ord[3L]]

  alpha_eval <- evals[[ord[1L]]]

  conv <- alpha_fit
  same <- 0L
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    a_coef <- if (max.iter == 1L) {
      0
    } else {
      2 - 2 * ((iter - 1) / (max.iter - 1))
    }

    candidates <- .soviclust_v3_gwo_move(
      swarm = search,
      alpha_pos = alpha_pos,
      beta_pos = beta_pos,
      delta_pos = delta_pos,
      a_coef = a_coef,
      seed = randomN,
      iter = iter,
      data = ctx$data
    )

    evals <- lapply(
      candidates,
      function(cand) {
        .soviclust_v3_eval(
          ctx, cand, m, distance, order, alpha, a, b
        )
      }
    )

    search <- lapply(evals, `[[`, "search_centers")
    fit <- vapply(evals, `[[`, numeric(1), "fitness")

    for (i in order(fit)) {
      score <- fit[i]
      pos <- search[[i]]

      if (score < alpha_fit) {
        delta_fit <- beta_fit
        delta_pos <- beta_pos

        beta_fit <- alpha_fit
        beta_pos <- alpha_pos

        alpha_fit <- score
        alpha_pos <- pos
        alpha_eval <- evals[[i]]

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

    conv <- c(conv, alpha_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= wolf.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, alpha_pos, alpha_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}


# =============================================================================
# HHO-FGWC v3
# =============================================================================

.soviclust_v3_hho_levy <- function(n, beta, seed) {
  if (exists("rlevy", mode = "function")) {
    return(rlevy(n, beta, seed))
  }

  sigma <- (
    gamma(1 + beta) *
      sin(pi * beta / 2) /
      (
        gamma((1 + beta) / 2) *
          beta *
          2^((beta - 1) / 2)
      )
  )^(1 / beta)

  set.seed(seed)
  u <- rnorm(n, 0, sigma)

  set.seed(seed + 1L)
  v <- rnorm(n)

  u / abs(v)^(1 / beta)
}


hhofgwc <- function(
    data, pop = NA, distmat = NA, ncluster = 2, m = 2,
    distance = "euclidean", order = 2, alpha = 0.7, a = 1, b = 1,
    error = 1e-5, max.iter = 100, randomN = 0, vi.dist = "uniform",
    nhh = 10, hh.alg = "heidari", A = c(2, 1, 0.5), p = 0.5,
    hh.same = 10, levy.beta = 1.5, update.type = 5) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (nhh < 2L) stop("HHO requires at least 2 hawks.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, nhh, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness

  best_idx <- which.min(fit)
  rabbit <- search[[best_idx]]
  rabbit_eval <- evals[[best_idx]]
  rabbit_fit <- fit[best_idx]

  conv <- rabbit_fit
  same <- 0L
  iter_done <- 0L

  lower <- apply(ctx$data, 2, min)
  upper <- apply(ctx$data, 2, max)

  for (iter in seq_len(max.iter)) {
    iter_done <- iter
    current <- search
    current_fit <- fit
    Xmean <- Reduce("+", current) / nhh

    candidates <- vector("list", nhh)

    for (i in seq_len(nhh)) {
      if (tolower(hh.alg) == "bairathi" &&
          exists("hh.attack.bairathi", mode = "function")) {

        set.seed(randomN + iter * 10000L + i)
        rand_idx <- sample(seq_len(nhh), 1)

        candidates[[i]] <- hh.attack.bairathi(
          current[[i]],
          current,
          rabbit,
          A,
          p,
          rand_idx,
          randomN + iter * 10000L + i,
          best_idx
        )

      } else {
        set.seed(randomN + iter * 100000L + i * 100L + 1L)
        E0 <- runif(1, -1, 1)

        E <- A[1] * E0 * (1 - (iter - 1) / max(1, max.iter))

        set.seed(randomN + iter * 100000L + i * 100L + 2L)
        q <- runif(1)

        set.seed(randomN + iter * 100000L + i * 100L + 3L)
        r <- runif(1)

        set.seed(randomN + iter * 100000L + i * 100L + 4L)
        rand_idx <- sample(seq_len(nhh), 1)

        Xi <- current[[i]]
        Xrand <- current[[rand_idx]]

        if (abs(E) >= 1) {
          set.seed(randomN + iter * 100000L + i * 100L + 5L)
          r1 <- runif(1)

          set.seed(randomN + iter * 100000L + i * 100L + 6L)
          r2 <- runif(1)

          if (q >= 0.5) {
            cand <- Xrand - r1 * abs(Xrand - 2 * r2 * Xi)
          } else {
            set.seed(randomN + iter * 100000L + i * 100L + 7L)
            r3 <- runif(1)

            set.seed(randomN + iter * 100000L + i * 100L + 8L)
            r4 <- runif(1)

            lb <- matrix(lower, nrow = ncluster, ncol = ctx$p, byrow = TRUE)
            ub <- matrix(upper, nrow = ncluster, ncol = ctx$p, byrow = TRUE)

            cand <- (rabbit - Xmean) -
              r3 * (lb + r4 * (ub - lb))
          }

        } else {
          set.seed(randomN + iter * 100000L + i * 100L + 9L)
          J <- 2 * (1 - runif(1))

          if (r >= 0.5 && abs(E) >= 0.5) {
            deltaX <- rabbit - Xi
            cand <- deltaX - E * abs(J * rabbit - Xi)

          } else if (r >= 0.5 && abs(E) < 0.5) {
            deltaX <- rabbit - Xi
            cand <- rabbit - E * abs(deltaX)

          } else {
            if (abs(E) >= 0.5) {
              Y <- rabbit - E * abs(J * rabbit - Xi)
            } else {
              Y <- rabbit - E * abs(J * rabbit - Xmean)
            }

            set.seed(randomN + iter * 100000L + i * 100L + 10L)
            S <- matrix(
              rnorm(length(Xi)),
              nrow = nrow(Xi),
              ncol = ncol(Xi)
            )

            LF <- matrix(
              .soviclust_v3_hho_levy(
                length(Xi),
                levy.beta,
                randomN + iter * 100000L + i * 100L + 11L
              ),
              nrow = nrow(Xi),
              ncol = ncol(Xi)
            )

            Z <- Y + S * LF

            Y <- clamp_centroids(Y, ctx$data)
            Z <- clamp_centroids(Z, ctx$data)

            evY <- .soviclust_v3_eval(
              ctx, Y, m, distance, order, alpha, a, b
            )

            evZ <- .soviclust_v3_eval(
              ctx, Z, m, distance, order, alpha, a, b
            )

            if (evY$fitness < current_fit[i]) {
              cand <- Y
            } else if (evZ$fitness < current_fit[i]) {
              cand <- Z
            } else {
              cand <- Xi
            }
          }
        }

        candidates[[i]] <- cand
      }

      candidates[[i]] <- clamp_centroids(candidates[[i]], ctx$data)
    }

    evals <- lapply(
      candidates,
      function(cand) {
        .soviclust_v3_eval(
          ctx, cand, m, distance, order, alpha, a, b
        )
      }
    )

    search <- lapply(evals, `[[`, "search_centers")
    fit <- vapply(evals, `[[`, numeric(1), "fitness")

    cur_idx <- which.min(fit)

    if (fit[cur_idx] < rabbit_fit) {
      rabbit_fit <- fit[cur_idx]
      rabbit <- search[[cur_idx]]
      rabbit_eval <- evals[[cur_idx]]
      best_idx <- cur_idx
    }

    conv <- c(conv, rabbit_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= hh.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, rabbit, rabbit_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}


# =============================================================================
# IFA-FGWC v3
# =============================================================================

.soviclust_v3_ifa_random_matrix <- function(
    nr, nc, distr, seed,
    r.chaotic, m.chaotic,
    ind.levy, skew.levy, scale.levy) {

  n <- nr * nc

  vals <- .soviclust_v3_ei_dist(
    distr = distr,
    n = n,
    randomN = seed,
    r = r.chaotic,
    m = m.chaotic,
    ind = ind.levy,
    skew = skew.levy,
    sca = scale.levy
  )

  matrix(vals, nrow = nr, ncol = nc)
}


ifafgwc <- function(
    data, pop = NA, distmat = NA, ncluster = 2, m = 2,
    distance = "euclidean", order = 2, alpha = 0.7, a = 1, b = 1,
    error = 1e-5, max.iter = 100, randomN = 0, vi.dist = "uniform",
    ei.distr = "normal", fa.same = 10, nfly = 10, ffly.no = 2,
    ffly.dist = "euclidean", ffly.order = 2, gamma = 1,
    ffly.beta = 1, ffly.alpha = 1, r.chaotic = 4, m.chaotic = 0.7,
    ind.levy = 1, skew.levy = 0, scale.levy = 1,
    ffly.alpha.type = 4) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (nfly < 2L) stop("IFA requires at least 2 fireflies.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, nfly, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness

  best_idx <- which.min(fit)
  best_search <- search[[best_idx]]
  best_eval <- evals[[best_idx]]
  best_fit <- fit[best_idx]

  conv <- best_fit
  same <- 0L
  iter_done <- 0L
  alpha_random <- ffly.alpha

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    if (exists("update_alpha", mode = "function")) {
      alpha_random <- update_alpha(
        alpha_random,
        iter - 1L,
        max.iter,
        ffly.alpha.type
      )
    }

    elite_idx <- order(fit)[
      seq_len(min(max(1L, ffly.no), nfly))
    ]

    candidates <- search

    for (j in seq_len(nfly)) {
      cand <- search[[j]]
      moved <- FALSE

      for (k in elite_idx) {
        if (fit[k] >= fit[j] || k == j) next

        rdist_agent <- sqrt(sum((cand - search[[k]])^2))
        beta_eff <- ffly.beta * exp(-gamma * rdist_agent^2)

        ei <- .soviclust_v3_ifa_random_matrix(
          nrow(cand), ncol(cand), ei.distr,
          randomN + iter * 100000L + j * 1000L + k,
          r.chaotic, m.chaotic,
          ind.levy, skew.levy, scale.levy
        )

        cand <- cand +
          beta_eff * (search[[k]] - cand) +
          alpha_random * ei

        moved <- TRUE
      }

      if (!moved) {
        ei <- .soviclust_v3_ifa_random_matrix(
          nrow(cand), ncol(cand), ei.distr,
          randomN + iter * 100000L + j * 1000L + 999L,
          r.chaotic, m.chaotic,
          ind.levy, skew.levy, scale.levy
        )

        cand <- cand + alpha_random * ei
      }

      candidates[[j]] <- clamp_centroids(cand, ctx$data)
    }

    for (j in seq_len(nfly)) {
      ev <- .soviclust_v3_eval(
        ctx, candidates[[j]],
        m, distance, order, alpha, a, b
      )

      if (ev$fitness < fit[j]) {
        search[[j]] <- ev$search_centers
        evals[[j]] <- ev
        fit[j] <- ev$fitness
      }
    }

    cur_idx <- which.min(fit)

    if (fit[cur_idx] < best_fit) {
      best_fit <- fit[cur_idx]
      best_search <- search[[cur_idx]]
      best_eval <- evals[[cur_idx]]
    }

    conv <- c(conv, best_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= fa.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, best_search, best_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}


# =============================================================================
# PSO-FGWC v3
# =============================================================================

psofgwc <- function(
    data, pop = NA, distmat = NA, ncluster = 2, m = 2,
    distance = "euclidean", order = 2, alpha = 0.7, a = 1, b = 1,
    error = 1e-5, max.iter = 100, randomN = 0, vi.dist = "uniform",
    npar = 10, vmax = 0.7, pso.same = 10, c1 = 0.49, c2 = 0.49,
    w.inert = "sim.annealing", wmax = 0.9, wmin = 0.4, map = 0.4) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (npar < 2L) stop("PSO requires at least 2 particles.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, npar, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness

  velocity <- lapply(
    seq_len(npar),
    function(i) {
      set.seed(randomN + i * 1009L + 500000L)
      matrix(
        runif(ncluster * ctx$p, -vmax, vmax),
        nrow = ncluster,
        ncol = ctx$p
      )
    }
  )

  pbest <- search
  pbest_eval <- evals
  pbest_fit <- fit

  best_idx <- which.min(fit)
  gbest <- search[[best_idx]]
  gbest_eval <- evals[[best_idx]]
  gbest_fit <- fit[best_idx]

  conv <- gbest_fit
  same <- 0L
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    theta <- if (exists("update_inertia", mode = "function")) {
      update_inertia(
        w.inert, wmax, wmin, map,
        iter - 1L, max.iter
      )
    } else {
      wmax - (wmax - wmin) * ((iter - 1L) / max(1, max.iter - 1L))
    }

    candidates <- vector("list", npar)

    for (i in seq_len(npar)) {
      set.seed(randomN + iter * 100000L + i * 100L + 1L)
      r1 <- matrix(runif(ncluster * ctx$p), ncluster, ctx$p)

      set.seed(randomN + iter * 100000L + i * 100L + 2L)
      r2 <- matrix(runif(ncluster * ctx$p), ncluster, ctx$p)

      velocity[[i]] <-
        theta * velocity[[i]] +
        c1 * r1 * (pbest[[i]] - search[[i]]) +
        c2 * r2 * (gbest - search[[i]])

      velocity[[i]] <- pmin(
        pmax(velocity[[i]], -vmax),
        vmax
      )

      candidates[[i]] <- clamp_centroids(
        search[[i]] + velocity[[i]],
        ctx$data
      )
    }

    evals <- lapply(
      candidates,
      function(cand) {
        .soviclust_v3_eval(
          ctx, cand, m, distance, order, alpha, a, b
        )
      }
    )

    search <- lapply(evals, `[[`, "search_centers")
    fit <- vapply(evals, `[[`, numeric(1), "fitness")

    improved <- which(fit < pbest_fit)

    for (i in improved) {
      pbest[[i]] <- search[[i]]
      pbest_eval[[i]] <- evals[[i]]
      pbest_fit[i] <- fit[i]
    }

    cur_idx <- which.min(pbest_fit)

    if (pbest_fit[cur_idx] < gbest_fit) {
      gbest_fit <- pbest_fit[cur_idx]
      gbest <- pbest[[cur_idx]]
      gbest_eval <- pbest_eval[[cur_idx]]
    }

    conv <- c(conv, gbest_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= pso.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, gbest, gbest_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}


# =============================================================================
# TLBO-FGWC v3
# =============================================================================

tlbofgwc <- function(
    data, pop = NA, distmat = NA, ncluster = 2, m = 2,
    distance = "euclidean", order = 2, alpha = 0.7, a = 1, b = 1,
    error = 1e-5, max.iter = 100, randomN = 0, vi.dist = "uniform",
    nstud = 10, tlbo.same = 10, nselection = 10,
    elitism = FALSE, n.elite = 2) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (nstud < 2L) stop("TLBO requires at least 2 students.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, nstud, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness

  best_idx <- which.min(fit)
  best_search <- search[[best_idx]]
  best_eval <- evals[[best_idx]]
  best_fit <- fit[best_idx]

  conv <- best_fit
  same <- 0L
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    teacher_idx <- which.min(fit)
    teacher <- search[[teacher_idx]]
    population_mean <- Reduce("+", search) / nstud

    # Teacher phase: scalar TF in {1,2}; independent random matrix per learner.
    for (i in seq_len(nstud)) {
      step <- .soviclust_v3_tlbo_teacher_candidate(
        student = search[[i]],
        teacher = teacher,
        population_mean = population_mean,
        seed = randomN + iter * 100000L + i * 100L + 1L
      )

      cand <- clamp_centroids(step$candidate, ctx$data)

      ev <- .soviclust_v3_eval(
        ctx, cand, m, distance, order, alpha, a, b
      )

      if (ev$fitness < fit[i]) {
        search[[i]] <- ev$search_centers
        evals[[i]] <- ev
        fit[i] <- ev$fitness
      }
    }

    # Learner phase: each learner interacts with one different learner.
    for (i in seq_len(nstud)) {
      set.seed(randomN + iter * 100000L + i * 100L + 50L)
      partner_idx <- sample(setdiff(seq_len(nstud), i), 1)

      step <- .soviclust_v3_tlbo_learner_candidate(
        student = search[[i]],
        partner = search[[partner_idx]],
        student_fit = fit[i],
        partner_fit = fit[partner_idx],
        seed = randomN + iter * 100000L + i * 100L + 51L
      )

      cand <- clamp_centroids(step$candidate, ctx$data)

      ev <- .soviclust_v3_eval(
        ctx, cand, m, distance, order, alpha, a, b
      )

      if (ev$fitness < fit[i]) {
        search[[i]] <- ev$search_centers
        evals[[i]] <- ev
        fit[i] <- ev$fitness
      }
    }

    if (isTRUE(elitism) && n.elite > 0L) {
      worst <- order(fit, decreasing = TRUE)[
        seq_len(min(n.elite, nstud))
      ]

      for (i in worst) {
        search[[i]] <- best_search
        evals[[i]] <- best_eval
        fit[i] <- best_fit
      }
    }

    cur_idx <- which.min(fit)

    if (fit[cur_idx] < best_fit) {
      best_fit <- fit[cur_idx]
      best_search <- search[[cur_idx]]
      best_eval <- evals[[cur_idx]]
    }

    conv <- c(conv, best_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= tlbo.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, best_search, best_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}


# =============================================================================
# WOA-FGWC v3
# =============================================================================

woafgwc <- function(
    data, pop = NA, distmat = NA,
    ncluster = 2, m = 2,
    distance = "euclidean", order = 2,
    alpha = 0.7, a = 1, b = 1,
    error = 1e-5, max.iter = 100,
    randomN = 0, vi.dist = "uniform",
    nwhale = 10, woa.b = 1, woa.same = 10) {

  ptm <- proc.time()
  ctx <- .soviclust_v3_validate_common(
    data, pop, distmat, ncluster, m, alpha
  )

  if (nwhale < 2L) stop("WOA requires at least 2 whales.", call. = FALSE)

  state <- .soviclust_v3_init_population(
    ctx, nwhale, ncluster, vi.dist, randomN,
    m, distance, order, alpha, a, b
  )

  search <- state$search
  evals <- state$evals
  fit <- state$fitness

  best_idx <- which.min(fit)
  prey <- search[[best_idx]]
  prey_eval <- evals[[best_idx]]
  prey_fit <- fit[best_idx]

  conv <- prey_fit
  same <- 0L
  iter_done <- 0L

  for (iter in seq_len(max.iter)) {
    iter_done <- iter

    a_coef <- if (max.iter == 1L) {
      0
    } else {
      2 - 2 * ((iter - 1) / (max.iter - 1))
    }

    candidates <- .soviclust_v3_woa_move(
      swarm = search,
      prey = prey,
      a_coef = a_coef,
      b = woa.b,
      seed = randomN,
      iter = iter,
      data = ctx$data
    )

    evals <- lapply(
      candidates,
      function(cand) {
        .soviclust_v3_eval(
          ctx, cand, m, distance, order, alpha, a, b
        )
      }
    )

    search <- lapply(evals, `[[`, "search_centers")
    fit <- vapply(evals, `[[`, numeric(1), "fitness")

    cur_idx <- which.min(fit)

    if (fit[cur_idx] < prey_fit) {
      prey_fit <- fit[cur_idx]
      prey <- search[[cur_idx]]
      prey_eval <- evals[[cur_idx]]
    }

    conv <- c(conv, prey_fit)
    same <- .soviclust_v3_stagnation(conv, same, error)

    if (same >= woa.same) break
  }

  .soviclust_v3_result_m(
    ctx$data, prey, prey_eval, conv,
    iter_done, same, match.call(), ptm, m
  )
}
