# tests/testthat/test-optimizer-v3-lfgwc-evaluator.R
# =============================================================================
# Patch v3.2b — LFGWC model-specific evaluator
# =============================================================================


.v3_lfgwc_block_weights <- function(n = 8L) {

  if (n != 8L) {
    stop("Test fixture expects n = 8.")
  }

  W <- matrix(
    0,
    nrow = n,
    ncol = n
  )

  groups <- list(
    1:4,
    5:8
  )

  for (g in groups) {
    for (i in g) {
      nbr <- setdiff(
        g,
        i
      )
      W[
        i,
        nbr
      ] <- 1 / length(nbr)
    }
  }

  W
}


.v3_lfgwc_cross_weights <- function(n = 8L) {

  if (n != 8L) {
    stop("Test fixture expects n = 8.")
  }

  W <- matrix(
    0,
    nrow = n,
    ncol = n
  )

  for (i in seq_len(n)) {
    j <- if (i <= 4L) {
      i + 4L
    } else {
      i - 4L
    }

    W[i, j] <- 1
  }

  W
}


.v3_run_lfgwc_optimizer <- function(
    method,
    dat,
    evaluator,
    seed = 2026L,
    max_nfe = 200L) {

  common <- list(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1,
    error = 0,
    max.iter = 1000L,
    max_nfe = max_nfe,
    evaluator = evaluator,
    randomN = seed,
    vi.dist = "uniform"
  )

  extra <- switch(
    method,

    ABC = list(
      nfood = 5,
      n.onlooker = 3,
      limit = 3,
      pso = FALSE,
      abc.same = 5000L
    ),

    FPA = list(
      nflow = 5,
      p = 0.8,
      flow.same = 5000L
    ),

    GSA = list(
      npar = 5,
      par.no = 2,
      gsa.same = 5000L
    ),

    GWO = list(
      nwolf = 5,
      wolf.same = 5000L
    ),

    HHO = list(
      nhh = 5,
      hh.same = 5000L
    ),

    IFA = list(
      nfly = 5,
      ffly.no = 2,
      fa.same = 5000L
    ),

    PSO = list(
      npar = 5,
      pso.same = 5000L
    ),

    TLBO = list(
      nstud = 5,
      nselection = 5,
      tlbo.same = 5000L
    ),

    WOA = list(
      nwhale = 5,
      woa.same = 5000L
    ),

    stop(
      "Unknown optimizer method.",
      call. = FALSE
    )
  )

  fn_name <- switch(
    method,
    ABC = "abcfgwc",
    FPA = "fpafgwc",
    GSA = "gsafgwc",
    GWO = "gwofgwc",
    HHO = "hhofgwc",
    IFA = "ifafgwc",
    PSO = "psofgwc",
    TLBO = "tlbofgwc",
    WOA = "woafgwc"
  )

  fn <- get(
    fn_name,
    envir = v3_env,
    inherits = FALSE
  )

  do.call(
    fn,
    c(
      common,
      extra
    )
  )
}


test_that("LFGWC evaluator contract and spatial weights are validated", {

  W <- .v3_lfgwc_block_weights()

  ev <- v3_env$.soviclust_v3_lfgwc_evaluator(
    W
  )

  expect_true(
    is.list(ev)
  )

  expect_identical(
    ev$model_type,
    "LFGWC"
  )

  expect_identical(
    ev$fitness_type,
    "lfgwc_spatial_XB_feasible"
  )

  expect_true(
    is.function(
      ev$evaluate
    )
  )

  bad_sum <- W
  bad_sum[1, ] <- bad_sum[1, ] * 0.5

  expect_error(
    v3_env$.soviclust_v3_lfgwc_evaluator(
      bad_sum
    ),
    "row-standardized"
  )

  bad_diag <- W
  bad_diag[1, 1] <- 0.1
  bad_diag[1, 2] <- bad_diag[1, 2] - 0.1

  expect_error(
    v3_env$.soviclust_v3_lfgwc_evaluator(
      bad_diag
    ),
    "diagonal"
  )

  bad_negative <- W
  bad_negative[1, 2] <- -0.1

  expect_error(
    v3_env$.soviclust_v3_lfgwc_evaluator(
      bad_negative
    ),
    "non-negative"
  )
})


test_that("LFGWC evaluator equals one manual local projection", {

  dat <- v3_test_data()
  W <- .v3_lfgwc_block_weights()
  alpha <- 0.7

  evaluator <- v3_env$.soviclust_v3_lfgwc_evaluator(
    W
  )

  ctx <- v3_env$.soviclust_v3_validate_common(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = alpha,
    max_nfe = NULL,
    evaluator = evaluator
  )

  centers <- rbind(
    c(0.05, 0.05),
    c(5.00, 5.00)
  )

  got <- ctx$evaluator$evaluate(
    ctx = ctx,
    search_centers = centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = alpha,
    a = 1,
    b = 1,
    require_all_clusters = TRUE
  )

  base_u <- v3_env$membership_from_centroids(
    data = dat$x,
    centers = centers,
    m = 2,
    distance = "euclidean",
    order = 2
  )$u

  neighbor_u <- W %*% base_u

  manual_u <- (
    alpha * base_u +
      (1 - alpha) * neighbor_u
  )

  manual_u <- manual_u /
    rowSums(manual_u)

  manual_v <- v3_env$centroid_from_membership(
    data = dat$x,
    uij = manual_u,
    m = 2
  )

  d2 <- vapply(
    seq_len(
      nrow(manual_v)
    ),
    function(k) {
      rowSums(
        sweep(
          dat$x,
          2,
          manual_v[k, ],
          "-"
        )^2
      )
    },
    numeric(
      nrow(dat$x)
    )
  )

  manual_j <- sum(
    (manual_u^2) * d2
  )

  centroid_d2 <- as.matrix(
    stats::dist(
      manual_v
    )
  )^2

  diag(
    centroid_d2
  ) <- Inf

  manual_xb <- manual_j /
    (
      nrow(dat$x) *
        min(centroid_d2)
    )

  expect_equal(
    got$membership,
    manual_u,
    tolerance = 1e-14
  )

  expect_equal(
    got$centroid,
    manual_v,
    tolerance = 1e-14
  )

  expect_equal(
    got$spatial_obj,
    manual_j,
    tolerance = 1e-12
  )

  expect_equal(
    got$fitness,
    manual_xb,
    tolerance = 1e-12
  )

  expect_identical(
    got$model_type,
    "LFGWC"
  )

  expect_true(
    got$feasible
  )
})


test_that("alpha = 1 removes the local geographic contribution", {

  dat <- v3_test_data()
  W <- .v3_lfgwc_cross_weights()

  evaluator <- v3_env$.soviclust_v3_lfgwc_evaluator(
    W
  )

  ctx <- v3_env$.soviclust_v3_validate_common(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 1,
    max_nfe = NULL,
    evaluator = evaluator
  )

  centers <- rbind(
    c(0.05, 0.05),
    c(5.00, 5.00)
  )

  got <- ctx$evaluator$evaluate(
    ctx = ctx,
    search_centers = centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 1,
    a = 1,
    b = 1,
    require_all_clusters = TRUE
  )

  base_u <- v3_env$membership_from_centroids(
    data = dat$x,
    centers = centers,
    m = 2,
    distance = "euclidean",
    order = 2
  )$u

  expected_v <- v3_env$centroid_from_membership(
    data = dat$x,
    uij = base_u,
    m = 2
  )

  expect_equal(
    got$membership,
    base_u,
    tolerance = 1e-14
  )

  expect_equal(
    got$centroid,
    expected_v,
    tolerance = 1e-14
  )
})


test_that("LFGWC evaluator responds to local topology and differs from FGWC global projection", {

  dat <- v3_test_data()

  W_block <- .v3_lfgwc_block_weights()
  W_cross <- .v3_lfgwc_cross_weights()

  ev_block <- v3_env$.soviclust_v3_lfgwc_evaluator(
    W_block
  )

  ev_cross <- v3_env$.soviclust_v3_lfgwc_evaluator(
    W_cross
  )

  ev_global <- v3_env$.soviclust_v3_fgwc_evaluator()

  centers <- rbind(
    c(0.10, 0.10),
    c(4.90, 4.90)
  )

  make_ctx <- function(ev) {
    v3_env$.soviclust_v3_validate_common(
      data = dat$x,
      pop = dat$pop,
      distmat = dat$dmat,
      ncluster = 2,
      m = 2,
      alpha = 0.7,
      max_nfe = NULL,
      evaluator = ev
    )
  }

  eval_one <- function(ctx) {
    ctx$evaluator$evaluate(
      ctx = ctx,
      search_centers = centers,
      m = 2,
      distance = "euclidean",
      order = 2,
      alpha = 0.7,
      a = 1,
      b = 1,
      require_all_clusters = TRUE
    )
  }

  local_block <- eval_one(
    make_ctx(ev_block)
  )

  local_cross <- eval_one(
    make_ctx(ev_cross)
  )

  global_fgwc <- eval_one(
    make_ctx(ev_global)
  )

  expect_gt(
    max(
      abs(
        local_block$membership -
          local_cross$membership
      )
    ),
    1e-6
  )

  expect_gt(
    max(
      abs(
        local_block$membership -
          global_fgwc$membership
      )
    ),
    1e-8
  )
})


test_that("collapsed LFGWC candidate is infeasible", {

  dat <- v3_test_data()

  evaluator <- v3_env$.soviclust_v3_lfgwc_evaluator(
    .v3_lfgwc_block_weights()
  )

  ctx <- v3_env$.soviclust_v3_validate_common(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max_nfe = NULL,
    evaluator = evaluator
  )

  centers <- rbind(
    c(2.5, 2.5),
    c(2.5, 2.5)
  )

  got <- ctx$evaluator$evaluate(
    ctx = ctx,
    search_centers = centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1,
    require_all_clusters = TRUE
  )

  expect_false(
    got$feasible
  )

  expect_identical(
    got$occupied_clusters,
    1L
  )

  expect_true(
    is.infinite(
      got$fitness
    )
  )
})


test_that("centralized NFE gateway counts one LFGWC candidate as one NFE", {

  dat <- v3_test_data()

  evaluator <- v3_env$.soviclust_v3_lfgwc_evaluator(
    .v3_lfgwc_block_weights()
  )

  ctx <- v3_env$.soviclust_v3_validate_common(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max_nfe = 1L,
    evaluator = evaluator
  )

  centers <- rbind(
    c(0.05, 0.05),
    c(5.00, 5.00)
  )

  got <- v3_env$.soviclust_v3_eval(
    ctx = ctx,
    search_centers = centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1,
    require_all_clusters = TRUE
  )

  snap <- v3_env$.soviclust_v3_nfe_snapshot(
    ctx
  )

  expect_true(
    got$feasible
  )

  expect_identical(
    snap$nfe,
    1L
  )

  expect_identical(
    snap$nfe_optimization,
    1L
  )

  expect_identical(
    snap$nfe_initialization,
    0L
  )

  expect_identical(
    snap$nfe_remaining,
    0L
  )

  expect_true(
    snap$budget_exhausted
  )
})


test_that("all nine canonical optimizers accept LFGWC evaluator with exact NFE", {

  dat <- v3_test_data()

  evaluator <- v3_env$.soviclust_v3_lfgwc_evaluator(
    .v3_lfgwc_block_weights()
  )

  methods <- c(
    "ABC", "FPA", "GSA",
    "GWO", "HHO", "IFA",
    "PSO", "TLBO", "WOA"
  )

  for (method in methods) {

    res <- .v3_run_lfgwc_optimizer(
      method = method,
      dat = dat,
      evaluator = evaluator,
      seed = 2026L,
      max_nfe = 200L
    )

    expect_identical(
      res$nfe,
      200L,
      info = method
    )

    expect_identical(
      res$nfe,
      res$nfe_initialization +
        res$nfe_optimization,
      info = method
    )

    expect_identical(
      res$nfe_remaining,
      0L,
      info = method
    )

    expect_true(
      res$budget_exhausted,
      info = method
    )

    expect_identical(
      res$termination_reason,
      "max_nfe",
      info = method
    )

    expect_identical(
      res$fitness_type,
      "lfgwc_spatial_XB_feasible",
      info = method
    )

    expect_true(
      is.finite(
        res$f_obj
      ),
      info = method
    )

    expect_identical(
      as.integer(
        res$occupied_clusters
      ),
      2L,
      info = method
    )

    expect_equal(
      rowSums(
        res$membership
      ),
      rep(
        1,
        nrow(dat$x)
      ),
      tolerance = 1e-12,
      info = method
    )
  }
})


test_that("LFGWC optimizer result is reproducible for fixed seed and exact budget", {

  dat <- v3_test_data()

  evaluator <- v3_env$.soviclust_v3_lfgwc_evaluator(
    .v3_lfgwc_block_weights()
  )

  a <- .v3_run_lfgwc_optimizer(
    method = "GWO",
    dat = dat,
    evaluator = evaluator,
    seed = 2026L,
    max_nfe = 200L
  )

  b <- .v3_run_lfgwc_optimizer(
    method = "GWO",
    dat = dat,
    evaluator = evaluator,
    seed = 2026L,
    max_nfe = 200L
  )

  expect_identical(
    a$nfe,
    b$nfe
  )

  expect_equal(
    a$f_obj,
    b$f_obj,
    tolerance = 1e-14
  )

  expect_equal(
    a$search_centroid,
    b$search_centroid,
    tolerance = 1e-14
  )

  expect_equal(
    a$centroid,
    b$centroid,
    tolerance = 1e-14
  )

  expect_equal(
    a$membership,
    b$membership,
    tolerance = 1e-14
  )

  expect_identical(
    a$cluster,
    b$cluster
  )
})
