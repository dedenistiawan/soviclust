# tests/testthat/test-optimizer-v3-lfgwc-runtime.R
# =============================================================================
# Patch v3.2c — LFGWC runtime integration
# =============================================================================

.find_lfgwc_wrapper_v32c <- function() {
  # Development/source-tree layout:
  #   <project>/inst/app/R/LFGWC/lfgwc_wrapper.R
  #
  # Installed-package / R CMD check layout:
  #   <library>/soviclust/app/R/LFGWC/lfgwc_wrapper.R
  # because files under inst/ are installed at package top level.

  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  probe <- root

  repeat {
    candidate <- file.path(
      probe, "inst", "app", "R", "LFGWC", "lfgwc_wrapper.R"
    )

    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }

    parent <- dirname(probe)
    if (identical(parent, probe)) break
    probe <- parent
  }

  installed <- system.file(
    "app", "R", "LFGWC", "lfgwc_wrapper.R",
    package = "soviclust"
  )

  if (nzchar(installed) && file.exists(installed)) {
    return(normalizePath(installed, winslash = "/", mustWork = TRUE))
  }

  stop(
    paste0(
      "Could not locate LFGWC wrapper in either source-tree layout ",
      "(inst/app/...) or installed-package layout (app/...)."
    ),
    call. = FALSE
  )
}

if (!exists("%||%", envir = v3_env, inherits = TRUE)) {
  assign(
    "%||%",
    function(x, y) if (is.null(x)) y else x,
    envir = v3_env
  )
}

suppressMessages(
  sys.source(
    .find_lfgwc_wrapper_v32c(),
    envir = v3_env
  )
)

.v3_runtime_block_weights <- function(n = 8L) {
  if (n != 8L) stop("Runtime test fixture expects n = 8.")

  W <- matrix(0, nrow = n, ncol = n)

  for (g in list(1:4, 5:8)) {
    for (i in g) {
      nbr <- setdiff(g, i)
      W[i, nbr] <- 1 / length(nbr)
    }
  }

  W
}

.v3_runtime_opt_params <- function(method, max_nfe = 200L) {
  common <- list(
    vi_dist = "uniform",
    npar = 5L,
    same = 5000L,
    max_nfe = max_nfe
  )

  specific <- switch(
    method,
    abc = list(n_onlooker = 3L, limit = 3L, pso = FALSE),
    fpa = list(p = 0.8),
    gsa = list(par_no = 2L),
    gwo = list(),
    hho = list(),
    ifa = list(par_no = 2L),
    pso = list(),
    tlbo = list(nselection = 5L),
    woa = list(),
    stop("Unknown method.", call. = FALSE)
  )

  c(common, specific)
}

test_that("LFGWC runtime dispatcher routes all nine optimizers through evaluator v3", {

  dat <- v3_test_data()
  W <- .v3_runtime_block_weights()

  methods <- c(
    "abc", "fpa", "gsa",
    "gwo", "hho", "ifa",
    "pso", "tlbo", "woa"
  )

  for (method in methods) {
    res <- v3_env$lfgwc_with_optimizer(
      data = dat$x,
      pop_vec = dat$pop,
      dist_mat = dat$dmat,
      W_std = W,
      ncluster = 2,
      m = 2,
      alpha = 0.7,
      a = 1,
      b = 1,
      max_iter = 1000L,
      error = 0,
      randomN = 2026L,
      algorithm = method,
      opt_params = .v3_runtime_opt_params(method, max_nfe = 200L)
    )

    expect_identical(res$model_type, "LFGWC", info = method)
    expect_identical(
      res$fitness_type,
      "lfgwc_spatial_XB_feasible",
      info = method
    )
    expect_identical(res$nfe, 200L, info = method)
    expect_identical(
      res$nfe,
      res$nfe_initialization + res$nfe_optimization,
      info = method
    )
    expect_identical(res$nfe_remaining, 0L, info = method)
    expect_true(res$budget_exhausted, info = method)
    expect_identical(res$termination_reason, "max_nfe", info = method)
    expect_true(is.matrix(res$search_centroid), info = method)
    expect_equal(
      rowSums(res$membership),
      rep(1, nrow(dat$x)),
      tolerance = 1e-12,
      info = method
    )
    expect_identical(
      as.integer(res$occupied_clusters),
      2L,
      info = method
    )
    expect_true(is.list(res$validation), info = method)
  }
})

test_that("LFGWC runtime wrapper does not apply a second geographic projection", {

  dat <- v3_test_data()
  W <- .v3_runtime_block_weights()

  evaluator <- v3_env$.soviclust_v3_lfgwc_evaluator(W)

  direct <- v3_env$gwofgwc(
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
    evaluator = evaluator,
    max_nfe = 200L,
    randomN = 2026L,
    vi.dist = "uniform",
    nwolf = 5L,
    wolf.same = 5000L
  )

  wrapped <- v3_env$lfgwc_with_optimizer(
    data = dat$x,
    pop_vec = dat$pop,
    dist_mat = dat$dmat,
    W_std = W,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    a = 1,
    b = 1,
    max_iter = 1000L,
    error = 0,
    randomN = 2026L,
    algorithm = "gwo",
    opt_params = list(
      vi_dist = "uniform",
      npar = 5L,
      same = 5000L,
      max_nfe = 200L
    )
  )

  expect_equal(wrapped$f_obj, direct$f_obj, tolerance = 1e-14)
  expect_equal(
    wrapped$search_centroid,
    direct$search_centroid,
    tolerance = 1e-14
  )
  expect_equal(wrapped$centroid, direct$centroid, tolerance = 1e-14)
  expect_equal(wrapped$membership, direct$membership, tolerance = 1e-14)
  expect_identical(wrapped$cluster, direct$cluster)
  expect_identical(wrapped$nfe, direct$nfe)
})

test_that("run_lfgwc_shiny exposes v3 optimizer metadata on production path", {

  dat <- v3_test_data()

  raw <- data.frame(
    DISTRICTCODE = sprintf("%03d", seq_len(nrow(dat$x))),
    KABUPATEN = paste("District", seq_len(nrow(dat$x))),
    x1 = dat$x[, 1],
    x2 = dat$x[, 2],
    stringsAsFactors = FALSE
  )

  got <- v3_env$run_lfgwc_shiny(
    data_source = "raw",
    raw_data = raw,
    sovi_result = NULL,
    selected_vars = c("x1", "x2"),
    pop_vec = dat$pop,
    dist_mat = dat$dmat,
    algorithm = "gwo",
    ncluster = 2,
    lfgwc_params = list(
      m = 2,
      alpha = 0.7,
      a = 1,
      b = 1,
      max_iter = 1000L,
      error = 0,
      randomN = 2026L,
      dthr = -99,
      si = FALSE,
      exp_d = 2
    ),
    opt_params = list(
      vi_dist = "uniform",
      npar = 5L,
      same = 5000L,
      max_nfe = 100L
    ),
    id_col = "DISTRICTCODE",
    name_col = "KABUPATEN"
  )

  expect_identical(got$model_type, "LFGWC")
  expect_identical(got$fitness_type, "lfgwc_spatial_XB_feasible")
  expect_identical(got$nfe, 100L)
  expect_identical(
    got$nfe,
    got$nfe_initialization + got$nfe_optimization
  )
  expect_identical(got$nfe_remaining, 0L)
  expect_true(got$budget_exhausted)
  expect_identical(got$termination_reason, "max_nfe")
  expect_true(is.matrix(got$search_centroid))
  expect_equal(
    unname(rowSums(got$lfgwc_raw$membership)),
    rep(1, nrow(dat$x)),
    tolerance = 1e-12
  )
  expect_identical(nrow(got$result_df), nrow(dat$x))
})

