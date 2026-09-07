# tests/testthat/test-optimizer-v3-generic-evaluator.R
# =============================================================================
# Patch v3.2a — Generic Evaluator Architecture
# =============================================================================

test_that("default evaluator contract resolves to FGWC", {
  ev <- v3_env$.soviclust_v3_fgwc_evaluator()

  expect_true(is.list(ev))
  expect_identical(ev$model_type, "FGWC")
  expect_identical(ev$fitness_type, "spatial_XB_feasible")
  expect_true(is.function(ev$evaluate))

  resolved <- v3_env$.soviclust_v3_resolve_evaluator(NULL)
  expect_identical(resolved$model_type, "FGWC")
  expect_identical(resolved$fitness_type, "spatial_XB_feasible")
})

test_that("invalid evaluator contracts are rejected", {
  expect_error(
    v3_env$.soviclust_v3_validate_evaluator(list()),
    "missing field"
  )

  expect_error(
    v3_env$.soviclust_v3_validate_evaluator(
      list(
        model_type = "TEST",
        fitness_type = "test",
        evaluate = 1
      )
    ),
    "must be a function"
  )
})

test_that("generic FGWC evaluator matches legacy evaluator", {
  dat <- v3_test_data()

  ctx <- v3_env$.soviclust_v3_validate_common(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max_nfe = NULL,
    evaluator = v3_env$.soviclust_v3_fgwc_evaluator()
  )

  centers <- rbind(
    c(0.05, 0.05),
    c(5.00, 5.00)
  )

  legacy <- v3_env$evaluate_optimizer_candidate_v3(
    data = ctx$data,
    search_centers = centers,
    mi.mj = ctx$mi.mj,
    distmat = ctx$distmat,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    beta = ctx$beta,
    a = 1,
    b = 1,
    require_all_clusters = TRUE
  )

  generic <- ctx$evaluator$evaluate(
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

  expect_equal(generic$fitness, legacy$fitness, tolerance = 1e-14)
  expect_equal(generic$spatial_obj, legacy$spatial_obj, tolerance = 1e-14)
  expect_equal(generic$membership, legacy$membership, tolerance = 1e-14)
  expect_equal(generic$centroid, legacy$centroid, tolerance = 1e-14)
  expect_identical(generic$cluster, legacy$cluster)
  expect_identical(generic$occupied_clusters, legacy$occupied_clusters)
  expect_identical(generic$feasible, legacy$feasible)
})

test_that("all nine optimizers preserve FGWC behavior with explicit evaluator", {
  dat <- v3_test_data()

  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  explicit_evaluator <- v3_env$.soviclust_v3_fgwc_evaluator()

  for (method in methods) {
    implicit <- run_v3_optimizer(
      method,
      dat,
      seed = 2026L,
      max_nfe = 100L
    )

    explicit <- run_v3_optimizer(
      method,
      dat,
      seed = 2026L,
      max_nfe = 100L,
      evaluator = explicit_evaluator
    )

    expect_identical(implicit$nfe, explicit$nfe, info = method)
    expect_identical(
      implicit$nfe_initialization,
      explicit$nfe_initialization,
      info = method
    )
    expect_identical(
      implicit$nfe_optimization,
      explicit$nfe_optimization,
      info = method
    )
    expect_identical(
      implicit$termination_reason,
      explicit$termination_reason,
      info = method
    )
    expect_identical(
      explicit$fitness_type,
      implicit$fitness_type,
      info = method
    )
    expect_identical(
      implicit$fitness_type,
      "spatial_XB_feasible",
      info = method
    )
    expect_equal(explicit$f_obj, implicit$f_obj, tolerance = 1e-12, info = method)
    expect_equal(
      explicit$spatial_obj,
      implicit$spatial_obj,
      tolerance = 1e-12,
      info = method
    )
    expect_equal(
      explicit$search_centroid,
      implicit$search_centroid,
      tolerance = 1e-12,
      info = method
    )
    expect_equal(
      explicit$centroid,
      implicit$centroid,
      tolerance = 1e-12,
      info = method
    )
    expect_equal(
      explicit$membership,
      implicit$membership,
      tolerance = 1e-12,
      info = method
    )
    expect_identical(explicit$cluster, implicit$cluster, info = method)
  }
})

test_that("custom evaluator fitness metadata flows through result", {
  dat <- v3_test_data()

  custom <- v3_env$.soviclust_v3_fgwc_evaluator()
  custom$model_type <- "TEST-FGWC"
  custom$fitness_type <- "test_spatial_XB_feasible"

  res <- run_v3_optimizer(
    "GWO",
    dat,
    seed = 2026L,
    max_nfe = 100L,
    evaluator = custom
  )

  expect_identical(
    res$fitness_type,
    "test_spatial_XB_feasible"
  )

  expect_lte(res$nfe, 100L)
  expect_identical(
    res$nfe,
    res$nfe_initialization + res$nfe_optimization
  )
})

test_that("dead typo helper is absent after v3.2a cleanup", {
  expect_false(
    exists(
      "n.soviclust_v3_new_position",
      envir = v3_env,
      inherits = FALSE
    )
  )

  expect_true(
    exists(
      ".soviclust_v3_new_position",
      envir = v3_env,
      inherits = FALSE,
      mode = "function"
    )
  )
})
