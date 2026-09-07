# tests/testthat/test-optimizer-v3-nfe.R
# =============================================================================
# Patch v3.1a â€” NFE instrumentation tests
#
# Scope:
#   * count one .soviclust_v3_eval() candidate call as one NFE;
#   * separate initialization and optimization evaluations;
#   * expose NFE accounting in all nine optimizer result objects;
#   * preserve deterministic optimizer behavior.
#
# max_nfe enforcement is intentionally NOT part of Patch v3.1a.
# =============================================================================


test_that("v3 NFE tracker starts at zero and preserves accounting invariant", {
  dat <- v3_test_data()
  ctx <- v3_env$.soviclust_v3_validate_common(
    dat$x, dat$pop, dat$dmat,
    ncluster = 2, m = 2, alpha = 0.7
  )

  snap <- v3_env$.soviclust_v3_nfe_snapshot(ctx)

  expect_identical(snap$nfe, 0L)
  expect_identical(snap$nfe_initialization, 0L)
  expect_identical(snap$nfe_optimization, 0L)
  expect_identical(
    snap$nfe,
    snap$nfe_initialization + snap$nfe_optimization
  )
})


test_that("centralized evaluator counts exactly one NFE per candidate call", {
  dat <- v3_test_data()
  ctx <- v3_env$.soviclust_v3_validate_common(
    dat$x, dat$pop, dat$dmat,
    ncluster = 2, m = 2, alpha = 0.7
  )
  centers <- rbind(
    c(0.05, 0.05),
    c(5.00, 5.00)
  )

  v3_env$.soviclust_v3_set_nfe_phase(ctx, "initialization")
  invisible(v3_env$.soviclust_v3_eval(
    ctx, centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1
  ))

  snap1 <- v3_env$.soviclust_v3_nfe_snapshot(ctx)
  expect_identical(snap1$nfe, 1L)
  expect_identical(snap1$nfe_initialization, 1L)
  expect_identical(snap1$nfe_optimization, 0L)

  v3_env$.soviclust_v3_set_nfe_phase(ctx, "optimization")
  invisible(v3_env$.soviclust_v3_eval(
    ctx, centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1
  ))

  snap2 <- v3_env$.soviclust_v3_nfe_snapshot(ctx)
  expect_identical(snap2$nfe, 2L)
  expect_identical(snap2$nfe_initialization, 1L)
  expect_identical(snap2$nfe_optimization, 1L)
  expect_identical(
    snap2$nfe,
    snap2$nfe_initialization + snap2$nfe_optimization
  )
})


test_that("all v3 optimizers expose valid NFE accounting", {
  dat <- v3_test_data()
  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  for (method in methods) {
    res <- run_v3_optimizer(method, dat, seed = 42L)

    expect_true(all(c(
      "nfe",
      "nfe_initialization",
      "nfe_optimization"
    ) %in% names(res)))
    expect_type(res$nfe, "integer")
    expect_type(res$nfe_initialization, "integer")
    expect_type(res$nfe_optimization, "integer")
    expect_gt(res$nfe, 0L)
    expect_gt(res$nfe_initialization, 0L)
    expect_gt(res$nfe_optimization, 0L)
    expect_identical(
      res$nfe,
      res$nfe_initialization + res$nfe_optimization
    )
    expect_identical(
      res$fitness_type,
      "spatial_XB_feasible"
    )
    expect_identical(res$occupied_clusters, 2L)
    expect_equal(length(unique(res$cluster)), 2L)
  }
})


test_that("NFE accounting is deterministic for a fixed seed", {
  dat <- v3_test_data()
  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  for (method in methods) {
    a <- run_v3_optimizer(method, dat, seed = 2026L)
    b <- run_v3_optimizer(method, dat, seed = 2026L)

    expect_identical(a$nfe, b$nfe)
    expect_identical(
      a$nfe_initialization,
      b$nfe_initialization
    )
    expect_identical(
      a$nfe_optimization,
      b$nfe_optimization
    )
    expect_equal(a$f_obj, b$f_obj, tolerance = 1e-12)
    expect_equal(
      a$search_centroid,
      b$search_centroid,
      tolerance = 1e-12
    )
  }
})


test_that("NFE reveals different per-iteration evaluation costs", {
  dat <- v3_test_data()

  # Fixture: population size = 5 and max.iter = 3.
  # GWO evaluates one candidate per wolf per iteration: 5 * 3 = 15.
  gwo <- run_v3_optimizer("GWO", dat, seed = 42L)
  expect_identical(gwo$nfe_optimization, 15L)

  # TLBO evaluates all students in teacher AND learner phases:
  # 2 * 5 * 3 = 30 candidate evaluations.
  tlbo <- run_v3_optimizer("TLBO", dat, seed = 42L)
  expect_identical(tlbo$nfe_optimization, 30L)

  expect_gt(tlbo$nfe_optimization, gwo$nfe_optimization)
})