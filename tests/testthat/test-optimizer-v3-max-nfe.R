# tests/testthat/test-optimizer-v3-max-nfe.R
# =============================================================================
# Patch v3.1b — exact max_nfe budget-control tests
# =============================================================================


test_that("max_nfe validation accepts NULL/positive integers and rejects invalid values", {
  expect_silent(v3_env$.soviclust_v3_new_nfe_tracker(NULL))
  expect_silent(v3_env$.soviclust_v3_new_nfe_tracker(1L))
  expect_silent(v3_env$.soviclust_v3_new_nfe_tracker(100))

  expect_error(
    v3_env$.soviclust_v3_new_nfe_tracker(0),
    "max_nfe"
  )
  expect_error(
    v3_env$.soviclust_v3_new_nfe_tracker(-1),
    "max_nfe"
  )
  expect_error(
    v3_env$.soviclust_v3_new_nfe_tracker(1.5),
    "max_nfe"
  )
  expect_error(
    v3_env$.soviclust_v3_new_nfe_tracker(Inf),
    "max_nfe"
  )
})


test_that("central evaluator never increments beyond max_nfe", {
  dat <- v3_test_data()

  ctx <- v3_env$.soviclust_v3_validate_common(
    dat$x,
    dat$pop,
    dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max_nfe = 2L
  )

  centers <- rbind(
    c(0.05, 0.05),
    c(5.00, 5.00)
  )

  invisible(v3_env$.soviclust_v3_eval(
    ctx, centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1
  ))

  invisible(v3_env$.soviclust_v3_eval(
    ctx, centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1
  ))

  expect_error(
    v3_env$.soviclust_v3_eval(
      ctx, centers,
      m = 2,
      distance = "euclidean",
      order = 2,
      alpha = 0.7,
      a = 1,
      b = 1
    ),
    "max_nfe"
  )

  snap <- v3_env$.soviclust_v3_nfe_snapshot(ctx)

  expect_identical(snap$nfe, 2L)
  expect_identical(snap$max_nfe, 2L)
  expect_identical(snap$nfe_remaining, 0L)
  expect_true(snap$budget_exhausted)
  expect_identical(snap$termination_reason, "max_nfe")
})


test_that("budget-aware evaluator returns a sentinel without overshooting", {
  dat <- v3_test_data()

  ctx <- v3_env$.soviclust_v3_validate_common(
    dat$x,
    dat$pop,
    dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max_nfe = 1L
  )

  centers <- rbind(
    c(0.05, 0.05),
    c(5.00, 5.00)
  )

  first <- v3_env$.soviclust_v3_try_eval(
    ctx, centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1
  )

  skipped <- v3_env$.soviclust_v3_try_eval(
    ctx, centers,
    m = 2,
    distance = "euclidean",
    order = 2,
    alpha = 0.7,
    a = 1,
    b = 1
  )

  expect_false(isTRUE(first$budget_skipped))
  expect_true(isTRUE(skipped$budget_skipped))
  expect_true(is.infinite(skipped$fitness))

  snap <- v3_env$.soviclust_v3_nfe_snapshot(ctx)
  expect_identical(snap$nfe, 1L)
  expect_true(snap$budget_exhausted)
  expect_identical(snap$termination_reason, "max_nfe")
})


test_that("max_nfe NULL preserves Patch v3.1a behavior for all optimizers", {
  dat <- v3_test_data()
  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  for (method in methods) {
    implicit <- run_v3_optimizer(
      method,
      dat,
      seed = 2026L
    )

    explicit <- run_v3_optimizer(
      method,
      dat,
      seed = 2026L,
      max_nfe = NULL
    )

    expect_identical(implicit$nfe, explicit$nfe)
    expect_identical(
      implicit$nfe_initialization,
      explicit$nfe_initialization
    )
    expect_identical(
      implicit$nfe_optimization,
      explicit$nfe_optimization
    )
    expect_equal(
      implicit$f_obj,
      explicit$f_obj,
      tolerance = 1e-12
    )
    expect_equal(
      implicit$search_centroid,
      explicit$search_centroid,
      tolerance = 1e-12
    )
    expect_equal(
      implicit$membership,
      explicit$membership,
      tolerance = 1e-12
    )

    expect_true(is.na(explicit$max_nfe))
    expect_true(is.na(explicit$nfe_remaining))
    expect_false(explicit$budget_exhausted)
    expect_true(
      explicit$termination_reason %in%
        c("max_iter", "convergence")
    )
  }
})


test_that("all nine optimizers obey an exact total NFE cap", {
  dat <- v3_test_data()
  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  for (method in methods) {
    baseline <- run_v3_optimizer(
      method,
      dat,
      seed = 2026L
    )

    # Give every method exactly seven optimization evaluations after
    # reproducing its deterministic initialization.
    budget <- as.integer(
      baseline$nfe_initialization + 7L
    )

    limited <- run_v3_optimizer(
      method,
      dat,
      seed = 2026L,
      max_nfe = budget
    )

    repeat_limited <- run_v3_optimizer(
      method,
      dat,
      seed = 2026L,
      max_nfe = budget
    )

    expect_lte(limited$nfe, budget)
    expect_identical(limited$nfe, budget)
    expect_identical(limited$max_nfe, budget)

    expect_identical(
      limited$nfe_initialization,
      baseline$nfe_initialization
    )
    expect_identical(
      limited$nfe_optimization,
      7L
    )

    expect_true(limited$budget_exhausted)
    expect_identical(limited$nfe_remaining, 0L)
    expect_identical(
      limited$termination_reason,
      "max_nfe"
    )

    expect_identical(
      limited$nfe,
      limited$nfe_initialization +
        limited$nfe_optimization
    )

    expect_true(is.finite(limited$f_obj))
    expect_identical(limited$occupied_clusters, 2L)
    expect_equal(length(unique(limited$cluster)), 2L)

    expect_identical(
      limited$nfe,
      repeat_limited$nfe
    )
    expect_equal(
      limited$f_obj,
      repeat_limited$f_obj,
      tolerance = 1e-12
    )
    expect_equal(
      limited$search_centroid,
      repeat_limited$search_centroid,
      tolerance = 1e-12
    )
  }
})


test_that("too-small max_nfe fails cleanly during initialization", {
  dat <- v3_test_data()

  expect_error(
    run_v3_optimizer(
      "GWO",
      dat,
      seed = 2026L,
      max_nfe = 1L
    ),
    "max_nfe.*initialization"
  )
})
