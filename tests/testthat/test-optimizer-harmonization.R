# tests/testthat/test-optimizer-harmonization.R
# =============================================================================
# Patch-v3 architecture tests.
# IMPORTANT: this file intentionally uses ONLY `v3_env`.
# =============================================================================


test_that("isolated v3 engine is active before architecture tests", {
  expect_true(
    exists(
      "evaluate_optimizer_candidate_v3",
      envir = v3_env,
      inherits = FALSE,
      mode = "function"
    )
  )

  expect_true(
    .v3_loader_mode %in% c(
      "source-project",
      "installed-package"
    )
  )
})


test_that("v3 unified evaluator penalizes collapsed or infeasible partitions", {
  dat <- v3_test_data()

  popmat <- matrix(dat$pop, ncol = 1)
  mimj <- popmat %*% t(popmat)

  collapsed <- rbind(
    colMeans(dat$x),
    colMeans(dat$x)
  )

  ev <- v3_env$evaluate_optimizer_candidate_v3(
    data = dat$x,
    search_centers = collapsed,
    mi.mj = mimj,
    distmat = dat$dmat,
    m = 2,
    alpha = 0.7,
    beta = 0.3,
    a = 1,
    b = 1
  )

  expect_false(ev$feasible)
  expect_true(is.infinite(ev$fitness))
})


test_that("v3 evaluator keeps optimizer state separate from spatial centroid", {
  dat <- v3_test_data()

  popmat <- matrix(dat$pop, ncol = 1)
  mimj <- popmat %*% t(popmat)

  raw <- rbind(
    c(0.20, 0.15),
    c(4.80, 4.85)
  )

  ev <- v3_env$evaluate_optimizer_candidate_v3(
    data = dat$x,
    search_centers = raw,
    mi.mj = mimj,
    distmat = dat$dmat,
    m = 2,
    alpha = 0.7,
    beta = 0.3,
    a = 1,
    b = 1
  )

  expect_equal(
    ev$search_centers,
    raw,
    tolerance = 1e-12
  )

  expect_equal(
    rowSums(ev$membership),
    rep(1, nrow(dat$x)),
    tolerance = 1e-10
  )

  expect_true(all(is.finite(ev$centroid)))
})


test_that("all nine v3 optimizers use feasible spatial-XB fitness", {
  dat <- v3_test_data()

  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  for (method in methods) {
    res <- run_v3_optimizer(method, dat)

    expect_s3_class(
      res,
      "fgwc"
    )

    expect_identical(
      res$fitness_type,
      "spatial_XB_feasible",
      info = paste(method, "fitness_type")
    )

    expect_true(
      is.matrix(res$search_centroid),
      info = paste(method, "search_centroid")
    )

    expect_true(
      is.matrix(res$centroid),
      info = paste(method, "spatial centroid")
    )

    expected_xb <- v3_env$XB1(
      data = dat$x,
      uij = res$membership,
      vi = res$centroid,
      m = 2
    )

    expect_equal(
      res$f_obj,
      expected_xb,
      tolerance = 1e-8,
      info = paste(method, "f_obj")
    )

    expected_j <- v3_env$fgwc_objective(
      data = dat$x,
      uij = res$membership,
      centers = res$centroid,
      m = 2,
      distance = "euclidean",
      order = 2
    )

    expect_equal(
      res$spatial_obj,
      expected_j,
      tolerance = 1e-8,
      info = paste(method, "spatial_obj")
    )

    expect_equal(
      rowSums(res$membership),
      rep(1, nrow(dat$x)),
      tolerance = 1e-8,
      info = paste(method, "membership")
    )

    expect_equal(
      length(unique(res$cluster)),
      2,
      info = paste(method, "occupancy")
    )

    expect_true(
      all(diff(res$converg) <= 1e-8),
      info = paste(method, "convergence monotonicity")
    )

    expect_equal(
      tail(res$converg, 1),
      res$f_obj,
      tolerance = 1e-8,
      info = paste(method, "convergence tail")
    )
  }
})


test_that("all nine v3 optimizers are reproducible for a fixed seed", {
  dat <- v3_test_data()

  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  for (method in methods) {
    r1 <- run_v3_optimizer(method, dat, seed = 42)
    r2 <- run_v3_optimizer(method, dat, seed = 42)

    expect_equal(
      r1$f_obj,
      r2$f_obj,
      tolerance = 1e-10,
      info = paste(method, "fitness reproducibility")
    )

    expect_equal(
      r1$search_centroid,
      r2$search_centroid,
      tolerance = 1e-10,
      info = paste(method, "search-state reproducibility")
    )

    expect_equal(
      r1$centroid,
      r2$centroid,
      tolerance = 1e-10,
      info = paste(method, "spatial-centroid reproducibility")
    )

    expect_equal(
      r1$membership,
      r2$membership,
      tolerance = 1e-10,
      info = paste(method, "membership reproducibility")
    )
  }
})
