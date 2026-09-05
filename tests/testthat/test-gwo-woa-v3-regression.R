# tests/testthat/test-gwo-woa-v3-regression.R
# =============================================================================
# Regression tests protecting GWO/WOA movement formulas.
# Uses ONLY the isolated `v3_env`.
# =============================================================================


test_that("v3 GWO movement matches canonical Alpha-Beta-Delta equations", {
  dat <- v3_test_data()

  swarm <- list(
    rbind(c(0.20, 0.20), c(4.80, 4.80)),
    rbind(c(0.30, 0.10), c(4.90, 5.00)),
    rbind(c(0.10, 0.30), c(5.00, 4.90))
  )

  alpha_pos <- swarm[[1]]
  beta_pos <- swarm[[2]]
  delta_pos <- swarm[[3]]

  seed <- 10
  iter <- 1
  a_coef <- 2

  got <- v3_env$.soviclust_v3_gwo_move(
    swarm,
    alpha_pos,
    beta_pos,
    delta_pos,
    a_coef,
    seed,
    iter,
    data = dat$x
  )

  expected <- lapply(seq_along(swarm), function(i) {
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

    v3_env$clamp_centroids(
      (X1 + X2 + X3) / 3,
      dat$x
    )
  })

  expect_equal(got, expected, tolerance = 1e-12)
})


test_that("v3 WOA movement matches the protected legacy movement", {
  dat <- v3_test_data()

  swarm <- list(
    rbind(c(0.20, 0.20), c(4.80, 4.80)),
    rbind(c(0.30, 0.10), c(4.90, 5.00)),
    rbind(c(0.10, 0.30), c(5.00, 4.90))
  )

  prey <- swarm[[1]]

  legacy <- v3_env$woa.move(
    swarm = swarm,
    prey = prey,
    a_coef = 2,
    b = 1,
    nwhale = 3,
    seed = 10,
    iter = 1,
    data = dat$x
  )

  v3 <- v3_env$.soviclust_v3_woa_move(
    swarm = swarm,
    prey = prey,
    a_coef = 2,
    b = 1,
    seed = 10,
    iter = 1,
    data = dat$x
  )

  expect_equal(v3, legacy, tolerance = 1e-12)
})


test_that("GWO and WOA retain raw optimizer state separately", {
  dat <- v3_test_data()

  gwo <- run_v3_optimizer("GWO", dat, seed = 42)
  woa <- run_v3_optimizer("WOA", dat, seed = 42)

  for (res in list(gwo, woa)) {
    expect_identical(
      res$fitness_type,
      "spatial_XB_feasible"
    )

    expect_true(is.matrix(res$search_centroid))
    expect_true(is.matrix(res$centroid))

    expect_equal(
      rowSums(res$membership),
      rep(1, nrow(dat$x)),
      tolerance = 1e-10
    )

    expect_equal(
      length(unique(res$cluster)),
      2
    )
  }
})
