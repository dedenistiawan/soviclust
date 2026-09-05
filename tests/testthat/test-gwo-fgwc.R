# tests/testthat/test-gwo-fgwc.R

test_that("GWO-FGWC is reproducible for a fixed seed", {
  dat <- make_fgwc_test_data()

  args <- list(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max.iter = 8,
    error = 1e-10,
    randomN = 42,
    nwolf = 5,
    wolf.same = 8
  )

  r1 <- do.call(alg_env$gwofgwc, args)
  r2 <- do.call(alg_env$gwofgwc, args)

  expect_equal(r1$centroid, r2$centroid, tolerance = 1e-12)
  expect_equal(r1$membership, r2$membership, tolerance = 1e-12)
  expect_equal(r1$converg, r2$converg, tolerance = 1e-12)
})


test_that("GWO global-best convergence is non-increasing", {
  dat <- make_fgwc_test_data()

  res <- alg_env$gwofgwc(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max.iter = 10,
    error = 1e-12,
    randomN = 1,
    nwolf = 5,
    wolf.same = 10
  )

  expect_true(all(diff(res$converg) <= 1e-12))
  expect_equal(rowSums(res$membership), rep(1, nrow(dat$x)),
               tolerance = 1e-10)
  expect_true(is.finite(res$f_obj))
  expect_equal(res$f_obj, tail(res$converg, 1), tolerance = 1e-12)
})
