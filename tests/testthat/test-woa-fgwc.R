# tests/testthat/test-woa-fgwc.R

test_that("WOA movement is reproducible and remains finite", {
  dat <- make_fgwc_test_data()

  swarm <- list(
    rbind(c(0.2, 0.2), c(4.8, 4.8)),
    rbind(c(0.3, 0.1), c(4.9, 5.0)),
    rbind(c(0.1, 0.3), c(5.0, 4.9))
  )
  prey <- swarm[[1]]

  a <- alg_env$woa.move(
    swarm, prey, a_coef = 2, b = 1,
    nwhale = 3, seed = 10, iter = 1,
    data = dat$x
  )
  b <- alg_env$woa.move(
    swarm, prey, a_coef = 2, b = 1,
    nwhale = 3, seed = 10, iter = 1,
    data = dat$x
  )

  expect_equal(a, b, tolerance = 1e-12)
  expect_true(all(vapply(a, function(z) all(is.finite(z)), logical(1))))
})


test_that("WOA-FGWC is reproducible for a fixed seed", {
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
    nwhale = 5,
    woa.b = 1,
    woa.same = 8
  )

  r1 <- do.call(alg_env$woafgwc, args)
  r2 <- do.call(alg_env$woafgwc, args)

  expect_equal(r1$centroid, r2$centroid, tolerance = 1e-12)
  expect_equal(r1$membership, r2$membership, tolerance = 1e-12)
  expect_equal(r1$converg, r2$converg, tolerance = 1e-12)
})


test_that("WOA global-best convergence is non-increasing", {
  dat <- make_fgwc_test_data()

  res <- alg_env$woafgwc(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max.iter = 10,
    error = 1e-12,
    randomN = 1,
    nwhale = 5,
    woa.same = 10
  )

  expect_true(all(diff(res$converg) <= 1e-12))
  expect_equal(rowSums(res$membership), rep(1, nrow(dat$x)),
               tolerance = 1e-10)
  expect_true(is.finite(res$f_obj))
  expect_equal(res$f_obj, tail(res$converg, 1), tolerance = 1e-12)
})
