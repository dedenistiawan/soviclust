# tests/testthat/test-fgwc-core.R

test_that("membership rows sum to one and zero-distance is handled exactly", {
  x <- rbind(
    c(0, 0),
    c(1, 1),
    c(2, 2)
  )
  centers <- rbind(
    c(0, 0),
    c(2, 2)
  )

  res <- alg_env$uij(x, centers, m = 2, distance = "euclidean", order = 2)

  expect_equal(rowSums(res$u), rep(1, nrow(x)), tolerance = 1e-12)
  expect_true(all(is.finite(res$u)))
  expect_equal(res$u[1, ], c(1, 0), tolerance = 1e-12)
  expect_equal(res$u[3, ], c(0, 1), tolerance = 1e-12)
})


test_that("duplicate zero-distance centroids split membership without NaN", {
  x <- matrix(c(0, 0), nrow = 1)
  centers <- rbind(c(0, 0), c(0, 0))

  res <- alg_env$uij(x, centers, m = 2, distance = "euclidean", order = 2)

  expect_equal(res$u[1, ], c(0.5, 0.5), tolerance = 1e-12)
  expect_true(all(is.finite(res$u)))
})


test_that("CE handles exact zero memberships", {
  u <- rbind(
    c(1, 0),
    c(0, 1)
  )

  expect_equal(alg_env$CE1(u), 0, tolerance = 1e-12)
})


test_that("Xie-Beni matches an independent manual calculation", {
  x <- rbind(
    c(0, 0),
    c(1, 0),
    c(4, 0),
    c(5, 0)
  )

  v <- rbind(
    c(0.5, 0),
    c(4.5, 0)
  )

  u <- rbind(
    c(0.9, 0.1),
    c(0.8, 0.2),
    c(0.2, 0.8),
    c(0.1, 0.9)
  )

  m <- 2

  d <- outer(
    seq_len(nrow(x)),
    seq_len(nrow(v)),
    Vectorize(function(i, k) sum((x[i, ] - v[k, ])^2))
  )

  centroid_dist <- as.matrix(stats::dist(v))^2
  diag(centroid_dist) <- Inf

  expected <- sum((u^m) * d) /
    (nrow(x) * min(centroid_dist))

  expect_equal(
    alg_env$XB1(x, u, v, m),
    expected,
    tolerance = 1e-12
  )
})


test_that("Kwon index matches the published formula", {
  x <- rbind(
    c(0, 0),
    c(1, 0),
    c(4, 0),
    c(5, 0)
  )

  v <- rbind(
    c(0.5, 0),
    c(4.5, 0)
  )

  u <- rbind(
    c(0.9, 0.1),
    c(0.8, 0.2),
    c(0.2, 0.8),
    c(0.1, 0.9)
  )

  m <- 2

  d <- outer(
    seq_len(nrow(x)),
    seq_len(nrow(v)),
    Vectorize(function(i, k) sum((x[i, ] - v[k, ])^2))
  )

  xbar <- colMeans(x)
  penalty <- mean(vapply(
    seq_len(nrow(v)),
    function(k) sum((v[k, ] - xbar)^2),
    numeric(1)
  ))

  centroid_dist <- as.matrix(stats::dist(v))^2
  diag(centroid_dist) <- Inf

  expected <- (sum((u^m) * d) + penalty) /
    min(centroid_dist)

  expect_equal(
    alg_env$Kwon1(x, u, v, m),
    expected,
    tolerance = 1e-12
  )
})


test_that("classical FGWC produces a finite normalized solution", {
  dat <- make_fgwc_test_data()

  res <- alg_env$fgwcuv(
    data = dat$x,
    pop = dat$pop,
    distmat = dat$dmat,
    kind = "v",
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    max.iter = 30,
    error = 1e-8,
    randomN = 123
  )

  expect_s3_class(res, "fgwc")
  expect_equal(dim(res$membership), c(nrow(dat$x), 2L))
  expect_equal(rowSums(res$membership), rep(1, nrow(dat$x)),
               tolerance = 1e-10)
  expect_true(all(is.finite(res$membership)))
  expect_true(is.finite(res$f_obj))
  expect_gte(res$f_obj, 0)
  expect_true(all(is.finite(res$centroid)))
})
