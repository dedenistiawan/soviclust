# tests/testthat/test-optimizer-harmonization.R
# =============================================================================
# Objective-harmonization tests for FGWC metaheuristic optimizers.
# =============================================================================

test_that("common optimizer fitness is exactly jfgwcv", {
  dat <- make_fgwc_test_data()

  centers <- rbind(
    c(0.10, 0.10),
    c(5.00, 5.00)
  )

  direct <- alg_env$jfgwcv(
    dat$x,
    centers,
    m = 2,
    distance = "euclidean",
    order = 2
  )

  common <- alg_env$optimizer_fitness(
    dat$x,
    centers,
    m = 2,
    distance = "euclidean",
    order = 2
  )

  expect_equal(common, direct, tolerance = 1e-12)
})


test_that("legacy spatial arguments do not change common optimizer fitness", {
  dat <- make_fgwc_test_data()

  centers <- rbind(
    c(0.10, 0.10),
    c(5.00, 5.00)
  )

  popmat <- matrix(dat$pop, ncol = 1)
  mimj <- popmat %*% t(popmat)

  plain <- alg_env$optimizer_fitness(
    dat$x, centers, 2, "euclidean", 2
  )

  legacy_signature <- alg_env$optimizer_fitness(
    dat$x, centers, 2, "euclidean", 2,
    mimj, dat$dmat, 0.7, 0.3, 1, 1
  )

  expect_equal(legacy_signature, plain, tolerance = 1e-12)
})


test_that("no optimizer source still uses mixed jfgwcv2 fitness", {
  files <- c(
    "abcfgwc.R", "fpafgwc.R", "gsafgwc.R",
    "gwofgwc.R", "hhofgwc.R", "ifafgwc.R",
    "psofgwc.R", "tlbofgwc.R", "woafgwc.R"
  )

  for (f in files) {
    txt <- paste(
      readLines(file.path(shared_dir, f), warn = FALSE),
      collapse = "\n"
    )

    expect_false(
      grepl("jfgwcv2\\(", txt, perl = TRUE),
      info = paste(f, "still uses jfgwcv2()")
    )

    expect_false(
      grepl("jfgwcv\\(", txt, perl = TRUE),
      info = paste(f, "still directly uses jfgwcv(); use optimizer_fitness()")
    )
  }
})


run_small_optimizer <- function(method, dat) {
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
    max.iter = 3,
    randomN = 42,
    vi.dist = "uniform"
  )

  extra <- switch(
    method,
    ABC = list(
      nfood = 5,
      n.onlooker = 3,
      limit = 3,
      pso = FALSE,
      abc.same = 100
    ),
    FPA = list(
      nflow = 5,
      p = 0.8,
      flow.same = 100
    ),
    GSA = list(
      npar = 5,
      par.no = 2,
      gsa.same = 100
    ),
    GWO = list(
      nwolf = 5,
      wolf.same = 100
    ),
    HHO = list(
      nhh = 5,
      hh.same = 100
    ),
    IFA = list(
      nfly = 5,
      ffly.no = 2,
      fa.same = 100
    ),
    PSO = list(
      npar = 5,
      pso.same = 100
    ),
    TLBO = list(
      nstud = 5,
      tlbo.same = 100,
      nselection = 5
    ),
    WOA = list(
      nwhale = 5,
      woa.same = 100
    )
  )

  fn <- switch(
    method,
    ABC = optimizer_envs$ABC$abcfgwc,
    FPA = optimizer_envs$FPA$fpafgwc,
    GSA = optimizer_envs$GSA$gsafgwc,
    GWO = optimizer_envs$GWO$gwofgwc,
    HHO = optimizer_envs$HHO$hhofgwc,
    IFA = optimizer_envs$IFA$ifafgwc,
    PSO = optimizer_envs$PSO$psofgwc,
    TLBO = optimizer_envs$TLBO$tlbofgwc,
    WOA = optimizer_envs$WOA$woafgwc
  )

  do.call(fn, c(common, extra))
}


test_that("all nine optimizers report the same fitness definition", {
  dat <- make_fgwc_test_data()

  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  for (method in methods) {
    res <- run_small_optimizer(method, dat)

    expect_s3_class(
      res,
      "fgwc"
    )

    expect_identical(
      res$fitness_type,
      "jfgwcv",
      info = paste(method, "fitness_type")
    )

    expected_fitness <- alg_env$optimizer_fitness(
      dat$x,
      res$centroid,
      m = 2,
      distance = "euclidean",
      order = 2
    )

    expect_equal(
      res$f_obj,
      expected_fitness,
      tolerance = 1e-8,
      info = paste(method, "f_obj must equal common optimizer fitness")
    )

    expected_spatial <- alg_env$optimizer_spatial_objective(
      dat$x,
      res$membership,
      res$centroid,
      m = 2,
      distance = "euclidean",
      order = 2
    )

    expect_equal(
      res$spatial_obj,
      expected_spatial,
      tolerance = 1e-8,
      info = paste(method, "spatial_obj")
    )

    expect_true(
      is.finite(res$f_obj),
      info = paste(method, "finite f_obj")
    )

    expect_true(
      is.finite(res$spatial_obj),
      info = paste(method, "finite spatial_obj")
    )

    expect_equal(
      rowSums(res$membership),
      rep(1, nrow(dat$x)),
      tolerance = 1e-8,
      info = paste(method, "membership normalization")
    )

    expect_true(
      all(diff(res$converg) <= 1e-8),
      info = paste(method, "global-best convergence must be non-increasing")
    )

    expect_equal(
      tail(res$converg, 1),
      res$f_obj,
      tolerance = 1e-8,
      info = paste(method, "last convergence value must equal f_obj")
    )
  }
})


test_that("optimizer results are reproducible for a fixed seed", {
  dat <- make_fgwc_test_data()

  methods <- c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )

  for (method in methods) {
    r1 <- run_small_optimizer(method, dat)
    r2 <- run_small_optimizer(method, dat)

    expect_equal(
      r1$f_obj,
      r2$f_obj,
      tolerance = 1e-10,
      info = paste(method, "fitness reproducibility")
    )

    expect_equal(
      r1$centroid,
      r2$centroid,
      tolerance = 1e-10,
      info = paste(method, "centroid reproducibility")
    )

    expect_equal(
      r1$membership,
      r2$membership,
      tolerance = 1e-10,
      info = paste(method, "membership reproducibility")
    )
  }
})


test_that("ABC trial counter increments when no candidate improves", {
  dat <- make_fgwc_test_data()
  env <- optimizer_envs$ABC

  popmat <- matrix(dat$pop, ncol = 1)
  mimj <- popmat %*% t(popmat)

  oldswarm <- list(
    rbind(c(0.10, 0.10), c(5.00, 5.00)),
    rbind(c(0.20, 0.10), c(4.90, 5.00))
  )

  newswarm <- oldswarm

  oldfit <- vapply(
    oldswarm,
    function(v) alg_env$optimizer_fitness(
      dat$x, v, 2, "euclidean", 2
    ),
    numeric(1)
  )

  newfit <- oldfit + 1

  ans <- env$compare(
    newswarm = newswarm,
    oldswarm = oldswarm,
    newfit = newfit,
    oldfit = oldfit,
    t = c(0, 0),
    data = dat$x,
    m = 2,
    distance = "euclidean",
    order = 2,
    mi.mj = mimj,
    dist = dat$dmat,
    alpha = 0.7,
    beta = 0.3,
    a = 1,
    b = 1
  )

  expect_equal(ans$t, c(1, 1))
  expect_true(all(is.finite(ans$prob)))
  expect_equal(sum(ans$prob), 1, tolerance = 1e-12)
})


test_that("GSA force_v returns finite updated velocity", {
  dat <- make_fgwc_test_data()
  env <- optimizer_envs$GSA

  popmat <- matrix(dat$pop, ncol = 1)
  mimj <- popmat %*% t(popmat)

  # Build the particle structure with the shared initializer used by
  # the FGWC metaheuristic family. The GSA-specific test is for force_v(),
  # so the initializer itself is not under test here.
  expect_true(is.function(optimizer_envs$IFA$init.swarm))

  par <- optimizer_envs$IFA$init.swarm(
    data = dat$x,
    pop = mimj,
    distmat = dat$dmat,
    distance = "euclidean",
    order = 2,
    vi.dist = "uniform",
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    a = 1,
    b = 1,
    randomN = 42,
    nfly = 5
  )

  v0 <- lapply(
    seq_len(5),
    function(i) matrix(0, nrow = 2, ncol = ncol(dat$x))
  )

  v1 <- env$force_v(
    par = par,
    no = 2,
    G = 1,
    v = v0,
    vmax = 0.7,
    par.dist = "euclidean",
    par.order = 2,
    randomN = 42
  )

  flat <- unlist(v1)

  expect_true(all(is.finite(flat)))
  expect_true(any(abs(flat) > 0))
})


test_that("IFA movement is assigned back to firefly positions", {
  dat <- make_fgwc_test_data()
  env <- optimizer_envs$IFA

  popmat <- matrix(dat$pop, ncol = 1)
  mimj <- popmat %*% t(popmat)

  ffly <- env$init.swarm(
    data = dat$x,
    pop = mimj,
    distmat = dat$dmat,
    distance = "euclidean",
    order = 2,
    vi.dist = "uniform",
    ncluster = 2,
    m = 2,
    alpha = 0.7,
    a = 1,
    b = 1,
    randomN = 42,
    nfly = 5
  )

  moved <- env$moving(
    ffly.all = ffly,
    no = 2,
    ff.beta = 1,
    gamma = 1,
    ff.alpha = 1,
    ffly.dist = "euclidean",
    ffly.order = 2,
    ei.distr = "normal",
    r.chaotic = 4,
    m.chaotic = 0.7,
    ind.levy = 1,
    skew.levy = 0,
    sca.levy = 1,
    data = dat$x,
    m = 2,
    distance = "euclidean",
    order = 2,
    mi.mj = mimj,
    dist = dat$dmat,
    alpha = 0.7,
    beta = 0.3,
    a = 1,
    b = 1,
    randomN = 42
  )

  before <- unlist(ffly$centroid)
  after <- unlist(moved)

  expect_true(all(is.finite(after)))
  expect_true(any(abs(after - before) > 1e-12))
})
