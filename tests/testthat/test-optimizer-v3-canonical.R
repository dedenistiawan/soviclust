# tests/testthat/test-optimizer-v3-canonical.R
# =============================================================================
# Canonical behavior tests for ABC, GSA, TLBO using the isolated v3 engine.
# =============================================================================


test_that("ABC canonical neighbor changes exactly one decision variable", {
  source <- rbind(
    c(1, 2),
    c(3, 4)
  )

  partner <- rbind(
    c(5, 6),
    c(7, 8)
  )

  dimension <- 3L
  phi <- 0.25

  cand <- v3_env$.soviclust_v3_abc_neighbor(
    source,
    partner,
    dimension = dimension,
    phi = phi
  )

  before <- as.vector(source)
  after <- as.vector(cand)

  changed <- which(abs(after - before) > 1e-14)

  expect_equal(changed, dimension)

  expected <- before[dimension] +
    phi * (
      before[dimension] -
        as.vector(partner)[dimension]
    )

  expect_equal(
    after[dimension],
    expected,
    tolerance = 1e-12
  )
})


test_that("ABC onlooker probabilities favor lower fitness", {
  fitness <- c(1, 5, 20)

  probs <- v3_env$.soviclust_v3_abc_probabilities(
    fitness
  )

  expect_equal(sum(probs), 1, tolerance = 1e-12)
  expect_true(probs[1] > probs[2])
  expect_true(probs[2] > probs[3])

  idx <- v3_env$.soviclust_v3_abc_onlooker_indices(
    fitness,
    n = 5000,
    seed = 123
  )

  counts <- tabulate(idx, nbins = 3)

  expect_true(counts[1] > counts[2])
  expect_true(counts[2] > counts[3])
})


test_that("GSA distance is one scalar full-agent Euclidean distance", {
  xi <- rbind(
    c(0, 0),
    c(1, 1)
  )

  xj <- rbind(
    c(3, 4),
    c(1, 5)
  )

  expected <- sqrt(sum((xi - xj)^2))

  got <- v3_env$.soviclust_v3_gsa_distance(
    xi,
    xj
  )

  expect_length(got, 1)
  expect_equal(got, expected, tolerance = 1e-12)
})


test_that("GSA masses are normalized and favor better fitness", {
  fit <- c(1, 2, 5, 9)

  mass <- v3_env$.soviclust_v3_gsa_masses(fit)

  expect_equal(sum(mass), 1, tolerance = 1e-12)
  expect_true(all(mass > 0))
  expect_true(mass[1] > mass[2])
  expect_true(mass[2] > mass[3])
  expect_true(mass[3] > mass[4])
})


test_that("GSA velocity update is finite and respects vmax", {
  search <- list(
    rbind(c(0, 0), c(4, 4)),
    rbind(c(0.2, 0.1), c(4.2, 3.9)),
    rbind(c(0.5, 0.4), c(4.4, 4.3))
  )

  fit <- c(1, 2, 3)

  velocity <- lapply(
    search,
    function(x) matrix(0, nrow(x), ncol(x))
  )

  v <- v3_env$.soviclust_v3_gsa_velocity_step(
    search = search,
    fitness = fit,
    velocity = velocity,
    G0 = 1,
    gsa.alpha = 20,
    iter = 1,
    max.iter = 10,
    kbest = 2,
    vmax = 0.7,
    seed = 99
  )

  vals <- unlist(v)

  expect_true(all(is.finite(vals)))
  expect_true(any(abs(vals) > 0))
  expect_true(all(abs(vals) <= 0.7 + 1e-12))
})


test_that("TLBO teaching factor is scalar and belongs to {1,2}", {
  student <- rbind(
    c(1, 2),
    c(3, 4)
  )

  teacher <- rbind(
    c(0.5, 1.5),
    c(2.5, 3.5)
  )

  mean_pos <- rbind(
    c(1.5, 2.5),
    c(3.5, 4.5)
  )

  out <- v3_env$.soviclust_v3_tlbo_teacher_candidate(
    student,
    teacher,
    mean_pos,
    seed = 123
  )

  expect_length(out$TF, 1)
  expect_true(out$TF %in% c(1, 2))
  expect_equal(dim(out$r), dim(student))
})


test_that("TLBO teacher random matrix differs across learner seeds", {
  student <- rbind(
    c(1, 2),
    c(3, 4)
  )

  teacher <- rbind(
    c(0.5, 1.5),
    c(2.5, 3.5)
  )

  mean_pos <- rbind(
    c(1.5, 2.5),
    c(3.5, 4.5)
  )

  a <- v3_env$.soviclust_v3_tlbo_teacher_candidate(
    student,
    teacher,
    mean_pos,
    seed = 100
  )

  b <- v3_env$.soviclust_v3_tlbo_teacher_candidate(
    student,
    teacher,
    mean_pos,
    seed = 101
  )

  expect_false(isTRUE(all.equal(a$r, b$r)))
})


test_that("TLBO learner update follows relative fitness", {
  student <- matrix(
    c(1, 2, 3, 4),
    nrow = 2
  )

  partner <- matrix(
    c(5, 6, 7, 8),
    nrow = 2
  )

  better <- v3_env$.soviclust_v3_tlbo_learner_candidate(
    student,
    partner,
    student_fit = 1,
    partner_fit = 2,
    seed = 7
  )

  worse <- v3_env$.soviclust_v3_tlbo_learner_candidate(
    student,
    partner,
    student_fit = 2,
    partner_fit = 1,
    seed = 7
  )

  expect_equal(
    better$candidate,
    student +
      better$r * (student - partner),
    tolerance = 1e-12
  )

  expect_equal(
    worse$candidate,
    student +
      worse$r * (partner - student),
    tolerance = 1e-12
  )
})
