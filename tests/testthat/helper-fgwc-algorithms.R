# tests/testthat/helper-fgwc-algorithms.R
# Load the computational files from inst/app into an isolated test environment.

alg_env <- new.env(parent = globalenv())

shared_dir <- system.file(
  "app", "R", "shared", "function",
  package = "soviclust"
)

if (!nzchar(shared_dir)) {
  stop("Unable to locate soviclust shared algorithm directory for tests.")
}

for (f in c(
  "fgwc.R",
  "index.R",
  "ifafgwc.R",
  "gwofgwc.R",
  "woafgwc.R"
)) {
  sys.source(file.path(shared_dir, f), envir = alg_env)
}


make_fgwc_test_data <- function() {
  x <- rbind(
    c(0.00, 0.05),
    c(0.10, 0.00),
    c(0.15, 0.10),
    c(0.05, 0.15),
    c(4.90, 5.00),
    c(5.00, 4.90),
    c(5.10, 5.00),
    c(5.00, 5.10)
  )

  pop <- c(100, 110, 105, 95, 120, 115, 125, 118)

  coords <- cbind(
    seq_len(nrow(x)),
    rep(0, nrow(x))
  )

  dmat <- as.matrix(stats::dist(coords))
  diag(dmat) <- 0

  list(
    x = x,
    pop = pop,
    dmat = dmat
  )
}
