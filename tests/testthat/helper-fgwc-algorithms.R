# tests/testthat/helper-fgwc-algorithms.R
# =============================================================================
# Load the app-side clustering engine in the same order as inst/app/global.R.
# Patch v3 intentionally sources optimizer_v3.R LAST so its public optimizer
# entry points override the legacy implementations while preserving the legacy
# files for provenance and rollback.
# =============================================================================

alg_env <- new.env(parent = globalenv())

# Prefer the active SOURCE PROJECT during devtools::test(). This prevents an
# older installed soviclust version from shadowing newly patched source files.
candidate <- file.path(
  getwd(), "inst", "app", "R", "shared", "function"
)

if (dir.exists(candidate)) {
  shared_dir <- normalizePath(
    candidate,
    winslash = "/",
    mustWork = TRUE
  )
} else {
  # R CMD check executes tests against the temporary installed package.
  shared_dir <- system.file(
    "app", "R", "shared", "function",
    package = "soviclust"
  )
}

if (!nzchar(shared_dir) || !dir.exists(shared_dir)) {
  stop("Unable to locate soviclust shared algorithm directory for tests.")
}

source_order <- c(
  "fgwc.R",
  "index.R",
  "ei.R",
  "abcfgwc.R",
  "fpafgwc.R",
  "gsafgwc.R",
  "gwofgwc.R",
  "hhofgwc.R",
  "ifafgwc.R",
  "psofgwc.R",
  "tlbofgwc.R",
  "woafgwc.R",
  "optimizer_v3.R"
)

missing_files <- source_order[
  !file.exists(file.path(shared_dir, source_order))
]

if (length(missing_files) > 0L) {
  stop(
    "Missing shared algorithm file(s): ",
    paste(missing_files, collapse = ", ")
  )
}

for (f in source_order) {
  sys.source(
    file.path(shared_dir, f),
    envir = alg_env
  )
}

# Backward-compatible aliases used by the existing test suite and validation
# scripts. All v3 public functions now live in one isolated environment.
optimizer_envs <- setNames(
  rep(list(alg_env), 9L),
  c(
    "ABC", "FPA", "GSA", "GWO", "HHO",
    "IFA", "PSO", "TLBO", "WOA"
  )
)

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
