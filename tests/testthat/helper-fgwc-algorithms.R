# tests/testthat/helper-fgwc-algorithms.R
# =============================================================================
# Portable test harness for FGWC-family algorithms.
#
# Works in BOTH:
#   1. devtools::test() from the source project; and
#   2. R CMD check / devtools::check() from the temporary installed package.
#
# Source-project files are preferred when available. During R CMD check, the
# helper falls back to system.file() inside the temporary installed package.
# =============================================================================

find_soviclust_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      file.exists(file.path(p, "DESCRIPTION")) &&
      dir.exists(file.path(
        p, "inst", "app", "R", "shared", "function"
      ))
    ) {
      return(p)
    }

    parent <- dirname(p)

    if (identical(parent, p)) {
      break
    }

    p <- parent
  }

  NULL
}


resolve_shared_dir <- function() {
  # Preferred during interactive development / devtools::test().
  root <- find_soviclust_root()

  if (!is.null(root)) {
    candidate <- file.path(
      root, "inst", "app", "R", "shared", "function"
    )

    if (dir.exists(candidate)) {
      return(candidate)
    }
  }

  # Fallback during R CMD check. devtools::check() builds and installs the
  # CURRENT source package into a temporary library, so this is not stale.
  candidate <- system.file(
    "app", "R", "shared", "function",
    package = "soviclust"
  )

  if (nzchar(candidate) && dir.exists(candidate)) {
    return(candidate)
  }

  stop(
    "Unable to locate soviclust FGWC algorithm source directory.",
    call. = FALSE
  )
}


shared_dir <- resolve_shared_dir()


# -----------------------------------------------------------------------------
# Common numerical core
# -----------------------------------------------------------------------------

alg_env <- new.env(parent = globalenv())

for (f in c("fgwc.R", "index.R", "ei.R")) {
  path <- file.path(shared_dir, f)

  if (!file.exists(path)) {
    stop("Missing algorithm source for tests: ", path, call. = FALSE)
  }

  sys.source(path, envir = alg_env)
}

# Legacy source uses these functions without namespace qualification.
alg_env$cdist <- rdist::cdist
alg_env$rstable <- stabledist::rstable


# -----------------------------------------------------------------------------
# Isolated optimizer environments
# -----------------------------------------------------------------------------

make_optimizer_env <- function(files) {
  env <- new.env(parent = alg_env)

  env$cdist <- rdist::cdist
  env$rstable <- stabledist::rstable

  for (f in files) {
    path <- file.path(shared_dir, f)

    if (!file.exists(path)) {
      stop("Missing optimizer source for tests: ", path, call. = FALSE)
    }

    sys.source(path, envir = env)
  }

  env
}


optimizer_envs <- list(
  ABC  = make_optimizer_env("abcfgwc.R"),
  FPA  = make_optimizer_env("fpafgwc.R"),

  # GSA depends on intel.ffly(), defined by the IFA source.
  GSA  = make_optimizer_env(c("ifafgwc.R", "gsafgwc.R")),

  HHO  = make_optimizer_env("hhofgwc.R"),
  IFA  = make_optimizer_env("ifafgwc.R"),
  PSO  = make_optimizer_env("psofgwc.R"),
  TLBO = make_optimizer_env("tlbofgwc.R"),

  # GWO and WOA reuse init.swarm() from the IFA implementation.
  GWO  = make_optimizer_env(c("ifafgwc.R", "gwofgwc.R")),
  WOA  = make_optimizer_env(c("ifafgwc.R", "woafgwc.R"))
)


# -----------------------------------------------------------------------------
# Compatibility aliases for existing tests
# -----------------------------------------------------------------------------

alg_env$abcfgwc <- optimizer_envs$ABC$abcfgwc
alg_env$fpafgwc <- optimizer_envs$FPA$fpafgwc
alg_env$gsafgwc <- optimizer_envs$GSA$gsafgwc
alg_env$hhofgwc <- optimizer_envs$HHO$hhofgwc
alg_env$ifafgwc <- optimizer_envs$IFA$ifafgwc
alg_env$psofgwc <- optimizer_envs$PSO$psofgwc
alg_env$tlbofgwc <- optimizer_envs$TLBO$tlbofgwc
alg_env$gwofgwc <- optimizer_envs$GWO$gwofgwc
alg_env$woafgwc <- optimizer_envs$WOA$woafgwc

alg_env$compare <- optimizer_envs$ABC$compare
alg_env$force_v <- optimizer_envs$GSA$force_v
alg_env$moving <- optimizer_envs$IFA$moving
alg_env$woa.move <- optimizer_envs$WOA$woa.move
alg_env$init.swarm <- optimizer_envs$IFA$init.swarm


# -----------------------------------------------------------------------------
# Fail early if expected functions are unavailable
# -----------------------------------------------------------------------------

required_common <- c(
  "uij",
  "XB1",
  "Kwon1",
  "optimizer_fitness",
  "optimizer_spatial_objective"
)

required_optimizer <- c(
  "abcfgwc",
  "fpafgwc",
  "gsafgwc",
  "gwofgwc",
  "hhofgwc",
  "ifafgwc",
  "psofgwc",
  "tlbofgwc",
  "woafgwc"
)

bad_common <- required_common[
  !vapply(
    required_common,
    function(nm) is.function(alg_env[[nm]]),
    logical(1)
  )
]

bad_optimizer <- required_optimizer[
  !vapply(
    required_optimizer,
    function(nm) is.function(alg_env[[nm]]),
    logical(1)
  )
]

if (length(bad_common) > 0L || length(bad_optimizer) > 0L) {
  stop(
    "FGWC test harness failed to load functions. Common: ",
    paste(bad_common, collapse = ", "),
    "; optimizers: ",
    paste(bad_optimizer, collapse = ", "),
    call. = FALSE
  )
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
