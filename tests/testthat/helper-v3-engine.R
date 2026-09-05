# tests/testthat/helper-v3-engine.R
# =============================================================================
# Isolated Patch-v3 test engine — robust source-root detection
#
# This helper NEVER relies on the installed package while an active source tree
# can be found. It searches upward from:
#   1. this helper file's own location (when available),
#   2. testthat::test_path(),
#   3. getwd().
#
# A source root is identified structurally, not by the Package field:
#   DESCRIPTION
#   inst/app/global.R
#   inst/app/R/shared/function/optimizer_v3.R
#   tests/testthat/
#
# Patch-v3 tests use only `v3_env`; legacy `alg_env`/`optimizer_envs` cannot
# overwrite it.
# =============================================================================


.v3_this_file <- tryCatch(
  normalizePath(
    sys.frame(1)$ofile,
    winslash = "/",
    mustWork = TRUE
  ),
  error = function(e) ""
)


.v3_find_source_root <- function() {
  starts <- character()

  if (nzchar(.v3_this_file)) {
    starts <- c(
      starts,
      dirname(.v3_this_file)
    )
  }

  if (requireNamespace("testthat", quietly = TRUE)) {
    tp <- tryCatch(
      testthat::test_path(),
      error = function(e) ""
    )

    if (nzchar(tp)) {
      starts <- c(starts, tp)
    }
  }

  starts <- c(
    starts,
    getwd()
  )

  starts <- unique(
    starts[nzchar(starts)]
  )

  for (start in starts) {
    p <- tryCatch(
      normalizePath(
        start,
        winslash = "/",
        mustWork = TRUE
      ),
      error = function(e) ""
    )

    if (!nzchar(p)) next

    repeat {
      required_files <- c(
        file.path(p, "DESCRIPTION"),
        file.path(p, "inst", "app", "global.R"),
        file.path(
          p,
          "inst", "app", "R", "shared", "function",
          "optimizer_v3.R"
        )
      )

      if (
        all(file.exists(required_files)) &&
        dir.exists(
          file.path(p, "tests", "testthat")
        )
      ) {
        return(p)
      }

      parent <- dirname(p)

      if (identical(parent, p)) {
        break
      }

      p <- parent
    }
  }

  ""
}


.v3_source_root <- .v3_find_source_root()


if (nzchar(.v3_source_root)) {
  .v3_shared_dir <- normalizePath(
    file.path(
      .v3_source_root,
      "inst", "app", "R", "shared", "function"
    ),
    winslash = "/",
    mustWork = TRUE
  )

  .v3_loader_mode <- "source-project"

} else {
  .v3_shared_dir <- system.file(
    "app", "R", "shared", "function",
    package = "soviclust"
  )

  .v3_loader_mode <- "installed-package"
}


if (
  !nzchar(.v3_shared_dir) ||
  !dir.exists(.v3_shared_dir)
) {
  stop(
    paste0(
      "Patch-v3 tests cannot locate shared algorithm directory.\n",
      "helper file: ", .v3_this_file, "\n",
      "getwd(): ", getwd()
    ),
    call. = FALSE
  )
}


v3_env <- new.env(parent = globalenv())


.v3_source_order <- c(
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


.v3_missing_files <- .v3_source_order[
  !file.exists(
    file.path(
      .v3_shared_dir,
      .v3_source_order
    )
  )
]


if (length(.v3_missing_files)) {
  stop(
    paste0(
      "Patch-v3 test engine missing file(s): ",
      paste(.v3_missing_files, collapse = ", "),
      "\nmode: ", .v3_loader_mode,
      "\nsource root: ", .v3_source_root,
      "\nhelper file: ", .v3_this_file,
      "\ndir: ", .v3_shared_dir
    ),
    call. = FALSE
  )
}


for (.v3_file in .v3_source_order) {
  sys.source(
    file.path(
      .v3_shared_dir,
      .v3_file
    ),
    envir = v3_env
  )
}


# ---------------------------------------------------------------------------
# Hard guards
# ---------------------------------------------------------------------------

.v3_required_functions <- c(
  "evaluate_optimizer_candidate_v3",
  ".soviclust_v3_abc_neighbor",
  ".soviclust_v3_gsa_distance",
  ".soviclust_v3_tlbo_teacher_candidate",
  ".soviclust_v3_gwo_move",
  ".soviclust_v3_woa_move",
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


.v3_missing_functions <- .v3_required_functions[
  !vapply(
    .v3_required_functions,
    function(nm) {
      exists(
        nm,
        envir = v3_env,
        inherits = FALSE,
        mode = "function"
      )
    },
    logical(1)
  )
]


if (length(.v3_missing_functions)) {
  stop(
    paste0(
      "Patch-v3 isolated test engine failed to load: ",
      paste(.v3_missing_functions, collapse = ", "),
      "\nmode: ", .v3_loader_mode,
      "\nsource root: ", .v3_source_root,
      "\ndir: ", .v3_shared_dir
    ),
    call. = FALSE
  )
}


.v3_body_has <- function(
    fn_name,
    text
) {
  fn <- get(
    fn_name,
    envir = v3_env,
    inherits = FALSE
  )

  grepl(
    text,
    paste(
      deparse(body(fn)),
      collapse = "\n"
    ),
    fixed = TRUE
  )
}


.v3_public_checks <- c(
  ABC = .v3_body_has(
    "abcfgwc",
    ".soviclust_v3_"
  ),
  FPA = .v3_body_has(
    "fpafgwc",
    ".soviclust_v3_"
  ),
  GSA = .v3_body_has(
    "gsafgwc",
    ".soviclust_v3_"
  ),
  GWO = .v3_body_has(
    "gwofgwc",
    ".soviclust_v3_"
  ),
  HHO = .v3_body_has(
    "hhofgwc",
    ".soviclust_v3_"
  ),
  IFA = .v3_body_has(
    "ifafgwc",
    ".soviclust_v3_"
  ),
  PSO = .v3_body_has(
    "psofgwc",
    ".soviclust_v3_"
  ),
  TLBO = .v3_body_has(
    "tlbofgwc",
    ".soviclust_v3_"
  ),
  WOA = .v3_body_has(
    "woafgwc",
    ".soviclust_v3_"
  )
)


if (!all(.v3_public_checks)) {
  stop(
    paste0(
      "Patch-v3 public overrides are not active for: ",
      paste(
        names(.v3_public_checks)[
          !.v3_public_checks
        ],
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

v3_test_data <- function() {
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

  pop <- c(
    100, 110, 105, 95,
    120, 115, 125, 118
  )

  coords <- cbind(
    seq_len(nrow(x)),
    rep(0, nrow(x))
  )

  dmat <- as.matrix(
    stats::dist(coords)
  )

  diag(dmat) <- 0

  list(
    x = x,
    pop = pop,
    dmat = dmat
  )
}


run_v3_optimizer <- function(
    method,
    dat,
    seed = 42L) {

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
    randomN = seed,
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

  fn_name <- switch(
    method,
    ABC = "abcfgwc",
    FPA = "fpafgwc",
    GSA = "gsafgwc",
    GWO = "gwofgwc",
    HHO = "hhofgwc",
    IFA = "ifafgwc",
    PSO = "psofgwc",
    TLBO = "tlbofgwc",
    WOA = "woafgwc"
  )

  fn <- get(
    fn_name,
    envir = v3_env,
    inherits = FALSE
  )

  do.call(
    fn,
    c(common, extra)
  )
}
