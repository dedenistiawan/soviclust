# =============================================================================
# soviclust — V8-B Exact NFE Budget-Control Smoke
# =============================================================================
#
# Purpose
# -------
# Validate Patch v3.1b exact total-NFE budget control on all nine optimizers
# using the bundled Indonesia 514 x 15 dataset.
#
# IMPORTANT
# ---------
# This is a SOFTWARE / BUDGET-CONTROL REGRESSION GATE.
# It is NOT yet the formal NFE-fair optimizer benchmark.
#
# Shared configuration
# --------------------
#   c          = 4
#   m          = 2
#   alpha      = 0.7
#   a = b      = 1
#   population = 10
#   max_nfe    = 200 TOTAL evaluations
#   seed       = 2026
#   fitness    = spatial_XB_feasible
#
# The run uses a large max.iter and disables early stopping so that max_nfe,
# rather than iteration count or stagnation, becomes the intended stop rule.
#
# Required Patch v3.1b output fields
# -----------------------------------
#   nfe
#   nfe_initialization
#   nfe_optimization
#   max_nfe
#   budget_exhausted
#   nfe_remaining
#   termination_reason
#
# Gate requirements
# -----------------
#   nfe == max_nfe
#   nfe == nfe_initialization + nfe_optimization
#   nfe_remaining == 0
#   budget_exhausted == TRUE
#   termination_reason == "max_nfe"
#   fitness_type == "spatial_XB_feasible"
#   occupied clusters == 4/4
#   same seed -> same NFE and same solution
#
# Outputs
# -------
# validation_results/v8b_exact_nfe_budget_smoke/
#   v8b_config.csv
#   v8b_optimizer_summary.csv
#   v8b_cluster_support.csv
#
# Run from project root:
#   source("validation/v8b_exact_nfe_budget_smoke.R")
#
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

SEED <- 2026L

NCLUSTER <- 4L
M <- 2

ALPHA <- 0.7
A_SPATIAL <- 1
B_SPATIAL <- 1

DISTANCE <- "euclidean"
ORDER <- 2

POP_SIZE <- 10L

MAX_NFE <- 200L

# Intentionally large so max_nfe, not iteration count, is the binding limit.
MAX_ITER <- 1000L

# Disable early stopping / stagnation termination.
SAME_LIMIT <- MAX_ITER + 1000L
ERROR_TOL <- 0

REPRODUCIBILITY_RUN <- TRUE
REPRO_TOL <- 1e-10


# -----------------------------------------------------------------------------
# 1. Locate project and load isolated Patch-v3 engine
# -----------------------------------------------------------------------------

find_project_root <- function(start = getwd()) {

  p <- normalizePath(
    start,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {

    required <- c(
      file.path(p, "DESCRIPTION"),
      file.path(
        p,
        "inst", "app", "R", "shared", "function",
        "optimizer_v3.R"
      ),
      file.path(
        p,
        "tests", "testthat",
        "helper-v3-engine.R"
      )
    )

    if (all(file.exists(required))) {
      return(p)
    }

    parent <- dirname(p)

    if (identical(parent, p)) {
      break
    }

    p <- parent
  }

  stop(
    "Unable to locate soviclust source root.",
    call. = FALSE
  )
}


root <- find_project_root()

helper_path <- file.path(
  root,
  "tests",
  "testthat",
  "helper-v3-engine.R"
)

helper_env <- new.env(
  parent = globalenv()
)

sys.source(
  helper_path,
  envir = helper_env
)

v3 <- helper_env$v3_env

if (!is.environment(v3)) {
  stop(
    "Patch-v3 isolated test engine was not created.",
    call. = FALSE
  )
}


required_engine_functions <- c(
  ".soviclust_v3_validate_common",
  ".soviclust_v3_eval",
  ".soviclust_v3_try_eval",
  ".soviclust_v3_nfe_snapshot"
)

missing_engine_functions <- required_engine_functions[
  !vapply(
    required_engine_functions,
    exists,
    logical(1),
    envir = v3,
    inherits = FALSE,
    mode = "function"
  )
]

if (length(missing_engine_functions)) {
  stop(
    paste0(
      "Patch v3.1b exact-budget infrastructure is incomplete. Missing: ",
      paste(
        missing_engine_functions,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 2. Load bundled Indonesia 514 x 15 validation data
# -----------------------------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop(
    "Package 'readxl' is required.",
    call. = FALSE
  )
}


data_path <- file.path(
  root,
  "inst",
  "extdata",
  "sovi_data_kab_514_15.xlsx"
)

pop_path <- file.path(
  root,
  "inst",
  "extdata",
  "sovi_data_pop_514.xlsx"
)

dist_path <- file.path(
  root,
  "inst",
  "extdata",
  "Distance_matrix_514.xlsx"
)


raw_data <- as.data.frame(
  readxl::read_excel(
    data_path
  )
)

indicator_cols <- tail(
  names(raw_data),
  15L
)

x_raw <- data.matrix(
  raw_data[
    ,
    indicator_cols,
    drop = FALSE
  ]
)

x <- unclass(
  scale(x_raw)
)

storage.mode(x) <- "double"


pop_df <- as.data.frame(
  readxl::read_excel(
    pop_path
  )
)

numeric_pop <- which(
  vapply(
    pop_df,
    is.numeric,
    logical(1)
  )
)

name_hits <- intersect(
  grep(
    "pop|population|penduduk",
    names(pop_df),
    ignore.case = TRUE
  ),
  numeric_pop
)

pop_col <- if (length(name_hits)) {
  name_hits[1L]
} else {
  tail(
    numeric_pop,
    1L
  )
}

pop <- as.numeric(
  pop_df[[pop_col]]
)


dist_df <- as.data.frame(
  readxl::read_excel(
    dist_path
  )
)

if (
  ncol(dist_df) == nrow(x) + 1L ||
  !is.numeric(dist_df[[1L]])
) {
  dist_df <- dist_df[
    ,
    -1L,
    drop = FALSE
  ]
}

distmat <- data.matrix(
  dist_df
)

diag(distmat) <- 0


# -----------------------------------------------------------------------------
# 3. Utilities
# -----------------------------------------------------------------------------

hard_counts <- function(
    u,
    k = NCLUSTER) {

  tabulate(
    apply(
      as.matrix(u),
      1,
      which.max
    ),
    nbins = k
  )
}


same_object <- function(
    a,
    b,
    tol = REPRO_TOL) {

  isTRUE(
    all.equal(
      a,
      b,
      tolerance = tol,
      check.attributes = FALSE
    )
  )
}


safe_runtime_seconds <- function(res) {

  tm <- res$time

  if (is.null(tm)) {
    return(NA_real_)
  }

  if (
    !is.null(names(tm)) &&
    "elapsed" %in% names(tm)
  ) {
    return(
      as.numeric(
        tm[["elapsed"]]
      )
    )
  }

  if (length(tm) >= 3L) {
    return(
      as.numeric(
        tm[3L]
      )
    )
  }

  NA_real_
}


# -----------------------------------------------------------------------------
# 4. Shared optimizer configuration
# -----------------------------------------------------------------------------

common_args <- list(
  data = x,
  pop = pop,
  distmat = distmat,
  ncluster = NCLUSTER,
  m = M,
  distance = DISTANCE,
  order = ORDER,
  alpha = ALPHA,
  a = A_SPATIAL,
  b = B_SPATIAL,
  error = ERROR_TOL,
  max.iter = MAX_ITER,
  max_nfe = MAX_NFE,
  randomN = SEED,
  vi.dist = "uniform"
)


optimizer_specs <- list(

  ABC = list(
    fun = "abcfgwc",
    extra = list(
      nfood = POP_SIZE,
      n.onlooker = max(
        1L,
        floor(
          POP_SIZE / 2L
        )
      ),
      limit = 4,
      pso = FALSE,
      abc.same = SAME_LIMIT
    )
  ),

  FPA = list(
    fun = "fpafgwc",
    extra = list(
      nflow = POP_SIZE,
      p = 0.8,
      flow.same = SAME_LIMIT
    )
  ),

  GSA = list(
    fun = "gsafgwc",
    extra = list(
      npar = POP_SIZE,
      par.no = 2,
      gsa.same = SAME_LIMIT,
      G = 1,
      vmax = 0.7,
      new = FALSE
    )
  ),

  GWO = list(
    fun = "gwofgwc",
    extra = list(
      nwolf = POP_SIZE,
      wolf.same = SAME_LIMIT
    )
  ),

  HHO = list(
    fun = "hhofgwc",
    extra = list(
      nhh = POP_SIZE,
      hh.alg = "heidari",
      hh.same = SAME_LIMIT
    )
  ),

  IFA = list(
    fun = "ifafgwc",
    extra = list(
      nfly = POP_SIZE,
      ffly.no = 2,
      fa.same = SAME_LIMIT
    )
  ),

  PSO = list(
    fun = "psofgwc",
    extra = list(
      npar = POP_SIZE,
      pso.same = SAME_LIMIT
    )
  ),

  TLBO = list(
    fun = "tlbofgwc",
    extra = list(
      nstud = POP_SIZE,
      nselection = POP_SIZE,
      tlbo.same = SAME_LIMIT
    )
  ),

  WOA = list(
    fun = "woafgwc",
    extra = list(
      nwhale = POP_SIZE,
      woa.same = SAME_LIMIT
    )
  )
)


run_method <- function(
    method,
    seed = SEED) {

  spec <- optimizer_specs[[method]]

  fn <- get(
    spec$fun,
    envir = v3,
    inherits = FALSE
  )

  args <- c(
    common_args,
    spec$extra
  )

  args$randomN <- seed

  do.call(
    fn,
    args
  )
}


# -----------------------------------------------------------------------------
# 5. Run exact-budget smoke
# -----------------------------------------------------------------------------

methods <- names(
  optimizer_specs
)

results <- list()
repro_results <- list()
errors <- list()

summary_rows <- list()
support_rows <- list()


for (method in methods) {

  cat(
    "\n[",
    method,
    "] V8-B exact-NFE budget smoke...\n",
    sep = ""
  )


  res <- tryCatch(
    run_method(
      method,
      SEED
    ),
    error = function(e) e
  )


  if (inherits(res, "error")) {

    errors[[method]] <- conditionMessage(
      res
    )

    summary_rows[[method]] <- data.frame(
      method = method,
      status = "FAIL",
      failure_reason = paste0(
        "runtime error: ",
        conditionMessage(res)
      ),
      max_nfe = MAX_NFE,
      nfe = NA_integer_,
      nfe_initialization = NA_integer_,
      nfe_optimization = NA_integer_,
      nfe_remaining = NA_integer_,
      budget_exhausted = NA,
      termination_reason = NA_character_,
      exact_budget = FALSE,
      nfe_invariant = FALSE,
      budget_metadata_ok = FALSE,
      nfe_reproducible = FALSE,
      solution_reproducible = FALSE,
      fitness_type_ok = FALSE,
      occupied = NA_integer_,
      occupancy_ok = FALSE,
      min_cluster_size = NA_integer_,
      final_XB = NA_real_,
      spatial_J = NA_real_,
      iterations = NA_integer_,
      runtime_sec = NA_real_,
      stringsAsFactors = FALSE
    )

    next
  }


  results[[method]] <- res


  repro <- NULL

  if (REPRODUCIBILITY_RUN) {

    repro <- tryCatch(
      run_method(
        method,
        SEED
      ),
      error = function(e) e
    )

    if (!inherits(repro, "error")) {
      repro_results[[method]] <- repro
    }
  }


  required_fields <- c(
    "nfe",
    "nfe_initialization",
    "nfe_optimization",
    "max_nfe",
    "budget_exhausted",
    "nfe_remaining",
    "termination_reason",
    "fitness_type",
    "membership",
    "centroid",
    "search_centroid",
    "cluster",
    "f_obj",
    "spatial_obj"
  )


  fields_ok <- all(
    required_fields %in% names(res)
  )


  nfe <- if (fields_ok) {
    as.integer(res$nfe)
  } else {
    NA_integer_
  }

  nfe_initialization <- if (fields_ok) {
    as.integer(res$nfe_initialization)
  } else {
    NA_integer_
  }

  nfe_optimization <- if (fields_ok) {
    as.integer(res$nfe_optimization)
  } else {
    NA_integer_
  }


  exact_budget <- (
    fields_ok &&
    identical(
      nfe,
      as.integer(MAX_NFE)
    ) &&
    identical(
      as.integer(res$max_nfe),
      as.integer(MAX_NFE)
    )
  )


  nfe_invariant <- (
    fields_ok &&
    identical(
      nfe,
      as.integer(
        nfe_initialization +
          nfe_optimization
      )
    )
  )


  budget_metadata_ok <- (
    fields_ok &&
    isTRUE(
      res$budget_exhausted
    ) &&
    identical(
      as.integer(
        res$nfe_remaining
      ),
      0L
    ) &&
    identical(
      res$termination_reason,
      "max_nfe"
    )
  )


  counts <- if (fields_ok) {
    hard_counts(
      res$membership
    )
  } else {
    rep(
      NA_integer_,
      NCLUSTER
    )
  }


  occupied <- if (all(is.finite(counts))) {
    sum(
      counts > 0L
    )
  } else {
    NA_integer_
  }


  occupancy_ok <- (
    is.finite(occupied) &&
    occupied == NCLUSTER
  )


  min_cluster_size <- if (
    all(is.finite(counts))
  ) {
    min(counts)
  } else {
    NA_integer_
  }


  fitness_type_ok <- (
    fields_ok &&
    identical(
      res$fitness_type,
      "spatial_XB_feasible"
    )
  )


  nfe_reproducible <- FALSE
  solution_reproducible <- FALSE


  if (
    REPRODUCIBILITY_RUN &&
    !is.null(repro) &&
    !inherits(repro, "error")
  ) {

    nfe_reproducible <- (
      identical(
        as.integer(res$nfe),
        as.integer(repro$nfe)
      ) &&
      identical(
        as.integer(res$nfe_initialization),
        as.integer(
          repro$nfe_initialization
        )
      ) &&
      identical(
        as.integer(res$nfe_optimization),
        as.integer(
          repro$nfe_optimization
        )
      ) &&
      identical(
        as.integer(res$nfe_remaining),
        as.integer(
          repro$nfe_remaining
        )
      ) &&
      identical(
        res$termination_reason,
        repro$termination_reason
      )
    )


    solution_reproducible <- (
      same_object(
        res$f_obj,
        repro$f_obj
      ) &&
      same_object(
        res$search_centroid,
        repro$search_centroid
      ) &&
      same_object(
        res$centroid,
        repro$centroid
      ) &&
      same_object(
        res$membership,
        repro$membership
      ) &&
      identical(
        res$cluster,
        repro$cluster
      )
    )
  }


  gate_pass <- all(
    fields_ok,
    exact_budget,
    nfe_invariant,
    budget_metadata_ok,
    nfe_reproducible,
    solution_reproducible,
    fitness_type_ok,
    occupancy_ok
  )


  status <- if (gate_pass) {
    "PASS"
  } else {
    "FAIL"
  }


  failures <- character()


  add_failure <- function(
      condition,
      text) {

    if (!isTRUE(condition)) {
      failures <<- c(
        failures,
        text
      )
    }
  }


  add_failure(
    fields_ok,
    "missing result field(s)"
  )

  add_failure(
    exact_budget,
    "exact total NFE"
  )

  add_failure(
    nfe_invariant,
    "NFE invariant"
  )

  add_failure(
    budget_metadata_ok,
    "budget metadata"
  )

  add_failure(
    nfe_reproducible,
    "NFE reproducibility"
  )

  add_failure(
    solution_reproducible,
    "solution reproducibility"
  )

  add_failure(
    fitness_type_ok,
    "fitness type"
  )

  add_failure(
    occupancy_ok,
    "4/4 occupancy"
  )


  failure_reason <- if (
    length(failures)
  ) {
    paste(
      failures,
      collapse = "; "
    )
  } else {
    ""
  }


  summary_rows[[method]] <- data.frame(
    method = method,
    status = status,
    failure_reason = failure_reason,
    max_nfe = as.integer(res$max_nfe),
    nfe = nfe,
    nfe_initialization = nfe_initialization,
    nfe_optimization = nfe_optimization,
    nfe_remaining = as.integer(
      res$nfe_remaining
    ),
    budget_exhausted = isTRUE(
      res$budget_exhausted
    ),
    termination_reason = as.character(
      res$termination_reason
    ),
    exact_budget = exact_budget,
    nfe_invariant = nfe_invariant,
    budget_metadata_ok = budget_metadata_ok,
    nfe_reproducible = nfe_reproducible,
    solution_reproducible = solution_reproducible,
    fitness_type_ok = fitness_type_ok,
    occupied = occupied,
    occupancy_ok = occupancy_ok,
    min_cluster_size = min_cluster_size,
    final_XB = as.numeric(
      res$f_obj
    ),
    spatial_J = as.numeric(
      res$spatial_obj
    ),
    iterations = as.integer(
      res$iteration
    ),
    runtime_sec = safe_runtime_seconds(
      res
    ),
    stringsAsFactors = FALSE
  )


  if (fields_ok) {

    for (
      k in seq_len(
        NCLUSTER
      )
    ) {

      support_rows[[
        paste(
          method,
          k,
          sep = "_"
        )
      ]] <- data.frame(
        method = method,
        cluster = k,
        hard_count = counts[k],
        hard_proportion = counts[k] /
          nrow(x),
        soft_mass = sum(
          res$membership[
            ,
            k
          ]
        ),
        effective_mass_m2 = sum(
          res$membership[
            ,
            k
          ]^M
        ),
        stringsAsFactors = FALSE
      )
    }
  }


  cat(
    sprintf(
      paste0(
        "[%s] %s | NFE=%d/%d ",
        "(init=%d, opt=%d) | ",
        "remaining=%d | reason=%s | ",
        "XB=%.8f | occupied=%d/%d | ",
        "repro=%s\n"
      ),
      method,
      status,
      nfe,
      MAX_NFE,
      nfe_initialization,
      nfe_optimization,
      as.integer(
        res$nfe_remaining
      ),
      as.character(
        res$termination_reason
      ),
      as.numeric(
        res$f_obj
      ),
      occupied,
      NCLUSTER,
      nfe_reproducible &&
        solution_reproducible
    )
  )
}


# -----------------------------------------------------------------------------
# 6. Combine results
# -----------------------------------------------------------------------------

summary_df <- do.call(
  rbind,
  summary_rows
)

rownames(
  summary_df
) <- NULL


support_df <- if (
  length(
    support_rows
  )
) {

  do.call(
    rbind,
    support_rows
  )

} else {

  data.frame()
}


if (
  nrow(
    support_df
  )
) {

  rownames(
    support_df
  ) <- NULL
}


# -----------------------------------------------------------------------------
# 7. Technical gate
# -----------------------------------------------------------------------------

n_pass <- sum(
  summary_df$status == "PASS"
)

n_fail <- sum(
  summary_df$status == "FAIL"
)


all_exact_budget <- all(
  summary_df$exact_budget
)

all_budget_exhausted <- all(
  summary_df$budget_exhausted
)

all_max_nfe_reason <- all(
  summary_df$termination_reason ==
    "max_nfe"
)

all_nfe_invariant <- all(
  summary_df$nfe_invariant
)

all_reproducible <- all(
  summary_df$nfe_reproducible &
    summary_df$solution_reproducible
)

all_occupancy <- all(
  summary_df$occupancy_ok
)


overall_status <- if (
  n_fail == 0L &&
  all_exact_budget &&
  all_budget_exhausted &&
  all_max_nfe_reason &&
  all_nfe_invariant &&
  all_reproducible &&
  all_occupancy
) {
  "PASS"
} else {
  "FAIL_INVESTIGATE"
}


# -----------------------------------------------------------------------------
# 8. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v8b_exact_nfe_budget_smoke"
)


dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


config_df <- data.frame(
  parameter = c(
    "seed",
    "ncluster",
    "m",
    "alpha",
    "a",
    "b",
    "population",
    "max_nfe",
    "max_iter",
    "error",
    "same_limit",
    "reproducibility_run",
    "fitness_type"
  ),
  value = c(
    SEED,
    NCLUSTER,
    M,
    ALPHA,
    A_SPATIAL,
    B_SPATIAL,
    POP_SIZE,
    MAX_NFE,
    MAX_ITER,
    ERROR_TOL,
    SAME_LIMIT,
    REPRODUCIBILITY_RUN,
    "spatial_XB_feasible"
  ),
  stringsAsFactors = FALSE
)


utils::write.csv(
  config_df,
  file.path(
    out_dir,
    "v8b_config.csv"
  ),
  row.names = FALSE
)


utils::write.csv(
  summary_df,
  file.path(
    out_dir,
    "v8b_optimizer_summary.csv"
  ),
  row.names = FALSE
)


if (
  nrow(
    support_df
  )
) {

  utils::write.csv(
    support_df,
    file.path(
      out_dir,
      "v8b_cluster_support.csv"
    ),
    row.names = FALSE
  )
}


if (
  length(
    errors
  )
) {

  error_df <- data.frame(
    method = names(
      errors
    ),
    error = unlist(
      errors,
      use.names = FALSE
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    error_df,
    file.path(
      out_dir,
      "v8b_errors.csv"
    ),
    row.names = FALSE
  )
}


# -----------------------------------------------------------------------------
# 9. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("====================================================================\n")
cat("V8-B — Exact NFE Budget-Control Smoke\n")
cat("====================================================================\n")
cat(
  "Dataset             :",
  nrow(x),
  "x",
  ncol(x),
  "\n"
)
cat(
  "Clusters            :",
  NCLUSTER,
  "\n"
)
cat(
  "Fuzzifier           :",
  M,
  "\n"
)
cat(
  "Spatial alpha       :",
  ALPHA,
  "\n"
)
cat(
  "Population          :",
  POP_SIZE,
  "\n"
)
cat(
  "max_nfe             :",
  MAX_NFE,
  "\n"
)
cat(
  "max.iter safety cap :",
  MAX_ITER,
  "\n"
)
cat(
  "Seed                :",
  SEED,
  "\n"
)
cat(
  "Fitness             : spatial_XB_feasible\n"
)
cat(
  "Reproducibility run :",
  REPRODUCIBILITY_RUN,
  "\n"
)
cat("====================================================================\n")


cat(
  "\n[1] EXACT BUDGET PROFILE\n"
)


print(
  summary_df[
    ,
    c(
      "method",
      "status",
      "max_nfe",
      "nfe",
      "nfe_initialization",
      "nfe_optimization",
      "nfe_remaining",
      "termination_reason",
      "exact_budget",
      "nfe_invariant",
      "occupied",
      "final_XB",
      "iterations"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


cat(
  "\n[2] CROSS-OPTIMIZER BUDGET GATE\n"
)

cat(
  "All exact budget       :",
  all_exact_budget,
  "\n"
)

cat(
  "All budget exhausted   :",
  all_budget_exhausted,
  "\n"
)

cat(
  "All reason=max_nfe      :",
  all_max_nfe_reason,
  "\n"
)

cat(
  "All NFE invariant      :",
  all_nfe_invariant,
  "\n"
)

cat(
  "All reproducible       :",
  all_reproducible,
  "\n"
)

cat(
  "All occupancy 4/4      :",
  all_occupancy,
  "\n"
)


cat(
  "\n[3] TECHNICAL GATE\n"
)

cat(
  "PASS    :",
  n_pass,
  "\n"
)

cat(
  "FAIL    :",
  n_fail,
  "\n"
)

cat(
  "OVERALL :",
  overall_status,
  "\n"
)


cat(
  "\nInterpretation:\n"
)

cat(
  "- PASS demonstrates that Patch v3.1b enforces the same total NFE budget\n",
  "  for all nine optimizers without overshoot.\n",
  sep = ""
)

cat(
  "- This smoke test validates budget mechanics and reproducibility only.\n",
  "  Do NOT rank optimizer quality from this single seed / single budget run.\n",
  sep = ""
)

cat(
  "- After this gate passes, the next stage is the V8 NFE-fair pilot with\n",
  "  multiple seeds and a pre-specified equal NFE budget.\n",
  sep = ""
)


cat(
  "\nResults written to:\n  ",
  out_dir,
  "\n",
  sep = ""
)


if (
  !identical(
    overall_status,
    "PASS"
  )
) {

  stop(
    paste0(
      "V8-B exact NFE budget-control gate failed. ",
      "Inspect output CSV before continuing."
    ),
    call. = FALSE
  )
}


invisible(
  list(
    config = config_df,
    summary = summary_df,
    support = support_df,
    results = results,
    reproducibility_results = repro_results,
    errors = errors,
    overall_status = overall_status
  )
)
