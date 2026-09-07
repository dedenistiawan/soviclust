# =============================================================================
# soviclust — V9-A FGWC v3.2a Behavior-Preservation Regression Smoke
# =============================================================================
#
# Purpose
# -------
# Prove that Patch v3.2a (generic evaluator architecture) is a pure
# architectural refactor for FGWC on the bundled Indonesia 514 x 15 dataset.
#
# This gate compares:
#   A. Current default evaluator (`evaluator = NULL`)
#   B. Current explicit `.soviclust_v3_fgwc_evaluator()`
#   C. Committed V8-B v3.1b baseline summary
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
# Main gate requirements
# ----------------------
# 1. Default and explicit FGWC evaluator are solution-equivalent:
#      - NFE metadata
#      - f_obj / spatial_obj
#      - search_centroid
#      - final centroid
#      - membership
#      - cluster
#
# 2. Current v3.2a result matches committed V8-B v3.1b baseline:
#      - final_XB
#      - spatial_J
#      - NFE / init NFE / optimization NFE
#      - iterations
#      - occupied clusters
#      - minimum hard-cluster size
#      - termination_reason
#
# 3. Exact-budget and feasibility semantics remain intact:
#      - nfe == 200
#      - nfe == nfe_initialization + nfe_optimization
#      - nfe_remaining == 0
#      - budget_exhausted == TRUE
#      - termination_reason == "max_nfe"
#      - fitness_type == "spatial_XB_feasible"
#      - occupied clusters == 4
#
# Outputs
# -------
# validation_results/v9a_fgwc_v32a_regression_smoke/
#   v9a_config.csv
#   v9a_regression_summary.csv
#   v9a_cluster_support.csv
#   v9a_gate_summary.csv
#
# Run from project root:
#   source("validation/v9a_fgwc_v32a_regression_smoke.R")
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

# Keep iteration/stagnation limits non-binding so max_nfe is the stop rule.
MAX_ITER <- 1000L
SAME_LIMIT <- MAX_ITER + 1000L
ERROR_TOL <- 0

# Numerical tolerance for v3.1b-v3.2a equality.
TOL <- 1e-10


# -----------------------------------------------------------------------------
# 1. Locate project root
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
      ),
      file.path(
        p,
        "validation_results",
        "v8b_exact_nfe_budget_smoke",
        "v8b_optimizer_summary.csv"
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
    paste0(
      "Unable to locate soviclust source root with the committed V8-B baseline. ",
      "Run this script from the project checkout."
    ),
    call. = FALSE
  )
}


root <- find_project_root()


# -----------------------------------------------------------------------------
# 2. Load isolated Patch-v3 engine
# -----------------------------------------------------------------------------

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
  ".soviclust_v3_nfe_snapshot",
  ".soviclust_v3_fgwc_evaluator",
  ".soviclust_v3_resolve_evaluator"
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
      "Patch v3.2a generic-evaluator infrastructure is incomplete. Missing: ",
      paste(
        missing_engine_functions,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


explicit_fgwc_evaluator <- v3$.soviclust_v3_fgwc_evaluator()

if (
  !is.list(explicit_fgwc_evaluator) ||
  !identical(explicit_fgwc_evaluator$model_type, "FGWC") ||
  !identical(
    explicit_fgwc_evaluator$fitness_type,
    "spatial_XB_feasible"
  )
) {
  stop(
    "Default explicit FGWC evaluator metadata is invalid.",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 3. Load bundled Indonesia 514 x 15 validation data
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


if (
  nrow(x) != 514L ||
  ncol(x) != 15L
) {
  stop(
    sprintf(
      "Unexpected validation data dimension: %d x %d; expected 514 x 15.",
      nrow(x),
      ncol(x)
    ),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 4. Load committed V8-B v3.1b baseline
# -----------------------------------------------------------------------------

baseline_path <- file.path(
  root,
  "validation_results",
  "v8b_exact_nfe_budget_smoke",
  "v8b_optimizer_summary.csv"
)

baseline <- read.csv(
  baseline_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_baseline_cols <- c(
  "method",
  "status",
  "max_nfe",
  "nfe",
  "nfe_initialization",
  "nfe_optimization",
  "nfe_remaining",
  "budget_exhausted",
  "termination_reason",
  "occupied",
  "min_cluster_size",
  "final_XB",
  "spatial_J",
  "iterations"
)

missing_baseline_cols <- setdiff(
  required_baseline_cols,
  names(baseline)
)

if (length(missing_baseline_cols)) {
  stop(
    paste0(
      "Committed V8-B baseline is missing column(s): ",
      paste(
        missing_baseline_cols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 5. Utilities
# -----------------------------------------------------------------------------

same_object <- function(
    a,
    b,
    tol = TOL) {

  isTRUE(
    all.equal(
      a,
      b,
      tolerance = tol,
      check.attributes = FALSE
    )
  )
}


same_number <- function(
    a,
    b,
    tol = TOL) {

  if (
    length(a) != 1L ||
    length(b) != 1L ||
    !is.finite(a) ||
    !is.finite(b)
  ) {
    return(FALSE)
  }

  isTRUE(
    all.equal(
      as.numeric(a),
      as.numeric(b),
      tolerance = tol,
      check.attributes = FALSE
    )
  )
}


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


bool_text <- function(x) {
  if (isTRUE(x)) "TRUE" else "FALSE"
}


# -----------------------------------------------------------------------------
# 6. Shared optimizer configuration
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
    evaluator = NULL) {

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

  args$evaluator <- evaluator

  do.call(
    fn,
    args
  )
}


# -----------------------------------------------------------------------------
# 7. Run V9-A behavior-preservation gate
# -----------------------------------------------------------------------------

methods <- names(
  optimizer_specs
)

summary_rows <- list()
support_rows <- list()


cat("\n")
cat("============================================================\n")
cat("V9-A FGWC v3.2a Behavior-Preservation Regression Smoke\n")
cat("============================================================\n")
cat(sprintf("Dataset : %d x %d\n", nrow(x), ncol(x)))
cat(sprintf("Seed    : %d\n", SEED))
cat(sprintf("Max NFE : %d\n", MAX_NFE))
cat("Compare : default v3.2a vs explicit FGWC evaluator vs V8-B baseline\n")
cat("============================================================\n")


for (method in methods) {

  cat(
    "\n[",
    method,
    "] running default evaluator...\n",
    sep = ""
  )

  implicit <- tryCatch(
    run_method(
      method,
      evaluator = NULL
    ),
    error = function(e) e
  )

  if (inherits(implicit, "error")) {

    summary_rows[[method]] <- data.frame(
      method = method,
      status = "FAIL",
      failure_reason = paste0(
        "default evaluator runtime error: ",
        conditionMessage(implicit)
      ),
      baseline_XB = NA_real_,
      current_XB = NA_real_,
      xb_abs_diff = NA_real_,
      baseline_spatial_J = NA_real_,
      current_spatial_J = NA_real_,
      spatial_J_abs_diff = NA_real_,
      baseline_match = FALSE,
      explicit_equivalent = FALSE,
      exact_budget = FALSE,
      nfe_invariant = FALSE,
      budget_metadata_ok = FALSE,
      fitness_type_ok = FALSE,
      occupancy_ok = FALSE,
      nfe = NA_integer_,
      nfe_initialization = NA_integer_,
      nfe_optimization = NA_integer_,
      occupied = NA_integer_,
      min_cluster_size = NA_integer_,
      iterations = NA_integer_,
      stringsAsFactors = FALSE
    )

    next
  }


  cat(
    "[",
    method,
    "] running explicit FGWC evaluator...\n",
    sep = ""
  )

  explicit <- tryCatch(
    run_method(
      method,
      evaluator = explicit_fgwc_evaluator
    ),
    error = function(e) e
  )

  if (inherits(explicit, "error")) {

    summary_rows[[method]] <- data.frame(
      method = method,
      status = "FAIL",
      failure_reason = paste0(
        "explicit evaluator runtime error: ",
        conditionMessage(explicit)
      ),
      baseline_XB = NA_real_,
      current_XB = as.numeric(implicit$f_obj),
      xb_abs_diff = NA_real_,
      baseline_spatial_J = NA_real_,
      current_spatial_J = as.numeric(implicit$spatial_obj),
      spatial_J_abs_diff = NA_real_,
      baseline_match = FALSE,
      explicit_equivalent = FALSE,
      exact_budget = FALSE,
      nfe_invariant = FALSE,
      budget_metadata_ok = FALSE,
      fitness_type_ok = FALSE,
      occupancy_ok = FALSE,
      nfe = as.integer(implicit$nfe),
      nfe_initialization = as.integer(implicit$nfe_initialization),
      nfe_optimization = as.integer(implicit$nfe_optimization),
      occupied = NA_integer_,
      min_cluster_size = NA_integer_,
      iterations = as.integer(implicit$iteration),
      stringsAsFactors = FALSE
    )

    next
  }


  base <- baseline[
    baseline$method == method,
    ,
    drop = FALSE
  ]

  if (nrow(base) != 1L) {
    stop(
      paste0(
        "Expected exactly one V8-B baseline row for ",
        method,
        "."
      ),
      call. = FALSE
    )
  }


  counts <- hard_counts(
    implicit$membership
  )

  occupied <- sum(
    counts > 0L
  )

  min_cluster_size <- min(
    counts
  )


  # ---------------------------------------------------------------------------
  # Current default vs explicit generic FGWC evaluator
  # ---------------------------------------------------------------------------

  explicit_equivalent <- all(
    identical(
      as.integer(implicit$nfe),
      as.integer(explicit$nfe)
    ),
    identical(
      as.integer(implicit$nfe_initialization),
      as.integer(explicit$nfe_initialization)
    ),
    identical(
      as.integer(implicit$nfe_optimization),
      as.integer(explicit$nfe_optimization)
    ),
    identical(
      as.integer(implicit$nfe_remaining),
      as.integer(explicit$nfe_remaining)
    ),
    identical(
      implicit$termination_reason,
      explicit$termination_reason
    ),
    identical(
      implicit$fitness_type,
      explicit$fitness_type
    ),
    same_number(
      implicit$f_obj,
      explicit$f_obj
    ),
    same_number(
      implicit$spatial_obj,
      explicit$spatial_obj
    ),
    same_object(
      implicit$search_centroid,
      explicit$search_centroid
    ),
    same_object(
      implicit$centroid,
      explicit$centroid
    ),
    same_object(
      implicit$membership,
      explicit$membership
    ),
    identical(
      implicit$cluster,
      explicit$cluster
    )
  )


  # ---------------------------------------------------------------------------
  # Current v3.2a vs committed V8-B v3.1b baseline
  # ---------------------------------------------------------------------------

  baseline_match <- all(
    identical(
      as.integer(implicit$max_nfe),
      as.integer(base$max_nfe)
    ),
    identical(
      as.integer(implicit$nfe),
      as.integer(base$nfe)
    ),
    identical(
      as.integer(implicit$nfe_initialization),
      as.integer(base$nfe_initialization)
    ),
    identical(
      as.integer(implicit$nfe_optimization),
      as.integer(base$nfe_optimization)
    ),
    identical(
      as.integer(implicit$nfe_remaining),
      as.integer(base$nfe_remaining)
    ),
    identical(
      isTRUE(implicit$budget_exhausted),
      isTRUE(base$budget_exhausted)
    ),
    identical(
      as.character(implicit$termination_reason),
      as.character(base$termination_reason)
    ),
    same_number(
      implicit$f_obj,
      base$final_XB
    ),
    same_number(
      implicit$spatial_obj,
      base$spatial_J
    ),
    identical(
      as.integer(implicit$iteration),
      as.integer(base$iterations)
    ),
    identical(
      as.integer(occupied),
      as.integer(base$occupied)
    ),
    identical(
      as.integer(min_cluster_size),
      as.integer(base$min_cluster_size)
    )
  )


  # ---------------------------------------------------------------------------
  # Budget/fitness/feasibility invariants
  # ---------------------------------------------------------------------------

  exact_budget <- (
    identical(
      as.integer(implicit$nfe),
      as.integer(MAX_NFE)
    ) &&
    identical(
      as.integer(implicit$max_nfe),
      as.integer(MAX_NFE)
    )
  )


  nfe_invariant <- identical(
    as.integer(implicit$nfe),
    as.integer(
      implicit$nfe_initialization +
        implicit$nfe_optimization
    )
  )


  budget_metadata_ok <- (
    isTRUE(
      implicit$budget_exhausted
    ) &&
    identical(
      as.integer(
        implicit$nfe_remaining
      ),
      0L
    ) &&
    identical(
      implicit$termination_reason,
      "max_nfe"
    )
  )


  fitness_type_ok <- (
    identical(
      implicit$fitness_type,
      "spatial_XB_feasible"
    ) &&
    identical(
      explicit$fitness_type,
      "spatial_XB_feasible"
    )
  )


  occupancy_ok <- (
    occupied == NCLUSTER
  )


  gate_pass <- all(
    explicit_equivalent,
    baseline_match,
    exact_budget,
    nfe_invariant,
    budget_metadata_ok,
    fitness_type_ok,
    occupancy_ok
  )


  failures <- character()

  if (!explicit_equivalent) {
    failures <- c(
      failures,
      "default vs explicit evaluator mismatch"
    )
  }

  if (!baseline_match) {
    failures <- c(
      failures,
      "v3.1b V8-B baseline mismatch"
    )
  }

  if (!exact_budget) {
    failures <- c(
      failures,
      "exact NFE budget"
    )
  }

  if (!nfe_invariant) {
    failures <- c(
      failures,
      "NFE invariant"
    )
  }

  if (!budget_metadata_ok) {
    failures <- c(
      failures,
      "budget metadata"
    )
  }

  if (!fitness_type_ok) {
    failures <- c(
      failures,
      "fitness metadata"
    )
  }

  if (!occupancy_ok) {
    failures <- c(
      failures,
      "4/4 occupancy"
    )
  }


  status <- if (gate_pass) {
    "PASS"
  } else {
    "FAIL"
  }


  summary_rows[[method]] <- data.frame(
    method = method,
    status = status,
    failure_reason = paste(
      failures,
      collapse = "; "
    ),
    baseline_XB = as.numeric(
      base$final_XB
    ),
    current_XB = as.numeric(
      implicit$f_obj
    ),
    xb_abs_diff = abs(
      as.numeric(implicit$f_obj) -
        as.numeric(base$final_XB)
    ),
    baseline_spatial_J = as.numeric(
      base$spatial_J
    ),
    current_spatial_J = as.numeric(
      implicit$spatial_obj
    ),
    spatial_J_abs_diff = abs(
      as.numeric(implicit$spatial_obj) -
        as.numeric(base$spatial_J)
    ),
    baseline_match = baseline_match,
    explicit_equivalent = explicit_equivalent,
    exact_budget = exact_budget,
    nfe_invariant = nfe_invariant,
    budget_metadata_ok = budget_metadata_ok,
    fitness_type_ok = fitness_type_ok,
    occupancy_ok = occupancy_ok,
    nfe = as.integer(
      implicit$nfe
    ),
    nfe_initialization = as.integer(
      implicit$nfe_initialization
    ),
    nfe_optimization = as.integer(
      implicit$nfe_optimization
    ),
    occupied = as.integer(
      occupied
    ),
    min_cluster_size = as.integer(
      min_cluster_size
    ),
    iterations = as.integer(
      implicit$iteration
    ),
    stringsAsFactors = FALSE
  )


  for (k in seq_len(NCLUSTER)) {

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
        implicit$membership[
          ,
          k
        ]
      ),
      effective_mass_m2 = sum(
        implicit$membership[
          ,
          k
        ]^M
      ),
      stringsAsFactors = FALSE
    )
  }


  cat(
    sprintf(
      paste0(
        "[%s] %s | ",
        "XB baseline=%.12f current=%.12f | ",
        "dXB=%.3e | ",
        "NFE=%d (%d+%d) | ",
        "occupied=%d/%d | ",
        "explicit=%s | baseline=%s\n"
      ),
      method,
      status,
      as.numeric(
        base$final_XB
      ),
      as.numeric(
        implicit$f_obj
      ),
      abs(
        as.numeric(implicit$f_obj) -
          as.numeric(base$final_XB)
      ),
      as.integer(
        implicit$nfe
      ),
      as.integer(
        implicit$nfe_initialization
      ),
      as.integer(
        implicit$nfe_optimization
      ),
      occupied,
      NCLUSTER,
      bool_text(
        explicit_equivalent
      ),
      bool_text(
        baseline_match
      )
    )
  )
}


# -----------------------------------------------------------------------------
# 8. Combine results
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
# 9. Technical gate
# -----------------------------------------------------------------------------

n_pass <- sum(
  summary_df$status == "PASS"
)

n_fail <- sum(
  summary_df$status == "FAIL"
)


all_explicit_equivalent <- all(
  summary_df$explicit_equivalent
)

all_baseline_match <- all(
  summary_df$baseline_match
)

all_exact_budget <- all(
  summary_df$exact_budget
)

all_nfe_invariant <- all(
  summary_df$nfe_invariant
)

all_budget_metadata_ok <- all(
  summary_df$budget_metadata_ok
)

all_fitness_type_ok <- all(
  summary_df$fitness_type_ok
)

all_occupancy_ok <- all(
  summary_df$occupancy_ok
)


overall_pass <- all(
  n_pass == length(methods),
  n_fail == 0L,
  all_explicit_equivalent,
  all_baseline_match,
  all_exact_budget,
  all_nfe_invariant,
  all_budget_metadata_ok,
  all_fitness_type_ok,
  all_occupancy_ok
)


gate_df <- data.frame(
  gate = c(
    "planned_methods",
    "passed_methods",
    "failed_methods",
    "all_default_explicit_equivalent",
    "all_v8b_baseline_match",
    "all_exact_budget",
    "all_nfe_invariant",
    "all_budget_metadata_ok",
    "all_fitness_type_ok",
    "all_occupancy_4_of_4",
    "overall_gate"
  ),
  value = c(
    as.character(
      length(methods)
    ),
    as.character(
      n_pass
    ),
    as.character(
      n_fail
    ),
    bool_text(
      all_explicit_equivalent
    ),
    bool_text(
      all_baseline_match
    ),
    bool_text(
      all_exact_budget
    ),
    bool_text(
      all_nfe_invariant
    ),
    bool_text(
      all_budget_metadata_ok
    ),
    bool_text(
      all_fitness_type_ok
    ),
    bool_text(
      all_occupancy_ok
    ),
    if (overall_pass) {
      "PASS"
    } else {
      "FAIL"
    }
  ),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# 10. Save outputs
# -----------------------------------------------------------------------------

output_dir <- file.path(
  root,
  "validation_results",
  "v9a_fgwc_v32a_regression_smoke"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


config_df <- data.frame(
  parameter = c(
    "validation",
    "dataset_n",
    "dataset_p",
    "ncluster",
    "m",
    "alpha",
    "a",
    "b",
    "population",
    "max_nfe",
    "max_iter_safety",
    "same_limit",
    "error_tol",
    "seed",
    "distance",
    "order",
    "fitness",
    "tolerance",
    "baseline_file"
  ),
  value = c(
    "V9-A FGWC v3.2a behavior-preservation regression smoke",
    nrow(x),
    ncol(x),
    NCLUSTER,
    M,
    ALPHA,
    A_SPATIAL,
    B_SPATIAL,
    POP_SIZE,
    MAX_NFE,
    MAX_ITER,
    SAME_LIMIT,
    ERROR_TOL,
    SEED,
    DISTANCE,
    ORDER,
    "spatial_XB_feasible",
    format(
      TOL,
      scientific = TRUE
    ),
    "validation_results/v8b_exact_nfe_budget_smoke/v8b_optimizer_summary.csv"
  ),
  stringsAsFactors = FALSE
)


write.csv(
  config_df,
  file.path(
    output_dir,
    "v9a_config.csv"
  ),
  row.names = FALSE
)


write.csv(
  summary_df,
  file.path(
    output_dir,
    "v9a_regression_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  support_df,
  file.path(
    output_dir,
    "v9a_cluster_support.csv"
  ),
  row.names = FALSE
)


write.csv(
  gate_df,
  file.path(
    output_dir,
    "v9a_gate_summary.csv"
  ),
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 11. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V9-A TECHNICAL GATE SUMMARY\n")
cat("============================================================\n")

print(
  summary_df[
    ,
    c(
      "method",
      "status",
      "baseline_XB",
      "current_XB",
      "xb_abs_diff",
      "baseline_match",
      "explicit_equivalent",
      "nfe",
      "nfe_initialization",
      "nfe_optimization",
      "occupied",
      "min_cluster_size"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n")
print(
  gate_df,
  row.names = FALSE
)

cat("\nOutputs written to:\n")
cat(
  normalizePath(
    output_dir,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n"
)

cat("\n")


if (overall_pass) {

  cat("OVERALL V9-A GATE: PASS\n")
  cat(
    paste0(
      "Conclusion: Patch v3.2a is behavior-preserving for FGWC under the ",
      "Indonesia 514 x 15 regression configuration.\n"
    )
  )

} else {

  cat("OVERALL V9-A GATE: FAIL\n")

  failed <- summary_df[
    summary_df$status != "PASS",
    ,
    drop = FALSE
  ]

  if (nrow(failed)) {
    cat("\nFailed methods:\n")
    print(
      failed[
        ,
        c(
          "method",
          "failure_reason"
        ),
        drop = FALSE
      ],
      row.names = FALSE
    )
  }

  stop(
    "V9-A regression gate failed. Review CSV outputs before committing Patch v3.2a.",
    call. = FALSE
  )
}
