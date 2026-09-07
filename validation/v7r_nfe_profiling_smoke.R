# =============================================================================
# soviclust — V7-R-NFE Patch v3.1a NFE Profiling Smoke
# =============================================================================
#
# Purpose
# -------
# Empirically profile the Number of Function Evaluations (NFE) used by all
# nine Patch-v3 optimizers after Patch v3.1a NFE instrumentation.
#
# SAME core configuration as V7-R:
#   - Indonesia 514 x 15 standardized indicators
#   - c = 4, m = 2, alpha = 0.7, a = b = 1
#   - population = 10, max.iter = 10, seed = 2026
#   - early stopping disabled
#   - fitness = spatial_XB_feasible
#
# IMPORTANT:
# This is NOT an NFE-fair optimizer benchmark and MUST NOT be used to rank
# optimizer quality. It validates NFE accounting and reveals the evaluation
# cost of equal-iteration runs before Patch v3.1b adds max_nfe control.
#
# Run:
#   source("validation/v7r_nfe_profiling_smoke.R")
# =============================================================================

SEED <- 2026L
NCLUSTER <- 4L
M <- 2
ALPHA <- 0.7
A_SPATIAL <- 1
B_SPATIAL <- 1
DISTANCE <- "euclidean"
ORDER <- 2
POP_SIZE <- 10L
MAX_ITER <- 10L
SAME_LIMIT <- MAX_ITER + 100L
ERROR_TOL <- 0
REPRODUCIBILITY_RUN <- TRUE
REPRO_TOL <- 1e-10


find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    required <- c(
      file.path(p, "DESCRIPTION"),
      file.path(p, "inst", "app", "R", "shared", "function", "optimizer_v3.R"),
      file.path(p, "tests", "testthat", "helper-v3-engine.R")
    )
    if (all(file.exists(required))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  stop("Unable to locate soviclust source root.", call. = FALSE)
}

root <- find_project_root()

helper <- file.path(root, "tests", "testthat", "helper-v3-engine.R")
helper_env <- new.env(parent = globalenv())
sys.source(helper, envir = helper_env)
v3 <- helper_env$v3_env

if (!is.environment(v3)) {
  stop("Patch-v3 isolated test engine was not created.", call. = FALSE)
}

required_engine_functions <- c(
  ".soviclust_v3_validate_common",
  ".soviclust_v3_init_population",
  ".soviclust_v3_eval",
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
      "Patch v3.1a NFE instrumentation is incomplete. Missing: ",
      paste(missing_engine_functions, collapse = ", ")
    ),
    call. = FALSE
  )
}


if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required.", call. = FALSE)
}

data_path <- file.path(root, "inst", "extdata", "sovi_data_kab_514_15.xlsx")
pop_path <- file.path(root, "inst", "extdata", "sovi_data_pop_514.xlsx")
dist_path <- file.path(root, "inst", "extdata", "Distance_matrix_514.xlsx")

raw_data <- as.data.frame(readxl::read_excel(data_path))
indicator_cols <- tail(names(raw_data), 15L)
x_raw <- data.matrix(raw_data[, indicator_cols, drop = FALSE])
x <- unclass(scale(x_raw))
storage.mode(x) <- "double"

pop_df <- as.data.frame(readxl::read_excel(pop_path))
numeric_pop <- which(vapply(pop_df, is.numeric, logical(1)))
name_hits <- intersect(
  grep("pop|population|penduduk", names(pop_df), ignore.case = TRUE),
  numeric_pop
)
pop_col <- if (length(name_hits)) name_hits[1L] else tail(numeric_pop, 1L)
pop <- as.numeric(pop_df[[pop_col]])

dist_df <- as.data.frame(readxl::read_excel(dist_path))
if (ncol(dist_df) == nrow(x) + 1L || !is.numeric(dist_df[[1L]])) {
  dist_df <- dist_df[, -1L, drop = FALSE]
}
distmat <- data.matrix(dist_df)
diag(distmat) <- 0


hard_counts <- function(u, k = NCLUSTER) {
  tabulate(apply(as.matrix(u), 1, which.max), nbins = k)
}

safe_runtime_seconds <- function(res) {
  tm <- res$time
  if (is.null(tm)) return(NA_real_)
  if (!is.null(names(tm)) && "elapsed" %in% names(tm)) {
    return(as.numeric(tm[["elapsed"]]))
  }
  if (length(tm) >= 3L) return(as.numeric(tm[3L]))
  NA_real_
}

same_matrix <- function(a, b, tol = REPRO_TOL) {
  isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE))
}

is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
  length(x) == 1L && is.finite(x) && abs(x - round(x)) <= tol
}


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
  randomN = SEED,
  vi.dist = "uniform"
)

optimizer_specs <- list(
  ABC = list(
    fun = "abcfgwc",
    extra = list(
      nfood = POP_SIZE,
      n.onlooker = max(1L, floor(POP_SIZE / 2L)),
      limit = 4,
      pso = FALSE,
      abc.same = SAME_LIMIT
    )
  ),
  FPA = list(
    fun = "fpafgwc",
    extra = list(nflow = POP_SIZE, p = 0.8, flow.same = SAME_LIMIT)
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
    extra = list(nwolf = POP_SIZE, wolf.same = SAME_LIMIT)
  ),
  HHO = list(
    fun = "hhofgwc",
    extra = list(nhh = POP_SIZE, hh.alg = "heidari", hh.same = SAME_LIMIT)
  ),
  IFA = list(
    fun = "ifafgwc",
    extra = list(nfly = POP_SIZE, ffly.no = 2, fa.same = SAME_LIMIT)
  ),
  PSO = list(
    fun = "psofgwc",
    extra = list(npar = POP_SIZE, pso.same = SAME_LIMIT)
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
    extra = list(nwhale = POP_SIZE, woa.same = SAME_LIMIT)
  )
)

run_method <- function(method, seed = SEED) {
  spec <- optimizer_specs[[method]]
  fn <- get(spec$fun, envir = v3, inherits = FALSE)
  args <- c(common_args, spec$extra)
  args$randomN <- seed
  do.call(fn, args)
}


methods <- names(optimizer_specs)
results <- list()
repro_results <- list()
errors <- list()
summary_rows <- list()
support_rows <- list()

for (method in methods) {
  cat("\n[", method, "] Patch v3.1a NFE profiling...\n", sep = "")

  res <- tryCatch(
    run_method(method, SEED),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    errors[[method]] <- conditionMessage(res)
    summary_rows[[method]] <- data.frame(
      method = method,
      status = "FAIL",
      failure_reason = paste0("runtime error: ", conditionMessage(res)),
      nfe = NA_integer_,
      nfe_initialization = NA_integer_,
      nfe_optimization = NA_integer_,
      iterations = NA_integer_,
      init_nfe_per_agent = NA_real_,
      optimization_nfe_per_iteration = NA_real_,
      total_nfe_per_iteration = NA_real_,
      final_XB = NA_real_,
      spatial_J = NA_real_,
      occupied = NA_integer_,
      min_cluster_size = NA_integer_,
      runtime_sec = NA_real_,
      nfe_fields_present = FALSE,
      nfe_integer = FALSE,
      nfe_positive = FALSE,
      nfe_invariant = FALSE,
      nfe_reproducible = FALSE,
      solution_reproducible = FALSE,
      fitness_type_ok = FALSE,
      occupancy_ok = FALSE,
      expected_fixed_opt_nfe = NA_integer_,
      fixed_opt_nfe_match = NA,
      stringsAsFactors = FALSE
    )
    next
  }

  results[[method]] <- res

  repro <- NULL
  if (REPRODUCIBILITY_RUN) {
    repro <- tryCatch(
      run_method(method, SEED),
      error = function(e) e
    )
    if (!inherits(repro, "error")) {
      repro_results[[method]] <- repro
    }
  }

  required_nfe_fields <- c(
    "nfe",
    "nfe_initialization",
    "nfe_optimization"
  )

  nfe_fields_present <- all(required_nfe_fields %in% names(res))

  if (nfe_fields_present) {
    nfe <- as.integer(res$nfe)
    nfe_initialization <- as.integer(res$nfe_initialization)
    nfe_optimization <- as.integer(res$nfe_optimization)
  } else {
    nfe <- NA_integer_
    nfe_initialization <- NA_integer_
    nfe_optimization <- NA_integer_
  }

  nfe_integer <- (
    nfe_fields_present &&
    is_whole_number(res$nfe) &&
    is_whole_number(res$nfe_initialization) &&
    is_whole_number(res$nfe_optimization)
  )

  nfe_positive <- (
    nfe_fields_present &&
    nfe > 0L &&
    nfe_initialization > 0L &&
    nfe_optimization > 0L
  )

  nfe_invariant <- (
    nfe_fields_present &&
    identical(
      nfe,
      as.integer(nfe_initialization + nfe_optimization)
    )
  )

  iterations <- as.integer(res$iteration)

  init_nfe_per_agent <- if (is.finite(nfe_initialization)) {
    nfe_initialization / POP_SIZE
  } else {
    NA_real_
  }

  optimization_nfe_per_iteration <- if (
    is.finite(nfe_optimization) &&
    is.finite(iterations) &&
    iterations > 0L
  ) {
    nfe_optimization / iterations
  } else {
    NA_real_
  }

  total_nfe_per_iteration <- if (
    is.finite(nfe) &&
    is.finite(iterations) &&
    iterations > 0L
  ) {
    nfe / iterations
  } else {
    NA_real_
  }

  # Fixed-cost optimization phases under the current Patch-v3 implementation.
  # ABC and HHO are intentionally NA because their counts are state-dependent.
  expected_fixed_opt_nfe <- switch(
    method,
    FPA = POP_SIZE * iterations,
    GSA = POP_SIZE * iterations,
    GWO = POP_SIZE * iterations,
    IFA = POP_SIZE * iterations,
    PSO = POP_SIZE * iterations,
    TLBO = 2L * POP_SIZE * iterations,
    WOA = POP_SIZE * iterations,
    NA_integer_
  )

  fixed_opt_nfe_match <- if (is.na(expected_fixed_opt_nfe)) {
    NA
  } else {
    identical(
      nfe_optimization,
      as.integer(expected_fixed_opt_nfe)
    )
  }

  counts <- hard_counts(res$membership)
  occupied <- sum(counts > 0L)
  occupancy_ok <- occupied == NCLUSTER
  min_cluster_size <- min(counts)

  fitness_type_ok <- identical(
    res$fitness_type,
    "spatial_XB_feasible"
  )

  nfe_reproducible <- FALSE
  solution_reproducible <- FALSE

  if (
    REPRODUCIBILITY_RUN &&
    !is.null(repro) &&
    !inherits(repro, "error")
  ) {
    repro_has_nfe <- all(required_nfe_fields %in% names(repro))

    if (repro_has_nfe) {
      nfe_reproducible <- (
        identical(as.integer(res$nfe), as.integer(repro$nfe)) &&
        identical(
          as.integer(res$nfe_initialization),
          as.integer(repro$nfe_initialization)
        ) &&
        identical(
          as.integer(res$nfe_optimization),
          as.integer(repro$nfe_optimization)
        )
      )
    }

    solution_reproducible <- (
      isTRUE(all.equal(res$f_obj, repro$f_obj, tolerance = REPRO_TOL)) &&
      same_matrix(res$search_centroid, repro$search_centroid) &&
      same_matrix(res$centroid, repro$centroid) &&
      same_matrix(res$membership, repro$membership)
    )
  }

  core_pass <- all(
    nfe_fields_present,
    nfe_integer,
    nfe_positive,
    nfe_invariant,
    nfe_reproducible,
    solution_reproducible,
    fitness_type_ok,
    occupancy_ok
  )

  if (!is.na(fixed_opt_nfe_match)) {
    core_pass <- core_pass && fixed_opt_nfe_match
  }

  status <- if (core_pass) "PASS" else "FAIL"

  failures <- character()
  add_failure <- function(condition, text) {
    if (!isTRUE(condition)) failures <<- c(failures, text)
  }

  add_failure(nfe_fields_present, "missing NFE fields")
  add_failure(nfe_integer, "NFE is not integer-like")
  add_failure(nfe_positive, "non-positive NFE")
  add_failure(nfe_invariant, "NFE accounting invariant")
  add_failure(nfe_reproducible, "NFE reproducibility")
  add_failure(solution_reproducible, "solution reproducibility")
  add_failure(fitness_type_ok, "fitness_type")
  add_failure(occupancy_ok, "hard-cluster occupancy")

  if (!is.na(fixed_opt_nfe_match)) {
    add_failure(
      fixed_opt_nfe_match,
      "fixed optimization-NFE expectation"
    )
  }

  failure_reason <- if (length(failures)) {
    paste(failures, collapse = "; ")
  } else {
    ""
  }

  summary_rows[[method]] <- data.frame(
    method = method,
    status = status,
    failure_reason = failure_reason,
    nfe = nfe,
    nfe_initialization = nfe_initialization,
    nfe_optimization = nfe_optimization,
    iterations = iterations,
    init_nfe_per_agent = init_nfe_per_agent,
    optimization_nfe_per_iteration = optimization_nfe_per_iteration,
    total_nfe_per_iteration = total_nfe_per_iteration,
    final_XB = as.numeric(res$f_obj),
    spatial_J = as.numeric(res$spatial_obj),
    occupied = occupied,
    min_cluster_size = min_cluster_size,
    runtime_sec = safe_runtime_seconds(res),
    nfe_fields_present = nfe_fields_present,
    nfe_integer = nfe_integer,
    nfe_positive = nfe_positive,
    nfe_invariant = nfe_invariant,
    nfe_reproducible = nfe_reproducible,
    solution_reproducible = solution_reproducible,
    fitness_type_ok = fitness_type_ok,
    occupancy_ok = occupancy_ok,
    expected_fixed_opt_nfe = expected_fixed_opt_nfe,
    fixed_opt_nfe_match = fixed_opt_nfe_match,
    stringsAsFactors = FALSE
  )

  for (k in seq_len(NCLUSTER)) {
    support_rows[[paste(method, k, sep = "_")]] <- data.frame(
      method = method,
      cluster = k,
      hard_count = counts[k],
      hard_proportion = counts[k] / nrow(x),
      soft_mass = sum(res$membership[, k]),
      effective_mass_m2 = sum(res$membership[, k]^M),
      stringsAsFactors = FALSE
    )
  }

  cat(
    sprintf(
      paste0(
        "[%s] %s | NFE=%d (init=%d, opt=%d) | ",
        "iter=%d | opt-NFE/iter=%.2f | XB=%.8f | ",
        "occupied=%d/%d | repro-NFE=%s\n"
      ),
      method,
      status,
      nfe,
      nfe_initialization,
      nfe_optimization,
      iterations,
      optimization_nfe_per_iteration,
      res$f_obj,
      occupied,
      NCLUSTER,
      nfe_reproducible
    )
  )
}


summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL

support_df <- if (length(support_rows)) {
  do.call(rbind, support_rows)
} else {
  data.frame()
}
if (nrow(support_df)) rownames(support_df) <- NULL


valid_nfe <- summary_df$nfe[is.finite(summary_df$nfe)]
valid_opt_nfe <- summary_df$nfe_optimization[
  is.finite(summary_df$nfe_optimization)
]

if (length(valid_nfe)) {
  min_total_nfe <- min(valid_nfe)
  max_total_nfe <- max(valid_nfe)
  total_nfe_ratio <- max_total_nfe / min_total_nfe
} else {
  min_total_nfe <- NA_real_
  max_total_nfe <- NA_real_
  total_nfe_ratio <- NA_real_
}

if (length(valid_opt_nfe)) {
  min_opt_nfe <- min(valid_opt_nfe)
  max_opt_nfe <- max(valid_opt_nfe)
  opt_nfe_ratio <- max_opt_nfe / min_opt_nfe
} else {
  min_opt_nfe <- NA_real_
  max_opt_nfe <- NA_real_
  opt_nfe_ratio <- NA_real_
}

summary_df$total_nfe_ratio_to_min <- if (is.finite(min_total_nfe)) {
  summary_df$nfe / min_total_nfe
} else {
  NA_real_
}

summary_df$optimization_nfe_ratio_to_min <- if (is.finite(min_opt_nfe)) {
  summary_df$nfe_optimization / min_opt_nfe
} else {
  NA_real_
}


n_pass <- sum(summary_df$status == "PASS")
n_fail <- sum(summary_df$status == "FAIL")
overall_status <- if (n_fail > 0L) "FAIL_INVESTIGATE" else "PASS"


out_dir <- file.path(
  root,
  "validation_results",
  "v7r_nfe_profiling_smoke"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

config_df <- data.frame(
  parameter = c(
    "seed",
    "ncluster",
    "m",
    "alpha",
    "a",
    "b",
    "population",
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
  file.path(out_dir, "v7r_nfe_config.csv"),
  row.names = FALSE
)

utils::write.csv(
  summary_df,
  file.path(out_dir, "v7r_nfe_optimizer_profile.csv"),
  row.names = FALSE
)

if (nrow(support_df)) {
  utils::write.csv(
    support_df,
    file.path(out_dir, "v7r_nfe_cluster_support.csv"),
    row.names = FALSE
  )
}

if (length(errors)) {
  error_df <- data.frame(
    method = names(errors),
    error = unlist(errors, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    error_df,
    file.path(out_dir, "v7r_nfe_errors.csv"),
    row.names = FALSE
  )
}

capture.output(
  sessionInfo(),
  file = file.path(out_dir, "v7r_nfe_sessionInfo.txt")
)


cat("\n")
cat("====================================================================\n")
cat("V7-R-NFE — Patch v3.1a NFE Profiling Smoke\n")
cat("====================================================================\n")
cat("Dataset             :", nrow(x), "x", ncol(x), "\n")
cat("Clusters            :", NCLUSTER, "\n")
cat("Fuzzifier           :", M, "\n")
cat("Spatial alpha       :", ALPHA, "\n")
cat("Population          :", POP_SIZE, "\n")
cat("Iterations          :", MAX_ITER, "\n")
cat("Seed                :", SEED, "\n")
cat("Fitness             : spatial_XB_feasible\n")
cat("Reproducibility run :", REPRODUCIBILITY_RUN, "\n")
cat("====================================================================\n")

cat("\n[1] NFE PROFILE\n")
print(
  summary_df[
    ,
    c(
      "method",
      "status",
      "nfe",
      "nfe_initialization",
      "nfe_optimization",
      "iterations",
      "optimization_nfe_per_iteration",
      "total_nfe_ratio_to_min",
      "final_XB",
      "occupied",
      "nfe_reproducible"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n[2] CROSS-OPTIMIZER NFE SPREAD\n")
cat("Minimum total NFE         :", min_total_nfe, "\n")
cat("Maximum total NFE         :", max_total_nfe, "\n")
cat("Max/min total NFE ratio   :", sprintf("%.3f", total_nfe_ratio), "\n")
cat("Minimum optimization NFE  :", min_opt_nfe, "\n")
cat("Maximum optimization NFE  :", max_opt_nfe, "\n")
cat("Max/min opt NFE ratio     :", sprintf("%.3f", opt_nfe_ratio), "\n")

cat("\n[3] TECHNICAL GATE\n")
cat("PASS    :", n_pass, "\n")
cat("FAIL    :", n_fail, "\n")
cat("OVERALL :", overall_status, "\n")

cat("\nInterpretation:\n")
cat(
  "- PASS means Patch v3.1a NFE accounting is internally consistent,\n",
  "  reproducible, and preserves the Patch-v3 solution contract.\n",
  sep = ""
)
cat(
  "- Different NFE totals under the same max.iter empirically demonstrate\n",
  "  why equal-iteration optimizer comparisons are not computationally fair.\n",
  sep = ""
)
cat(
  "- final_XB is regression context only; do NOT rank optimizers from this run.\n"
)
cat(
  "- Use this profile to design Patch v3.1b exact max_nfe enforcement.\n"
)

cat(
  "\nResults written to:\n  ",
  out_dir,
  "\n",
  sep = ""
)

if (!identical(overall_status, "PASS")) {
  stop(
    "V7-R-NFE profiling gate failed. Inspect output CSV before Patch v3.1b.",
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
