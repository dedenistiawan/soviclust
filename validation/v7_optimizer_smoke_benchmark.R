# =============================================================================
# soviclust — V7 Optimizer Smoke Benchmark
# =============================================================================
#
# Purpose
# -------
# Technical/structural smoke validation of the nine FGWC metaheuristic
# optimizers after Algorithm Correctness Patch v1 and Objective Harmonization
# Patch v2.
#
# THIS IS NOT THE 30-RUN PERFORMANCE BENCHMARK.
# Do not interpret the lowest f_obj in this script as proof that an optimizer
# is superior. Different metaheuristics can perform different numbers of
# objective evaluations within one nominal iteration.
#
# Run from the ROOT of the soviclust source project:
#
#   source("validation/v7_optimizer_smoke_benchmark.R")
#
# Outputs are written to:
#
#   validation_results/v7_optimizer_smoke/
#
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

SMOKE_SEED       <- 2026L
NCLUSTER         <- 4L
FUZZIFIER_M      <- 2
SPATIAL_ALPHA    <- 0.7
SPATIAL_A        <- 1
SPATIAL_B        <- 1
DISTANCE_METRIC  <- "euclidean"
MINKOWSKI_ORDER  <- 2

POP_SIZE         <- 10L
MAX_ITER          <- 10L

# Setting error = 0 prevents tolerance-based stagnation from triggering because
# the implementations use abs(delta) < error.
ERROR_TOL         <- 0

# Larger than MAX_ITER; used by optimizer-specific stagnation controls.
SAME_LIMIT        <- MAX_ITER + 100L

# Run every method a second time with the same seed to verify full-data
# reproducibility. Recommended for V7.
RUN_REPRO_CHECK   <- TRUE

# Structural tolerances.
ROW_SUM_TOL       <- 1e-8
CONV_TOL          <- 1e-8
REPRO_TOL         <- 1e-10
NEAR_DUPLICATE_TOL <- 1e-8

# Diagnostic warning only. This is NOT a universal clustering threshold.
# It simply flags unusually close centroids on standardized input data.
COLLAPSE_WARN_TOL <- 1e-3


# -----------------------------------------------------------------------------
# 1. Locate project and load current algorithm source
# -----------------------------------------------------------------------------

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      file.exists(file.path(p, "DESCRIPTION")) &&
      dir.exists(file.path(p, "inst", "app", "R", "shared", "function"))
    ) {
      return(p)
    }

    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }

  stop(
    "Unable to locate the soviclust project root. ",
    "Run this script from inside the soviclust source project.",
    call. = FALSE
  )
}


project_root <- find_project_root()

helper_path <- file.path(
  project_root,
  "tests", "testthat", "helper-fgwc-algorithms.R"
)

if (!file.exists(helper_path)) {
  stop(
    "Required test harness not found: ",
    helper_path,
    call. = FALSE
  )
}

# This helper loads the CURRENT source tree and creates isolated optimizer
# environments so generic legacy helper names do not overwrite one another.
source(helper_path, local = .GlobalEnv)

required_methods <- c(
  "ABC", "FPA", "GSA", "GWO", "HHO",
  "IFA", "PSO", "TLBO", "WOA"
)

if (!exists("optimizer_envs", inherits = FALSE)) {
  stop("`optimizer_envs` was not created by the test harness.", call. = FALSE)
}

missing_envs <- setdiff(required_methods, names(optimizer_envs))
if (length(missing_envs) > 0L) {
  stop(
    "Missing optimizer environment(s): ",
    paste(missing_envs, collapse = ", "),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 2. Load bundled 514-district data directly from the source project
# -----------------------------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required.", call. = FALSE)
}

data_path <- file.path(
  project_root, "inst", "extdata", "sovi_data_kab_514_15.xlsx"
)
pop_path <- file.path(
  project_root, "inst", "extdata", "sovi_data_pop_514.xlsx"
)
dist_path <- file.path(
  project_root, "inst", "extdata", "Distance_matrix_514.xlsx"
)

for (p in c(data_path, pop_path, dist_path)) {
  if (!file.exists(p)) {
    stop("Bundled validation input not found: ", p, call. = FALSE)
  }
}


raw_data <- as.data.frame(readxl::read_excel(data_path))

if (nrow(raw_data) != 514L) {
  stop(
    "Expected 514 rows in bundled SoVI data; found ",
    nrow(raw_data), ".",
    call. = FALSE
  )
}

if (ncol(raw_data) < 15L) {
  stop("Bundled SoVI data has fewer than 15 columns.", call. = FALSE)
}

# The bundled file is explicitly the 15-indicator dataset. Identifier/name
# fields precede the indicator block, so the final 15 columns are used.
indicator_cols <- tail(names(raw_data), 15L)

indicator_df <- raw_data[, indicator_cols, drop = FALSE]

non_numeric <- !vapply(indicator_df, is.numeric, logical(1))

if (any(non_numeric)) {
  stop(
    "The following expected indicator column(s) are not numeric: ",
    paste(names(indicator_df)[non_numeric], collapse = ", "),
    call. = FALSE
  )
}

x_raw <- data.matrix(indicator_df)

if (any(!is.finite(x_raw))) {
  stop("Indicator matrix contains NA/NaN/Inf values.", call. = FALSE)
}

indicator_sd <- apply(x_raw, 2, stats::sd)

if (any(!is.finite(indicator_sd)) || any(indicator_sd <= 0)) {
  bad <- names(indicator_sd)[!is.finite(indicator_sd) | indicator_sd <= 0]
  stop(
    "Constant/invalid indicator(s): ",
    paste(bad, collapse = ", "),
    call. = FALSE
  )
}

# Standardization is used so all 15 indicators contribute on a common scale.
x <- scale(x_raw)
x <- unclass(x)
storage.mode(x) <- "double"


# Population ---------------------------------------------------------------

pop_df <- as.data.frame(readxl::read_excel(pop_path))

if (nrow(pop_df) != nrow(x)) {
  stop(
    "Population rows (", nrow(pop_df),
    ") differ from indicator rows (", nrow(x), ").",
    call. = FALSE
  )
}

numeric_pop <- which(vapply(pop_df, is.numeric, logical(1)))

if (length(numeric_pop) == 0L) {
  stop("No numeric population column found.", call. = FALSE)
}

name_hits <- grep(
  "pop|population|penduduk",
  names(pop_df),
  ignore.case = TRUE
)
name_hits <- intersect(name_hits, numeric_pop)

if (length(name_hits) >= 1L) {
  pop_col <- name_hits[1L]
} else {
  # Bundled population data normally has population as the final numeric field.
  pop_col <- tail(numeric_pop, 1L)
  warning(
    "No population-like column name detected. Using final numeric column: ",
    names(pop_df)[pop_col],
    call. = FALSE
  )
}

pop <- as.numeric(pop_df[[pop_col]])

if (
  length(pop) != nrow(x) ||
  any(!is.finite(pop)) ||
  any(pop <= 0)
) {
  stop("Population vector is invalid.", call. = FALSE)
}


# Distance matrix ----------------------------------------------------------

dist_df <- as.data.frame(readxl::read_excel(dist_path))

# Handle an optional row-label column.
if (ncol(dist_df) == nrow(x) + 1L) {
  dist_df <- dist_df[, -1L, drop = FALSE]
} else if (
  ncol(dist_df) > 0L &&
  !is.numeric(dist_df[[1L]])
) {
  dist_df <- dist_df[, -1L, drop = FALSE]
}

distmat <- data.matrix(dist_df)

if (!identical(dim(distmat), c(nrow(x), nrow(x)))) {
  stop(
    "Distance matrix must be ",
    nrow(x), " x ", nrow(x),
    "; found ",
    paste(dim(distmat), collapse = " x "),
    ".",
    call. = FALSE
  )
}

if (any(!is.finite(distmat)) || any(distmat < 0)) {
  stop("Distance matrix contains invalid values.", call. = FALSE)
}

diag(distmat) <- 0


# -----------------------------------------------------------------------------
# 3. Benchmark call definitions
# -----------------------------------------------------------------------------

common_args <- list(
  data = x,
  pop = pop,
  distmat = distmat,
  ncluster = NCLUSTER,
  m = FUZZIFIER_M,
  distance = DISTANCE_METRIC,
  order = MINKOWSKI_ORDER,
  alpha = SPATIAL_ALPHA,
  a = SPATIAL_A,
  b = SPATIAL_B,
  error = ERROR_TOL,
  max.iter = MAX_ITER,
  randomN = SMOKE_SEED,
  vi.dist = "uniform"
)


method_specs <- list(
  ABC = list(
    fn = optimizer_envs$ABC$abcfgwc,
    extra = list(
      nfood = POP_SIZE,
      n.onlooker = max(2L, floor(POP_SIZE / 2L)),
      limit = 4L,
      pso = FALSE,
      abc.same = SAME_LIMIT
    )
  ),

  FPA = list(
    fn = optimizer_envs$FPA$fpafgwc,
    extra = list(
      nflow = POP_SIZE,
      p = 0.8,
      flow.same = SAME_LIMIT
    )
  ),

  GSA = list(
    fn = optimizer_envs$GSA$gsafgwc,
    extra = list(
      npar = POP_SIZE,
      par.no = 2L,
      gsa.same = SAME_LIMIT
    )
  ),

  GWO = list(
    fn = optimizer_envs$GWO$gwofgwc,
    extra = list(
      nwolf = POP_SIZE,
      wolf.same = SAME_LIMIT
    )
  ),

  HHO = list(
    fn = optimizer_envs$HHO$hhofgwc,
    extra = list(
      nhh = POP_SIZE,
      hh.alg = "heidari",
      hh.same = SAME_LIMIT
    )
  ),

  IFA = list(
    fn = optimizer_envs$IFA$ifafgwc,
    extra = list(
      nfly = POP_SIZE,
      ffly.no = 2L,
      fa.same = SAME_LIMIT
    )
  ),

  PSO = list(
    fn = optimizer_envs$PSO$psofgwc,
    extra = list(
      npar = POP_SIZE,
      pso.same = SAME_LIMIT
    )
  ),

  TLBO = list(
    fn = optimizer_envs$TLBO$tlbofgwc,
    extra = list(
      nstud = POP_SIZE,
      nselection = POP_SIZE,
      tlbo.same = SAME_LIMIT
    )
  ),

  WOA = list(
    fn = optimizer_envs$WOA$woafgwc,
    extra = list(
      nwhale = POP_SIZE,
      woa.same = SAME_LIMIT
    )
  )
)


# -----------------------------------------------------------------------------
# 4. Validation utilities
# -----------------------------------------------------------------------------

num_equal <- function(x, y, tol = 1e-8) {
  isTRUE(
    length(x) == length(y) &&
      all(is.finite(c(x, y))) &&
      max(abs(x - y)) <= tol
  )
}


max_abs_diff <- function(x, y) {
  if (is.null(x) || is.null(y)) return(Inf)

  x <- as.numeric(x)
  y <- as.numeric(y)

  if (length(x) != length(y)) return(Inf)
  if (any(!is.finite(x)) || any(!is.finite(y))) return(Inf)

  max(abs(x - y))
}


min_centroid_separation <- function(centers) {
  centers <- as.matrix(centers)

  if (nrow(centers) < 2L) return(NA_real_)

  d <- as.matrix(stats::dist(centers))
  diag(d) <- Inf

  min(d)
}


extract_validation <- function(res) {
  wanted <- c("PC", "CE", "SC", "SI", "XB", "IFV", "Kwon")
  vals <- unlist(res$validation, use.names = TRUE)

  out <- setNames(rep(NA_real_, length(wanted)), wanted)

  for (nm in wanted) {
    if (nm %in% names(vals)) {
      out[nm] <- as.numeric(vals[[nm]])
    }
  }

  out
}


validate_result <- function(method, res, elapsed_sec, repro = NULL) {
  membership <- as.matrix(res$membership)
  centroid <- as.matrix(res$centroid)
  conv <- as.numeric(res$converg)
  cluster <- as.integer(res$cluster)

  cvi <- extract_validation(res)

  membership_dim_ok <- identical(
    dim(membership),
    c(nrow(x), NCLUSTER)
  )

  centroid_dim_ok <- identical(
    dim(centroid),
    c(NCLUSTER, ncol(x))
  )

  row_sum_error <- if (membership_dim_ok) {
    max(abs(rowSums(membership) - 1))
  } else {
    Inf
  }

  membership_finite <- membership_dim_ok &&
    all(is.finite(membership)) &&
    all(membership >= -ROW_SUM_TOL) &&
    all(membership <= 1 + ROW_SUM_TOL)

  centroid_finite <- centroid_dim_ok && all(is.finite(centroid))

  convergence_finite <- length(conv) >= 2L && all(is.finite(conv))
  convergence_nonincreasing <- convergence_finite &&
    all(diff(conv) <= CONV_TOL)

  tail_matches_fobj <- convergence_finite &&
    is.finite(res$f_obj) &&
    abs(tail(conv, 1L) - res$f_obj) <= CONV_TOL

  iteration_ok <- !is.null(res$iteration) &&
    is.finite(res$iteration) &&
    as.integer(res$iteration) == MAX_ITER

  occupied <- length(unique(cluster[is.finite(cluster)]))

  sizes <- tabulate(
    cluster,
    nbins = NCLUSTER
  )

  min_size <- if (length(sizes)) min(sizes) else 0L
  max_size <- if (length(sizes)) max(sizes) else 0L

  min_sep <- if (centroid_finite) {
    min_centroid_separation(centroid)
  } else {
    NA_real_
  }

  cvi_finite <- all(is.finite(cvi))

  fitness_type_ok <- identical(res$fitness_type, "jfgwcv")

  f_obj_finite <- length(res$f_obj) == 1L && is.finite(res$f_obj)
  spatial_obj_finite <- length(res$spatial_obj) == 1L &&
    is.finite(res$spatial_obj)

  class_ok <- inherits(res, "fgwc")

  near_duplicate <- !is.finite(min_sep) ||
    min_sep <= NEAR_DUPLICATE_TOL

  collapse_warning <- is.finite(min_sep) &&
    min_sep < COLLAPSE_WARN_TOL

  cluster_complete <- occupied == NCLUSTER && min_size > 0L

  reproducible <- if (is.null(repro)) {
    NA
  } else {
    isTRUE(repro$reproducible)
  }

  technical_pass <- all(c(
    class_ok,
    fitness_type_ok,
    f_obj_finite,
    spatial_obj_finite,
    membership_dim_ok,
    membership_finite,
    row_sum_error <= ROW_SUM_TOL,
    centroid_finite,
    convergence_nonincreasing,
    tail_matches_fobj,
    iteration_ok,
    cvi_finite,
    cluster_complete,
    !near_duplicate
  ))

  if (RUN_REPRO_CHECK) {
    technical_pass <- technical_pass && isTRUE(reproducible)
  }

  health <- if (!technical_pass) {
    "FAIL"
  } else if (collapse_warning || min_size < 5L) {
    "PASS_WITH_WARNING"
  } else {
    "PASS"
  }

  data.frame(
    method = method,
    health = health,
    class_ok = class_ok,
    fitness_type = as.character(res$fitness_type),
    f_obj = as.numeric(res$f_obj),
    spatial_obj = as.numeric(res$spatial_obj),
    spatial_minus_fitness = as.numeric(res$spatial_obj - res$f_obj),
    iteration = as.integer(res$iteration),
    elapsed_sec = elapsed_sec,
    PC = cvi["PC"],
    CE = cvi["CE"],
    SC = cvi["SC"],
    SI = cvi["SI"],
    XB = cvi["XB"],
    IFV = cvi["IFV"],
    Kwon = cvi["Kwon"],
    occupied_clusters = occupied,
    min_cluster_size = min_size,
    max_cluster_size = max_size,
    min_centroid_separation = min_sep,
    collapse_warning = collapse_warning,
    max_membership_rowsum_error = row_sum_error,
    convergence_nonincreasing = convergence_nonincreasing,
    convergence_tail_matches_fobj = tail_matches_fobj,
    all_cvi_finite = cvi_finite,
    reproducible = reproducible,
    repro_f_obj_diff = if (is.null(repro)) NA_real_ else repro$f_obj_diff,
    repro_centroid_maxdiff = if (is.null(repro)) NA_real_ else repro$centroid_diff,
    repro_membership_maxdiff = if (is.null(repro)) NA_real_ else repro$membership_diff,
    technical_pass = technical_pass,
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# 5. Run V7
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("soviclust — V7 Optimizer Smoke Benchmark\n")
cat("============================================================\n")
cat("Dataset       :", nrow(x), "districts x", ncol(x), "standardized indicators\n")
cat("Indicators    :", paste(indicator_cols, collapse = ", "), "\n")
cat("Population col:", names(pop_df)[pop_col], "\n")
cat("Clusters      :", NCLUSTER, "\n")
cat("Fuzzifier m   :", FUZZIFIER_M, "\n")
cat("Spatial alpha :", SPATIAL_ALPHA, "\n")
cat("Population N  :", POP_SIZE, "search agents\n")
cat("Max iterations:", MAX_ITER, "\n")
cat("Seed          :", SMOKE_SEED, "\n")
cat("Repro check   :", RUN_REPRO_CHECK, "\n")
cat("Fitness       : jfgwcv via optimizer_fitness()\n")
cat("============================================================\n\n")


results <- list()
summary_rows <- list()
cluster_rows <- list()
errors <- list()


for (method in required_methods) {
  cat(sprintf("[%s] running...\n", method))

  spec <- method_specs[[method]]
  args <- c(common_args, spec$extra)

  timing <- system.time({
    first <- tryCatch(
      do.call(spec$fn, args),
      error = function(e) e
    )
  })

  elapsed <- unname(timing[["elapsed"]])

  if (inherits(first, "error")) {
    msg <- conditionMessage(first)

    cat(sprintf("[%s] ERROR: %s\n\n", method, msg))

    errors[[method]] <- msg

    summary_rows[[method]] <- data.frame(
      method = method,
      health = "ERROR",
      class_ok = FALSE,
      fitness_type = NA_character_,
      f_obj = NA_real_,
      spatial_obj = NA_real_,
      spatial_minus_fitness = NA_real_,
      iteration = NA_integer_,
      elapsed_sec = elapsed,
      PC = NA_real_,
      CE = NA_real_,
      SC = NA_real_,
      SI = NA_real_,
      XB = NA_real_,
      IFV = NA_real_,
      Kwon = NA_real_,
      occupied_clusters = NA_integer_,
      min_cluster_size = NA_integer_,
      max_cluster_size = NA_integer_,
      min_centroid_separation = NA_real_,
      collapse_warning = NA,
      max_membership_rowsum_error = NA_real_,
      convergence_nonincreasing = FALSE,
      convergence_tail_matches_fobj = FALSE,
      all_cvi_finite = FALSE,
      reproducible = FALSE,
      repro_f_obj_diff = NA_real_,
      repro_centroid_maxdiff = NA_real_,
      repro_membership_maxdiff = NA_real_,
      technical_pass = FALSE,
      stringsAsFactors = FALSE
    )

    next
  }

  repro <- NULL

  if (RUN_REPRO_CHECK) {
    second <- tryCatch(
      do.call(spec$fn, args),
      error = function(e) e
    )

    if (inherits(second, "error")) {
      errors[[paste0(method, "_repro")]] <- conditionMessage(second)

      repro <- list(
        reproducible = FALSE,
        f_obj_diff = Inf,
        centroid_diff = Inf,
        membership_diff = Inf
      )
    } else {
      f_diff <- abs(first$f_obj - second$f_obj)
      c_diff <- max_abs_diff(first$centroid, second$centroid)
      u_diff <- max_abs_diff(first$membership, second$membership)

      repro <- list(
        reproducible = all(c(
          is.finite(f_diff),
          is.finite(c_diff),
          is.finite(u_diff),
          f_diff <= REPRO_TOL,
          c_diff <= REPRO_TOL,
          u_diff <= REPRO_TOL
        )),
        f_obj_diff = f_diff,
        centroid_diff = c_diff,
        membership_diff = u_diff
      )
    }
  }

  results[[method]] <- first

  row <- validate_result(
    method = method,
    res = first,
    elapsed_sec = elapsed,
    repro = repro
  )

  summary_rows[[method]] <- row

  sizes <- tabulate(
    as.integer(first$cluster),
    nbins = NCLUSTER
  )

  cluster_rows[[method]] <- data.frame(
    method = method,
    cluster = seq_len(NCLUSTER),
    n = sizes,
    stringsAsFactors = FALSE
  )

  cat(
    sprintf(
      "[%s] %s | f_obj=%.8f | occupied=%d/%d | min_sep=%.6g | iter=%d | %.2fs\n\n",
      method,
      row$health,
      row$f_obj,
      row$occupied_clusters,
      NCLUSTER,
      row$min_centroid_separation,
      row$iteration,
      elapsed
    )
  )
}


summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL

cluster_df <- if (length(cluster_rows) > 0L) {
  do.call(rbind, cluster_rows)
} else {
  data.frame()
}


# -----------------------------------------------------------------------------
# 6. Save reproducibility evidence
# -----------------------------------------------------------------------------

output_dir <- file.path(
  project_root,
  "validation_results",
  "v7_optimizer_smoke"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  summary_df,
  file.path(output_dir, "v7_smoke_summary.csv"),
  row.names = FALSE
)

if (nrow(cluster_df) > 0L) {
  utils::write.csv(
    cluster_df,
    file.path(output_dir, "v7_cluster_sizes.csv"),
    row.names = FALSE
  )
}

config_df <- data.frame(
  parameter = c(
    "seed",
    "ncluster",
    "m",
    "alpha",
    "a",
    "b",
    "distance",
    "order",
    "population_size",
    "max_iter",
    "error_tol",
    "same_limit",
    "repro_check"
  ),
  value = c(
    SMOKE_SEED,
    NCLUSTER,
    FUZZIFIER_M,
    SPATIAL_ALPHA,
    SPATIAL_A,
    SPATIAL_B,
    DISTANCE_METRIC,
    MINKOWSKI_ORDER,
    POP_SIZE,
    MAX_ITER,
    ERROR_TOL,
    SAME_LIMIT,
    RUN_REPRO_CHECK
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  config_df,
  file.path(output_dir, "v7_config.csv"),
  row.names = FALSE
)

indicator_out <- data.frame(
  position = seq_along(indicator_cols),
  indicator = indicator_cols,
  original_mean = colMeans(x_raw),
  original_sd = apply(x_raw, 2, stats::sd),
  stringsAsFactors = FALSE
)

utils::write.csv(
  indicator_out,
  file.path(output_dir, "v7_indicators.csv"),
  row.names = FALSE
)

# Save full objects for post-hoc inspection without rerunning algorithms.
saveRDS(
  results,
  file.path(output_dir, "v7_smoke_results.rds")
)

if (length(errors) > 0L) {
  error_df <- data.frame(
    method = names(errors),
    message = unlist(errors, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    error_df,
    file.path(output_dir, "v7_errors.csv"),
    row.names = FALSE
  )
}

session_lines <- capture.output(sessionInfo())
writeLines(
  session_lines,
  file.path(output_dir, "v7_sessionInfo.txt")
)


# -----------------------------------------------------------------------------
# 7. Console report
# -----------------------------------------------------------------------------

display_cols <- c(
  "method",
  "health",
  "f_obj",
  "spatial_obj",
  "iteration",
  "occupied_clusters",
  "min_cluster_size",
  "min_centroid_separation",
  "collapse_warning",
  "reproducible",
  "technical_pass"
)

cat("\n")
cat("============================================================\n")
cat("V7 SUMMARY\n")
cat("============================================================\n")
print(summary_df[, display_cols, drop = FALSE], row.names = FALSE)

n_pass <- sum(summary_df$technical_pass %in% TRUE)
n_total <- nrow(summary_df)

cat("\nTechnical pass:", n_pass, "/", n_total, "\n")

if (all(summary_df$technical_pass %in% TRUE)) {
  cat("\nV7 TECHNICAL SMOKE RESULT: PASS\n")
  cat(
    "All nine optimizers returned structurally valid, finite, reproducible ",
    "FGWC solutions under the common jfgwcv fitness.\n",
    sep = ""
  )

  if (any(summary_df$collapse_warning %in% TRUE)) {
    cat(
      "NOTE: At least one optimizer triggered the centroid-separation ",
      "diagnostic warning. Inspect v7_smoke_summary.csv before V8.\n",
      sep = ""
    )
  }
} else {
  cat("\nV7 TECHNICAL SMOKE RESULT: FAIL / INVESTIGATE\n")
  cat(
    "Do not proceed to the 30-run benchmark. Inspect methods with ",
    "technical_pass = FALSE.\n",
    sep = ""
  )
}

cat("\nResults written to:\n")
cat("  ", output_dir, "\n", sep = "")

cat("\nIMPORTANT:\n")
cat(
  "Do NOT rank optimizer performance from this smoke run. ",
  "Equal population and iteration counts do not guarantee equal objective-",
  "function evaluation budgets across different metaheuristics.\n",
  sep = ""
)

cat("\nNext stage after a clean V7:\n")
cat(
  "  V8 — Experimental Fairness Protocol: common evaluation budget, ",
  "common seed schedule, parameter policy, and 30-run design.\n",
  sep = ""
)
