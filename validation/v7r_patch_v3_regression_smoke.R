# =============================================================================
# soviclust — V7-R Patch v3 Regression Smoke Benchmark
# =============================================================================
#
# Purpose
# -------
# End-to-end smoke validation of Patch v3 on the bundled Indonesia dataset:
# 514 districts/cities x 15 standardized indicators.
#
# This is NOT a performance-ranking experiment and is NOT NFE-fair.
# It is a technical regression gate before V8 / formal 30-run benchmarking.
#
# Checks for all 9 optimizers:
#   - Patch-v3 engine really active
#   - class == "fgwc"
#   - fitness_type == "spatial_XB_feasible"
#   - finite objective / spatial diagnostic
#   - normalized spatial membership
#   - all requested hard clusters occupied
#   - returned f_obj equals XB(U*, V*)
#   - returned spatial_obj equals FGWC J(U*, V*)
#   - best-so-far convergence non-increasing
#   - tail(converg) == f_obj
#   - raw search centroid retained separately
#   - no exact/near centroid collapse
#   - final fitness is no worse than the shared initial feasible best
#   - deterministic reproducibility under identical seed
#
# References also reported:
#   - theoretical collapsed solution (should be infeasible / Inf XB)
#   - K-means centroid reference evaluated through the same Patch-v3 evaluator
#
# Run:
#   source("validation/v7r_patch_v3_regression_smoke.R")
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
MAX_ITER <- 10L

# Disable early stopping for this smoke benchmark.
SAME_LIMIT <- MAX_ITER + 100L
ERROR_TOL <- 0

REPRODUCIBILITY_RUN <- TRUE

TOL <- 1e-8
REPRO_TOL <- 1e-10

# Diagnostic only. Near-collapse triggers PASS_WITH_WARNING, not automatic FAIL.
NEAR_COLLAPSE_SEP <- 1e-3


# -----------------------------------------------------------------------------
# 1. Locate project and load isolated Patch-v3 engine
# -----------------------------------------------------------------------------

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    required <- c(
      file.path(p, "DESCRIPTION"),
      file.path(
        p, "inst", "app", "R", "shared", "function", "optimizer_v3.R"
      ),
      file.path(
        p, "tests", "testthat", "helper-v3-engine.R"
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
    "Unable to locate soviclust source root with Patch v3 installed.",
    call. = FALSE
  )
}


root <- find_project_root()

helper <- file.path(
  root,
  "tests",
  "testthat",
  "helper-v3-engine.R"
)

helper_env <- new.env(parent = globalenv())
sys.source(helper, envir = helper_env)

v3 <- helper_env$v3_env

if (!is.environment(v3)) {
  stop("Patch-v3 test engine was not created.", call. = FALSE)
}

if (
  !exists(
    "evaluate_optimizer_candidate_v3",
    envir = v3,
    inherits = FALSE,
    mode = "function"
  )
) {
  stop("Patch-v3 evaluator is not loaded.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 2. Load bundled 514 x 15 validation data
# -----------------------------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required.", call. = FALSE)
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
  readxl::read_excel(data_path)
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

x <- unclass(scale(x_raw))
storage.mode(x) <- "double"


pop_df <- as.data.frame(
  readxl::read_excel(pop_path)
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
  tail(numeric_pop, 1L)
}

pop <- as.numeric(
  pop_df[[pop_col]]
)


dist_df <- as.data.frame(
  readxl::read_excel(dist_path)
)

if (
  ncol(dist_df) == nrow(x) + 1L ||
  !is.numeric(dist_df[[1L]])
) {
  dist_df <- dist_df[, -1L, drop = FALSE]
}

distmat <- data.matrix(dist_df)
diag(distmat) <- 0


# -----------------------------------------------------------------------------
# 3. Shared context / baseline references
# -----------------------------------------------------------------------------

ctx <- v3$.soviclust_v3_validate_common(
  data = x,
  pop = pop,
  distmat = distmat,
  ncluster = NCLUSTER,
  m = M,
  alpha = ALPHA
)


# Shared initial feasible population.
initial_state <- v3$.soviclust_v3_init_population(
  ctx = ctx,
  n_agents = POP_SIZE,
  ncluster = NCLUSTER,
  vi.dist = "uniform",
  randomN = SEED,
  m = M,
  distance = DISTANCE,
  order = ORDER,
  alpha = ALPHA,
  a = A_SPATIAL,
  b = B_SPATIAL
)

initial_best_index <- which.min(
  initial_state$fitness
)

initial_best_xb <- min(
  initial_state$fitness
)


# Theoretical collapsed centroid reference.
collapsed_centers <- matrix(
  rep(
    colMeans(x),
    each = NCLUSTER
  ),
  nrow = NCLUSTER,
  byrow = FALSE
)

collapsed_eval <- v3$evaluate_optimizer_candidate_v3(
  data = x,
  search_centers = collapsed_centers,
  mi.mj = ctx$mi.mj,
  distmat = ctx$distmat,
  m = M,
  distance = DISTANCE,
  order = ORDER,
  alpha = ALPHA,
  beta = ctx$beta,
  a = A_SPATIAL,
  b = B_SPATIAL,
  require_all_clusters = TRUE
)


# K-means reference, evaluated by the SAME spatial evaluator.
set.seed(SEED)

km <- stats::kmeans(
  x,
  centers = NCLUSTER,
  nstart = 25,
  iter.max = 100
)

kmeans_eval <- v3$evaluate_optimizer_candidate_v3(
  data = x,
  search_centers = km$centers,
  mi.mj = ctx$mi.mj,
  distmat = ctx$distmat,
  m = M,
  distance = DISTANCE,
  order = ORDER,
  alpha = ALPHA,
  beta = ctx$beta,
  a = A_SPATIAL,
  b = B_SPATIAL,
  require_all_clusters = TRUE
)


reference_df <- data.frame(
  reference = c(
    "Shared initial feasible best",
    "Collapsed centroid reference",
    "KMeans centroid reference"
  ),
  spatial_XB = c(
    initial_best_xb,
    collapsed_eval$fitness,
    kmeans_eval$fitness
  ),
  feasible = c(
    TRUE,
    collapsed_eval$feasible,
    kmeans_eval$feasible
  ),
  occupied = c(
    initial_state$evals[[initial_best_index]]$occupied_clusters,
    collapsed_eval$occupied_clusters,
    kmeans_eval$occupied_clusters
  ),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# 4. Utility functions
# -----------------------------------------------------------------------------

min_pairwise_separation <- function(centers) {
  centers <- as.matrix(centers)

  if (nrow(centers) < 2L) {
    return(Inf)
  }

  d <- as.matrix(
    stats::dist(centers)
  )

  vals <- d[
    upper.tri(d)
  ]

  min(vals)
}


hard_counts <- function(u, k = NCLUSTER) {
  tabulate(
    apply(
      as.matrix(u),
      1,
      which.max
    ),
    nbins = k
  )
}


same_matrix <- function(a, b, tol = REPRO_TOL) {
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
    return(as.numeric(tm[["elapsed"]]))
  }

  if (length(tm) >= 3L) {
    return(as.numeric(tm[3L]))
  }

  NA_real_
}


# -----------------------------------------------------------------------------
# 5. Optimizer runners
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


run_method <- function(method, seed = SEED) {
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
# 6. Run benchmark
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
    "] Patch-v3 smoke run...\n",
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
    errors[[method]] <- conditionMessage(res)

    summary_rows[[method]] <- data.frame(
      method = method,
      status = "FAIL",
      failure_reason = paste0(
        "runtime error: ",
        conditionMessage(res)
      ),
      final_XB = NA_real_,
      spatial_J = NA_real_,
      initial_best_XB = initial_best_xb,
      improvement_from_initial = NA_real_,
      improved_or_equal_initial = FALSE,
      kmeans_spatial_XB = kmeans_eval$fitness,
      ratio_to_kmeans = NA_real_,
      occupied = NA_integer_,
      min_cluster_size = NA_integer_,
      spatial_min_sep = NA_real_,
      search_min_sep = NA_real_,
      mean_max_membership = NA_real_,
      convergence_monotone = FALSE,
      convergence_tail_match = FALSE,
      objective_match = FALSE,
      spatial_J_match = FALSE,
      membership_normalized = FALSE,
      reproducible = FALSE,
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


  # ---------------------------------------------------------------------------
  # Technical checks
  # ---------------------------------------------------------------------------

  class_ok <- inherits(
    res,
    "fgwc"
  )

  fitness_type_ok <- identical(
    res$fitness_type,
    "spatial_XB_feasible"
  )

  finite_ok <- (
    is.finite(res$f_obj) &&
    is.finite(res$spatial_obj) &&
    all(is.finite(res$membership)) &&
    all(is.finite(res$centroid)) &&
    all(is.finite(res$search_centroid))
  )

  row_sum_error <- max(
    abs(
      rowSums(
        res$membership
      ) - 1
    )
  )

  membership_normalized <- (
    is.finite(row_sum_error) &&
    row_sum_error <= TOL
  )

  counts <- hard_counts(
    res$membership
  )

  occupied <- sum(
    counts > 0L
  )

  occupancy_ok <- (
    occupied == NCLUSTER
  )

  min_cluster_size <- min(
    counts
  )


  expected_xb <- v3$XB1(
    data = x,
    uij = res$membership,
    vi = res$centroid,
    m = M
  )

  objective_match <- isTRUE(
    all.equal(
      as.numeric(res$f_obj),
      as.numeric(expected_xb),
      tolerance = TOL
    )
  )


  expected_j <- v3$fgwc_objective(
    data = x,
    uij = res$membership,
    centers = res$centroid,
    m = M,
    distance = DISTANCE,
    order = ORDER
  )

  spatial_j_match <- isTRUE(
    all.equal(
      as.numeric(res$spatial_obj),
      as.numeric(expected_j),
      tolerance = TOL
    )
  )


  convergence_monotone <- (
    length(res$converg) >= 1L &&
    all(
      diff(
        res$converg
      ) <= TOL
    )
  )

  convergence_tail_match <- isTRUE(
    all.equal(
      as.numeric(
        tail(
          res$converg,
          1L
        )
      ),
      as.numeric(
        res$f_obj
      ),
      tolerance = TOL
    )
  )


  initial_ok <- (
    is.finite(res$f_obj) &&
    res$f_obj <= initial_best_xb + TOL
  )

  improvement <- (
    initial_best_xb -
    res$f_obj
  )


  spatial_min_sep <- min_pairwise_separation(
    res$centroid
  )

  search_min_sep <- min_pairwise_separation(
    res$search_centroid
  )

  exact_collapse <- (
    !is.finite(spatial_min_sep) ||
    spatial_min_sep <= .Machine$double.eps^0.5
  )

  near_collapse <- (
    is.finite(spatial_min_sep) &&
    spatial_min_sep < NEAR_COLLAPSE_SEP
  )


  mean_max_membership <- mean(
    apply(
      res$membership,
      1,
      max
    )
  )


  reproducible <- FALSE

  if (
    REPRODUCIBILITY_RUN &&
    !is.null(repro) &&
    !inherits(repro, "error")
  ) {
    reproducible <- (
      isTRUE(
        all.equal(
          res$f_obj,
          repro$f_obj,
          tolerance = REPRO_TOL
        )
      ) &&
      same_matrix(
        res$search_centroid,
        repro$search_centroid
      ) &&
      same_matrix(
        res$centroid,
        repro$centroid
      ) &&
      same_matrix(
        res$membership,
        repro$membership
      )
    )
  }


  core_pass <- all(
    class_ok,
    fitness_type_ok,
    finite_ok,
    membership_normalized,
    occupancy_ok,
    objective_match,
    spatial_j_match,
    convergence_monotone,
    convergence_tail_match,
    initial_ok,
    !exact_collapse,
    reproducible
  )


  status <- if (!core_pass) {
    "FAIL"
  } else if (near_collapse) {
    "PASS_WITH_WARNING"
  } else {
    "PASS"
  }


  failures <- character()

  add_failure <- function(condition, text) {
    if (!condition) {
      failures <<- c(
        failures,
        text
      )
    }
  }

  add_failure(
    class_ok,
    "class"
  )

  add_failure(
    fitness_type_ok,
    "fitness_type"
  )

  add_failure(
    finite_ok,
    "non-finite output"
  )

  add_failure(
    membership_normalized,
    "membership normalization"
  )

  add_failure(
    occupancy_ok,
    "hard-cluster occupancy"
  )

  add_failure(
    objective_match,
    "XB mismatch"
  )

  add_failure(
    spatial_j_match,
    "spatial-J mismatch"
  )

  add_failure(
    convergence_monotone,
    "non-monotone best convergence"
  )

  add_failure(
    convergence_tail_match,
    "convergence tail != f_obj"
  )

  add_failure(
    initial_ok,
    "worse than shared initial feasible best"
  )

  add_failure(
    !exact_collapse,
    "centroid collapse"
  )

  add_failure(
    reproducible,
    "reproducibility"
  )


  failure_reason <- if (length(failures)) {
    paste(
      failures,
      collapse = "; "
    )
  } else if (near_collapse) {
    paste0(
      "near-collapse warning: spatial min separation < ",
      NEAR_COLLAPSE_SEP
    )
  } else {
    ""
  }


  summary_rows[[method]] <- data.frame(
    method = method,
    status = status,
    failure_reason = failure_reason,
    final_XB = as.numeric(res$f_obj),
    spatial_J = as.numeric(res$spatial_obj),
    initial_best_XB = initial_best_xb,
    improvement_from_initial = improvement,
    improved_or_equal_initial = initial_ok,
    kmeans_spatial_XB = kmeans_eval$fitness,
    ratio_to_kmeans = as.numeric(
      res$f_obj /
      kmeans_eval$fitness
    ),
    occupied = occupied,
    min_cluster_size = min_cluster_size,
    spatial_min_sep = spatial_min_sep,
    search_min_sep = search_min_sep,
    mean_max_membership = mean_max_membership,
    convergence_monotone = convergence_monotone,
    convergence_tail_match = convergence_tail_match,
    objective_match = objective_match,
    spatial_J_match = spatial_j_match,
    membership_normalized = membership_normalized,
    reproducible = reproducible,
    iterations = as.integer(res$iteration),
    runtime_sec = safe_runtime_seconds(res),
    stringsAsFactors = FALSE
  )


  for (k in seq_len(NCLUSTER)) {
    support_rows[[paste(method, k, sep = "_")]] <- data.frame(
      method = method,
      cluster = k,
      hard_count = counts[k],
      hard_proportion = counts[k] / nrow(x),
      soft_mass = sum(
        res$membership[, k]
      ),
      effective_mass_m2 = sum(
        res$membership[, k]^M
      ),
      mean_membership = mean(
        res$membership[, k]
      ),
      max_membership = max(
        res$membership[, k]
      ),
      q95_membership = as.numeric(
        stats::quantile(
          res$membership[, k],
          probs = 0.95,
          names = FALSE
        )
      ),
      stringsAsFactors = FALSE
    )
  }


  cat(
    sprintf(
      "[%s] %s | XB=%.8f | initial=%.8f | occupied=%d/%d | min_sep=%.6f | min_n=%d | repro=%s\n",
      method,
      status,
      res$f_obj,
      initial_best_xb,
      occupied,
      NCLUSTER,
      spatial_min_sep,
      min_cluster_size,
      reproducible
    )
  )
}


summary_df <- do.call(
  rbind,
  summary_rows
)

rownames(
  summary_df
) <- NULL


support_df <- if (length(support_rows)) {
  do.call(
    rbind,
    support_rows
  )
} else {
  data.frame()
}

if (nrow(support_df)) {
  rownames(
    support_df
  ) <- NULL
}


# -----------------------------------------------------------------------------
# 7. Overall decision
# -----------------------------------------------------------------------------

n_pass <- sum(
  summary_df$status == "PASS"
)

n_warn <- sum(
  summary_df$status == "PASS_WITH_WARNING"
)

n_fail <- sum(
  summary_df$status == "FAIL"
)


overall_status <- if (n_fail > 0L) {
  "FAIL_INVESTIGATE"
} else if (n_warn > 0L) {
  "PASS_WITH_WARNING"
} else {
  "PASS"
}


# -----------------------------------------------------------------------------
# 8. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7r_patch_v3_regression_smoke"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


utils::write.csv(
  reference_df,
  file.path(
    out_dir,
    "v7r_reference_candidates.csv"
  ),
  row.names = FALSE
)


utils::write.csv(
  summary_df,
  file.path(
    out_dir,
    "v7r_optimizer_summary.csv"
  ),
  row.names = FALSE
)


if (nrow(support_df)) {
  utils::write.csv(
    support_df,
    file.path(
      out_dir,
      "v7r_cluster_support.csv"
    ),
    row.names = FALSE
  )
}


if (length(errors)) {
  error_df <- data.frame(
    method = names(errors),
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
      "v7r_errors.csv"
    ),
    row.names = FALSE
  )
}


saveRDS(
  list(
    config = list(
      seed = SEED,
      ncluster = NCLUSTER,
      m = M,
      alpha = ALPHA,
      a = A_SPATIAL,
      b = B_SPATIAL,
      population = POP_SIZE,
      max_iter = MAX_ITER,
      error = ERROR_TOL,
      same_limit = SAME_LIMIT,
      reproducibility = REPRODUCIBILITY_RUN
    ),
    references = list(
      initial_state = initial_state,
      initial_best_index = initial_best_index,
      initial_best_xb = initial_best_xb,
      collapsed = collapsed_eval,
      kmeans = kmeans_eval
    ),
    results = results,
    reproducibility_results = repro_results,
    summary = summary_df,
    support = support_df,
    errors = errors,
    overall_status = overall_status
  ),
  file.path(
    out_dir,
    "v7r_results.rds"
  )
)


# -----------------------------------------------------------------------------
# 9. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7-R — Patch v3 Regression Smoke Benchmark\n")
cat("============================================================\n")
cat("Dataset             :", nrow(x), "x", ncol(x), "\n")
cat("Clusters            :", NCLUSTER, "\n")
cat("Fuzzifier           :", M, "\n")
cat("Spatial alpha       :", ALPHA, "\n")
cat("Population          :", POP_SIZE, "\n")
cat("Iterations          :", MAX_ITER, "\n")
cat("Seed                :", SEED, "\n")
cat("Fitness             : spatial_XB_feasible\n")
cat("Reproducibility run :", REPRODUCIBILITY_RUN, "\n")
cat("============================================================\n")


cat("\n[1] REFERENCES\n")
print(
  reference_df,
  row.names = FALSE
)


cat("\n[2] OPTIMIZER SUMMARY\n")

print(
  summary_df[
    ,
    c(
      "method",
      "status",
      "final_XB",
      "initial_best_XB",
      "improvement_from_initial",
      "kmeans_spatial_XB",
      "occupied",
      "min_cluster_size",
      "spatial_min_sep",
      "mean_max_membership",
      "reproducible"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


cat("\n[3] TECHNICAL GATE\n")
cat("PASS              :", n_pass, "\n")
cat("PASS_WITH_WARNING :", n_warn, "\n")
cat("FAIL              :", n_fail, "\n")
cat("OVERALL           :", overall_status, "\n")


cat("\nInterpretation:\n")

cat(
  "- PASS means the optimizer satisfies the Patch-v3 technical contract.\n"
)

cat(
  "- PASS_WITH_WARNING means the contract passes but centroid separation is\n",
  "  very small and requires inspection before formal benchmarking.\n",
  sep = ""
)

cat(
  "- FAIL means Patch v3 must be investigated before V8.\n"
)

cat(
  "- `ratio_to_kmeans` is descriptive only. V7-R is not NFE-fair and must not\n",
  "  be used to rank optimizers scientifically.\n",
  sep = ""
)

cat(
  "\nResults written to:\n  ",
  out_dir,
  "\n",
  sep = ""
)
