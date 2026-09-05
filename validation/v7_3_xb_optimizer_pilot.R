# =============================================================================
# soviclust — V7.3 Separation-Aware Optimizer Fitness Pilot
# =============================================================================
#
# Purpose
# -------
# Pilot a separation-aware optimizer search criterion WITHOUT modifying package
# source code.
#
# V7.1/V7.2 showed:
#   - the common jfgwcv objective rewards the fully collapsed solution;
#   - XB/Kwon penalize collapse;
#   - spatial_XB is a strong FINAL-PARTITION diagnostic.
#
# Important architectural choice
# ------------------------------
# Current optimizer implementations already perform:
#
#   candidate centroid
#     -> FCM membership
#     -> spatial renew_uij()
#     -> spatial centroid recomputation
#     -> optimizer_fitness()
#
# Therefore, putting another spatial projection inside optimizer_fitness() would
# double-project every candidate.
#
# V7.3 instead uses:
#
#   SEARCH FITNESS  = base_XB on the candidate centroid already produced by the
#                     optimizer's existing spatial projection
#
#   FINAL DIAGNOSTIC = spatial_XB on the optimizer's returned spatial membership
#                      and centroid
#
# This is a temporary in-memory override. Package source files are NOT changed.
#
# Run from package root:
#
#   source("validation/v7_3_xb_optimizer_pilot.R")
#
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

PILOT_SEED <- 2026L
NCLUSTER <- 4L
M <- 2
ALPHA <- 0.7
A <- 1
B <- 1
DISTANCE <- "euclidean"
ORDER <- 2

POP_SIZE <- 10L
MAX_ITER <- 10L
ERROR_TOL <- 0
SAME_LIMIT <- MAX_ITER + 100L

RUN_REPRO_CHECK <- TRUE

ROW_SUM_TOL <- 1e-8
CONV_TOL <- 1e-8
REPRO_TOL <- 1e-10

# These are diagnostic warnings, not universal clustering thresholds.
COLLAPSE_WARN_TOL <- 1e-3
NEAR_DUPLICATE_TOL <- 1e-8


# -----------------------------------------------------------------------------
# 1. Locate project and load current source
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

  stop("Unable to locate soviclust project root.", call. = FALSE)
}

root <- find_project_root()

helper <- file.path(
  root, "tests", "testthat", "helper-fgwc-algorithms.R"
)

if (!file.exists(helper)) {
  stop("Missing helper-fgwc-algorithms.R", call. = FALSE)
}

source(helper, local = .GlobalEnv)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 2. Load same 514 x 15 data used in V7/V7.1/V7.2
# -----------------------------------------------------------------------------

data_path <- file.path(
  root, "inst", "extdata", "sovi_data_kab_514_15.xlsx"
)
pop_path <- file.path(
  root, "inst", "extdata", "sovi_data_pop_514.xlsx"
)
dist_path <- file.path(
  root, "inst", "extdata", "Distance_matrix_514.xlsx"
)

raw_data <- as.data.frame(readxl::read_excel(data_path))
indicator_cols <- tail(names(raw_data), 15L)

x_raw <- data.matrix(raw_data[, indicator_cols, drop = FALSE])

if (any(!is.finite(x_raw))) {
  stop("Indicator data contains non-finite values.", call. = FALSE)
}

sds <- apply(x_raw, 2, stats::sd)
if (any(!is.finite(sds)) || any(sds <= 0)) {
  stop("Indicator data contains constant/invalid columns.", call. = FALSE)
}

x <- unclass(scale(x_raw))
storage.mode(x) <- "double"

pop_df <- as.data.frame(readxl::read_excel(pop_path))
num_pop <- which(vapply(pop_df, is.numeric, logical(1)))
name_hits <- intersect(
  grep("pop|population|penduduk", names(pop_df), ignore.case = TRUE),
  num_pop
)
pop_col <- if (length(name_hits)) name_hits[1L] else tail(num_pop, 1L)
pop <- as.numeric(pop_df[[pop_col]])

dist_df <- as.data.frame(readxl::read_excel(dist_path))

if (ncol(dist_df) == nrow(x) + 1L || !is.numeric(dist_df[[1L]])) {
  dist_df <- dist_df[, -1L, drop = FALSE]
}

distmat <- data.matrix(dist_df)
diag(distmat) <- 0

if (!identical(dim(distmat), c(nrow(x), nrow(x)))) {
  stop("Distance matrix dimension mismatch.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 3. Temporary separation-aware optimizer fitness
# -----------------------------------------------------------------------------
#
# Minimize Xie-Beni on the centroid candidate received by optimizer_fitness().
#
# Since the optimizer has ALREADY performed its normal FGWC spatial projection
# before this call, this avoids a second spatial projection.
# -----------------------------------------------------------------------------

pilot_xb_fitness <- function(data, centers, m,
                             distance = "euclidean", order = 2, ...) {
  centers <- as.matrix(centers)

  memb <- alg_env$membership_from_centroids(
    data = data,
    centers = centers,
    m = m,
    distance = distance,
    order = order
  )$u

  alg_env$XB1(
    data = data,
    uij = memb,
    vi = centers,
    m = m
  )
}


# Inject the temporary fitness into every isolated optimizer environment.
# No file is written and no source package function is modified.
for (method in names(optimizer_envs)) {
  optimizer_envs[[method]]$optimizer_fitness <- pilot_xb_fitness
}


# Sanity test: theoretical collapsed centroids must be infinitely bad.
grand_mean <- colMeans(x)

collapsed_centers <- matrix(
  rep(grand_mean, each = NCLUSTER),
  nrow = NCLUSTER,
  byrow = FALSE
)

collapsed_pilot_fitness <- pilot_xb_fitness(
  x, collapsed_centers, M, DISTANCE, ORDER
)

if (!is.infinite(collapsed_pilot_fitness)) {
  stop(
    "Pilot XB fitness did not penalize exact centroid collapse as expected.",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 4. Optimizer specifications
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
  a = A,
  b = B,
  error = ERROR_TOL,
  max.iter = MAX_ITER,
  randomN = PILOT_SEED,
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

methods <- names(method_specs)


# -----------------------------------------------------------------------------
# 5. Helpers
# -----------------------------------------------------------------------------

min_sep <- function(centers) {
  d <- as.matrix(stats::dist(as.matrix(centers)))
  diag(d) <- Inf
  min(d)
}


max_abs_diff <- function(x, y) {
  if (is.null(x) || is.null(y)) return(Inf)

  x <- as.numeric(x)
  y <- as.numeric(y)

  if (length(x) != length(y)) return(Inf)
  if (any(!is.finite(x)) || any(!is.finite(y))) return(Inf)

  max(abs(x - y))
}


hard_sizes <- function(u, k) {
  cl <- apply(as.matrix(u), 1, which.max)
  tabulate(cl, nbins = k)
}


base_xb_from_centers <- function(centers) {
  pilot_xb_fitness(
    x, centers, M, DISTANCE, ORDER
  )
}


spatial_xb_from_result <- function(res) {
  alg_env$XB1(
    data = x,
    uij = as.matrix(res$membership),
    vi = as.matrix(res$centroid),
    m = M
  )
}


spatial_kwon_from_result <- function(res) {
  alg_env$Kwon1(
    data = x,
    uij = as.matrix(res$membership),
    vi = as.matrix(res$centroid),
    m = M
  )
}


spatial_j_from_result <- function(res) {
  alg_env$optimizer_spatial_objective(
    data = x,
    membership = as.matrix(res$membership),
    centers = as.matrix(res$centroid),
    m = M,
    distance = DISTANCE,
    order = ORDER
  )
}


# -----------------------------------------------------------------------------
# 6. Load V7 baseline for comparison
# -----------------------------------------------------------------------------

baseline_path <- file.path(
  root,
  "validation_results",
  "v7_optimizer_smoke",
  "v7_smoke_results.rds"
)

if (!file.exists(baseline_path)) {
  stop("V7 baseline RDS not found. Run V7 first.", call. = FALSE)
}

baseline <- readRDS(baseline_path)

baseline_df <- do.call(
  rbind,
  lapply(names(baseline), function(method) {
    res <- baseline[[method]]
    sizes <- hard_sizes(res$membership, NCLUSTER)

    data.frame(
      method = method,
      baseline_J = as.numeric(res$f_obj),
      baseline_spatial_XB = spatial_xb_from_result(res),
      baseline_spatial_Kwon = spatial_kwon_from_result(res),
      baseline_min_sep = min_sep(res$centroid),
      baseline_occupied = sum(sizes > 0),
      baseline_min_cluster_size = min(sizes),
      stringsAsFactors = FALSE
    )
  })
)

rownames(baseline_df) <- NULL


# -----------------------------------------------------------------------------
# 7. Run pilot
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("soviclust — V7.3 Separation-Aware Fitness Pilot\n")
cat("============================================================\n")
cat("Search fitness     : base_XB (temporary in-memory override)\n")
cat("Final diagnostic  : spatial_XB on returned FGWC partition\n")
cat("Dataset           :", nrow(x), "x", ncol(x), "standardized indicators\n")
cat("Clusters          :", NCLUSTER, "\n")
cat("m                 :", M, "\n")
cat("alpha             :", ALPHA, "\n")
cat("Search agents     :", POP_SIZE, "\n")
cat("Iterations        :", MAX_ITER, "\n")
cat("Seed              :", PILOT_SEED, "\n")
cat("Repro check       :", RUN_REPRO_CHECK, "\n")
cat("Collapsed XB      :", collapsed_pilot_fitness, "\n")
cat("============================================================\n\n")


pilot_results <- list()
rows <- list()
errors <- list()

for (method in methods) {
  cat(sprintf("[%s] running XB pilot...\n", method))

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
    errors[[method]] <- conditionMessage(first)

    rows[[method]] <- data.frame(
      method = method,
      health = "ERROR",
      pilot_XB = NA_real_,
      spatial_XB = NA_real_,
      spatial_Kwon = NA_real_,
      spatial_J = NA_real_,
      min_sep = NA_real_,
      occupied = NA_integer_,
      min_cluster_size = NA_integer_,
      max_cluster_size = NA_integer_,
      iteration = NA_integer_,
      elapsed_sec = elapsed,
      convergence_nonincreasing = FALSE,
      tail_matches_fitness = FALSE,
      reproducible = FALSE,
      repro_fitness_diff = NA_real_,
      repro_centroid_maxdiff = NA_real_,
      repro_membership_maxdiff = NA_real_,
      technical_pass = FALSE,
      stringsAsFactors = FALSE
    )

    cat(sprintf("[%s] ERROR: %s\n\n", method, errors[[method]]))
    next
  }

  # Output metadata in source still says jfgwcv because source is intentionally
  # not modified. Override only in the saved pilot object.
  first$fitness_type <- "base_XB_pilot"

  repro <- list(
    reproducible = NA,
    fitness_diff = NA_real_,
    centroid_diff = NA_real_,
    membership_diff = NA_real_
  )

  if (RUN_REPRO_CHECK) {
    second <- tryCatch(
      do.call(spec$fn, args),
      error = function(e) e
    )

    if (inherits(second, "error")) {
      errors[[paste0(method, "_repro")]] <- conditionMessage(second)
      repro$reproducible <- FALSE
      repro$fitness_diff <- Inf
      repro$centroid_diff <- Inf
      repro$membership_diff <- Inf
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
        fitness_diff = f_diff,
        centroid_diff = c_diff,
        membership_diff = u_diff
      )
    }
  }

  pilot_results[[method]] <- first

  membership <- as.matrix(first$membership)
  centers <- as.matrix(first$centroid)
  conv <- as.numeric(first$converg)
  sizes <- hard_sizes(membership, NCLUSTER)

  row_sum_error <- max(abs(rowSums(membership) - 1))
  sep <- min_sep(centers)

  expected_pilot_fitness <- base_xb_from_centers(centers)
  sx <- spatial_xb_from_result(first)
  sk <- spatial_kwon_from_result(first)
  sj <- spatial_j_from_result(first)

  conv_ok <- length(conv) >= 2L &&
    all(is.finite(conv)) &&
    all(diff(conv) <= CONV_TOL)

  tail_ok <- length(conv) >= 1L &&
    is.finite(first$f_obj) &&
    abs(tail(conv, 1L) - first$f_obj) <= CONV_TOL

  fitness_ok <- (
    (is.infinite(first$f_obj) && is.infinite(expected_pilot_fitness)) ||
      (
        is.finite(first$f_obj) &&
        is.finite(expected_pilot_fitness) &&
        abs(first$f_obj - expected_pilot_fitness) <= CONV_TOL
      )
  )

  occupied <- sum(sizes > 0)

  technical_pass <- all(c(
    inherits(first, "fgwc"),
    fitness_ok,
    is.finite(first$f_obj),
    is.finite(sx),
    is.finite(sk),
    is.finite(sj),
    identical(dim(membership), c(nrow(x), NCLUSTER)),
    all(is.finite(membership)),
    row_sum_error <= ROW_SUM_TOL,
    identical(dim(centers), c(NCLUSTER, ncol(x))),
    all(is.finite(centers)),
    conv_ok,
    tail_ok,
    as.integer(first$iteration) == MAX_ITER,
    occupied == NCLUSTER,
    min(sizes) > 0L,
    is.finite(sep),
    sep > NEAR_DUPLICATE_TOL
  ))

  if (RUN_REPRO_CHECK) {
    technical_pass <- technical_pass && isTRUE(repro$reproducible)
  }

  warning_flag <- technical_pass && (
    sep < COLLAPSE_WARN_TOL ||
      min(sizes) < 5L
  )

  health <- if (!technical_pass) {
    "FAIL"
  } else if (warning_flag) {
    "PASS_WITH_WARNING"
  } else {
    "PASS"
  }

  rows[[method]] <- data.frame(
    method = method,
    health = health,
    pilot_XB = as.numeric(first$f_obj),
    spatial_XB = sx,
    spatial_Kwon = sk,
    spatial_J = sj,
    min_sep = sep,
    occupied = occupied,
    min_cluster_size = min(sizes),
    max_cluster_size = max(sizes),
    iteration = as.integer(first$iteration),
    elapsed_sec = elapsed,
    convergence_nonincreasing = conv_ok,
    tail_matches_fitness = tail_ok,
    reproducible = repro$reproducible,
    repro_fitness_diff = repro$fitness_diff,
    repro_centroid_maxdiff = repro$centroid_diff,
    repro_membership_maxdiff = repro$membership_diff,
    technical_pass = technical_pass,
    stringsAsFactors = FALSE
  )

  cat(
    sprintf(
      "[%s] %s | XB=%.6f | spatial_XB=%.6f | min_sep=%.5f | clusters=%d/%d | %.2fs\n\n",
      method,
      health,
      first$f_obj,
      sx,
      sep,
      occupied,
      NCLUSTER,
      elapsed
    )
  )
}

pilot_df <- do.call(rbind, rows)
rownames(pilot_df) <- NULL


# -----------------------------------------------------------------------------
# 8. Compare against V7 jfgwcv baseline
# -----------------------------------------------------------------------------

comparison <- merge(
  baseline_df,
  pilot_df,
  by = "method",
  all.x = TRUE,
  all.y = TRUE,
  sort = FALSE
)

comparison$spatial_XB_ratio_vs_baseline <- with(
  comparison,
  spatial_XB / baseline_spatial_XB
)

comparison$min_sep_ratio_vs_baseline <- with(
  comparison,
  min_sep / baseline_min_sep
)

comparison$cluster_occupancy_improved <- with(
  comparison,
  occupied > baseline_occupied
)

comparison$collapse_improved <- with(
  comparison,
  is.finite(min_sep) &
    is.finite(baseline_min_sep) &
    min_sep > baseline_min_sep
)


# -----------------------------------------------------------------------------
# 9. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7_3_xb_pilot"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  pilot_df,
  file.path(out_dir, "v7_3_xb_pilot_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  comparison,
  file.path(out_dir, "v7_3_vs_v7_comparison.csv"),
  row.names = FALSE
)

saveRDS(
  pilot_results,
  file.path(out_dir, "v7_3_xb_pilot_results.rds")
)

config <- data.frame(
  parameter = c(
    "search_fitness",
    "final_primary_diagnostic",
    "seed",
    "ncluster",
    "m",
    "alpha",
    "a",
    "b",
    "population_size",
    "max_iter",
    "reproducibility_check"
  ),
  value = c(
    "base_XB_after_existing_spatial_projection",
    "spatial_XB",
    PILOT_SEED,
    NCLUSTER,
    M,
    ALPHA,
    A,
    B,
    POP_SIZE,
    MAX_ITER,
    RUN_REPRO_CHECK
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  config,
  file.path(out_dir, "v7_3_config.csv"),
  row.names = FALSE
)

if (length(errors) > 0L) {
  utils::write.csv(
    data.frame(
      method = names(errors),
      message = unlist(errors, use.names = FALSE),
      stringsAsFactors = FALSE
    ),
    file.path(out_dir, "v7_3_errors.csv"),
    row.names = FALSE
  )
}


# -----------------------------------------------------------------------------
# 10. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7.3 PILOT SUMMARY\n")
cat("============================================================\n")

print(
  pilot_df[, c(
    "method",
    "health",
    "pilot_XB",
    "spatial_XB",
    "min_sep",
    "occupied",
    "min_cluster_size",
    "reproducible",
    "technical_pass"
  )],
  row.names = FALSE
)

cat("\n")
cat("============================================================\n")
cat("V7.3 vs V7 COLLAPSE COMPARISON\n")
cat("============================================================\n")

print(
  comparison[, c(
    "method",
    "baseline_spatial_XB",
    "spatial_XB",
    "spatial_XB_ratio_vs_baseline",
    "baseline_min_sep",
    "min_sep",
    "min_sep_ratio_vs_baseline",
    "baseline_occupied",
    "occupied"
  )],
  row.names = FALSE
)

n_pass <- sum(pilot_df$technical_pass %in% TRUE)
n_total <- nrow(pilot_df)

cat("\nTechnical pass:", n_pass, "/", n_total, "\n")

median_xb_ratio <- stats::median(
  comparison$spatial_XB_ratio_vs_baseline[
    is.finite(comparison$spatial_XB_ratio_vs_baseline)
  ]
)

median_sep_ratio <- stats::median(
  comparison$min_sep_ratio_vs_baseline[
    is.finite(comparison$min_sep_ratio_vs_baseline)
  ]
)

cat(
  "Median spatial-XB ratio vs V7:",
  format(median_xb_ratio, digits = 6), "\n"
)
cat(
  "Median min-separation ratio vs V7:",
  format(median_sep_ratio, digits = 6), "\n"
)

all_occupied <- all(pilot_df$occupied == NCLUSTER, na.rm = TRUE)
all_repro <- all(pilot_df$reproducible %in% TRUE)

cat("\n============================================================\n")
cat("V7.3 DECISION\n")
cat("============================================================\n")

if (
  n_pass == n_total &&
  all_occupied &&
  all_repro &&
  is.finite(median_xb_ratio) &&
  median_xb_ratio < 1 &&
  is.finite(median_sep_ratio) &&
  median_sep_ratio > 1
) {
  cat("V7.3 PILOT RESULT: PROMISING\n")
  cat(
    "The temporary separation-aware search fitness removed the technical\n",
    "collapse failure across all nine optimizers and improved final spatial\n",
    "separation relative to the V7 jfgwcv baseline.\n",
    sep = ""
  )
  cat(
    "\nNext step: V7.4 / Patch-v3 design review before modifying package source.\n"
  )
} else {
  cat("V7.3 PILOT RESULT: NOT YET SUFFICIENT\n")
  cat(
    "Do not create Patch v3 yet. Inspect failed methods and the V7 comparison\n",
    "before choosing a permanent optimizer fitness.\n",
    sep = ""
  )
}

cat("\nIMPORTANT:\n")
cat(
  "V7.3 still does NOT establish optimizer superiority. It only evaluates\n",
  "whether a separation-aware search criterion prevents the collapse found\n",
  "under jfgwcv.\n",
  sep = ""
)

cat("\nResults written to:\n  ", out_dir, "\n", sep = "")
