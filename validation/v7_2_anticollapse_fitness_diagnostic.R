# =============================================================================
# soviclust — V7.2 Anti-Collapse Fitness Diagnostic
# =============================================================================
#
# Purpose
# -------
# Compare alternative optimizer fitness definitions after V7.1 demonstrated
# that the common centroid-only jfgwcv objective rewards the collapsed
# centroid solution on the bundled 514 x 15 standardized dataset.
#
# This script DOES NOT modify package source code.
#
# Candidate criteria:
#   1. current J / jfgwcv;
#   2. base Xie-Beni (XB);
#   3. base Kwon;
#   4. spatial XB after FGWC membership projection;
#   5. spatial Kwon after FGWC membership projection.
#
# Run from project root:
#
#   source("validation/v7_2_anticollapse_fitness_diagnostic.R")
#
# Requires:
#   validation_results/v7_optimizer_smoke/v7_smoke_results.rds
#
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

SEED <- 2026L
NCLUSTER <- 4L
M <- 2
DISTANCE <- "euclidean"
ORDER <- 2

ALPHA <- 0.7
A <- 1
B <- 1

N_RANDOM_CANDIDATES <- 200L
N_PROJECTION_STEPS <- 20L


# -----------------------------------------------------------------------------
# 1. Locate project and source current helpers
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

helper_path <- file.path(
  root, "tests", "testthat", "helper-fgwc-algorithms.R"
)

if (!file.exists(helper_path)) {
  stop("Missing helper-fgwc-algorithms.R", call. = FALSE)
}

source(helper_path, local = .GlobalEnv)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 2. Load the same data used in V7/V7.1
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

pop_mat <- matrix(pop, ncol = 1)
mimj <- pop_mat %*% t(pop_mat)
beta <- 1 - ALPHA


# -----------------------------------------------------------------------------
# 3. Utility functions
# -----------------------------------------------------------------------------

min_sep <- function(centers) {
  d <- as.matrix(stats::dist(as.matrix(centers)))
  diag(d) <- Inf
  min(d)
}


hard_occupied <- function(u) {
  length(unique(apply(as.matrix(u), 1, which.max)))
}


evaluate_candidate <- function(centers) {
  centers <- as.matrix(centers)

  base <- alg_env$membership_from_centroids(
    x, centers, M, DISTANCE, ORDER
  )
  base_u <- base$u

  base_j <- alg_env$optimizer_fitness(
    x, centers, M, DISTANCE, ORDER
  )

  base_xb <- alg_env$XB1(
    x, base_u, centers, M
  )

  base_kwon <- alg_env$Kwon1(
    x, base_u, centers, M
  )

  spatial_u <- alg_env$renew_uij(
    x, base_u, mimj, distmat,
    ALPHA, beta, A, B
  )

  spatial_centers <- alg_env$centroid_from_membership(
    x, spatial_u, M
  )

  spatial_j <- alg_env$optimizer_spatial_objective(
    x, spatial_u, spatial_centers, M, DISTANCE, ORDER
  )

  spatial_xb <- alg_env$XB1(
    x, spatial_u, spatial_centers, M
  )

  spatial_kwon <- alg_env$Kwon1(
    x, spatial_u, spatial_centers, M
  )

  list(
    base_u = base_u,
    spatial_u = spatial_u,
    spatial_centers = spatial_centers,
    base_J = base_j,
    base_XB = base_xb,
    base_Kwon = base_kwon,
    spatial_J = spatial_j,
    spatial_XB = spatial_xb,
    spatial_Kwon = spatial_kwon,
    base_min_sep = min_sep(centers),
    spatial_min_sep = min_sep(spatial_centers),
    base_occupied = hard_occupied(base_u),
    spatial_occupied = hard_occupied(spatial_u)
  )
}


finite_or_inf_ok <- function(x) {
  length(x) == 1L && !is.na(x) && !is.nan(x)
}


# -----------------------------------------------------------------------------
# 4. Reference solutions
# -----------------------------------------------------------------------------

grand_mean <- colMeans(x)

collapsed_centers <- matrix(
  rep(grand_mean, each = NCLUSTER),
  nrow = NCLUSTER,
  byrow = FALSE
)

set.seed(SEED)
km <- stats::kmeans(
  x,
  centers = NCLUSTER,
  nstart = 50,
  iter.max = 100
)
kmeans_centers <- km$centers

random_centers <- alg_env$gen_vi(
  x, NCLUSTER, "uniform", SEED
)

reference_centers <- list(
  theoretical_collapsed = collapsed_centers,
  kmeans_centers = kmeans_centers,
  random_centers = random_centers
)

reference_rows <- lapply(
  names(reference_centers),
  function(nm) {
    ev <- evaluate_candidate(reference_centers[[nm]])

    data.frame(
      solution = nm,
      base_J = ev$base_J,
      base_XB = ev$base_XB,
      base_Kwon = ev$base_Kwon,
      spatial_J = ev$spatial_J,
      spatial_XB = ev$spatial_XB,
      spatial_Kwon = ev$spatial_Kwon,
      base_min_sep = ev$base_min_sep,
      spatial_min_sep = ev$spatial_min_sep,
      base_occupied = ev$base_occupied,
      spatial_occupied = ev$spatial_occupied,
      stringsAsFactors = FALSE
    )
  }
)

reference_df <- do.call(rbind, reference_rows)
rownames(reference_df) <- NULL


# -----------------------------------------------------------------------------
# 5. Re-evaluate V7 outputs
# -----------------------------------------------------------------------------

v7_path <- file.path(
  root,
  "validation_results",
  "v7_optimizer_smoke",
  "v7_smoke_results.rds"
)

if (!file.exists(v7_path)) {
  stop("V7 result RDS not found. Run V7 first.", call. = FALSE)
}

v7 <- readRDS(v7_path)

v7_rows <- lapply(
  names(v7),
  function(method) {
    res <- v7[[method]]

    base_centers <- as.matrix(res$centroid)
    base_u <- alg_env$membership_from_centroids(
      x, base_centers, M, DISTANCE, ORDER
    )$u

    # Evaluate the actual final spatial partition too.
    final_u <- as.matrix(res$membership)
    final_centers <- as.matrix(res$centroid)

    data.frame(
      method = method,
      reported_f_obj = as.numeric(res$f_obj),
      current_base_J = alg_env$optimizer_fitness(
        x, base_centers, M, DISTANCE, ORDER
      ),
      base_XB = alg_env$XB1(
        x, base_u, base_centers, M
      ),
      base_Kwon = alg_env$Kwon1(
        x, base_u, base_centers, M
      ),
      final_spatial_J = alg_env$optimizer_spatial_objective(
        x, final_u, final_centers, M, DISTANCE, ORDER
      ),
      final_spatial_XB = alg_env$XB1(
        x, final_u, final_centers, M
      ),
      final_spatial_Kwon = alg_env$Kwon1(
        x, final_u, final_centers, M
      ),
      min_sep = min_sep(final_centers),
      occupied = hard_occupied(final_u),
      stringsAsFactors = FALSE
    )
  }
)

v7_df <- do.call(rbind, v7_rows)
rownames(v7_df) <- NULL


# -----------------------------------------------------------------------------
# 6. Spatial-projection trajectory from K-means
# -----------------------------------------------------------------------------

trajectory <- vector("list", N_PROJECTION_STEPS + 1L)

centers <- kmeans_centers

for (step in 0:N_PROJECTION_STEPS) {
  base_u <- alg_env$membership_from_centroids(
    x, centers, M, DISTANCE, ORDER
  )$u

  if (step == 0L) {
    current_u <- base_u
  } else {
    current_u <- alg_env$renew_uij(
      x, base_u, mimj, distmat,
      ALPHA, beta, A, B
    )
    centers <- alg_env$centroid_from_membership(
      x, current_u, M
    )
  }

  trajectory[[step + 1L]] <- data.frame(
    step = step,
    J = alg_env$optimizer_fitness(
      x, centers, M, DISTANCE, ORDER
    ),
    XB = alg_env$XB1(
      x, current_u, centers, M
    ),
    Kwon = alg_env$Kwon1(
      x, current_u, centers, M
    ),
    min_sep = min_sep(centers),
    occupied = hard_occupied(current_u),
    stringsAsFactors = FALSE
  )
}

trajectory_df <- do.call(rbind, trajectory)


# -----------------------------------------------------------------------------
# 7. Random-candidate landscape survey
# -----------------------------------------------------------------------------

random_rows <- vector("list", N_RANDOM_CANDIDATES)

for (i in seq_len(N_RANDOM_CANDIDATES)) {
  centers_i <- alg_env$gen_vi(
    x,
    NCLUSTER,
    "uniform",
    SEED + i
  )

  ev <- evaluate_candidate(centers_i)

  random_rows[[i]] <- data.frame(
    candidate = i,
    base_J = ev$base_J,
    base_XB = ev$base_XB,
    base_Kwon = ev$base_Kwon,
    spatial_J = ev$spatial_J,
    spatial_XB = ev$spatial_XB,
    spatial_Kwon = ev$spatial_Kwon,
    base_min_sep = ev$base_min_sep,
    spatial_min_sep = ev$spatial_min_sep,
    spatial_occupied = ev$spatial_occupied,
    stringsAsFactors = FALSE
  )
}

random_df <- do.call(rbind, random_rows)


# -----------------------------------------------------------------------------
# 8. Candidate-fitness decision table
# -----------------------------------------------------------------------------

get_ref <- function(solution, col) {
  reference_df[
    reference_df$solution == solution,
    col,
    drop = TRUE
  ]
}


fitness_candidates <- data.frame(
  fitness = c(
    "base_J",
    "base_XB",
    "base_Kwon",
    "spatial_J",
    "spatial_XB",
    "spatial_Kwon"
  ),
  stringsAsFactors = FALSE
)

fitness_candidates$collapsed <- vapply(
  fitness_candidates$fitness,
  function(nm) get_ref("theoretical_collapsed", nm),
  numeric(1)
)

fitness_candidates$kmeans <- vapply(
  fitness_candidates$fitness,
  function(nm) get_ref("kmeans_centers", nm),
  numeric(1)
)

fitness_candidates$random <- vapply(
  fitness_candidates$fitness,
  function(nm) get_ref("random_centers", nm),
  numeric(1)
)

# Lower is better for all six candidate criteria.
fitness_candidates$collapse_penalized <- with(
  fitness_candidates,
  is.infinite(collapsed) |
    collapsed > kmeans
)

fitness_candidates$kmeans_beats_random <- with(
  fitness_candidates,
  kmeans < random
)

fitness_candidates$reference_pass <- with(
  fitness_candidates,
  collapse_penalized & kmeans_beats_random
)

# Evaluate whether the criterion gets WORSE as the alpha=.7 projection drives
# K-means toward collapse.
trajectory_start <- trajectory_df[trajectory_df$step == 0L, ]
trajectory_end <- trajectory_df[
  trajectory_df$step == N_PROJECTION_STEPS,
]

trajectory_map <- c(
  base_J = "J",
  base_XB = "XB",
  base_Kwon = "Kwon",
  spatial_J = "J",
  spatial_XB = "XB",
  spatial_Kwon = "Kwon"
)

fitness_candidates$projection_start <- vapply(
  fitness_candidates$fitness,
  function(nm) {
    trajectory_start[[trajectory_map[[nm]]]]
  },
  numeric(1)
)

fitness_candidates$projection_end <- vapply(
  fitness_candidates$fitness,
  function(nm) {
    trajectory_end[[trajectory_map[[nm]]]]
  },
  numeric(1)
)

fitness_candidates$projection_penalizes_collapse <- with(
  fitness_candidates,
  is.infinite(projection_end) |
    projection_end > projection_start
)

fitness_candidates$diagnostic_pass <- with(
  fitness_candidates,
  reference_pass & projection_penalizes_collapse
)


# -----------------------------------------------------------------------------
# 9. Random-landscape summaries
# -----------------------------------------------------------------------------

random_summary <- do.call(
  rbind,
  lapply(
    c(
      "base_J", "base_XB", "base_Kwon",
      "spatial_J", "spatial_XB", "spatial_Kwon"
    ),
    function(nm) {
      vals <- random_df[[nm]]
      ord <- order(vals, na.last = NA)

      top_n <- min(10L, length(ord))
      top <- ord[seq_len(top_n)]

      sep_col <- if (grepl("^spatial_", nm)) {
        "spatial_min_sep"
      } else {
        "base_min_sep"
      }

      data.frame(
        fitness = nm,
        finite_fraction = mean(is.finite(vals)),
        min_value = min(vals[is.finite(vals)]),
        median_value = stats::median(vals[is.finite(vals)]),
        median_sep_all = stats::median(random_df[[sep_col]]),
        median_sep_top10 = stats::median(
          random_df[[sep_col]][top]
        ),
        stringsAsFactors = FALSE
      )
    }
  )
)


# -----------------------------------------------------------------------------
# 10. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7_2_anticollapse_fitness"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  reference_df,
  file.path(out_dir, "v7_2_reference_fitness.csv"),
  row.names = FALSE
)

utils::write.csv(
  v7_df,
  file.path(out_dir, "v7_2_v7_final_fitness.csv"),
  row.names = FALSE
)

utils::write.csv(
  trajectory_df,
  file.path(out_dir, "v7_2_projection_trajectory.csv"),
  row.names = FALSE
)

utils::write.csv(
  random_df,
  file.path(out_dir, "v7_2_random_candidate_survey.csv"),
  row.names = FALSE
)

utils::write.csv(
  fitness_candidates,
  file.path(out_dir, "v7_2_fitness_decision_table.csv"),
  row.names = FALSE
)

utils::write.csv(
  random_summary,
  file.path(out_dir, "v7_2_random_landscape_summary.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    reference = reference_df,
    v7 = v7_df,
    trajectory = trajectory_df,
    random = random_df,
    decision = fitness_candidates,
    random_summary = random_summary
  ),
  file.path(out_dir, "v7_2_results.rds")
)


# -----------------------------------------------------------------------------
# 11. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7.2 — Anti-Collapse Fitness Diagnostic\n")
cat("============================================================\n")

cat("\n[1] REFERENCE SOLUTIONS\n")
print(reference_df, row.names = FALSE)

cat("\n[2] FITNESS DECISION TABLE\n")
print(
  fitness_candidates[, c(
    "fitness",
    "collapsed",
    "kmeans",
    "random",
    "collapse_penalized",
    "kmeans_beats_random",
    "projection_penalizes_collapse",
    "diagnostic_pass"
  )],
  row.names = FALSE
)

cat("\n[3] V7 FINAL SOLUTIONS UNDER SEPARATION-AWARE INDICES\n")
print(
  v7_df[, c(
    "method",
    "reported_f_obj",
    "final_spatial_XB",
    "final_spatial_Kwon",
    "min_sep",
    "occupied"
  )],
  row.names = FALSE
)

cat("\n[4] ALPHA=0.7 PROJECTION: START VS FINAL\n")
print(
  trajectory_df[
    trajectory_df$step %in% c(0L, N_PROJECTION_STEPS),
    ,
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n[5] RANDOM LANDSCAPE SUMMARY\n")
print(random_summary, row.names = FALSE)

passed <- fitness_candidates$fitness[
  fitness_candidates$diagnostic_pass
]

cat("\n============================================================\n")
cat("V7.2 DIAGNOSTIC RESULT\n")
cat("============================================================\n")

if (length(passed) == 0L) {
  cat("No candidate fitness passed all anti-collapse diagnostic criteria.\n")
  cat("Do NOT patch optimizer fitness yet.\n")
} else {
  cat(
    "Candidate fitness(es) passing diagnostic criteria:",
    paste(passed, collapse = ", "),
    "\n"
  )

  if ("spatial_XB" %in% passed) {
    cat(
      "\nLeading candidate: spatial_XB\n",
      "Reason: evaluates the actual spatially adjusted FGWC partition while\n",
      "combining fuzzy compactness with explicit centroid separation.\n",
      sep = ""
    )
  } else if ("base_XB" %in% passed) {
    cat(
      "\nLeading candidate: base_XB\n",
      "Reason: explicit compactness/separation penalty without evaluating the\n",
      "post-spatial partition.\n",
      sep = ""
    )
  }

  cat(
    "\nDo NOT modify source yet. The next step is a short optimizer pilot using\n",
    "the leading candidate fitness before a package-wide Patch v3.\n",
    sep = ""
  )
}

cat("\nResults written to:\n  ", out_dir, "\n", sep = "")
