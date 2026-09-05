# =============================================================================
# soviclust — V7.1 Objective & Spatial Projection Degeneracy Diagnostic
# =============================================================================
#
# Purpose
# -------
# Diagnose why several V7 optimizers converge to approximately 1923.75 and
# whether the collapse is caused by:
#   (a) the common jfgwcv objective itself, or
#   (b) repeated FGWC spatial membership projection + centroid recomputation.
#
# This script DOES NOT modify package source files.
#
# Run from the root of the soviclust source project:
#
#   source("validation/v7_1_objective_degeneracy_diagnostic.R")
#
# Expected inputs:
#   validation_results/v7_optimizer_smoke/v7_smoke_results.rds
#
# Outputs:
#   validation_results/v7_1_degeneracy/
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
A <- 1
B <- 1

ALPHA_GRID <- c(1.0, 0.9, 0.7, 0.5)
N_PROJECTION_STEPS <- 20L

COLLAPSE_FOBJ_TOL <- 1e-3
COLLAPSE_SEP_TOL <- 1e-3


# -----------------------------------------------------------------------------
# 1. Locate project
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

helper <- file.path(root, "tests", "testthat", "helper-fgwc-algorithms.R")
if (!file.exists(helper)) {
  stop("Missing helper-fgwc-algorithms.R", call. = FALSE)
}

source(helper, local = .GlobalEnv)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 2. Load and standardize the same 514 x 15 data used in V7
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


# -----------------------------------------------------------------------------
# 3. Helpers
# -----------------------------------------------------------------------------

min_sep <- function(centers) {
  centers <- as.matrix(centers)
  d <- as.matrix(stats::dist(centers))
  diag(d) <- Inf
  min(d)
}

membership_entropy <- function(u) {
  u <- alg_env$normalize_membership(u)
  z <- ifelse(u > 0, u * log(u), 0)
  mean(-rowSums(z))
}

mean_max_membership <- function(u) {
  mean(apply(u, 1, max))
}

occupied_clusters <- function(u) {
  length(unique(apply(u, 1, which.max)))
}

centroid_distance_to_grand_mean <- function(centers, gm) {
  d <- sweep(as.matrix(centers), 2, gm, "-")
  sqrt(rowSums(d^2))
}


# -----------------------------------------------------------------------------
# 4. Theoretical collapsed-centroid solution
# -----------------------------------------------------------------------------
#
# For z-score standardization using sample SD:
#   sum_i (x_ij - mean_j)^2 = n - 1
#
# For p variables:
#   total centered SS = (n - 1) * p
#
# If c centroids are identical at the grand mean, all memberships are 1/c.
# Therefore:
#
#   Jcollapse = c * (1/c)^m * total_SS
#             = c^(1-m) * total_SS
#
# For n=514, p=15, c=4, m=2:
#   Jcollapse = 1923.75
# -----------------------------------------------------------------------------

n <- nrow(x)
p <- ncol(x)
c <- NCLUSTER

grand_mean <- colMeans(x)
total_ss <- sum(sweep(x, 2, grand_mean, "-")^2)

collapsed_centers <- matrix(
  rep(grand_mean, each = c),
  nrow = c,
  byrow = FALSE
)

collapsed_fitness_direct <- alg_env$optimizer_fitness(
  x, collapsed_centers, M, DISTANCE, ORDER
)

collapsed_fitness_theory <- (c^(1 - M)) * total_ss

collapsed_u <- alg_env$membership_from_centroids(
  x, collapsed_centers, M, DISTANCE, ORDER
)$u


# -----------------------------------------------------------------------------
# 5. Non-collapsed reference candidates
# -----------------------------------------------------------------------------

set.seed(SEED)
km <- stats::kmeans(
  x,
  centers = NCLUSTER,
  nstart = 50,
  iter.max = 100
)

km_centers <- km$centers

km_fitness <- alg_env$optimizer_fitness(
  x, km_centers, M, DISTANCE, ORDER
)

set.seed(SEED)
random_centers <- alg_env$gen_vi(
  x, NCLUSTER, "uniform", SEED
)

random_fitness <- alg_env$optimizer_fitness(
  x, random_centers, M, DISTANCE, ORDER
)

reference_df <- data.frame(
  solution = c(
    "theoretical_collapsed",
    "kmeans_centers",
    "random_centers"
  ),
  f_obj = c(
    collapsed_fitness_direct,
    km_fitness,
    random_fitness
  ),
  min_centroid_separation = c(
    min_sep(collapsed_centers),
    min_sep(km_centers),
    min_sep(random_centers)
  ),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# 6. Compare V7 optimizer outputs with the theoretical collapse
# -----------------------------------------------------------------------------

v7_rds <- file.path(
  root,
  "validation_results",
  "v7_optimizer_smoke",
  "v7_smoke_results.rds"
)

if (!file.exists(v7_rds)) {
  stop(
    "V7 result RDS not found: ", v7_rds,
    "\nRun V7 first.",
    call. = FALSE
  )
}

v7 <- readRDS(v7_rds)

v7_diag <- do.call(
  rbind,
  lapply(names(v7), function(method) {
    res <- v7[[method]]
    centers <- as.matrix(res$centroid)
    u <- as.matrix(res$membership)

    gm_d <- centroid_distance_to_grand_mean(centers, grand_mean)

    data.frame(
      method = method,
      f_obj = as.numeric(res$f_obj),
      delta_from_theoretical_collapse =
        as.numeric(res$f_obj - collapsed_fitness_theory),
      min_centroid_separation = min_sep(centers),
      mean_centroid_distance_to_grand_mean = mean(gm_d),
      max_centroid_distance_to_grand_mean = max(gm_d),
      mean_max_membership = mean_max_membership(u),
      mean_membership_entropy = membership_entropy(u),
      occupied_clusters = occupied_clusters(u),
      collapse_fitness_flag =
        abs(res$f_obj - collapsed_fitness_theory) <= COLLAPSE_FOBJ_TOL,
      collapse_separation_flag =
        min_sep(centers) <= COLLAPSE_SEP_TOL,
      stringsAsFactors = FALSE
    )
  })
)

rownames(v7_diag) <- NULL


# -----------------------------------------------------------------------------
# 7. Isolate the spatial projection operator
# -----------------------------------------------------------------------------
#
# Start from the SAME non-collapsed k-means centroids.
# No metaheuristic is involved here.
#
# At every step:
#   centroid
#     -> ordinary FCM membership
#     -> FGWC spatial renew_uij
#     -> centroid recomputation
#
# If alpha < 1 drives the centroids toward one another while alpha = 1 does
# not, the collapse mechanism is in the spatial projection dynamics rather
# than in GWO/WOA/TLBO/etc.
# -----------------------------------------------------------------------------

projection_rows <- list()
idx <- 0L

for (alpha in ALPHA_GRID) {
  beta <- 1 - alpha
  centers <- km_centers

  # step 0
  u0 <- alg_env$membership_from_centroids(
    x, centers, M, DISTANCE, ORDER
  )$u

  idx <- idx + 1L
  projection_rows[[idx]] <- data.frame(
    alpha = alpha,
    step = 0L,
    f_obj = alg_env$optimizer_fitness(
      x, centers, M, DISTANCE, ORDER
    ),
    spatial_obj = alg_env$optimizer_spatial_objective(
      x, u0, centers, M, DISTANCE, ORDER
    ),
    min_centroid_separation = min_sep(centers),
    mean_max_membership = mean_max_membership(u0),
    mean_membership_entropy = membership_entropy(u0),
    occupied_clusters = occupied_clusters(u0),
    stringsAsFactors = FALSE
  )

  for (step in seq_len(N_PROJECTION_STEPS)) {
    base_u <- alg_env$membership_from_centroids(
      x, centers, M, DISTANCE, ORDER
    )$u

    spatial_u <- alg_env$renew_uij(
      x,
      base_u,
      mimj,
      distmat,
      alpha,
      beta,
      A,
      B
    )

    centers <- alg_env$centroid_from_membership(
      x,
      spatial_u,
      M
    )

    idx <- idx + 1L
    projection_rows[[idx]] <- data.frame(
      alpha = alpha,
      step = step,
      f_obj = alg_env$optimizer_fitness(
        x, centers, M, DISTANCE, ORDER
      ),
      spatial_obj = alg_env$optimizer_spatial_objective(
        x, spatial_u, centers, M, DISTANCE, ORDER
      ),
      min_centroid_separation = min_sep(centers),
      mean_max_membership = mean_max_membership(spatial_u),
      mean_membership_entropy = membership_entropy(spatial_u),
      occupied_clusters = occupied_clusters(spatial_u),
      stringsAsFactors = FALSE
    )
  }
}

projection_df <- do.call(rbind, projection_rows)


# -----------------------------------------------------------------------------
# 8. Static TLBO implementation audit
# -----------------------------------------------------------------------------

tlbo_path <- file.path(
  root, "inst", "app", "R", "shared", "function", "tlbofgwc.R"
)

tlbo_text <- paste(
  readLines(tlbo_path, encoding = "UTF-8", warn = FALSE),
  collapse = "\n"
)

tlbo_audit <- data.frame(
  check = c(
    "continuous_teaching_factor_pattern",
    "canonical_round_teaching_factor_pattern",
    "nselection_declared",
    "nselection_used_beyond_signature"
  ),
  detected = c(
    grepl("1\\s*\\+\\s*runif", tlbo_text, perl = TRUE),
    grepl("round\\s*\\(\\s*1\\s*\\+\\s*runif", tlbo_text, perl = TRUE),
    grepl("nselection", tlbo_text, fixed = TRUE),
    {
      hits <- gregexpr("nselection", tlbo_text, fixed = TRUE)[[1]]
      n_hits <- if (hits[1] == -1L) 0L else length(hits)
      n_hits > 3L
    }
  ),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# 9. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7_1_degeneracy"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  reference_df,
  file.path(out_dir, "v7_1_reference_objectives.csv"),
  row.names = FALSE
)

utils::write.csv(
  v7_diag,
  file.path(out_dir, "v7_1_v7_optimizer_collapse_diagnostics.csv"),
  row.names = FALSE
)

utils::write.csv(
  projection_df,
  file.path(out_dir, "v7_1_spatial_projection_trajectory.csv"),
  row.names = FALSE
)

utils::write.csv(
  tlbo_audit,
  file.path(out_dir, "v7_1_tlbo_static_audit.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    theoretical = list(
      grand_mean = grand_mean,
      total_ss = total_ss,
      collapsed_fitness_theory = collapsed_fitness_theory,
      collapsed_fitness_direct = collapsed_fitness_direct
    ),
    reference = reference_df,
    v7 = v7_diag,
    projection = projection_df,
    tlbo_audit = tlbo_audit
  ),
  file.path(out_dir, "v7_1_diagnostic_results.rds")
)


# -----------------------------------------------------------------------------
# 10. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7.1 — Objective & Spatial Projection Degeneracy Diagnostic\n")
cat("============================================================\n")

cat("\n[1] THEORETICAL COLLAPSED SOLUTION\n")
cat("n                          :", n, "\n")
cat("p                          :", p, "\n")
cat("c                          :", c, "\n")
cat("m                          :", M, "\n")
cat("Total centered SS          :", format(total_ss, digits = 12), "\n")
cat("Theory collapsed objective :", format(collapsed_fitness_theory, digits = 12), "\n")
cat("Direct collapsed objective :", format(collapsed_fitness_direct, digits = 12), "\n")
cat(
  "Absolute difference        :",
  format(abs(collapsed_fitness_theory - collapsed_fitness_direct), digits = 12),
  "\n"
)

cat("\n[2] REFERENCE CANDIDATES\n")
print(reference_df, row.names = FALSE)

cat("\n[3] V7 OPTIMIZER DISTANCE FROM COLLAPSE\n")
print(
  v7_diag[, c(
    "method",
    "f_obj",
    "delta_from_theoretical_collapse",
    "min_centroid_separation",
    "mean_max_membership",
    "occupied_clusters",
    "collapse_fitness_flag",
    "collapse_separation_flag"
  )],
  row.names = FALSE
)

cat("\n[4] SPATIAL PROJECTION — FINAL STEP BY ALPHA\n")
projection_final <- projection_df[
  projection_df$step == N_PROJECTION_STEPS,
  ,
  drop = FALSE
]
print(
  projection_final[, c(
    "alpha",
    "step",
    "f_obj",
    "spatial_obj",
    "min_centroid_separation",
    "mean_max_membership",
    "occupied_clusters"
  )],
  row.names = FALSE
)

cat("\n[5] TLBO STATIC AUDIT\n")
print(tlbo_audit, row.names = FALSE)

near_collapse_count <- sum(
  v7_diag$collapse_fitness_flag |
    v7_diag$collapse_separation_flag
)

cat("\n============================================================\n")
cat("DIAGNOSTIC FLAGS\n")
cat("============================================================\n")
cat(
  "V7 methods near theoretical collapse:",
  near_collapse_count, "/", nrow(v7_diag), "\n"
)

alpha07 <- projection_df[
  projection_df$alpha == 0.7 &
    projection_df$step == N_PROJECTION_STEPS,
  ,
  drop = FALSE
]

alpha10 <- projection_df[
  projection_df$alpha == 1.0 &
    projection_df$step == N_PROJECTION_STEPS,
  ,
  drop = FALSE
]

if (nrow(alpha07) == 1L && nrow(alpha10) == 1L) {
  cat(
    "alpha=0.7 final min separation:",
    format(alpha07$min_centroid_separation, digits = 8), "\n"
  )
  cat(
    "alpha=1.0 final min separation:",
    format(alpha10$min_centroid_separation, digits = 8), "\n"
  )
}

cat("\nInterpretation rule:\n")
cat(
  "- If k-means centers have an optimizer_fitness clearly BELOW 1923.75,\n",
  "  then jfgwcv itself is capable of preferring a non-collapsed solution.\n",
  sep = ""
)
cat(
  "- If repeated alpha=0.7 spatial projection drives min centroid separation\n",
  "  toward zero and f_obj toward 1923.75 while alpha=1 does not, the primary\n",
  "  collapse mechanism is the FGWC spatial projection dynamics / its coupling\n",
  "  to the optimizer representation, not a specific metaheuristic.\n",
  sep = ""
)
cat(
  "- If both conditions hold, do NOT patch TLBO alone and do NOT proceed to V8.\n",
  "  Redesign the optimizer-to-FGWC coupling/objective first.\n",
  sep = ""
)

cat("\nResults written to:\n  ", out_dir, "\n", sep = "")
