# =============================================================================
# soviclust — V7.6 Spatial Candidate Reachability Diagnostic
# =============================================================================
#
# Purpose
# -------
# Determine whether the five V7.5-stagnant optimizers can generate at least one
# candidate that improves the ACTUAL spatial FGWC partition when evaluated
# consistently as the pair:
#
#     raw candidate centroid
#        -> base membership
#        -> spatial membership U*
#        -> spatial centroid V*
#        -> spatial Xie-Beni fitness XB(U*, V*)
#
# Hard-cluster occupancy is imposed only for this diagnostic:
# candidates with fewer than `ncluster` hard clusters receive Inf fitness.
#
# Methods diagnosed:
#   ABC, GSA, GWO, TLBO, WOA
#
# This script does NOT modify package source code.
#
# Run:
#   source("validation/v7_6_spatial_candidate_reachability.R")
#
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

SEED <- 2026L
NCLUSTER <- 4L
M <- 2
ALPHA <- 0.7
BETA <- 1 - ALPHA
A_SPATIAL <- 1
B_SPATIAL <- 1
DISTANCE <- "euclidean"
ORDER <- 2
POP_SIZE <- 20L

IMPROVEMENT_TOL <- 1e-10


# -----------------------------------------------------------------------------
# 1. Project/source setup
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
# 2. Load V7 dataset
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

popmat <- matrix(pop, ncol = 1)
mimj <- popmat %*% t(popmat)


# -----------------------------------------------------------------------------
# 3. Unified spatial candidate evaluator
# -----------------------------------------------------------------------------

hard_counts <- function(u, k = NCLUSTER) {
  tabulate(
    apply(as.matrix(u), 1, which.max),
    nbins = k
  )
}


evaluate_spatial_candidate <- function(raw_centers,
                                       require_all_clusters = TRUE) {
  raw_centers <- alg_env$clamp_centroids(
    as.matrix(raw_centers),
    x
  )

  base_u <- alg_env$membership_from_centroids(
    x,
    raw_centers,
    M,
    DISTANCE,
    ORDER
  )$u

  spatial_u <- alg_env$renew_uij(
    x,
    base_u,
    mimj,
    distmat,
    ALPHA,
    BETA,
    A_SPATIAL,
    B_SPATIAL
  )

  spatial_centers <- alg_env$centroid_from_membership(
    x,
    spatial_u,
    M
  )

  counts <- hard_counts(spatial_u)
  occupied <- sum(counts > 0L)

  xb <- alg_env$XB1(
    data = x,
    uij = spatial_u,
    vi = spatial_centers,
    m = M
  )

  feasible <- is.finite(xb) &&
    (!require_all_clusters || occupied == NCLUSTER)

  fitness <- if (feasible) xb else Inf

  list(
    raw_centers = raw_centers,
    membership = spatial_u,
    centers = spatial_centers,
    raw_xb = xb,
    fitness = fitness,
    occupied = occupied,
    min_cluster_size = min(counts),
    cluster_sizes = counts
  )
}


# -----------------------------------------------------------------------------
# 4. Reconstruct common initial population
# -----------------------------------------------------------------------------
#
# init.swarm() already produces spatial membership + spatial centroids.
# For the current-state fitness we evaluate that existing pair directly,
# avoiding a second projection.
# -----------------------------------------------------------------------------

# Temporary base-XB override only to make init.swarm() construct its standard
# structure. Its stored I is ignored below.
tmp_fitness <- function(data, centers, m,
                        distance = "euclidean", order = 2, ...) {
  u <- alg_env$membership_from_centroids(
    data, centers, m, distance, order
  )$u

  alg_env$XB1(
    data = data,
    uij = u,
    vi = centers,
    m = m
  )
}

optimizer_envs$IFA$optimizer_fitness <- tmp_fitness

init <- optimizer_envs$IFA$init.swarm(
  data = x,
  pop = mimj,
  distmat = distmat,
  distance = DISTANCE,
  order = ORDER,
  vi.dist = "uniform",
  ncluster = NCLUSTER,
  m = M,
  alpha = ALPHA,
  a = A_SPATIAL,
  b = B_SPATIAL,
  randomN = SEED,
  nfly = POP_SIZE
)

current_centers <- init$centroid
current_membership <- init$membership

current_fitness <- vapply(
  seq_len(POP_SIZE),
  function(i) {
    counts <- hard_counts(current_membership[[i]])

    xb <- alg_env$XB1(
      data = x,
      uij = current_membership[[i]],
      vi = current_centers[[i]],
      m = M
    )

    if (sum(counts > 0L) == NCLUSTER && is.finite(xb)) {
      xb
    } else {
      Inf
    }
  },
  numeric(1)
)

current_best_idx <- which.min(current_fitness)
current_best <- current_fitness[current_best_idx]
gbest <- current_centers[[current_best_idx]]


# -----------------------------------------------------------------------------
# 5. Candidate scoring utility
# -----------------------------------------------------------------------------

score_candidates <- function(method, raw_candidates,
                             parent_index = seq_along(raw_candidates)) {
  evals <- lapply(
    raw_candidates,
    evaluate_spatial_candidate
  )

  cand_fit <- vapply(evals, `[[`, numeric(1), "fitness")
  occupied <- vapply(evals, `[[`, integer(1), "occupied")
  min_size <- vapply(evals, `[[`, integer(1), "min_cluster_size")

  parent_fit <- current_fitness[parent_index]

  data.frame(
    method = method,
    candidate = seq_along(raw_candidates),
    parent_index = parent_index,
    parent_fitness = parent_fit,
    candidate_fitness = cand_fit,
    improves_parent =
      is.finite(cand_fit) &
      (parent_fit - cand_fit) > IMPROVEMENT_TOL,
    improves_global_best =
      is.finite(cand_fit) &
      (current_best - cand_fit) > IMPROVEMENT_TOL,
    occupied = occupied,
    min_cluster_size = min_size,
    feasible = is.finite(cand_fit),
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# 6A. ABC employed-bee candidates
# -----------------------------------------------------------------------------

abc_env <- optimizer_envs$ABC

abc_raw <- abc_env$employed.bee(
  swarm = current_centers,
  fitness = current_fitness,
  pso = FALSE,
  gbest = gbest,
  seed = SEED,
  data = x,
  m = M,
  distance = DISTANCE,
  order = ORDER,
  mi.mj = mimj,
  dist = distmat,
  alpha = ALPHA,
  beta = BETA,
  a = A_SPATIAL,
  b = B_SPATIAL
)

abc_df <- score_candidates(
  "ABC_employed",
  abc_raw
)


# -----------------------------------------------------------------------------
# 6B. GSA one-step candidates
# -----------------------------------------------------------------------------

gsa_env <- optimizer_envs$GSA

# Build the structure expected by force_v().
gsa_par <- list(
  centroid = current_centers,
  membership = current_membership,
  I = current_fitness
)

v0 <- lapply(
  seq_len(POP_SIZE),
  function(i) {
    matrix(
      0,
      nrow = NCLUSTER,
      ncol = ncol(x)
    )
  }
)

gsa_v <- gsa_env$force_v(
  par = gsa_par,
  no = 2L,
  G = 1,
  v = v0,
  vmax = 0.7,
  par.dist = "euclidean",
  par.order = 2,
  randomN = SEED
)

gsa_raw <- lapply(
  seq_len(POP_SIZE),
  function(i) current_centers[[i]] + gsa_v[[i]]
)

gsa_df <- score_candidates(
  "GSA",
  gsa_raw
)


# -----------------------------------------------------------------------------
# 6C. GWO one-step candidates
# -----------------------------------------------------------------------------

sorted_idx <- order(current_fitness)
alpha_pos <- current_centers[[sorted_idx[1L]]]
beta_pos <- current_centers[[sorted_idx[2L]]]
delta_pos <- current_centers[[sorted_idx[3L]]]

# First-iteration canonical coefficient used by the current implementation.
a_coef <- 2

gwo_raw <- lapply(
  seq_len(POP_SIZE),
  function(i) {
    set.seed(SEED + 1L * 10000L + i)

    dd <- dim(alpha_pos)

    r1_a <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    r2_a <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    A1 <- 2 * a_coef * r1_a - a_coef
    C1 <- 2 * r2_a
    X1 <- alpha_pos -
      A1 * abs(C1 * alpha_pos - current_centers[[i]])

    r1_b <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    r2_b <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    A2 <- 2 * a_coef * r1_b - a_coef
    C2 <- 2 * r2_b
    X2 <- beta_pos -
      A2 * abs(C2 * beta_pos - current_centers[[i]])

    r1_d <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    r2_d <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
    A3 <- 2 * a_coef * r1_d - a_coef
    C3 <- 2 * r2_d
    X3 <- delta_pos -
      A3 * abs(C3 * delta_pos - current_centers[[i]])

    alg_env$clamp_centroids(
      (X1 + X2 + X3) / 3,
      x
    )
  }
)

gwo_df <- score_candidates(
  "GWO",
  gwo_raw
)


# -----------------------------------------------------------------------------
# 6D. TLBO teacher phase — current vs canonical teaching factor
# -----------------------------------------------------------------------------

teacher <- current_centers[[current_best_idx]]
class_average <- Reduce("+", current_centers) / POP_SIZE

# Current implementation: element-wise continuous TF in [1,2).
set.seed(SEED + 2L)
tf_current <- matrix(
  1 + runif(ncol(teacher) * nrow(teacher)),
  ncol = ncol(teacher)
)
set.seed(SEED + 3L)
r_current <- matrix(
  runif(ncol(teacher) * nrow(teacher)),
  ncol = ncol(teacher)
)

diff_current <- r_current * (
  teacher - tf_current * class_average
)

tlbo_current_raw <- lapply(
  current_centers,
  function(v) v + diff_current
)

tlbo_current_df <- score_candidates(
  "TLBO_current_TF",
  tlbo_current_raw
)

# Canonical TLBO diagnostic: scalar TF in {1,2}.
set.seed(SEED + 2L)
tf_canonical <- round(1 + runif(1))
set.seed(SEED + 3L)
r_canonical <- matrix(
  runif(ncol(teacher) * nrow(teacher)),
  ncol = ncol(teacher)
)

diff_canonical <- r_canonical * (
  teacher - tf_canonical * class_average
)

tlbo_canonical_raw <- lapply(
  current_centers,
  function(v) v + diff_canonical
)

tlbo_canonical_df <- score_candidates(
  "TLBO_canonical_TF",
  tlbo_canonical_raw
)


# -----------------------------------------------------------------------------
# 6E. WOA one-step candidates
# -----------------------------------------------------------------------------

woa_env <- optimizer_envs$WOA

woa_raw <- woa_env$woa.move(
  swarm = current_centers,
  prey = gbest,
  a_coef = 2,
  b = 1,
  nwhale = POP_SIZE,
  seed = SEED,
  iter = 1L,
  data = x
)

woa_df <- score_candidates(
  "WOA",
  woa_raw
)


# -----------------------------------------------------------------------------
# 7. Combine and summarize
# -----------------------------------------------------------------------------

candidate_df <- rbind(
  abc_df,
  gsa_df,
  gwo_df,
  tlbo_current_df,
  tlbo_canonical_df,
  woa_df
)

summary_df <- do.call(
  rbind,
  lapply(
    split(candidate_df, candidate_df$method),
    function(d) {
      finite_vals <- d$candidate_fitness[is.finite(d$candidate_fitness)]

      data.frame(
        method = d$method[1L],
        current_global_best = current_best,
        candidates = nrow(d),
        feasible_candidates = sum(d$feasible),
        parent_improvements = sum(d$improves_parent),
        global_best_improvements = sum(d$improves_global_best),
        best_candidate_fitness =
          if (length(finite_vals)) min(finite_vals) else Inf,
        best_improvement_over_global =
          if (length(finite_vals)) {
            current_best - min(finite_vals)
          } else {
            -Inf
          },
        median_candidate_fitness =
          if (length(finite_vals)) {
            stats::median(finite_vals)
          } else {
            Inf
          },
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(summary_df) <- NULL


# -----------------------------------------------------------------------------
# 8. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7_6_candidate_reachability"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  data.frame(
    candidate = seq_len(POP_SIZE),
    current_spatial_XB = current_fitness,
    is_global_best = seq_len(POP_SIZE) == current_best_idx,
    stringsAsFactors = FALSE
  ),
  file.path(out_dir, "v7_6_initial_spatial_fitness.csv"),
  row.names = FALSE
)

utils::write.csv(
  candidate_df,
  file.path(out_dir, "v7_6_candidate_detail.csv"),
  row.names = FALSE
)

utils::write.csv(
  summary_df,
  file.path(out_dir, "v7_6_reachability_summary.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    initial_fitness = current_fitness,
    initial_best_index = current_best_idx,
    initial_best = current_best,
    candidates = candidate_df,
    summary = summary_df,
    tlbo_tf = list(
      current_elementwise_tf = tf_current,
      canonical_scalar_tf = tf_canonical
    )
  ),
  file.path(out_dir, "v7_6_results.rds")
)


# -----------------------------------------------------------------------------
# 9. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7.6 — Spatial Candidate Reachability Diagnostic\n")
cat("============================================================\n")
cat("Population               :", POP_SIZE, "\n")
cat("Initial spatial best idx :", current_best_idx, "\n")
cat("Initial spatial best XB  :", format(current_best, digits = 12), "\n")
cat("Occupancy constraint     : all", NCLUSTER, "hard clusters required\n")
cat("============================================================\n\n")

print(
  summary_df[
    order(summary_df$method),
    ,
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\nTLBO canonical scalar TF used in diagnostic:", tf_canonical, "\n")

cat("\n============================================================\n")
cat("V7.6 INTERPRETATION\n")
cat("============================================================\n")

for (i in seq_len(nrow(summary_df))) {
  d <- summary_df[i, ]

  if (d$global_best_improvements > 0L) {
    cat(
      d$method, ": REACHABLE — operator generated",
      d$global_best_improvements,
      "candidate(s) better than the current spatial-XB global best.\n"
    )
  } else if (d$parent_improvements > 0L) {
    cat(
      d$method, ": PARTIALLY REACHABLE — improves parent candidates but not",
      "the current global best in this first-step diagnostic.\n"
    )
  } else {
    cat(
      d$method, ": NO FIRST-STEP IMPROVEMENT — no feasible candidate improved",
      "its parent under the unified spatial evaluator.\n"
    )
  }
}

cat("\nDecision rule:\n")
cat(
  "- If a previously stagnant optimizer is REACHABLE here, its movement\n",
  "  operator can work under spatial-XB and the current package's evaluation/\n",
  "  selection pipeline is the primary redesign target.\n",
  sep = ""
)
cat(
  "- If it remains NO FIRST-STEP IMPROVEMENT, inspect its operator scaling,\n",
  "  bounds, or algorithm-specific implementation before Patch v3.\n",
  sep = ""
)
cat(
  "- Compare TLBO_current_TF vs TLBO_canonical_TF to determine whether the\n",
  "  non-canonical continuous teaching factor materially affects reachability.\n",
  sep = ""
)

cat("\nResults written to:\n  ", out_dir, "\n", sep = "")
