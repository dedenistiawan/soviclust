# =============================================================================
# soviclust — V7.8 Operator Step-Scale & Boundary Diagnostic
# =============================================================================
#
# Purpose
# -------
# V7.6/V7.7 showed that ABC, GSA, GWO, TLBO, and WOA generate no improving
# first-step candidates under a unified spatial-XB evaluator.
#
# V7.8 separates:
#
#   A. bad movement DIRECTION
#   B. useful direction but excessive STEP MAGNITUDE (overshoot)
#
# For every canonical first-step candidate:
#
#   delta = candidate - parent
#
# evaluate:
#
#   parent + lambda * delta
#
# for a fixed diagnostic grid of lambda values.
#
# This is a DIAGNOSTIC ONLY. It does not modify optimizer formulas or package
# source files.
#
# If lambda = 1 fails but smaller lambda values improve spatial XB, the operator
# direction is productive and the primary issue is movement scaling / boundary
# handling.
#
# Run:
#
#   source("validation/v7_8_operator_step_scale_diagnostic.R")
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

LAMBDA_GRID <- c(
  1.00, 0.75, 0.50, 0.25, 0.10,
  0.05, 0.02, 0.01, 0.005
)

IMPROVEMENT_TOL <- 1e-10


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
  root,
  "tests",
  "testthat",
  "helper-fgwc-algorithms.R"
)

if (!file.exists(helper)) {
  stop("Missing helper-fgwc-algorithms.R", call. = FALSE)
}

source(helper, local = .GlobalEnv)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 2. Load the same V7 validation data
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

x_raw <- data.matrix(
  raw_data[, indicator_cols, drop = FALSE]
)

x <- unclass(scale(x_raw))
storage.mode(x) <- "double"

pop_df <- as.data.frame(readxl::read_excel(pop_path))

numeric_pop <- which(
  vapply(pop_df, is.numeric, logical(1))
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

pop <- as.numeric(pop_df[[pop_col]])

dist_df <- as.data.frame(readxl::read_excel(dist_path))

if (
  ncol(dist_df) == nrow(x) + 1L ||
  !is.numeric(dist_df[[1L]])
) {
  dist_df <- dist_df[, -1L, drop = FALSE]
}

distmat <- data.matrix(dist_df)
diag(distmat) <- 0

popmat <- matrix(pop, ncol = 1)
mimj <- popmat %*% t(popmat)


# Data bounds used by clamp_centroids().
feature_min <- apply(x, 2, min)
feature_max <- apply(x, 2, max)
feature_range <- feature_max - feature_min

feature_range_safe <- pmax(
  feature_range,
  .Machine$double.eps
)


# -----------------------------------------------------------------------------
# 3. Unified spatial evaluator
# -----------------------------------------------------------------------------

hard_counts <- function(u, k = NCLUSTER) {
  tabulate(
    apply(as.matrix(u), 1, which.max),
    nbins = k
  )
}


evaluate_spatial_candidate <- function(raw_centers) {
  raw_centers <- as.matrix(raw_centers)

  bounded_centers <- alg_env$clamp_centroids(
    raw_centers,
    x
  )

  base_u <- alg_env$membership_from_centroids(
    x,
    bounded_centers,
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

  feasible <- (
    occupied == NCLUSTER &&
    is.finite(xb)
  )

  list(
    fitness = if (feasible) xb else Inf,
    raw_xb = xb,
    occupied = occupied,
    min_cluster_size = min(counts),
    bounded_centers = bounded_centers,
    spatial_centers = spatial_centers,
    spatial_membership = spatial_u
  )
}


# -----------------------------------------------------------------------------
# 4. Reconstruct the same initial spatial population
# -----------------------------------------------------------------------------

tmp_fitness <- function(data, centers, m,
                        distance = "euclidean",
                        order = 2, ...) {
  u <- alg_env$membership_from_centroids(
    data,
    centers,
    m,
    distance,
    order
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

    if (
      sum(counts > 0L) == NCLUSTER &&
      is.finite(xb)
    ) {
      xb
    } else {
      Inf
    }
  },
  numeric(1)
)

best_idx <- which.min(current_fitness)
best_fit <- current_fitness[best_idx]
gbest <- current_centers[[best_idx]]


# -----------------------------------------------------------------------------
# 5. Generate the exact first-step raw candidates used in V7.6
# -----------------------------------------------------------------------------

candidate_sets <- list()


# ABC -------------------------------------------------------------------------

candidate_sets$ABC <- optimizer_envs$ABC$employed.bee(
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


# GSA -------------------------------------------------------------------------

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

gsa_velocity <- optimizer_envs$GSA$force_v(
  par = gsa_par,
  no = 2L,
  G = 1,
  v = v0,
  vmax = 0.7,
  par.dist = "euclidean",
  par.order = 2,
  randomN = SEED
)

candidate_sets$GSA <- lapply(
  seq_len(POP_SIZE),
  function(i) {
    current_centers[[i]] + gsa_velocity[[i]]
  }
)


# GWO -------------------------------------------------------------------------

ord <- order(current_fitness)

alpha_pos <- current_centers[[ord[1L]]]
beta_pos <- current_centers[[ord[2L]]]
delta_pos <- current_centers[[ord[3L]]]

a_coef <- 2

candidate_sets$GWO <- lapply(
  seq_len(POP_SIZE),
  function(i) {
    set.seed(SEED + 10000L + i)

    dd <- dim(alpha_pos)

    r1 <- matrix(
      runif(prod(dd)),
      nrow = dd[1],
      ncol = dd[2]
    )

    r2 <- matrix(
      runif(prod(dd)),
      nrow = dd[1],
      ncol = dd[2]
    )

    A1 <- 2 * a_coef * r1 - a_coef
    C1 <- 2 * r2

    X1 <- alpha_pos -
      A1 * abs(
        C1 * alpha_pos -
          current_centers[[i]]
      )

    r1 <- matrix(
      runif(prod(dd)),
      nrow = dd[1],
      ncol = dd[2]
    )

    r2 <- matrix(
      runif(prod(dd)),
      nrow = dd[1],
      ncol = dd[2]
    )

    A2 <- 2 * a_coef * r1 - a_coef
    C2 <- 2 * r2

    X2 <- beta_pos -
      A2 * abs(
        C2 * beta_pos -
          current_centers[[i]]
      )

    r1 <- matrix(
      runif(prod(dd)),
      nrow = dd[1],
      ncol = dd[2]
    )

    r2 <- matrix(
      runif(prod(dd)),
      nrow = dd[1],
      ncol = dd[2]
    )

    A3 <- 2 * a_coef * r1 - a_coef
    C3 <- 2 * r2

    X3 <- delta_pos -
      A3 * abs(
        C3 * delta_pos -
          current_centers[[i]]
      )

    (X1 + X2 + X3) / 3
  }
)


# TLBO current TF -------------------------------------------------------------

teacher <- current_centers[[best_idx]]
class_average <- Reduce("+", current_centers) / POP_SIZE

set.seed(SEED + 2L)

tf_current <- matrix(
  1 + runif(
    nrow(teacher) * ncol(teacher)
  ),
  ncol = ncol(teacher)
)

set.seed(SEED + 3L)

r_current <- matrix(
  runif(
    nrow(teacher) * ncol(teacher)
  ),
  ncol = ncol(teacher)
)

diff_current <- r_current * (
  teacher -
    tf_current * class_average
)

candidate_sets$TLBO_current <- lapply(
  current_centers,
  function(v) v + diff_current
)


# TLBO canonical TF -----------------------------------------------------------

set.seed(SEED + 2L)
tf_canonical <- round(1 + runif(1))

set.seed(SEED + 3L)

r_canonical <- matrix(
  runif(
    nrow(teacher) * ncol(teacher)
  ),
  ncol = ncol(teacher)
)

diff_canonical <- r_canonical * (
  teacher -
    tf_canonical * class_average
)

candidate_sets$TLBO_canonical <- lapply(
  current_centers,
  function(v) v + diff_canonical
)


# WOA -------------------------------------------------------------------------

candidate_sets$WOA <- optimizer_envs$WOA$woa.move(
  swarm = current_centers,
  prey = gbest,
  a_coef = 2,
  b = 1,
  nwhale = POP_SIZE,
  seed = SEED,
  iter = 1L,
  data = NULL
)


# -----------------------------------------------------------------------------
# 6. Movement diagnostics
# -----------------------------------------------------------------------------

outside_fraction <- function(mat) {
  mat <- as.matrix(mat)

  lower <- matrix(
    feature_min,
    nrow = nrow(mat),
    ncol = ncol(mat),
    byrow = TRUE
  )

  upper <- matrix(
    feature_max,
    nrow = nrow(mat),
    ncol = ncol(mat),
    byrow = TRUE
  )

  mean(mat < lower | mat > upper)
}


movement_rows <- list()
mr <- 0L

for (method in names(candidate_sets)) {
  candidates <- candidate_sets[[method]]

  for (i in seq_len(POP_SIZE)) {
    parent <- current_centers[[i]]
    cand <- candidates[[i]]
    delta <- cand - parent

    normalized_delta <- sweep(
      delta,
      2,
      feature_range_safe,
      "/"
    )

    mr <- mr + 1L

    movement_rows[[mr]] <- data.frame(
      method = method,
      candidate = i,
      parent_fitness = current_fitness[i],
      raw_step_frobenius = sqrt(sum(delta^2)),
      normalized_step_rms = sqrt(
        mean(normalized_delta^2)
      ),
      full_step_outside_fraction =
        outside_fraction(cand),
      raw_candidate_distance_from_global_best =
        sqrt(sum((cand - gbest)^2)),
      stringsAsFactors = FALSE
    )
  }
}

movement_df <- do.call(
  rbind,
  movement_rows
)


# -----------------------------------------------------------------------------
# 7. Diagnostic line-search along the SAME movement direction
# -----------------------------------------------------------------------------

line_rows <- list()
lr <- 0L

for (method in names(candidate_sets)) {
  candidates <- candidate_sets[[method]]

  for (i in seq_len(POP_SIZE)) {
    parent <- current_centers[[i]]
    raw_candidate <- candidates[[i]]
    delta <- raw_candidate - parent

    for (lambda in LAMBDA_GRID) {
      trial_raw <- parent + lambda * delta

      ev <- evaluate_spatial_candidate(
        trial_raw
      )

      lr <- lr + 1L

      line_rows[[lr]] <- data.frame(
        method = method,
        candidate = i,
        lambda = lambda,
        parent_fitness = current_fitness[i],
        candidate_fitness = ev$fitness,
        improves_parent =
          is.finite(ev$fitness) &&
          (
            current_fitness[i] -
              ev$fitness
          ) > IMPROVEMENT_TOL,
        improves_global_best =
          is.finite(ev$fitness) &&
          (
            best_fit -
              ev$fitness
          ) > IMPROVEMENT_TOL,
        occupied = ev$occupied,
        min_cluster_size =
          ev$min_cluster_size,
        preclamp_outside_fraction =
          outside_fraction(trial_raw),
        stringsAsFactors = FALSE
      )
    }
  }
}

line_df <- do.call(
  rbind,
  line_rows
)


# -----------------------------------------------------------------------------
# 8. Summaries
# -----------------------------------------------------------------------------

lambda_summary <- do.call(
  rbind,
  lapply(
    split(
      line_df,
      interaction(
        line_df$method,
        line_df$lambda,
        drop = TRUE
      )
    ),
    function(d) {
      vals <- d$candidate_fitness[
        is.finite(d$candidate_fitness)
      ]

      data.frame(
        method = d$method[1L],
        lambda = d$lambda[1L],
        feasible_candidates =
          sum(is.finite(d$candidate_fitness)),
        parent_improvements =
          sum(d$improves_parent),
        global_best_improvements =
          sum(d$improves_global_best),
        best_candidate_fitness =
          if (length(vals)) {
            min(vals)
          } else {
            Inf
          },
        median_candidate_fitness =
          if (length(vals)) {
            stats::median(vals)
          } else {
            Inf
          },
        median_outside_fraction =
          stats::median(
            d$preclamp_outside_fraction
          ),
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(lambda_summary) <- NULL


method_summary <- do.call(
  rbind,
  lapply(
    split(line_df, line_df$method),
    function(d) {
      improving <- d[
        d$improves_global_best,
        ,
        drop = FALSE
      ]

      parent_improving <- d[
        d$improves_parent,
        ,
        drop = FALSE
      ]

      finite_vals <- d$candidate_fitness[
        is.finite(d$candidate_fitness)
      ]

      data.frame(
        method = d$method[1L],
        any_parent_improvement =
          nrow(parent_improving) > 0L,
        any_global_best_improvement =
          nrow(improving) > 0L,
        best_lambda_global =
          if (nrow(improving)) {
            improving$lambda[
              which.min(
                improving$candidate_fitness
              )
            ]
          } else {
            NA_real_
          },
        best_global_candidate_fitness =
          if (nrow(improving)) {
            min(improving$candidate_fitness)
          } else if (length(finite_vals)) {
            min(finite_vals)
          } else {
            Inf
          },
        largest_lambda_with_parent_improvement =
          if (nrow(parent_improving)) {
            max(parent_improving$lambda)
          } else {
            NA_real_
          },
        smallest_lambda_with_parent_improvement =
          if (nrow(parent_improving)) {
            min(parent_improving$lambda)
          } else {
            NA_real_
          },
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(method_summary) <- NULL


movement_summary <- do.call(
  rbind,
  lapply(
    split(movement_df, movement_df$method),
    function(d) {
      data.frame(
        method = d$method[1L],
        median_raw_step =
          stats::median(
            d$raw_step_frobenius
          ),
        max_raw_step =
          max(d$raw_step_frobenius),
        median_normalized_step_rms =
          stats::median(
            d$normalized_step_rms
          ),
        max_normalized_step_rms =
          max(d$normalized_step_rms),
        median_full_step_outside_fraction =
          stats::median(
            d$full_step_outside_fraction
          ),
        max_full_step_outside_fraction =
          max(
            d$full_step_outside_fraction
          ),
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(movement_summary) <- NULL


# -----------------------------------------------------------------------------
# 9. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7_8_step_scale"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  movement_df,
  file.path(
    out_dir,
    "v7_8_movement_detail.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  movement_summary,
  file.path(
    out_dir,
    "v7_8_movement_summary.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  line_df,
  file.path(
    out_dir,
    "v7_8_lambda_candidate_detail.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  lambda_summary,
  file.path(
    out_dir,
    "v7_8_lambda_summary.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  method_summary,
  file.path(
    out_dir,
    "v7_8_method_decision.csv"
  ),
  row.names = FALSE
)

saveRDS(
  list(
    movement = movement_df,
    movement_summary = movement_summary,
    lambda_detail = line_df,
    lambda_summary = lambda_summary,
    decision = method_summary,
    initial_best_index = best_idx,
    initial_best_spatial_XB = best_fit,
    lambda_grid = LAMBDA_GRID,
    tlbo_canonical_tf = tf_canonical
  ),
  file.path(
    out_dir,
    "v7_8_results.rds"
  )
)


# -----------------------------------------------------------------------------
# 10. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7.8 — Operator Step-Scale & Boundary Diagnostic\n")
cat("============================================================\n")
cat("Population             :", POP_SIZE, "\n")
cat("Initial best agent     :", best_idx, "\n")
cat(
  "Initial best spatial XB:",
  format(best_fit, digits = 12),
  "\n"
)
cat(
  "Lambda grid           :",
  paste(LAMBDA_GRID, collapse = ", "),
  "\n"
)
cat("============================================================\n")

cat("\n[1] FULL-STEP MOVEMENT SCALE\n")
print(
  movement_summary[
    order(movement_summary$method),
    ,
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n[2] REACHABILITY BY STEP MULTIPLIER\n")
print(
  lambda_summary[
    order(
      lambda_summary$method,
      -lambda_summary$lambda
    ),
    c(
      "method",
      "lambda",
      "parent_improvements",
      "global_best_improvements",
      "best_candidate_fitness",
      "median_candidate_fitness",
      "median_outside_fraction"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n[3] METHOD-LEVEL DECISION\n")
print(
  method_summary[
    order(method_summary$method),
    ,
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n============================================================\n")
cat("V7.8 INTERPRETATION\n")
cat("============================================================\n")

for (i in seq_len(nrow(method_summary))) {
  d <- method_summary[i, ]

  if (d$any_global_best_improvement) {
    cat(
      d$method,
      ": OVERSHOOT/SCALING SIGNAL — the canonical direction can beat the",
      " current global best when shortened; best lambda =",
      d$best_lambda_global,
      ".\n"
    )
  } else if (d$any_parent_improvement) {
    cat(
      d$method,
      ": LOCAL SCALING SIGNAL — shortened steps improve at least one parent",
      " but do not beat the global best in this diagnostic.\n"
    )
  } else {
    cat(
      d$method,
      ": DIRECTION/IMPLEMENTATION SIGNAL — no tested shortening of the",
      " canonical first-step direction improves even a parent.\n"
    )
  }
}

cat("\nDecision rule:\n")
cat(
  "- Improvement only at lambda < 1 => full canonical step overshoots under\n",
  "  the current centroid search representation; investigate normalization,\n",
  "  velocity/bound controls, or algorithm-prescribed schedules.\n",
  sep = ""
)
cat(
  "- No improvement at any lambda => step magnitude alone cannot explain the\n",
  "  stagnation; audit the movement formula / state variables against the\n",
  "  canonical algorithm and the naspaclust adaptation.\n",
  sep = ""
)
cat(
  "- A high pre-clamp outside fraction indicates boundary handling is materially\n",
  "  changing the movement and should be standardized before benchmarking.\n",
  sep = ""
)

cat(
  "\nTLBO canonical scalar TF used:",
  tf_canonical,
  "\n"
)

cat(
  "\nResults written to:\n  ",
  out_dir,
  "\n",
  sep = ""
)
