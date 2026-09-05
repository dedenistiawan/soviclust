# =============================================================================
# soviclust — V7.7 Centroid Label-Alignment Reachability Diagnostic
# =============================================================================
#
# Purpose
# -------
# Test whether label switching among centroid rows is responsible for the poor
# one-step reachability observed in V7.6 for:
#
#   ABC, GSA, GWO, TLBO, WOA
#
# A clustering solution with c clusters has c! equivalent centroid-row
# permutations. Metaheuristic movement operators in soviclust currently perform
# row-wise arithmetic across agents. If rows are not aligned, an operator may
# subtract/average geometrically unrelated clusters.
#
# V7.7:
#   1. reconstructs the same spatial initial population;
#   2. aligns every agent to the current global-best centroid using the
#      permutation with minimum Frobenius distance;
#   3. permutes membership columns consistently;
#   4. runs the same one-step operator diagnostics;
#   5. compares UNALIGNED vs ALIGNED reachability under the same unified
#      spatial-XB evaluator and hard-cluster occupancy constraint.
#
# This script DOES NOT modify package source code.
#
# Run:
#   source("validation/v7_7_centroid_label_alignment_diagnostic.R")
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
# 2. Load the same standardized 514 x 15 validation data
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
# 3. Utility: all permutations for c=4
# -----------------------------------------------------------------------------

permute_vec <- function(v) {
  if (length(v) == 1L) {
    return(matrix(v, nrow = 1L))
  }

  out <- NULL

  for (i in seq_along(v)) {
    rest <- v[-i]
    sub <- permute_vec(rest)
    out <- rbind(
      out,
      cbind(v[i], sub)
    )
  }

  out
}

perms <- permute_vec(seq_len(NCLUSTER))


# -----------------------------------------------------------------------------
# 4. Unified spatial candidate evaluator
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

  list(
    membership = spatial_u,
    centers = spatial_centers,
    fitness = if (feasible) xb else Inf,
    raw_xb = xb,
    occupied = occupied,
    min_cluster_size = min(counts)
  )
}


# -----------------------------------------------------------------------------
# 5. Reconstruct initial spatial population
# -----------------------------------------------------------------------------

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

orig_centers <- init$centroid
orig_membership <- init$membership

orig_fitness <- vapply(
  seq_len(POP_SIZE),
  function(i) {
    counts <- hard_counts(orig_membership[[i]])

    xb <- alg_env$XB1(
      data = x,
      uij = orig_membership[[i]],
      vi = orig_centers[[i]],
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

best_idx <- which.min(orig_fitness)
best_fit <- orig_fitness[best_idx]
reference <- orig_centers[[best_idx]]


# -----------------------------------------------------------------------------
# 6. Align every agent to the global-best row ordering
# -----------------------------------------------------------------------------

align_to_reference <- function(centers, membership, reference) {
  centers <- as.matrix(centers)
  membership <- as.matrix(membership)

  costs <- apply(
    perms,
    1,
    function(pp) {
      sqrt(
        sum(
          (
            centers[pp, , drop = FALSE] -
              reference
          )^2
        )
      )
    }
  )

  idx <- which.min(costs)
  pp <- as.integer(perms[idx, ])

  raw_cost <- sqrt(sum((centers - reference)^2))
  aligned_cost <- costs[idx]

  list(
    centers = centers[pp, , drop = FALSE],
    membership = membership[, pp, drop = FALSE],
    permutation = pp,
    raw_cost = raw_cost,
    aligned_cost = aligned_cost,
    gain = raw_cost - aligned_cost
  )
}


aligned_objects <- lapply(
  seq_len(POP_SIZE),
  function(i) {
    align_to_reference(
      orig_centers[[i]],
      orig_membership[[i]],
      reference
    )
  }
)

aligned_centers <- lapply(aligned_objects, `[[`, "centers")
aligned_membership <- lapply(aligned_objects, `[[`, "membership")

alignment_df <- do.call(
  rbind,
  lapply(
    seq_len(POP_SIZE),
    function(i) {
      a <- aligned_objects[[i]]

      data.frame(
        candidate = i,
        is_global_best = i == best_idx,
        spatial_XB = orig_fitness[i],
        permutation = paste(a$permutation, collapse = "-"),
        raw_distance_to_best = a$raw_cost,
        aligned_distance_to_best = a$aligned_cost,
        alignment_gain = a$gain,
        permutation_changed =
          !identical(a$permutation, seq_len(NCLUSTER)),
        stringsAsFactors = FALSE
      )
    }
  )
)


# Fitness must be invariant under permutation.
aligned_fitness <- vapply(
  seq_len(POP_SIZE),
  function(i) {
    xb <- alg_env$XB1(
      data = x,
      uij = aligned_membership[[i]],
      vi = aligned_centers[[i]],
      m = M
    )
    xb
  },
  numeric(1)
)

if (max(abs(aligned_fitness - orig_fitness)) > 1e-8) {
  stop(
    "Cluster alignment changed XB fitness, which should be permutation invariant.",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 7. Candidate scorer
# -----------------------------------------------------------------------------

score_candidates <- function(method, raw_candidates,
                             parent_fit,
                             parent_index = seq_along(raw_candidates),
                             representation) {
  evals <- lapply(
    raw_candidates,
    evaluate_spatial_candidate
  )

  cand_fit <- vapply(evals, `[[`, numeric(1), "fitness")
  occupied <- vapply(evals, `[[`, integer(1), "occupied")
  min_size <- vapply(evals, `[[`, integer(1), "min_cluster_size")

  pf <- parent_fit[parent_index]

  data.frame(
    method = method,
    representation = representation,
    candidate = seq_along(raw_candidates),
    parent_index = parent_index,
    parent_fitness = pf,
    candidate_fitness = cand_fit,
    improves_parent =
      is.finite(cand_fit) &
      (pf - cand_fit) > IMPROVEMENT_TOL,
    improves_global_best =
      is.finite(cand_fit) &
      (best_fit - cand_fit) > IMPROVEMENT_TOL,
    occupied = occupied,
    min_cluster_size = min_size,
    feasible = is.finite(cand_fit),
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# 8. One-step operator generator
# -----------------------------------------------------------------------------

run_operator_set <- function(centers, memberships, fit, representation) {
  gbest <- centers[[which.min(fit)]]

  # ---------------------------------------------------------------------------
  # ABC employed phase
  # ---------------------------------------------------------------------------
  abc_raw <- optimizer_envs$ABC$employed.bee(
    swarm = centers,
    fitness = fit,
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

  abc <- score_candidates(
    "ABC_employed",
    abc_raw,
    fit,
    representation = representation
  )


  # ---------------------------------------------------------------------------
  # GSA
  # ---------------------------------------------------------------------------
  par <- list(
    centroid = centers,
    membership = memberships,
    I = fit
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

  gv <- optimizer_envs$GSA$force_v(
    par = par,
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
    function(i) centers[[i]] + gv[[i]]
  )

  gsa <- score_candidates(
    "GSA",
    gsa_raw,
    fit,
    representation = representation
  )


  # ---------------------------------------------------------------------------
  # GWO
  # ---------------------------------------------------------------------------
  sorted_idx <- order(fit)
  alpha_pos <- centers[[sorted_idx[1L]]]
  beta_pos <- centers[[sorted_idx[2L]]]
  delta_pos <- centers[[sorted_idx[3L]]]
  a_coef <- 2

  gwo_raw <- lapply(
    seq_len(POP_SIZE),
    function(i) {
      set.seed(SEED + 10000L + i)

      dd <- dim(alpha_pos)

      r1 <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      r2 <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      A1 <- 2 * a_coef * r1 - a_coef
      C1 <- 2 * r2
      X1 <- alpha_pos -
        A1 * abs(C1 * alpha_pos - centers[[i]])

      r1 <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      r2 <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      A2 <- 2 * a_coef * r1 - a_coef
      C2 <- 2 * r2
      X2 <- beta_pos -
        A2 * abs(C2 * beta_pos - centers[[i]])

      r1 <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      r2 <- matrix(runif(prod(dd)), nrow = dd[1], ncol = dd[2])
      A3 <- 2 * a_coef * r1 - a_coef
      C3 <- 2 * r2
      X3 <- delta_pos -
        A3 * abs(C3 * delta_pos - centers[[i]])

      alg_env$clamp_centroids(
        (X1 + X2 + X3) / 3,
        x
      )
    }
  )

  gwo <- score_candidates(
    "GWO",
    gwo_raw,
    fit,
    representation = representation
  )


  # ---------------------------------------------------------------------------
  # TLBO current and canonical TF
  # ---------------------------------------------------------------------------
  teacher <- centers[[which.min(fit)]]
  class_average <- Reduce("+", centers) / POP_SIZE

  set.seed(SEED + 2L)
  tf_current <- matrix(
    1 + runif(nrow(teacher) * ncol(teacher)),
    ncol = ncol(teacher)
  )

  set.seed(SEED + 3L)
  r_current <- matrix(
    runif(nrow(teacher) * ncol(teacher)),
    ncol = ncol(teacher)
  )

  diff_current <- r_current * (
    teacher - tf_current * class_average
  )

  tlbo_current_raw <- lapply(
    centers,
    function(v) v + diff_current
  )

  tlbo_current <- score_candidates(
    "TLBO_current_TF",
    tlbo_current_raw,
    fit,
    representation = representation
  )

  set.seed(SEED + 2L)
  tf_canonical <- round(1 + runif(1))

  set.seed(SEED + 3L)
  r_canonical <- matrix(
    runif(nrow(teacher) * ncol(teacher)),
    ncol = ncol(teacher)
  )

  diff_canonical <- r_canonical * (
    teacher - tf_canonical * class_average
  )

  tlbo_canonical_raw <- lapply(
    centers,
    function(v) v + diff_canonical
  )

  tlbo_canonical <- score_candidates(
    "TLBO_canonical_TF",
    tlbo_canonical_raw,
    fit,
    representation = representation
  )


  # ---------------------------------------------------------------------------
  # WOA
  # ---------------------------------------------------------------------------
  woa_raw <- optimizer_envs$WOA$woa.move(
    swarm = centers,
    prey = gbest,
    a_coef = 2,
    b = 1,
    nwhale = POP_SIZE,
    seed = SEED,
    iter = 1L,
    data = x
  )

  woa <- score_candidates(
    "WOA",
    woa_raw,
    fit,
    representation = representation
  )


  rbind(
    abc,
    gsa,
    gwo,
    tlbo_current,
    tlbo_canonical,
    woa
  )
}


# -----------------------------------------------------------------------------
# 9. Run UNALIGNED vs ALIGNED
# -----------------------------------------------------------------------------

unaligned_candidates <- run_operator_set(
  centers = orig_centers,
  memberships = orig_membership,
  fit = orig_fitness,
  representation = "UNALIGNED"
)

aligned_candidates <- run_operator_set(
  centers = aligned_centers,
  memberships = aligned_membership,
  fit = aligned_fitness,
  representation = "ALIGNED"
)

candidate_df <- rbind(
  unaligned_candidates,
  aligned_candidates
)


# -----------------------------------------------------------------------------
# 10. Summaries
# -----------------------------------------------------------------------------

summary_df <- do.call(
  rbind,
  lapply(
    split(
      candidate_df,
      interaction(
        candidate_df$method,
        candidate_df$representation,
        drop = TRUE
      )
    ),
    function(d) {
      vals <- d$candidate_fitness[is.finite(d$candidate_fitness)]

      data.frame(
        method = d$method[1L],
        representation = d$representation[1L],
        current_global_best = best_fit,
        candidates = nrow(d),
        feasible_candidates = sum(d$feasible),
        parent_improvements = sum(d$improves_parent),
        global_best_improvements = sum(d$improves_global_best),
        best_candidate_fitness =
          if (length(vals)) min(vals) else Inf,
        median_candidate_fitness =
          if (length(vals)) stats::median(vals) else Inf,
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(summary_df) <- NULL

wide_unaligned <- summary_df[
  summary_df$representation == "UNALIGNED",
  ,
  drop = FALSE
]
wide_aligned <- summary_df[
  summary_df$representation == "ALIGNED",
  ,
  drop = FALSE
]

comparison_df <- merge(
  wide_unaligned,
  wide_aligned,
  by = "method",
  suffixes = c("_unaligned", "_aligned"),
  all = TRUE,
  sort = FALSE
)

comparison_df$best_fitness_ratio_aligned_vs_unaligned <- with(
  comparison_df,
  best_candidate_fitness_aligned /
    best_candidate_fitness_unaligned
)

comparison_df$median_fitness_ratio_aligned_vs_unaligned <- with(
  comparison_df,
  median_candidate_fitness_aligned /
    median_candidate_fitness_unaligned
)

comparison_df$reachability_gained <- with(
  comparison_df,
  global_best_improvements_unaligned == 0L &
    global_best_improvements_aligned > 0L
)

comparison_df$parent_reachability_gained <- with(
  comparison_df,
  parent_improvements_unaligned == 0L &
    parent_improvements_aligned > 0L
)


# -----------------------------------------------------------------------------
# 11. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7_7_label_alignment"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  alignment_df,
  file.path(out_dir, "v7_7_alignment_map.csv"),
  row.names = FALSE
)

utils::write.csv(
  candidate_df,
  file.path(out_dir, "v7_7_candidate_detail.csv"),
  row.names = FALSE
)

utils::write.csv(
  summary_df,
  file.path(out_dir, "v7_7_reachability_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  comparison_df,
  file.path(out_dir, "v7_7_unaligned_vs_aligned.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    alignment = alignment_df,
    candidates = candidate_df,
    summary = summary_df,
    comparison = comparison_df,
    best_index = best_idx,
    best_spatial_XB = best_fit
  ),
  file.path(out_dir, "v7_7_results.rds")
)


# -----------------------------------------------------------------------------
# 12. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7.7 — Centroid Label-Alignment Reachability Diagnostic\n")
cat("============================================================\n")
cat("Population            :", POP_SIZE, "\n")
cat("Clusters              :", NCLUSTER, "\n")
cat("Permutations checked  :", nrow(perms), "per agent\n")
cat("Reference agent       :", best_idx, "\n")
cat("Reference spatial XB  :", format(best_fit, digits = 12), "\n")
cat("============================================================\n")

cat("\n[1] ALIGNMENT EFFECT ON INITIAL POPULATION\n")
cat(
  "Agents requiring non-identity permutation:",
  sum(alignment_df$permutation_changed), "/", POP_SIZE, "\n"
)
cat(
  "Median reduction in distance-to-reference:",
  format(stats::median(alignment_df$alignment_gain), digits = 8), "\n"
)
cat(
  "Maximum reduction in distance-to-reference:",
  format(max(alignment_df$alignment_gain), digits = 8), "\n"
)

cat("\n[2] UNALIGNED vs ALIGNED ONE-STEP REACHABILITY\n")
print(
  comparison_df[, c(
    "method",
    "parent_improvements_unaligned",
    "parent_improvements_aligned",
    "global_best_improvements_unaligned",
    "global_best_improvements_aligned",
    "best_candidate_fitness_unaligned",
    "best_candidate_fitness_aligned",
    "best_fitness_ratio_aligned_vs_unaligned",
    "reachability_gained"
  )],
  row.names = FALSE
)

cat("\n============================================================\n")
cat("V7.7 DECISION\n")
cat("============================================================\n")

gained <- comparison_df$method[
  comparison_df$reachability_gained |
    comparison_df$parent_reachability_gained
]

if (length(gained) > 0L) {
  cat(
    "Label alignment improves reachability for:",
    paste(gained, collapse = ", "),
    "\n"
  )
  cat(
    "This supports centroid label switching as a material optimization issue.\n"
  )
} else {
  cat(
    "No operator gained parent/global-best reachability after label alignment.\n"
  )
  cat(
    "Label switching may still distort movements, but it is not sufficient to\n",
    "explain the V7.6 stagnation. Search-space scaling/bound handling should be\n",
    "tested next before changing canonical operator formulas.\n",
    sep = ""
  )
}

cat("\nInterpretation rule:\n")
cat(
  "- Alignment is only a row permutation; it must NOT change initial XB.\n",
  "- A large drop in candidate XB or new improving candidates after alignment\n",
  "  means row-wise inter-agent arithmetic was mixing arbitrary cluster labels.\n",
  "- If alignment helps substantially, Patch v3 should align centroid rows to\n",
  "  a reference before inter-agent arithmetic while permuting membership columns\n",
  "  consistently. This preserves each clustering solution exactly.\n",
  sep = ""
)

cat("\nResults written to:\n  ", out_dir, "\n", sep = "")
