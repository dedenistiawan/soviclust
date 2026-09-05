# =============================================================================
# soviclust — V7.4 Initialization/Stagnation & Ghost-Cluster Diagnostic
# =============================================================================
#
# Purpose
# -------
# Diagnose two issues observed in V7.3:
#
# 1. ABC/GSA/GWO/TLBO/WOA returned exactly the same XB value (95.540273).
#    Determine whether this is simply the best common initial candidate and
#    whether those methods improved it at all.
#
# 2. HHO returned four separated centroids but only three hard clusters.
#    Determine whether the unused hard cluster still carries meaningful fuzzy
#    membership mass ("ghost cluster") and quantify this for all methods.
#
# This script DOES NOT modify package source code.
#
# Run from package root:
#
#   source("validation/v7_4_initialization_ghost_cluster_diagnostic.R")
#
# Requires V7.3 output:
#   validation_results/v7_3_xb_pilot/v7_3_xb_pilot_results.rds
#
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

SEED <- 2026L
NCLUSTER <- 4L
M <- 2
ALPHA <- 0.7
A <- 1
B <- 1
DISTANCE <- "euclidean"
ORDER <- 2
POP_SIZE <- 10L

SOLUTION_TOL <- 1e-8
IMPROVEMENT_TOL <- 1e-10


# -----------------------------------------------------------------------------
# 1. Locate project and load source/test harness
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
# 2. Load V7 data
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
# 3. Recreate V7.3 temporary XB fitness
# -----------------------------------------------------------------------------

pilot_xb_fitness <- function(data, centers, m,
                             distance = "euclidean", order = 2, ...) {
  centers <- as.matrix(centers)

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

for (method in names(optimizer_envs)) {
  optimizer_envs[[method]]$optimizer_fitness <- pilot_xb_fitness
}


# -----------------------------------------------------------------------------
# 4. Load V7.3 final results
# -----------------------------------------------------------------------------

v73_path <- file.path(
  root,
  "validation_results",
  "v7_3_xb_pilot",
  "v7_3_xb_pilot_results.rds"
)

if (!file.exists(v73_path)) {
  stop(
    "V7.3 result RDS not found: ",
    v73_path,
    call. = FALSE
  )
}

results <- readRDS(v73_path)

if (length(results) == 0L) {
  stop("V7.3 RDS contains no optimizer results.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 5. Reconstruct common 10-member initialization
# -----------------------------------------------------------------------------
#
# All legacy optimizers use the same init.swarm() implementation originating
# from ifafgwc.R (GWO/WOA explicitly source it too in the test harness).
# With the V7.3 XB override active, this reconstructs the common initial XB
# landscape used by ABC/FPA/GSA/GWO/HHO/PSO/TLBO/WOA.
# IFA has an additional separate one-firefly best initialization, so its
# convergence history is also analyzed directly below.
# -----------------------------------------------------------------------------

common_init <- optimizer_envs$IFA$init.swarm(
  data = x,
  pop = mimj,
  distmat = distmat,
  distance = DISTANCE,
  order = ORDER,
  vi.dist = "uniform",
  ncluster = NCLUSTER,
  m = M,
  alpha = ALPHA,
  a = A,
  b = B,
  randomN = SEED,
  nfly = POP_SIZE
)

initial_df <- data.frame(
  candidate = seq_len(POP_SIZE),
  XB = as.numeric(common_init$I),
  min_sep = vapply(
    common_init$centroid,
    function(v) {
      d <- as.matrix(stats::dist(v))
      diag(d) <- Inf
      min(d)
    },
    numeric(1)
  ),
  hard_occupied = vapply(
    common_init$membership,
    function(u) {
      length(unique(apply(as.matrix(u), 1, which.max)))
    },
    integer(1)
  ),
  stringsAsFactors = FALSE
)

initial_best_idx <- which.min(initial_df$XB)
initial_best_xb <- initial_df$XB[initial_best_idx]
initial_best_centroid <- common_init$centroid[[initial_best_idx]]
initial_best_membership <- common_init$membership[[initial_best_idx]]


# -----------------------------------------------------------------------------
# 6. Permutation-invariant centroid/membership comparison
# -----------------------------------------------------------------------------

permute_vec <- function(v) {
  if (length(v) == 1L) return(matrix(v, nrow = 1L))

  out <- NULL

  for (i in seq_along(v)) {
    rest <- v[-i]
    p_rest <- permute_vec(rest)

    block <- cbind(v[i], p_rest)
    out <- rbind(out, block)
  }

  out
}

perms <- permute_vec(seq_len(NCLUSTER))

solution_diff_to_reference <- function(centroid, membership,
                                       ref_centroid, ref_membership) {
  centroid <- as.matrix(centroid)
  membership <- as.matrix(membership)
  ref_centroid <- as.matrix(ref_centroid)
  ref_membership <- as.matrix(ref_membership)

  best_c <- Inf
  best_u <- Inf

  for (i in seq_len(nrow(perms))) {
    p <- perms[i, ]

    c_diff <- max(abs(centroid[p, , drop = FALSE] - ref_centroid))
    u_diff <- max(abs(membership[, p, drop = FALSE] - ref_membership))

    if (c_diff + u_diff < best_c + best_u) {
      best_c <- c_diff
      best_u <- u_diff
    }
  }

  list(
    centroid_maxdiff = best_c,
    membership_maxdiff = best_u,
    identical_solution =
      best_c <= SOLUTION_TOL &&
      best_u <= SOLUTION_TOL
  )
}


# -----------------------------------------------------------------------------
# 7. Convergence / initialization diagnostics
# -----------------------------------------------------------------------------

stagnation_rows <- lapply(
  names(results),
  function(method) {
    res <- results[[method]]
    conv <- as.numeric(res$converg)

    diff_init <- solution_diff_to_reference(
      res$centroid,
      res$membership,
      initial_best_centroid,
      initial_best_membership
    )

    start_fit <- if (length(conv)) conv[1L] else NA_real_
    final_fit <- as.numeric(res$f_obj)

    data.frame(
      method = method,
      common_initial_best_XB = initial_best_xb,
      convergence_start = start_fit,
      final_XB = final_fit,
      absolute_improvement = start_fit - final_fit,
      relative_improvement = if (
        is.finite(start_fit) &&
        start_fit != 0
      ) {
        (start_fit - final_fit) / abs(start_fit)
      } else {
        NA_real_
      },
      improved_from_own_start =
        is.finite(start_fit) &&
        is.finite(final_fit) &&
        (start_fit - final_fit) > IMPROVEMENT_TOL,
      final_equals_common_initial_XB =
        is.finite(final_fit) &&
        abs(final_fit - initial_best_xb) <= SOLUTION_TOL,
      final_centroid_diff_from_common_initial =
        diff_init$centroid_maxdiff,
      final_membership_diff_from_common_initial =
        diff_init$membership_maxdiff,
      final_solution_equals_common_initial =
        diff_init$identical_solution,
      convergence_length = length(conv),
      stringsAsFactors = FALSE
    )
  }
)

stagnation_df <- do.call(rbind, stagnation_rows)
rownames(stagnation_df) <- NULL


# -----------------------------------------------------------------------------
# 8. Fuzzy cluster support / ghost-cluster diagnostics
# -----------------------------------------------------------------------------

support_rows <- list()
k <- 0L

for (method in names(results)) {
  res <- results[[method]]
  u <- as.matrix(res$membership)

  hard <- apply(u, 1, which.max)
  hard_counts <- tabulate(hard, nbins = NCLUSTER)

  soft_mass <- colSums(u)
  effective_mass <- colSums(u^M)

  for (cl in seq_len(NCLUSTER)) {
    k <- k + 1L

    support_rows[[k]] <- data.frame(
      method = method,
      cluster = cl,
      hard_count = hard_counts[cl],
      hard_proportion = hard_counts[cl] / nrow(u),
      soft_mass = soft_mass[cl],
      soft_mass_proportion = soft_mass[cl] / nrow(u),
      effective_mass_m = effective_mass[cl],
      mean_membership = mean(u[, cl]),
      max_membership = max(u[, cl]),
      q95_membership = as.numeric(
        stats::quantile(u[, cl], probs = 0.95, names = FALSE)
      ),
      ghost_hard_cluster = hard_counts[cl] == 0L,
      stringsAsFactors = FALSE
    )
  }
}

support_df <- do.call(rbind, support_rows)


method_support <- do.call(
  rbind,
  lapply(
    split(support_df, support_df$method),
    function(d) {
      ghost <- d[d$ghost_hard_cluster, , drop = FALSE]

      data.frame(
        method = d$method[1L],
        occupied_hard_clusters = sum(d$hard_count > 0L),
        min_hard_count = min(d$hard_count),
        min_soft_mass = min(d$soft_mass),
        min_soft_mass_proportion = min(d$soft_mass_proportion),
        min_effective_mass = min(d$effective_mass_m),
        max_membership_of_weakest_hard_cluster =
          if (nrow(ghost) > 0L) {
            max(ghost$max_membership)
          } else {
            NA_real_
          },
        ghost_cluster_count = sum(d$ghost_hard_cluster),
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(method_support) <- NULL


# -----------------------------------------------------------------------------
# 9. Pairwise final-solution equality between optimizers
# -----------------------------------------------------------------------------

methods <- names(results)
pairs <- utils::combn(methods, 2L, simplify = FALSE)

pair_rows <- lapply(
  pairs,
  function(pp) {
    r1 <- results[[pp[1L]]]
    r2 <- results[[pp[2L]]]

    d <- solution_diff_to_reference(
      r1$centroid,
      r1$membership,
      r2$centroid,
      r2$membership
    )

    data.frame(
      method_1 = pp[1L],
      method_2 = pp[2L],
      f_obj_diff = abs(r1$f_obj - r2$f_obj),
      centroid_maxdiff = d$centroid_maxdiff,
      membership_maxdiff = d$membership_maxdiff,
      identical_solution = d$identical_solution,
      stringsAsFactors = FALSE
    )
  }
)

pairwise_df <- do.call(rbind, pair_rows)


# -----------------------------------------------------------------------------
# 10. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7_4_init_ghost_diagnostic"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  initial_df,
  file.path(out_dir, "v7_4_common_initial_population.csv"),
  row.names = FALSE
)

utils::write.csv(
  stagnation_df,
  file.path(out_dir, "v7_4_optimizer_improvement.csv"),
  row.names = FALSE
)

utils::write.csv(
  support_df,
  file.path(out_dir, "v7_4_cluster_fuzzy_support.csv"),
  row.names = FALSE
)

utils::write.csv(
  method_support,
  file.path(out_dir, "v7_4_method_support_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  pairwise_df,
  file.path(out_dir, "v7_4_pairwise_solution_identity.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    initial = initial_df,
    initial_best_idx = initial_best_idx,
    initial_best_xb = initial_best_xb,
    stagnation = stagnation_df,
    fuzzy_support = support_df,
    method_support = method_support,
    pairwise = pairwise_df
  ),
  file.path(out_dir, "v7_4_diagnostic_results.rds")
)


# -----------------------------------------------------------------------------
# 11. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7.4 — Initialization/Stagnation & Ghost-Cluster Diagnostic\n")
cat("============================================================\n")

cat("\n[1] COMMON INITIAL POPULATION\n")
print(initial_df, row.names = FALSE)

cat("\nCommon initial best candidate:", initial_best_idx, "\n")
cat("Common initial best XB       :", format(initial_best_xb, digits = 12), "\n")

cat("\n[2] OPTIMIZER IMPROVEMENT FROM START\n")
print(
  stagnation_df[, c(
    "method",
    "convergence_start",
    "final_XB",
    "absolute_improvement",
    "improved_from_own_start",
    "final_equals_common_initial_XB",
    "final_solution_equals_common_initial"
  )],
  row.names = FALSE
)

cat("\n[3] FUZZY SUPPORT BY METHOD\n")
print(
  method_support[
    order(method_support$method),
    ,
    drop = FALSE
  ],
  row.names = FALSE
)

hho_support <- support_df[
  support_df$method == "HHO",
  ,
  drop = FALSE
]

if (nrow(hho_support) > 0L) {
  cat("\n[4] HHO CLUSTER-LEVEL SUPPORT\n")
  print(
    hho_support[, c(
      "cluster",
      "hard_count",
      "soft_mass",
      "soft_mass_proportion",
      "effective_mass_m",
      "mean_membership",
      "max_membership",
      "q95_membership",
      "ghost_hard_cluster"
    )],
    row.names = FALSE
  )
}

identical_pairs <- pairwise_df[
  pairwise_df$identical_solution,
  ,
  drop = FALSE
]

cat("\n[5] IDENTICAL FINAL SOLUTION PAIRS\n")
if (nrow(identical_pairs) == 0L) {
  cat("None.\n")
} else {
  print(
    identical_pairs[, c(
      "method_1",
      "method_2",
      "f_obj_diff",
      "centroid_maxdiff",
      "membership_maxdiff"
    )],
    row.names = FALSE
  )
}

no_improvement <- stagnation_df$method[
  !stagnation_df$improved_from_own_start
]

ghost_methods <- method_support$method[
  method_support$ghost_cluster_count > 0L
]

cat("\n============================================================\n")
cat("V7.4 DIAGNOSTIC FLAGS\n")
cat("============================================================\n")
cat(
  "Methods with no improvement from own convergence start:",
  if (length(no_improvement)) {
    paste(no_improvement, collapse = ", ")
  } else {
    "None"
  },
  "\n"
)
cat(
  "Methods with hard ghost cluster(s):",
  if (length(ghost_methods)) {
    paste(ghost_methods, collapse = ", ")
  } else {
    "None"
  },
  "\n"
)

if (nrow(identical_pairs) > 0L) {
  cat(
    "Exact/permutation-equivalent final solutions were detected between ",
    "multiple optimizers.\n",
    sep = ""
  )
}

cat("\nInterpretation:\n")
cat(
  "- If methods with XB=95.540273 also equal the common initial best and show\n",
  "  zero improvement, V7.3 demonstrated anti-collapse behavior but NOT that\n",
  "  those optimizers can effectively optimize XB under the current coupling.\n",
  sep = ""
)
cat(
  "- If HHO's empty hard cluster still has appreciable soft mass, it is a\n",
  "  fuzzy 'ghost cluster': XB does not enforce hard-cluster occupancy.\n",
  sep = ""
)
cat(
  "- In that case, do not patch XB as the permanent objective yet. The next\n",
  "  design must address both search effectiveness and practical hard-cluster\n",
  "  occupancy for map/report outputs.\n",
  sep = ""
)

cat("\nResults written to:\n  ", out_dir, "\n", sep = "")
