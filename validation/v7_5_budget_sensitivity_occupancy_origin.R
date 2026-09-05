# =============================================================================
# soviclust — V7.5 Budget Sensitivity & Occupancy-Origin Diagnostic
# =============================================================================
#
# Purpose
# -------
# 1. Test whether the V7.3 XB stagnation of ABC/GSA/GWO/IFA/TLBO/WOA is mainly
#    a consequence of the intentionally tiny smoke budget (10 agents x 10 iter).
# 2. Determine whether HHO's hard ghost cluster exists in the ordinary
#    centroid-derived membership or is introduced by the FGWC spatial
#    membership adjustment.
#
# This script does NOT modify package source files.
#
# Run from the root of the soviclust source project:
#
#   source("validation/v7_5_budget_sensitivity_occupancy_origin.R")
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

# Extended diagnostic budget. Still a pilot, not the formal benchmark.
POP_SIZE <- 20L
MAX_ITER <- 30L

ERROR_TOL <- 0
SAME_LIMIT <- MAX_ITER + 100L

IMPROVEMENT_TOL <- 1e-10
ROW_SUM_TOL <- 1e-8


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

if (!identical(dim(distmat), c(nrow(x), nrow(x)))) {
  stop("Distance matrix dimension mismatch.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 3. Temporary XB search fitness used in V7.3
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
# 4. Helpers
# -----------------------------------------------------------------------------

min_sep <- function(centers) {
  d <- as.matrix(stats::dist(as.matrix(centers)))
  diag(d) <- Inf
  min(d)
}

hard_counts <- function(u, k = NCLUSTER) {
  tabulate(
    apply(as.matrix(u), 1, which.max),
    nbins = k
  )
}

spatial_xb <- function(res) {
  alg_env$XB1(
    data = x,
    uij = as.matrix(res$membership),
    vi = as.matrix(res$centroid),
    m = M
  )
}

cluster_support <- function(u) {
  u <- as.matrix(u)
  hc <- hard_counts(u)

  data.frame(
    cluster = seq_len(ncol(u)),
    hard_count = hc,
    soft_mass = colSums(u),
    effective_mass = colSums(u^M),
    mean_membership = colMeans(u),
    max_membership = apply(u, 2, max),
    q95_membership = apply(
      u, 2,
      function(z) as.numeric(stats::quantile(z, 0.95, names = FALSE))
    ),
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# 5. Occupancy origin using the saved V7.3 solutions
# -----------------------------------------------------------------------------

v73_path <- file.path(
  root,
  "validation_results",
  "v7_3_xb_pilot",
  "v7_3_xb_pilot_results.rds"
)

if (!file.exists(v73_path)) {
  stop("V7.3 result RDS not found.", call. = FALSE)
}

v73 <- readRDS(v73_path)

occupancy_rows <- list()
support_rows <- list()
ii <- 0L

for (method in names(v73)) {
  res <- v73[[method]]
  centers <- as.matrix(res$centroid)

  base_u <- alg_env$membership_from_centroids(
    x, centers, M, DISTANCE, ORDER
  )$u

  spatial_u <- as.matrix(res$membership)

  base_counts <- hard_counts(base_u)
  spatial_counts <- hard_counts(spatial_u)

  occupancy_rows[[method]] <- data.frame(
    method = method,
    base_occupied = sum(base_counts > 0L),
    spatial_occupied = sum(spatial_counts > 0L),
    base_min_hard_count = min(base_counts),
    spatial_min_hard_count = min(spatial_counts),
    base_min_max_membership = min(apply(base_u, 2, max)),
    spatial_min_max_membership = min(apply(spatial_u, 2, max)),
    occupancy_lost_after_spatial =
      sum(base_counts > 0L) > sum(spatial_counts > 0L),
    stringsAsFactors = FALSE
  )

  b <- cluster_support(base_u)
  b$method <- method
  b$membership_type <- "base"

  s <- cluster_support(spatial_u)
  s$method <- method
  s$membership_type <- "spatial"

  ii <- ii + 1L
  support_rows[[ii]] <- b
  ii <- ii + 1L
  support_rows[[ii]] <- s
}

occupancy_df <- do.call(rbind, occupancy_rows)
rownames(occupancy_df) <- NULL

support_df <- do.call(rbind, support_rows)
rownames(support_df) <- NULL


# -----------------------------------------------------------------------------
# 6. Extended-budget optimizer specifications
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
  randomN = SEED,
  vi.dist = "uniform"
)

specs <- list(
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
# 7. Run extended-budget diagnostic
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("V7.5 — Budget Sensitivity & Occupancy-Origin Diagnostic\n")
cat("============================================================\n")
cat("Search fitness :", "base_XB temporary override\n")
cat("Population     :", POP_SIZE, "\n")
cat("Iterations     :", MAX_ITER, "\n")
cat("Seed           :", SEED, "\n")
cat("============================================================\n\n")

extended_results <- list()
rows <- list()
errors <- list()

for (method in names(specs)) {
  cat(sprintf("[%s] extended-budget run...\n", method))

  spec <- specs[[method]]
  args <- c(common_args, spec$extra)

  timing <- system.time({
    res <- tryCatch(
      do.call(spec$fn, args),
      error = function(e) e
    )
  })

  elapsed <- unname(timing[["elapsed"]])

  if (inherits(res, "error")) {
    errors[[method]] <- conditionMessage(res)

    rows[[method]] <- data.frame(
      method = method,
      status = "ERROR",
      start_XB = NA_real_,
      final_XB = NA_real_,
      absolute_improvement = NA_real_,
      relative_improvement = NA_real_,
      improved = FALSE,
      spatial_XB = NA_real_,
      min_sep = NA_real_,
      occupied = NA_integer_,
      min_cluster_size = NA_integer_,
      iteration = NA_integer_,
      elapsed_sec = elapsed,
      stringsAsFactors = FALSE
    )

    cat(sprintf("[%s] ERROR: %s\n\n", method, errors[[method]]))
    next
  }

  extended_results[[method]] <- res

  conv <- as.numeric(res$converg)
  start_xb <- if (length(conv)) conv[1L] else NA_real_
  final_xb <- as.numeric(res$f_obj)
  improvement <- start_xb - final_xb

  counts <- hard_counts(res$membership)

  rows[[method]] <- data.frame(
    method = method,
    status = "OK",
    start_XB = start_xb,
    final_XB = final_xb,
    absolute_improvement = improvement,
    relative_improvement = if (
      is.finite(start_xb) && start_xb != 0
    ) {
      improvement / abs(start_xb)
    } else {
      NA_real_
    },
    improved =
      is.finite(improvement) &&
      improvement > IMPROVEMENT_TOL,
    spatial_XB = spatial_xb(res),
    min_sep = min_sep(res$centroid),
    occupied = sum(counts > 0L),
    min_cluster_size = min(counts),
    iteration = as.integer(res$iteration),
    elapsed_sec = elapsed,
    stringsAsFactors = FALSE
  )

  cat(
    sprintf(
      "[%s] start=%.6f final=%.6f improved=%s spatial_XB=%.6f clusters=%d/%d min_sep=%.5f\n\n",
      method,
      start_xb,
      final_xb,
      rows[[method]]$improved,
      rows[[method]]$spatial_XB,
      rows[[method]]$occupied,
      NCLUSTER,
      rows[[method]]$min_sep
    )
  )
}

extended_df <- do.call(rbind, rows)
rownames(extended_df) <- NULL


# -----------------------------------------------------------------------------
# 8. Compare V7.3 small budget vs V7.5 extended budget
# -----------------------------------------------------------------------------

small_rows <- lapply(
  names(v73),
  function(method) {
    res <- v73[[method]]
    conv <- as.numeric(res$converg)
    counts <- hard_counts(res$membership)

    data.frame(
      method = method,
      small_start_XB = conv[1L],
      small_final_XB = as.numeric(res$f_obj),
      small_improved = (conv[1L] - res$f_obj) > IMPROVEMENT_TOL,
      small_spatial_XB = spatial_xb(res),
      small_min_sep = min_sep(res$centroid),
      small_occupied = sum(counts > 0L),
      stringsAsFactors = FALSE
    )
  }
)

small_df <- do.call(rbind, small_rows)
rownames(small_df) <- NULL

comparison_df <- merge(
  small_df,
  extended_df,
  by = "method",
  all = TRUE,
  sort = FALSE
)

comparison_df$improvement_appears_with_budget <- with(
  comparison_df,
  !small_improved & improved
)

comparison_df$final_XB_ratio_extended_vs_small <- with(
  comparison_df,
  final_XB / small_final_XB
)

comparison_df$spatial_XB_ratio_extended_vs_small <- with(
  comparison_df,
  spatial_XB / small_spatial_XB
)


# -----------------------------------------------------------------------------
# 9. Save outputs
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v7_5_budget_occupancy"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  occupancy_df,
  file.path(out_dir, "v7_5_base_vs_spatial_occupancy.csv"),
  row.names = FALSE
)

utils::write.csv(
  support_df,
  file.path(out_dir, "v7_5_cluster_support_detail.csv"),
  row.names = FALSE
)

utils::write.csv(
  extended_df,
  file.path(out_dir, "v7_5_extended_budget_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  comparison_df,
  file.path(out_dir, "v7_5_small_vs_extended_budget.csv"),
  row.names = FALSE
)

saveRDS(
  extended_results,
  file.path(out_dir, "v7_5_extended_results.rds")
)

if (length(errors) > 0L) {
  utils::write.csv(
    data.frame(
      method = names(errors),
      message = unlist(errors, use.names = FALSE),
      stringsAsFactors = FALSE
    ),
    file.path(out_dir, "v7_5_errors.csv"),
    row.names = FALSE
  )
}


# -----------------------------------------------------------------------------
# 10. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("[1] BASE vs SPATIAL OCCUPANCY OF V7.3 FINAL SOLUTIONS\n")
cat("============================================================\n")
print(occupancy_df, row.names = FALSE)

cat("\n")
cat("============================================================\n")
cat("[2] EXTENDED-BUDGET XB SEARCH\n")
cat("============================================================\n")
print(
  extended_df[, c(
    "method",
    "start_XB",
    "final_XB",
    "absolute_improvement",
    "improved",
    "spatial_XB",
    "min_sep",
    "occupied",
    "min_cluster_size"
  )],
  row.names = FALSE
)

cat("\n")
cat("============================================================\n")
cat("[3] SMALL vs EXTENDED BUDGET\n")
cat("============================================================\n")
print(
  comparison_df[, c(
    "method",
    "small_improved",
    "improved",
    "improvement_appears_with_budget",
    "small_final_XB",
    "final_XB",
    "small_spatial_XB",
    "spatial_XB",
    "small_occupied",
    "occupied"
  )],
  row.names = FALSE
)

hho_row <- occupancy_df[
  occupancy_df$method == "HHO",
  ,
  drop = FALSE
]

cat("\n")
cat("============================================================\n")
cat("V7.5 DECISION FLAGS\n")
cat("============================================================\n")

if (nrow(hho_row) == 1L) {
  if (
    hho_row$base_occupied == NCLUSTER &&
    hho_row$spatial_occupied < NCLUSTER
  ) {
    cat(
      "HHO ghost-cluster origin: SPATIAL MEMBERSHIP ADJUSTMENT.\n",
      "The final centroid supports all 4 clusters under base membership, but\n",
      "renewed spatial membership removes one cluster from the hard map.\n",
      sep = ""
    )
  } else if (
    hho_row$base_occupied < NCLUSTER &&
    hho_row$spatial_occupied < NCLUSTER
  ) {
    cat(
      "HHO ghost-cluster origin: already present in centroid/base membership;\n",
      "spatial adjustment does not create it from scratch.\n",
      sep = ""
    )
  } else {
    cat("HHO occupancy pattern does not match the expected ghost cases.\n")
  }
}

previously_stagnant <- c(
  "ABC", "GSA", "GWO", "IFA", "TLBO", "WOA"
)

sub <- comparison_df[
  comparison_df$method %in% previously_stagnant,
  ,
  drop = FALSE
]

now_improving <- sub$method[sub$improvement_appears_with_budget]
still_stagnant <- sub$method[!sub$improved]

cat(
  "\nPreviously-stagnant methods that improve with larger budget:",
  if (length(now_improving)) paste(now_improving, collapse = ", ") else "None",
  "\n"
)

cat(
  "Previously-stagnant methods still showing no improvement:",
  if (length(still_stagnant)) paste(still_stagnant, collapse = ", ") else "None",
  "\n"
)

cat("\nInterpretation:\n")
cat(
  "- If most previously-stagnant methods improve at 20x30, V7.3 stagnation was\n",
  "  primarily a smoke-budget artifact rather than proof of implementation failure.\n",
  sep = ""
)
cat(
  "- If several remain exactly at their initial best, inspect their movement/\n",
  "  selection operators under XB before creating Patch v3.\n",
  sep = ""
)
cat(
  "- If HHO loses occupancy only after spatial adjustment, a permanent Patch v3\n",
  "  should evaluate the ACTUAL spatial membership + centroid pair and impose the\n",
  "  requested hard-cluster occupancy as a constraint at that same stage.\n",
  sep = ""
)

cat("\nResults written to:\n  ", out_dir, "\n", sep = "")
