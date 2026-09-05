# =============================================================================
# Diagnose FGWC centroid convergence and degeneracy
# =============================================================================
# Run from the root directory of the soviclust source project.
# This script uses the same naspaclust census2010 reference setup as the
# reference-equivalence validation.
# =============================================================================

if (!requireNamespace("naspaclust", quietly = TRUE)) {
  stop("Install 'naspaclust' first.", call. = FALSE)
}
if (!requireNamespace("rdist", quietly = TRUE)) {
  stop("Install 'rdist' first.", call. = FALSE)
}

alg_env <- new.env(parent = globalenv())
for (f in c(
  "inst/app/R/shared/function/index.R",
  "inst/app/R/shared/function/fgwc.R"
)) {
  if (!file.exists(f)) {
    stop("Run this script from the root of the soviclust project.", call. = FALSE)
  }
  sys.source(f, envir = alg_env)
}

data("census2010", package = "naspaclust", envir = environment())
data("census2010pop", package = "naspaclust", envir = environment())
data("census2010dist", package = "naspaclust", envir = environment())

raw <- as.data.frame(census2010)

usable <- vapply(
  raw,
  function(z) is.numeric(z) && all(is.finite(z)) && stats::sd(z) > 0,
  logical(1)
)

feature_names <- names(raw)[usable]
feature_names <- feature_names[seq_len(min(6L, length(feature_names)))]

minmax <- function(z) {
  r <- range(z)
  if (diff(r) == 0) return(rep(0, length(z)))
  (z - r[1]) / diff(r)
}

x <- as.matrix(apply(raw[, feature_names, drop = FALSE], 2, minmax))
pop <- as.numeric(census2010pop)
dmat <- as.matrix(census2010dist)

ncluster <- 3L
m <- 2
alpha <- 0.7
beta <- 1 - alpha
a <- 1
b <- 1
distance <- "euclidean"
order <- 2
seed <- 2026L
max_iter <- 500L

ref_gen_vi <- getFromNamespace("gen_vi", "naspaclust")
centers <- ref_gen_vi(
  data = x,
  ncluster = ncluster,
  gendist = "uniform",
  randomN = seed
)

popmat <- matrix(pop, ncol = 1)
mi.mj <- popmat %*% t(popmat)

trace <- data.frame(
  iteration = integer(max_iter),
  objective = numeric(max_iter),
  objective_abs_change = numeric(max_iter),
  objective_rel_change = numeric(max_iter),
  centroid_frobenius_change = numeric(max_iter),
  min_centroid_distance = numeric(max_iter),
  min_centroid_distance_sq = numeric(max_iter),
  PC = numeric(max_iter),
  CE = numeric(max_iter),
  XB = numeric(max_iter),
  Kwon = numeric(max_iter),
  occupied_hard_clusters = integer(max_iter),
  stringsAsFactors = FALSE
)

prev_obj <- NA_real_

for (iter in seq_len(max_iter)) {
  old_centers <- centers

  base_u <- alg_env$membership_from_centroids(
    x, old_centers, m, distance, order
  )$u

  spatial_u <- alg_env$renew_uij(
    x, base_u, mi.mj, dmat,
    alpha, beta, a, b
  )

  centers <- alg_env$centroid_from_membership(x, spatial_u, m)

  obj <- alg_env$fgwc_objective(
    x, spatial_u, centers, m, distance, order
  )

  cdist <- as.matrix(stats::dist(centers))
  diag(cdist) <- Inf
  minsep <- min(cdist)

  hard <- apply(spatial_u, 1, which.max)

  xb <- alg_env$XB1(x, spatial_u, centers, m)
  kw <- alg_env$Kwon1(x, spatial_u, centers, m)

  abs_change <- if (is.na(prev_obj)) NA_real_ else abs(obj - prev_obj)
  rel_change <- if (is.na(prev_obj)) {
    NA_real_
  } else {
    abs(obj - prev_obj) / max(abs(prev_obj), .Machine$double.eps)
  }

  trace[iter, ] <- list(
    iter,
    obj,
    abs_change,
    rel_change,
    alg_env$centroid_change(centers, old_centers),
    minsep,
    minsep^2,
    alg_env$PC1(spatial_u),
    alg_env$CE1(spatial_u),
    xb,
    kw,
    length(unique(hard))
  )

  prev_obj <- obj
}

# Reference package full result, for its own stopping point.
ref_full <- naspaclust::fgwcuv(
  data = x,
  pop = pop,
  distmat = dmat,
  kind = "v",
  ncluster = ncluster,
  m = m,
  distance = distance,
  order = order,
  alpha = alpha,
  a = a,
  b = b,
  max.iter = 500,
  error = 1e-8,
  randomN = seed
)

ref_stop <- as.integer(ref_full$iteration)

# Candidate plateau detections.
first_rel_1e6 <- which(trace$objective_rel_change < 1e-6)[1]
first_rel_1e8 <- which(trace$objective_rel_change < 1e-8)[1]
first_center_1e6 <- which(trace$centroid_frobenius_change < 1e-6)[1]
first_center_1e8 <- which(trace$centroid_frobenius_change < 1e-8)[1]

# Find first iteration at which centroid separation becomes very small.
first_sep_1e3 <- which(trace$min_centroid_distance < 1e-3)[1]
first_sep_1e4 <- which(trace$min_centroid_distance < 1e-4)[1]
first_sep_1e5 <- which(trace$min_centroid_distance < 1e-5)[1]

pick <- unique(c(
  1L, 2L, 5L, 10L, 25L, 50L, 100L, 150L, 200L,
  ref_stop, 300L, 400L, 500L,
  first_rel_1e6, first_rel_1e8,
  first_center_1e6, first_center_1e8,
  first_sep_1e3, first_sep_1e4, first_sep_1e5
))
pick <- pick[is.finite(pick) & pick >= 1 & pick <= max_iter]
pick <- sort(unique(as.integer(pick)))

summary_table <- trace[pick, c(
  "iteration",
  "objective",
  "objective_rel_change",
  "centroid_frobenius_change",
  "min_centroid_distance",
  "PC",
  "CE",
  "XB",
  "occupied_hard_clusters"
)]

cat("\n============================================================\n")
cat("FGWC CONVERGENCE / CENTROID-SEPARATION DIAGNOSTIC\n")
cat("============================================================\n")
cat("Features:", paste(feature_names, collapse = ", "), "\n")
cat("n =", nrow(x), "| p =", ncol(x), "| c =", ncluster, "\n")
cat("alpha =", alpha, "| m =", m, "| seed =", seed, "\n")
cat("naspaclust stopping iteration:", ref_stop, "\n\n")

cat("Key trajectory points:\n")
print(summary_table, row.names = FALSE, digits = 8)

cat("\nFirst objective relative change < 1e-6:", first_rel_1e6, "\n")
cat("First objective relative change < 1e-8:", first_rel_1e8, "\n")
cat("First centroid Frobenius change < 1e-6:", first_center_1e6, "\n")
cat("First centroid Frobenius change < 1e-8:", first_center_1e8, "\n")
cat("First min centroid distance < 1e-3:", first_sep_1e3, "\n")
cat("First min centroid distance < 1e-4:", first_sep_1e4, "\n")
cat("First min centroid distance < 1e-5:", first_sep_1e5, "\n")

# A compact diagnosis.
at_ref <- trace[ref_stop, ]
at_500 <- trace[500, ]

cat("\nAt naspaclust stopping iteration:\n")
cat("  objective =", at_ref$objective, "\n")
cat("  centroid change =", at_ref$centroid_frobenius_change, "\n")
cat("  min centroid distance =", at_ref$min_centroid_distance, "\n")
cat("  XB =", at_ref$XB, "\n")

cat("\nAt iteration 500:\n")
cat("  objective =", at_500$objective, "\n")
cat("  centroid change =", at_500$centroid_frobenius_change, "\n")
cat("  min centroid distance =", at_500$min_centroid_distance, "\n")
cat("  XB =", at_500$XB, "\n")

cat("\nObjective improvement from reference stop to iteration 500:\n")
cat("  absolute =", at_ref$objective - at_500$objective, "\n")
cat(
  "  relative =",
  (at_ref$objective - at_500$objective) /
    max(abs(at_ref$objective), .Machine$double.eps),
  "\n"
)

cat("\nCentroid-separation shrinkage factor (reference stop / iter 500):\n")
cat(
  " ",
  at_ref$min_centroid_distance /
    max(at_500$min_centroid_distance, .Machine$double.eps),
  "x\n"
)

# Save reproducible evidence.
results_dir <- file.path("validation_results", "fgwc_convergence")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  trace,
  file.path(results_dir, "fgwc_convergence_trace.csv"),
  row.names = FALSE
)
utils::write.csv(
  summary_table,
  file.path(results_dir, "fgwc_key_trajectory_points.csv"),
  row.names = FALSE
)

decision <- data.frame(
  naspaclust_stop_iteration = ref_stop,
  first_objective_rel_change_lt_1e6 = first_rel_1e6,
  first_objective_rel_change_lt_1e8 = first_rel_1e8,
  first_centroid_change_lt_1e6 = first_center_1e6,
  first_centroid_change_lt_1e8 = first_center_1e8,
  first_min_centroid_distance_lt_1e3 = first_sep_1e3,
  first_min_centroid_distance_lt_1e4 = first_sep_1e4,
  first_min_centroid_distance_lt_1e5 = first_sep_1e5,
  objective_at_reference_stop = at_ref$objective,
  objective_at_500 = at_500$objective,
  min_centroid_distance_at_reference_stop = at_ref$min_centroid_distance,
  min_centroid_distance_at_500 = at_500$min_centroid_distance,
  XB_at_reference_stop = at_ref$XB,
  XB_at_500 = at_500$XB
)

utils::write.csv(
  decision,
  file.path(results_dir, "fgwc_convergence_diagnosis.csv"),
  row.names = FALSE
)

capture.output(
  sessionInfo(),
  file = file.path(results_dir, "sessionInfo.txt")
)

cat("\nResults written to:\n", normalizePath(results_dir), "\n")
