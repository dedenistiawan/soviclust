# =============================================================================
# FGWC Reference Validation v2: soviclust vs naspaclust 0.2.2
# =============================================================================
#
# This version works around a naspaclust 0.2.2 implementation issue:
# fgwcuv() uses `if (is.na(vi))`, so passing a centroid matrix directly causes
# "the condition has length > 1".
#
# We DO NOT modify naspaclust.
# Instead:
#   1. obtain the exact centroid generator from the naspaclust namespace;
#   2. generate the common initial centroids with the same randomN;
#   3. let naspaclust regenerate those centroids internally (vi = NA);
#   4. pass the identical generated centroid matrix explicitly to soviclust.
#
# Run from the root directory of the soviclust source project.
# =============================================================================

if (!requireNamespace("naspaclust", quietly = TRUE)) {
  stop("Install 'naspaclust' first with install.packages('naspaclust').",
       call. = FALSE)
}

if (!requireNamespace("rdist", quietly = TRUE)) {
  stop("Install 'rdist' first.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 1. Load patched soviclust numerical core
# -----------------------------------------------------------------------------

alg_env <- new.env(parent = globalenv())

source_files <- c(
  "inst/app/R/shared/function/index.R",
  "inst/app/R/shared/function/fgwc.R"
)

missing_files <- source_files[!file.exists(source_files)]
if (length(missing_files) > 0L) {
  stop(
    "Run this script from the root of the soviclust project. Missing: ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

for (f in source_files) {
  sys.source(f, envir = alg_env)
}


# -----------------------------------------------------------------------------
# 2. Reference data bundled with naspaclust
# -----------------------------------------------------------------------------

data("census2010", package = "naspaclust", envir = environment())
data("census2010pop", package = "naspaclust", envir = environment())
data("census2010dist", package = "naspaclust", envir = environment())

raw <- as.data.frame(census2010)

usable <- vapply(
  raw,
  function(z) {
    is.numeric(z) &&
      all(is.finite(z)) &&
      stats::sd(z) > 0
  },
  logical(1)
)

feature_names <- names(raw)[usable]

if (length(feature_names) < 5L) {
  stop("Fewer than five usable numeric variables found in census2010.",
       call. = FALSE)
}

feature_names <- feature_names[seq_len(min(6L, length(feature_names)))]
x_raw <- as.matrix(raw[, feature_names, drop = FALSE])

minmax <- function(z) {
  rng <- range(z)
  if (diff(rng) == 0) return(rep(0, length(z)))
  (z - rng[1]) / diff(rng)
}

x <- apply(x_raw, 2, minmax)
x <- as.matrix(x)

pop <- as.numeric(census2010pop)
dmat <- as.matrix(census2010dist)

stopifnot(
  nrow(x) == length(pop),
  all(dim(dmat) == c(nrow(x), nrow(x)))
)


# -----------------------------------------------------------------------------
# 3. Common parameters and EXACT naspaclust initialization
# -----------------------------------------------------------------------------

ncluster <- 3L
m <- 2
alpha <- 0.7
a <- 1
b <- 1
distance <- "euclidean"
order <- 2
seed <- 2026L
tol <- 1e-10

# Obtain the reference generator without changing naspaclust.
ref_gen_vi <- getFromNamespace("gen_vi", "naspaclust")

# This is exactly what naspaclust will generate internally when vi = NA.
init_v <- ref_gen_vi(
  data = x,
  ncluster = ncluster,
  gendist = "uniform",
  randomN = seed
)


# -----------------------------------------------------------------------------
# 4. Independent comparison helpers
# -----------------------------------------------------------------------------

common_objective <- function(data, membership, centroid, m = 2) {
  d2 <- rdist::cdist(
    as.matrix(data),
    as.matrix(centroid),
    "euclidean",
    2
  )^2

  sum((as.matrix(membership)^m) * d2)
}


adjusted_rand_index <- function(x, y) {
  tab <- table(x, y)
  choose2 <- function(z) z * (z - 1) / 2

  sum_ij <- sum(choose2(tab))
  sum_i <- sum(choose2(rowSums(tab)))
  sum_j <- sum(choose2(colSums(tab)))
  n <- sum(tab)
  total <- choose2(n)

  if (total == 0) return(1)

  expected <- (sum_i * sum_j) / total
  max_index <- 0.5 * (sum_i + sum_j)
  denom <- max_index - expected

  if (abs(denom) < .Machine$double.eps) return(1)

  (sum_ij - expected) / denom
}


max_abs_diff <- function(a, b) {
  max(abs(as.numeric(a) - as.numeric(b)))
}


# -----------------------------------------------------------------------------
# 5. LEVEL A — ONE-STEP KERNEL EQUIVALENCE
# -----------------------------------------------------------------------------
#
# naspaclust must receive vi = NA because of its scalar is.na(vi) check.
# With the same randomN, it internally regenerates exactly `init_v`.
# soviclust receives `init_v` explicitly.
# -----------------------------------------------------------------------------

ref_1 <- naspaclust::fgwcuv(
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
  max.iter = 1,
  error = 0,
  randomN = seed
)

sovi_1 <- alg_env$fgwcuv(
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
  max.iter = 1,
  error = 0,
  randomN = seed,
  vi = init_v
)

one_step <- data.frame(
  Metric = c(
    "Max abs membership difference",
    "Max abs centroid difference",
    "Hard-label agreement",
    "soviclust membership row-sum error",
    "naspaclust membership row-sum error",
    "Common objective - soviclust",
    "Common objective - naspaclust"
  ),
  Value = c(
    max_abs_diff(sovi_1$membership, ref_1$membership),
    max_abs_diff(sovi_1$centroid, ref_1$centroid),
    mean(sovi_1$cluster == ref_1$cluster),
    max(abs(rowSums(sovi_1$membership) - 1)),
    max(abs(rowSums(ref_1$membership) - 1)),
    common_objective(x, sovi_1$membership, sovi_1$centroid, m),
    common_objective(x, ref_1$membership, ref_1$centroid, m)
  ),
  Criterion = c(
    "<= 1e-10",
    "<= 1e-10",
    "1.0",
    "<= 1e-10",
    "<= 1e-10",
    "descriptive",
    "descriptive"
  )
)

one_step$Pass <- c(
  one_step$Value[1] <= tol,
  one_step$Value[2] <= tol,
  abs(one_step$Value[3] - 1) <= tol,
  one_step$Value[4] <= tol,
  one_step$Value[5] <= tol,
  NA,
  NA
)


# Stable indices expected to agree when partition and centroids agree.
stable_indices <- c("PC", "CE", "SC", "SI", "IFV")

index_step <- data.frame(
  Index = stable_indices,
  naspaclust = vapply(
    stable_indices,
    function(z) as.numeric(ref_1$validation[[z]]),
    numeric(1)
  ),
  soviclust = vapply(
    stable_indices,
    function(z) as.numeric(sovi_1$validation[[z]]),
    numeric(1)
  )
)

index_step$AbsDifference <- abs(index_step$soviclust - index_step$naspaclust)


# Re-evaluate ALL indices with the corrected soviclust formulas.
corrected_ref_indices_1 <- alg_env$index_fgwc(
  data = x,
  cluster = ref_1$cluster,
  uij = ref_1$membership,
  vi = ref_1$centroid,
  m = m
)

corrected_sovi_indices_1 <- alg_env$index_fgwc(
  data = x,
  cluster = sovi_1$cluster,
  uij = sovi_1$membership,
  vi = sovi_1$centroid,
  m = m
)

corrected_index_step <- data.frame(
  Index = names(corrected_ref_indices_1),
  ReferenceOutput_Reevaluated = unlist(corrected_ref_indices_1),
  soviclust = unlist(corrected_sovi_indices_1)
)

corrected_index_step$AbsDifference <- abs(
  corrected_index_step$ReferenceOutput_Reevaluated -
    corrected_index_step$soviclust
)


# -----------------------------------------------------------------------------
# 6. LEVEL B — FULL-CONVERGENCE COMPARISON
# -----------------------------------------------------------------------------

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

sovi_full <- alg_env$fgwcuv(
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
  randomN = seed,
  vi = init_v
)

ref_common_obj <- common_objective(
  x, ref_full$membership, ref_full$centroid, m
)

sovi_common_obj <- common_objective(
  x, sovi_full$membership, sovi_full$centroid, m
)

ari <- adjusted_rand_index(ref_full$cluster, sovi_full$cluster)

full_summary <- data.frame(
  Metric = c(
    "Iterations",
    "Reported objective",
    "Common corrected objective",
    "Membership row-sum max error"
  ),
  naspaclust = c(
    ref_full$iteration,
    ref_full$f_obj,
    ref_common_obj,
    max(abs(rowSums(ref_full$membership) - 1))
  ),
  soviclust = c(
    sovi_full$iteration,
    sovi_full$f_obj,
    sovi_common_obj,
    max(abs(rowSums(sovi_full$membership) - 1))
  )
)

corrected_ref_indices_full <- alg_env$index_fgwc(
  data = x,
  cluster = ref_full$cluster,
  uij = ref_full$membership,
  vi = ref_full$centroid,
  m = m
)

corrected_sovi_indices_full <- alg_env$index_fgwc(
  data = x,
  cluster = sovi_full$cluster,
  uij = sovi_full$membership,
  vi = sovi_full$centroid,
  m = m
)

full_indices <- data.frame(
  Index = names(corrected_ref_indices_full),
  naspaclust_result_reevaluated = unlist(corrected_ref_indices_full),
  soviclust = unlist(corrected_sovi_indices_full)
)

full_indices$Difference <- (
  full_indices$soviclust -
    full_indices$naspaclust_result_reevaluated
)


# -----------------------------------------------------------------------------
# 7. Console report
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("FGWC REFERENCE VALIDATION v2: soviclust vs naspaclust\n")
cat("============================================================\n")
cat("naspaclust version:", as.character(packageVersion("naspaclust")), "\n")
cat("Features:", paste(feature_names, collapse = ", "), "\n")
cat("n =", nrow(x), "| p =", ncol(x), "| c =", ncluster, "\n")
cat("alpha =", alpha, "| m =", m, "| seed =", seed, "\n\n")

cat("LEVEL A - ONE-STEP KERNEL EQUIVALENCE\n")
cat("-------------------------------------\n")
print(one_step, row.names = FALSE)

cat("\nStable indices from each package:\n")
print(index_step, row.names = FALSE)

cat("\nAll indices reevaluated with corrected soviclust formulas:\n")
print(corrected_index_step, row.names = FALSE)

cat("\nLEVEL B - FULL-CONVERGENCE COMPARISON\n")
cat("-------------------------------------\n")
print(full_summary, row.names = FALSE)

cat("\nAdjusted Rand Index (label-invariant partition agreement):\n")
print(ari)

cat("\nFinal solutions reevaluated with common corrected indices:\n")
print(full_indices, row.names = FALSE)


# -----------------------------------------------------------------------------
# 8. Decision
# -----------------------------------------------------------------------------

kernel_pass <- all(one_step$Pass[!is.na(one_step$Pass)])

stable_index_pass <- all(
  is.finite(index_step$AbsDifference) &
    index_step$AbsDifference <= 1e-8
)

cat("\n============================================================\n")
cat("VALIDATION DECISION\n")
cat("============================================================\n")

if (kernel_pass && stable_index_pass) {
  cat("PASS: FGWC one-step numerical kernel is reference-equivalent.\n")
} else {
  cat("FAIL: One-step reference equivalence was not achieved.\n")
  cat("Inspect membership, centroid, and stable-index differences.\n")
}

cat(
  "Full convergence is diagnostic only because soviclust intentionally ",
  "uses corrected stopping/objective/index calculations.\n",
  sep = ""
)


# -----------------------------------------------------------------------------
# 9. Save results
# -----------------------------------------------------------------------------

results_dir <- file.path("validation_results", "fgwc_reference")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  one_step,
  file.path(results_dir, "01_one_step_equivalence.csv"),
  row.names = FALSE
)

utils::write.csv(
  index_step,
  file.path(results_dir, "02_one_step_stable_indices.csv"),
  row.names = FALSE
)

utils::write.csv(
  corrected_index_step,
  file.path(results_dir, "03_one_step_corrected_indices.csv"),
  row.names = FALSE
)

utils::write.csv(
  full_summary,
  file.path(results_dir, "04_full_convergence_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  full_indices,
  file.path(results_dir, "05_full_convergence_corrected_indices.csv"),
  row.names = FALSE
)

utils::write.csv(
  data.frame(
    adjusted_rand_index = ari,
    kernel_equivalence_pass = kernel_pass,
    stable_indices_pass = stable_index_pass,
    naspaclust_version = as.character(packageVersion("naspaclust")),
    R_version = R.version.string
  ),
  file.path(results_dir, "06_validation_decision.csv"),
  row.names = FALSE
)

capture.output(
  sessionInfo(),
  file = file.path(results_dir, "sessionInfo.txt")
)

cat("\nResults written to:\n", normalizePath(results_dir), "\n")
