# =============================================================================
# soviclust — V8 NFE-Fair Pilot
# =============================================================================
#
# Purpose
# -------
# Run a computationally fair multi-budget, multi-seed pilot benchmark for the
# nine Patch-v3 optimizers using EXACT TOTAL NFE budgets.
#
# This is the first optimizer-comparison experiment after Patch v3.1b.
# It is still a PILOT, not the final 30-run benchmark.
#
# Design
# ------
#   Optimizers : ABC, FPA, GSA, GWO, HHO, IFA, PSO, TLBO, WOA
#   Budgets    : 200, 500, 1000, 2000 TOTAL NFE
#   Seeds      : 2026, 2027, 2028, 2029, 2030
#   Runs       : 9 x 4 x 5 = 180
#   Dataset    : bundled Indonesia 514 x 15 standardized indicators
#   c          : 4
#   m          : 2
#   alpha      : 0.7
#   a = b      : 1
#   population : 10
#   fitness    : spatial_XB_feasible (lower is better)
#
# Fairness
# --------
# Every optimizer receives the same TOTAL candidate-evaluation budget.
# Initialization evaluations are included in max_nfe.
#
# Resume behavior
# ---------------
# Results are written after EVERY run. If the script is interrupted, rerunning
# it will skip already completed PASS runs with the same method/seed/budget.
#
# Main outputs
# ------------
# validation_results/v8_nfe_fair_pilot/
#   v8_pilot_config.csv
#   v8_pilot_runs.csv
#   v8_pilot_cluster_support.csv
#   v8_pilot_summary.csv
#   v8_pilot_seed_ranks.csv
#   v8_pilot_rank_summary.csv
#   v8_pilot_budget_improvement.csv
#   v8_pilot_monotonicity.csv
#   v8_pilot_gate_summary.csv
#
# Run from project root:
#   source("validation/v8_nfe_fair_pilot.R")
#
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Experiment configuration
# -----------------------------------------------------------------------------

SEEDS <- 2026:2030
BUDGETS <- c(200L, 500L, 1000L, 2000L)

METHODS <- c(
  "ABC", "FPA", "GSA", "GWO", "HHO",
  "IFA", "PSO", "TLBO", "WOA"
)

NCLUSTER <- 4L
M <- 2

ALPHA <- 0.7
A_SPATIAL <- 1
B_SPATIAL <- 1

DISTANCE <- "euclidean"
ORDER <- 2

POP_SIZE <- 10L

# Safety cap only. max_nfe should bind first.
MAX_ITER <- 5000L

# Disable stagnation stopping so equal-NFE budget is the intended stop rule.
SAME_LIMIT <- MAX_ITER + 1000L
ERROR_TOL <- 0

# Resume policy:
# TRUE  = rerun rows recorded as FAIL/ERROR
# FALSE = keep failed rows and skip them too
RERUN_FAILED <- TRUE

# Numerical tolerances
MONOTONIC_TOL <- 1e-10


# -----------------------------------------------------------------------------
# 1. Project-root detection
# -----------------------------------------------------------------------------

find_project_root <- function(start = getwd()) {

  p <- normalizePath(
    start,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {

    required <- c(
      file.path(p, "DESCRIPTION"),
      file.path(
        p,
        "inst", "app", "R", "shared", "function",
        "optimizer_v3.R"
      ),
      file.path(
        p,
        "tests", "testthat",
        "helper-v3-engine.R"
      )
    )

    if (all(file.exists(required))) {
      return(p)
    }

    parent <- dirname(p)

    if (identical(parent, p)) {
      break
    }

    p <- parent
  }

  stop(
    "Unable to locate the soviclust source-project root.",
    call. = FALSE
  )
}


root <- find_project_root()


# -----------------------------------------------------------------------------
# 2. Load isolated Patch-v3 engine
# -----------------------------------------------------------------------------

helper_path <- file.path(
  root,
  "tests",
  "testthat",
  "helper-v3-engine.R"
)

helper_env <- new.env(
  parent = globalenv()
)

sys.source(
  helper_path,
  envir = helper_env
)

v3 <- helper_env$v3_env

if (!is.environment(v3)) {
  stop(
    "Patch-v3 isolated optimizer environment was not created.",
    call. = FALSE
  )
}


required_engine_functions <- c(
  ".soviclust_v3_validate_common",
  ".soviclust_v3_eval",
  ".soviclust_v3_try_eval",
  ".soviclust_v3_nfe_snapshot"
)

missing_engine_functions <- required_engine_functions[
  !vapply(
    required_engine_functions,
    exists,
    logical(1),
    envir = v3,
    inherits = FALSE,
    mode = "function"
  )
]

if (length(missing_engine_functions)) {
  stop(
    paste0(
      "Patch v3.1b infrastructure is incomplete. Missing: ",
      paste(
        missing_engine_functions,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 3. Load bundled Indonesia validation data
# -----------------------------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop(
    "Package 'readxl' is required for the validation dataset.",
    call. = FALSE
  )
}


data_path <- file.path(
  root,
  "inst",
  "extdata",
  "sovi_data_kab_514_15.xlsx"
)

pop_path <- file.path(
  root,
  "inst",
  "extdata",
  "sovi_data_pop_514.xlsx"
)

dist_path <- file.path(
  root,
  "inst",
  "extdata",
  "Distance_matrix_514.xlsx"
)


required_files <- c(
  data_path,
  pop_path,
  dist_path
)

if (!all(file.exists(required_files))) {
  stop(
    paste0(
      "One or more bundled V8 validation files are missing:\n",
      paste(
        required_files[!file.exists(required_files)],
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


raw_data <- as.data.frame(
  readxl::read_excel(
    data_path
  )
)

indicator_cols <- tail(
  names(raw_data),
  15L
)

x_raw <- data.matrix(
  raw_data[
    ,
    indicator_cols,
    drop = FALSE
  ]
)

x <- unclass(
  scale(x_raw)
)

storage.mode(x) <- "double"


if (
  nrow(x) != 514L ||
  ncol(x) != 15L
) {
  stop(
    paste0(
      "Unexpected validation-data dimensions: ",
      nrow(x),
      " x ",
      ncol(x),
      ". Expected 514 x 15."
    ),
    call. = FALSE
  )
}


pop_df <- as.data.frame(
  readxl::read_excel(
    pop_path
  )
)

numeric_pop <- which(
  vapply(
    pop_df,
    is.numeric,
    logical(1)
  )
)

if (!length(numeric_pop)) {
  stop(
    "No numeric population column was found.",
    call. = FALSE
  )
}


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
  tail(
    numeric_pop,
    1L
  )
}

pop <- as.numeric(
  pop_df[[pop_col]]
)


dist_df <- as.data.frame(
  readxl::read_excel(
    dist_path
  )
)

if (
  ncol(dist_df) == nrow(x) + 1L ||
  !is.numeric(dist_df[[1L]])
) {
  dist_df <- dist_df[
    ,
    -1L,
    drop = FALSE
  ]
}

distmat <- data.matrix(
  dist_df
)

if (
  nrow(distmat) != nrow(x) ||
  ncol(distmat) != nrow(x)
) {
  stop(
    paste0(
      "Distance matrix has dimensions ",
      nrow(distmat),
      " x ",
      ncol(distmat),
      "; expected ",
      nrow(x),
      " x ",
      nrow(x),
      "."
    ),
    call. = FALSE
  )
}

diag(distmat) <- 0


# -----------------------------------------------------------------------------
# 4. Output paths
# -----------------------------------------------------------------------------

out_dir <- file.path(
  root,
  "validation_results",
  "v8_nfe_fair_pilot"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


runs_path <- file.path(
  out_dir,
  "v8_pilot_runs.csv"
)

support_path <- file.path(
  out_dir,
  "v8_pilot_cluster_support.csv"
)

config_path <- file.path(
  out_dir,
  "v8_pilot_config.csv"
)

summary_path <- file.path(
  out_dir,
  "v8_pilot_summary.csv"
)

seed_ranks_path <- file.path(
  out_dir,
  "v8_pilot_seed_ranks.csv"
)

rank_summary_path <- file.path(
  out_dir,
  "v8_pilot_rank_summary.csv"
)

budget_improvement_path <- file.path(
  out_dir,
  "v8_pilot_budget_improvement.csv"
)

monotonicity_path <- file.path(
  out_dir,
  "v8_pilot_monotonicity.csv"
)

gate_path <- file.path(
  out_dir,
  "v8_pilot_gate_summary.csv"
)


# -----------------------------------------------------------------------------
# 5. Utilities
# -----------------------------------------------------------------------------

hard_counts <- function(
    u,
    k = NCLUSTER) {

  tabulate(
    apply(
      as.matrix(u),
      1,
      which.max
    ),
    nbins = k
  )
}


min_centroid_separation <- function(centers) {

  centers <- as.matrix(centers)

  if (nrow(centers) < 2L) {
    return(NA_real_)
  }

  d <- as.matrix(
    stats::dist(
      centers
    )
  )

  diag(d) <- Inf

  min(
    d,
    na.rm = TRUE
  )
}


safe_runtime_seconds <- function(elapsed) {

  if (
    length(elapsed) != 1L ||
    !is.finite(elapsed)
  ) {
    return(NA_real_)
  }

  as.numeric(elapsed)
}


key_string <- function(
    method,
    seed,
    budget) {

  paste(
    method,
    seed,
    budget,
    sep = "::"
  )
}


append_or_replace_row <- function(
    df,
    row,
    key_cols = c(
      "method",
      "seed",
      "max_nfe"
    )) {

  if (is.null(df) || !nrow(df)) {
    return(row)
  }

  same <- rep(
    TRUE,
    nrow(df)
  )

  for (nm in key_cols) {
    same <- same &
      as.character(df[[nm]]) ==
      as.character(row[[nm]][1L])
  }

  if (any(same)) {
    df <- df[
      !same,
      ,
      drop = FALSE
    ]
  }

  rbind(
    df,
    row
  )
}


atomic_write_csv <- function(
    df,
    path) {

  tmp <- paste0(
    path,
    ".tmp"
  )

  utils::write.csv(
    df,
    tmp,
    row.names = FALSE,
    na = ""
  )

  if (file.exists(path)) {
    unlink(path)
  }

  ok <- file.rename(
    tmp,
    path
  )

  if (!ok) {
    stop(
      paste0(
        "Unable to write output file atomically: ",
        path
      ),
      call. = FALSE
    )
  }

  invisible(path)
}


# -----------------------------------------------------------------------------
# 6. Shared optimizer specifications
# -----------------------------------------------------------------------------

optimizer_specs <- list(

  ABC = list(
    fun = "abcfgwc",
    extra = list(
      nfood = POP_SIZE,
      n.onlooker = max(
        1L,
        floor(
          POP_SIZE / 2L
        )
      ),
      limit = 4,
      pso = FALSE,
      abc.same = SAME_LIMIT
    )
  ),

  FPA = list(
    fun = "fpafgwc",
    extra = list(
      nflow = POP_SIZE,
      p = 0.8,
      flow.same = SAME_LIMIT
    )
  ),

  GSA = list(
    fun = "gsafgwc",
    extra = list(
      npar = POP_SIZE,
      par.no = 2,
      gsa.same = SAME_LIMIT,
      G = 1,
      vmax = 0.7,
      new = FALSE
    )
  ),

  GWO = list(
    fun = "gwofgwc",
    extra = list(
      nwolf = POP_SIZE,
      wolf.same = SAME_LIMIT
    )
  ),

  HHO = list(
    fun = "hhofgwc",
    extra = list(
      nhh = POP_SIZE,
      hh.alg = "heidari",
      hh.same = SAME_LIMIT
    )
  ),

  IFA = list(
    fun = "ifafgwc",
    extra = list(
      nfly = POP_SIZE,
      ffly.no = 2,
      fa.same = SAME_LIMIT
    )
  ),

  PSO = list(
    fun = "psofgwc",
    extra = list(
      npar = POP_SIZE,
      pso.same = SAME_LIMIT
    )
  ),

  TLBO = list(
    fun = "tlbofgwc",
    extra = list(
      nstud = POP_SIZE,
      nselection = POP_SIZE,
      tlbo.same = SAME_LIMIT
    )
  ),

  WOA = list(
    fun = "woafgwc",
    extra = list(
      nwhale = POP_SIZE,
      woa.same = SAME_LIMIT
    )
  )
)


run_method <- function(
    method,
    seed,
    max_nfe) {

  spec <- optimizer_specs[[method]]

  fn <- get(
    spec$fun,
    envir = v3,
    inherits = FALSE
  )

  common_args <- list(
    data = x,
    pop = pop,
    distmat = distmat,
    ncluster = NCLUSTER,
    m = M,
    distance = DISTANCE,
    order = ORDER,
    alpha = ALPHA,
    a = A_SPATIAL,
    b = B_SPATIAL,
    error = ERROR_TOL,
    max.iter = MAX_ITER,
    max_nfe = as.integer(max_nfe),
    randomN = as.integer(seed),
    vi.dist = "uniform"
  )

  do.call(
    fn,
    c(
      common_args,
      spec$extra
    )
  )
}


# -----------------------------------------------------------------------------
# 7. Save experiment configuration
# -----------------------------------------------------------------------------

config_df <- data.frame(
  parameter = c(
    "dataset_n",
    "dataset_p",
    "ncluster",
    "m",
    "alpha",
    "a",
    "b",
    "population",
    "budgets",
    "seeds",
    "n_methods",
    "planned_runs",
    "max_iter_safety_cap",
    "same_limit",
    "error",
    "fitness_type",
    "budget_definition",
    "resume_enabled"
  ),
  value = c(
    nrow(x),
    ncol(x),
    NCLUSTER,
    M,
    ALPHA,
    A_SPATIAL,
    B_SPATIAL,
    POP_SIZE,
    paste(
      BUDGETS,
      collapse = ";"
    ),
    paste(
      SEEDS,
      collapse = ";"
    ),
    length(METHODS),
    length(METHODS) *
      length(BUDGETS) *
      length(SEEDS),
    MAX_ITER,
    SAME_LIMIT,
    ERROR_TOL,
    "spatial_XB_feasible",
    "total = initialization + optimization",
    TRUE
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  config_df,
  config_path
)


# -----------------------------------------------------------------------------
# 8. Load previous results for resume
# -----------------------------------------------------------------------------

runs_df <- if (file.exists(runs_path)) {
  utils::read.csv(
    runs_path,
    stringsAsFactors = FALSE
  )
} else {
  NULL
}


support_df <- if (file.exists(support_path)) {
  utils::read.csv(
    support_path,
    stringsAsFactors = FALSE
  )
} else {
  NULL
}


completed_pass_keys <- character()

if (
  !is.null(runs_df) &&
  nrow(runs_df)
) {

  pass_rows <- runs_df[
    runs_df$status == "PASS",
    ,
    drop = FALSE
  ]

  if (nrow(pass_rows)) {

    completed_pass_keys <- mapply(
      key_string,
      pass_rows$method,
      pass_rows$seed,
      pass_rows$max_nfe,
      USE.NAMES = FALSE
    )
  }
}


# -----------------------------------------------------------------------------
# 9. Execute 180-run NFE-fair pilot
# -----------------------------------------------------------------------------

planned_runs <- length(METHODS) *
  length(BUDGETS) *
  length(SEEDS)

run_counter <- 0L
executed_counter <- 0L
skipped_counter <- 0L


cat("\n")
cat("====================================================================\n")
cat("V8 — NFE-Fair Pilot\n")
cat("====================================================================\n")
cat(
  "Dataset      : ",
  nrow(x),
  " x ",
  ncol(x),
  "\n",
  sep = ""
)
cat(
  "Methods      : ",
  paste(
    METHODS,
    collapse = ", "
  ),
  "\n",
  sep = ""
)
cat(
  "Budgets      : ",
  paste(
    BUDGETS,
    collapse = ", "
  ),
  "\n",
  sep = ""
)
cat(
  "Seeds        : ",
  paste(
    SEEDS,
    collapse = ", "
  ),
  "\n",
  sep = ""
)
cat(
  "Planned runs : ",
  planned_runs,
  "\n",
  sep = ""
)
cat(
  "Population   : ",
  POP_SIZE,
  "\n",
  sep = ""
)
cat(
  "Fitness      : spatial_XB_feasible\n"
)
cat("====================================================================\n")


for (budget in BUDGETS) {

  cat(
    "\n======================== BUDGET ",
    budget,
    " ========================\n",
    sep = ""
  )

  for (seed in SEEDS) {

    cat(
      "\n--- Seed ",
      seed,
      " / budget ",
      budget,
      " ---\n",
      sep = ""
    )

    for (method in METHODS) {

      run_counter <- run_counter + 1L

      key <- key_string(
        method,
        seed,
        budget
      )


      already_pass <- key %in%
        completed_pass_keys


      already_failed <- FALSE

      if (
        !is.null(runs_df) &&
        nrow(runs_df)
      ) {

        matching <- (
          runs_df$method == method &
          runs_df$seed == seed &
          runs_df$max_nfe == budget
        )

        already_failed <- any(
          matching &
          runs_df$status != "PASS"
        )
      }


      if (
        already_pass ||
        (
          already_failed &&
          !RERUN_FAILED
        )
      ) {

        skipped_counter <- skipped_counter + 1L

        cat(
          sprintf(
            "[%3d/%3d] %-4s seed=%d NFE=%d : SKIP existing %s\n",
            run_counter,
            planned_runs,
            method,
            seed,
            budget,
            if (already_pass) {
              "PASS"
            } else {
              "FAIL"
            }
          )
        )

        next
      }


      executed_counter <- executed_counter + 1L


      cat(
        sprintf(
          "[%3d/%3d] %-4s seed=%d NFE=%d : ",
          run_counter,
          planned_runs,
          method,
          seed,
          budget
        )
      )


      start_time <- proc.time()[["elapsed"]]


      res <- tryCatch(
        run_method(
          method = method,
          seed = seed,
          max_nfe = budget
        ),
        error = function(e) e
      )


      elapsed <- proc.time()[["elapsed"]] -
        start_time


      if (inherits(res, "error")) {

        row <- data.frame(
          method = method,
          seed = as.integer(seed),
          max_nfe = as.integer(budget),
          status = "ERROR",
          failure_reason = conditionMessage(res),
          nfe = NA_integer_,
          nfe_initialization = NA_integer_,
          nfe_optimization = NA_integer_,
          nfe_remaining = NA_integer_,
          budget_exhausted = NA,
          termination_reason = NA_character_,
          exact_budget = FALSE,
          nfe_invariant = FALSE,
          fitness_type = NA_character_,
          final_XB = NA_real_,
          spatial_J = NA_real_,
          occupied = NA_integer_,
          min_cluster_size = NA_integer_,
          min_centroid_separation = NA_real_,
          iterations = NA_integer_,
          runtime_sec = safe_runtime_seconds(
            elapsed
          ),
          stringsAsFactors = FALSE
        )


        runs_df <- append_or_replace_row(
          runs_df,
          row
        )


        atomic_write_csv(
          runs_df,
          runs_path
        )


        cat(
          "ERROR | ",
          conditionMessage(res),
          "\n",
          sep = ""
        )

        next
      }


      counts <- hard_counts(
        res$membership
      )

      occupied <- sum(
        counts > 0L
      )

      min_cluster_size <- min(
        counts
      )


      exact_budget <- (
        identical(
          as.integer(res$nfe),
          as.integer(budget)
        ) &&
        identical(
          as.integer(res$max_nfe),
          as.integer(budget)
        ) &&
        identical(
          as.integer(res$nfe_remaining),
          0L
        ) &&
        isTRUE(
          res$budget_exhausted
        ) &&
        identical(
          res$termination_reason,
          "max_nfe"
        )
      )


      nfe_invariant <- identical(
        as.integer(res$nfe),
        as.integer(
          res$nfe_initialization +
            res$nfe_optimization
        )
      )


      fitness_type_ok <- identical(
        res$fitness_type,
        "spatial_XB_feasible"
      )


      occupancy_ok <- occupied ==
        NCLUSTER


      finite_solution <- all(
        is.finite(
          c(
            as.numeric(res$f_obj),
            as.numeric(res$spatial_obj),
            as.numeric(res$centroid),
            as.numeric(res$membership)
          )
        )
      )


      gate_pass <- all(
        exact_budget,
        nfe_invariant,
        fitness_type_ok,
        occupancy_ok,
        finite_solution
      )


      failures <- character()

      if (!exact_budget) {
        failures <- c(
          failures,
          "exact NFE budget"
        )
      }

      if (!nfe_invariant) {
        failures <- c(
          failures,
          "NFE invariant"
        )
      }

      if (!fitness_type_ok) {
        failures <- c(
          failures,
          "fitness type"
        )
      }

      if (!occupancy_ok) {
        failures <- c(
          failures,
          "4/4 occupancy"
        )
      }

      if (!finite_solution) {
        failures <- c(
          failures,
          "finite solution"
        )
      }


      row <- data.frame(
        method = method,
        seed = as.integer(seed),
        max_nfe = as.integer(budget),
        status = if (gate_pass) {
          "PASS"
        } else {
          "FAIL"
        },
        failure_reason = if (length(failures)) {
          paste(
            failures,
            collapse = "; "
          )
        } else {
          ""
        },
        nfe = as.integer(
          res$nfe
        ),
        nfe_initialization = as.integer(
          res$nfe_initialization
        ),
        nfe_optimization = as.integer(
          res$nfe_optimization
        ),
        nfe_remaining = as.integer(
          res$nfe_remaining
        ),
        budget_exhausted = isTRUE(
          res$budget_exhausted
        ),
        termination_reason = as.character(
          res$termination_reason
        ),
        exact_budget = exact_budget,
        nfe_invariant = nfe_invariant,
        fitness_type = as.character(
          res$fitness_type
        ),
        final_XB = as.numeric(
          res$f_obj
        ),
        spatial_J = as.numeric(
          res$spatial_obj
        ),
        occupied = as.integer(
          occupied
        ),
        min_cluster_size = as.integer(
          min_cluster_size
        ),
        min_centroid_separation =
          min_centroid_separation(
            res$centroid
          ),
        iterations = as.integer(
          res$iteration
        ),
        runtime_sec = safe_runtime_seconds(
          elapsed
        ),
        stringsAsFactors = FALSE
      )


      runs_df <- append_or_replace_row(
        runs_df,
        row
      )


      # Replace cluster-support rows for the same method/seed/budget.
      if (
        !is.null(support_df) &&
        nrow(support_df)
      ) {

        same_support <- (
          support_df$method == method &
          support_df$seed == seed &
          support_df$max_nfe == budget
        )

        support_df <- support_df[
          !same_support,
          ,
          drop = FALSE
        ]
      }


      new_support <- do.call(
        rbind,
        lapply(
          seq_len(NCLUSTER),
          function(k) {

            data.frame(
              method = method,
              seed = as.integer(seed),
              max_nfe = as.integer(budget),
              cluster = as.integer(k),
              hard_count = as.integer(
                counts[k]
              ),
              hard_proportion = as.numeric(
                counts[k] /
                  nrow(x)
              ),
              soft_mass = as.numeric(
                sum(
                  res$membership[
                    ,
                    k
                  ]
                )
              ),
              effective_mass_m2 = as.numeric(
                sum(
                  res$membership[
                    ,
                    k
                  ]^M
                )
              ),
              stringsAsFactors = FALSE
            )
          }
        )
      )


      if (
        is.null(support_df) ||
        !nrow(support_df)
      ) {

        support_df <- new_support

      } else {

        support_df <- rbind(
          support_df,
          new_support
        )
      }


      atomic_write_csv(
        runs_df,
        runs_path
      )

      atomic_write_csv(
        support_df,
        support_path
      )


      if (gate_pass) {

        completed_pass_keys <- unique(
          c(
            completed_pass_keys,
            key
          )
        )
      }


      cat(
        sprintf(
          paste0(
            "%s | XB=%.8f | init=%d opt=%d | ",
            "iter=%d | occupied=%d/%d | min=%d | %.2fs\n"
          ),
          row$status,
          row$final_XB,
          row$nfe_initialization,
          row$nfe_optimization,
          row$iterations,
          row$occupied,
          NCLUSTER,
          row$min_cluster_size,
          row$runtime_sec
        )
      )
    }
  }
}


# -----------------------------------------------------------------------------
# 10. Restrict summaries to the current experimental design
# -----------------------------------------------------------------------------

design_rows <- runs_df[
  runs_df$method %in% METHODS &
  runs_df$seed %in% SEEDS &
  runs_df$max_nfe %in% BUDGETS,
  ,
  drop = FALSE
]


# -----------------------------------------------------------------------------
# 11. Technical gate summary
# -----------------------------------------------------------------------------

expected_keys <- expand.grid(
  method = METHODS,
  seed = SEEDS,
  max_nfe = BUDGETS,
  stringsAsFactors = FALSE
)

expected_keys$key <- mapply(
  key_string,
  expected_keys$method,
  expected_keys$seed,
  expected_keys$max_nfe,
  USE.NAMES = FALSE
)


observed_keys <- mapply(
  key_string,
  design_rows$method,
  design_rows$seed,
  design_rows$max_nfe,
  USE.NAMES = FALSE
)


missing_keys <- setdiff(
  expected_keys$key,
  observed_keys
)


n_expected <- nrow(
  expected_keys
)

n_observed <- length(
  unique(
    observed_keys
  )
)

n_pass <- sum(
  design_rows$status == "PASS"
)

n_fail <- sum(
  design_rows$status != "PASS"
)


all_exact_budget <- (
  nrow(design_rows) > 0L &&
  all(
    design_rows$exact_budget[
      design_rows$status == "PASS"
    ]
  )
)


all_nfe_invariant <- (
  nrow(design_rows) > 0L &&
  all(
    design_rows$nfe_invariant[
      design_rows$status == "PASS"
    ]
  )
)


all_occupancy <- (
  nrow(design_rows) > 0L &&
  all(
    design_rows$occupied[
      design_rows$status == "PASS"
    ] == NCLUSTER
  )
)


gate_df <- data.frame(
  metric = c(
    "planned_runs",
    "observed_unique_runs",
    "pass_runs",
    "failed_or_error_runs",
    "missing_runs",
    "all_exact_budget",
    "all_nfe_invariant",
    "all_occupancy_4_of_4"
  ),
  value = c(
    n_expected,
    n_observed,
    n_pass,
    n_fail,
    length(missing_keys),
    all_exact_budget,
    all_nfe_invariant,
    all_occupancy
  ),
  stringsAsFactors = FALSE
)


atomic_write_csv(
  gate_df,
  gate_path
)


# -----------------------------------------------------------------------------
# 12. Descriptive summary by method and budget
# -----------------------------------------------------------------------------

pass_df <- design_rows[
  design_rows$status == "PASS",
  ,
  drop = FALSE
]


summary_rows <- list()
ii <- 0L


for (budget in BUDGETS) {

  for (method in METHODS) {

    z <- pass_df[
      pass_df$method == method &
      pass_df$max_nfe == budget,
      ,
      drop = FALSE
    ]

    ii <- ii + 1L


    if (!nrow(z)) {

      summary_rows[[ii]] <- data.frame(
        method = method,
        max_nfe = budget,
        n_runs = 0L,
        mean_XB = NA_real_,
        sd_XB = NA_real_,
        median_XB = NA_real_,
        min_XB = NA_real_,
        max_XB = NA_real_,
        mean_runtime_sec = NA_real_,
        median_runtime_sec = NA_real_,
        mean_iterations = NA_real_,
        mean_initialization_nfe = NA_real_,
        mean_optimization_nfe = NA_real_,
        mean_min_cluster_size = NA_real_,
        min_min_cluster_size = NA_real_,
        mean_min_centroid_separation = NA_real_,
        stringsAsFactors = FALSE
      )

      next
    }


    summary_rows[[ii]] <- data.frame(
      method = method,
      max_nfe = budget,
      n_runs = nrow(z),
      mean_XB = mean(
        z$final_XB
      ),
      sd_XB = if (nrow(z) > 1L) {
        stats::sd(
          z$final_XB
        )
      } else {
        NA_real_
      },
      median_XB = stats::median(
        z$final_XB
      ),
      min_XB = min(
        z$final_XB
      ),
      max_XB = max(
        z$final_XB
      ),
      mean_runtime_sec = mean(
        z$runtime_sec
      ),
      median_runtime_sec = stats::median(
        z$runtime_sec
      ),
      mean_iterations = mean(
        z$iterations
      ),
      mean_initialization_nfe = mean(
        z$nfe_initialization
      ),
      mean_optimization_nfe = mean(
        z$nfe_optimization
      ),
      mean_min_cluster_size = mean(
        z$min_cluster_size
      ),
      min_min_cluster_size = min(
        z$min_cluster_size
      ),
      mean_min_centroid_separation = mean(
        z$min_centroid_separation
      ),
      stringsAsFactors = FALSE
    )
  }
}


summary_df <- do.call(
  rbind,
  summary_rows
)

rownames(
  summary_df
) <- NULL


atomic_write_csv(
  summary_df,
  summary_path
)


# -----------------------------------------------------------------------------
# 13. Paired seed-level ranks (lower XB = better)
# -----------------------------------------------------------------------------

rank_rows <- list()
rr <- 0L


for (budget in BUDGETS) {

  for (seed in SEEDS) {

    z <- pass_df[
      pass_df$seed == seed &
      pass_df$max_nfe == budget,
      ,
      drop = FALSE
    ]


    if (!nrow(z)) {
      next
    }


    z$rank_XB <- rank(
      z$final_XB,
      ties.method = "average"
    )


    rr <- rr + 1L

    rank_rows[[rr]] <- z[
      ,
      c(
        "method",
        "seed",
        "max_nfe",
        "final_XB",
        "rank_XB"
      ),
      drop = FALSE
    ]
  }
}


seed_ranks_df <- if (
  length(rank_rows)
) {

  do.call(
    rbind,
    rank_rows
  )

} else {

  data.frame()
}


if (nrow(seed_ranks_df)) {

  rownames(
    seed_ranks_df
  ) <- NULL

  atomic_write_csv(
    seed_ranks_df,
    seed_ranks_path
  )
}


rank_summary_rows <- list()
rs <- 0L


for (budget in BUDGETS) {

  for (method in METHODS) {

    z <- seed_ranks_df[
      seed_ranks_df$method == method &
      seed_ranks_df$max_nfe == budget,
      ,
      drop = FALSE
    ]


    rs <- rs + 1L


    rank_summary_rows[[rs]] <- data.frame(
      method = method,
      max_nfe = budget,
      n_ranked_seeds = nrow(z),
      mean_rank = if (nrow(z)) {
        mean(
          z$rank_XB
        )
      } else {
        NA_real_
      },
      median_rank = if (nrow(z)) {
        stats::median(
          z$rank_XB
        )
      } else {
        NA_real_
      },
      best_rank = if (nrow(z)) {
        min(
          z$rank_XB
        )
      } else {
        NA_real_
      },
      worst_rank = if (nrow(z)) {
        max(
          z$rank_XB
        )
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }
}


rank_summary_df <- do.call(
  rbind,
  rank_summary_rows
)

rownames(
  rank_summary_df
) <- NULL


atomic_write_csv(
  rank_summary_df,
  rank_summary_path
)


# -----------------------------------------------------------------------------
# 14. Budget-to-budget improvement
# -----------------------------------------------------------------------------

budget_pairs <- data.frame(
  from_nfe = head(
    BUDGETS,
    -1L
  ),
  to_nfe = tail(
    BUDGETS,
    -1L
  )
)


improvement_rows <- list()
ir <- 0L


for (method in METHODS) {

  for (j in seq_len(nrow(budget_pairs))) {

    from_nfe <- budget_pairs$from_nfe[j]
    to_nfe <- budget_pairs$to_nfe[j]


    for (seed in SEEDS) {

      from_row <- pass_df[
        pass_df$method == method &
        pass_df$seed == seed &
        pass_df$max_nfe == from_nfe,
        ,
        drop = FALSE
      ]

      to_row <- pass_df[
        pass_df$method == method &
        pass_df$seed == seed &
        pass_df$max_nfe == to_nfe,
        ,
        drop = FALSE
      ]


      if (
        nrow(from_row) != 1L ||
        nrow(to_row) != 1L
      ) {
        next
      }


      xb_from <- from_row$final_XB
      xb_to <- to_row$final_XB

      abs_improvement <- xb_from -
        xb_to

      pct_improvement <- if (
        is.finite(xb_from) &&
        xb_from != 0
      ) {
        100 *
          abs_improvement /
          abs(xb_from)
      } else {
        NA_real_
      }


      ir <- ir + 1L

      improvement_rows[[ir]] <- data.frame(
        method = method,
        seed = seed,
        from_nfe = from_nfe,
        to_nfe = to_nfe,
        XB_from = xb_from,
        XB_to = xb_to,
        absolute_improvement = abs_improvement,
        percent_improvement = pct_improvement,
        non_worsening = xb_to <=
          xb_from +
          MONOTONIC_TOL,
        stringsAsFactors = FALSE
      )
    }
  }
}


improvement_df <- if (
  length(improvement_rows)
) {

  do.call(
    rbind,
    improvement_rows
  )

} else {

  data.frame()
}


if (nrow(improvement_df)) {

  rownames(
    improvement_df
  ) <- NULL

  atomic_write_csv(
    improvement_df,
    budget_improvement_path
  )
}


# -----------------------------------------------------------------------------
# 15. Monotonicity across increasing NFE budgets
# -----------------------------------------------------------------------------

monotonic_rows <- list()
mr <- 0L


for (method in METHODS) {

  for (seed in SEEDS) {

    z <- pass_df[
      pass_df$method == method &
      pass_df$seed == seed &
      pass_df$max_nfe %in% BUDGETS,
      ,
      drop = FALSE
    ]


    z <- z[
      order(
        z$max_nfe
      ),
      ,
      drop = FALSE
    ]


    complete_path <- (
      nrow(z) ==
      length(BUDGETS) &&
      identical(
        as.integer(
          z$max_nfe
        ),
        as.integer(
          BUDGETS
        )
      )
    )


    if (complete_path) {

      deltas <- diff(
        z$final_XB
      )

      monotone <- all(
        deltas <=
          MONOTONIC_TOL
      )

      largest_worsening <- max(
        c(
          0,
          deltas
        ),
        na.rm = TRUE
      )

    } else {

      monotone <- FALSE
      largest_worsening <- NA_real_
    }


    mr <- mr + 1L

    monotonic_rows[[mr]] <- data.frame(
      method = method,
      seed = seed,
      complete_budget_path = complete_path,
      monotone_non_worsening = monotone,
      largest_worsening = largest_worsening,
      XB_200 = if (
        any(
          z$max_nfe == 200L
        )
      ) {
        z$final_XB[
          z$max_nfe ==
            200L
        ][1L]
      } else {
        NA_real_
      },
      XB_500 = if (
        any(
          z$max_nfe == 500L
        )
      ) {
        z$final_XB[
          z$max_nfe ==
            500L
        ][1L]
      } else {
        NA_real_
      },
      XB_1000 = if (
        any(
          z$max_nfe == 1000L
        )
      ) {
        z$final_XB[
          z$max_nfe ==
            1000L
        ][1L]
      } else {
        NA_real_
      },
      XB_2000 = if (
        any(
          z$max_nfe == 2000L
        )
      ) {
        z$final_XB[
          z$max_nfe ==
            2000L
        ][1L]
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }
}


monotonic_df <- do.call(
  rbind,
  monotonic_rows
)

rownames(
  monotonic_df
) <- NULL


atomic_write_csv(
  monotonic_df,
  monotonicity_path
)


# -----------------------------------------------------------------------------
# 16. Final console summary
# -----------------------------------------------------------------------------

cat("\n")
cat("====================================================================\n")
cat("V8 NFE-FAIR PILOT — FINAL SUMMARY\n")
cat("====================================================================\n")
cat(
  "Planned unique runs : ",
  n_expected,
  "\n",
  sep = ""
)
cat(
  "Observed unique runs: ",
  n_observed,
  "\n",
  sep = ""
)
cat(
  "PASS runs           : ",
  n_pass,
  "\n",
  sep = ""
)
cat(
  "FAIL/ERROR runs     : ",
  n_fail,
  "\n",
  sep = ""
)
cat(
  "Missing runs        : ",
  length(missing_keys),
  "\n",
  sep = ""
)
cat(
  "Executed this call  : ",
  executed_counter,
  "\n",
  sep = ""
)
cat(
  "Skipped existing    : ",
  skipped_counter,
  "\n",
  sep = ""
)
cat(
  "Exact-budget gate   : ",
  all_exact_budget,
  "\n",
  sep = ""
)
cat(
  "NFE invariant gate  : ",
  all_nfe_invariant,
  "\n",
  sep = ""
)
cat(
  "Occupancy gate      : ",
  all_occupancy,
  "\n",
  sep = ""
)


complete_monotonicity <- all(
  monotonic_df$complete_budget_path
)

all_monotone <- all(
  monotonic_df$monotone_non_worsening
)


cat(
  "Complete budget path: ",
  complete_monotonicity,
  "\n",
  sep = ""
)

cat(
  "Budget monotonicity : ",
  all_monotone,
  "\n",
  sep = ""
)


overall_complete <- (
  n_observed == n_expected &&
  n_pass == n_expected &&
  n_fail == 0L &&
  length(missing_keys) == 0L &&
  all_exact_budget &&
  all_nfe_invariant &&
  all_occupancy &&
  complete_monotonicity &&
  all_monotone
)


cat(
  "OVERALL PILOT GATE  : ",
  if (overall_complete) {
    "PASS"
  } else {
    "FAIL/INCOMPLETE"
  },
  "\n",
  sep = ""
)


cat("\n")
cat("Median XB by method and budget:\n")


median_table <- reshape(
  summary_df[
    ,
    c(
      "method",
      "max_nfe",
      "median_XB"
    )
  ],
  idvar = "method",
  timevar = "max_nfe",
  direction = "wide"
)


median_table <- median_table[
  match(
    METHODS,
    median_table$method
  ),
  ,
  drop = FALSE
]


print(
  median_table,
  row.names = FALSE
)


cat("\n")
cat("Mean rank by method and budget (lower = better):\n")


rank_table <- reshape(
  rank_summary_df[
    ,
    c(
      "method",
      "max_nfe",
      "mean_rank"
    )
  ],
  idvar = "method",
  timevar = "max_nfe",
  direction = "wide"
)


rank_table <- rank_table[
  match(
    METHODS,
    rank_table$method
  ),
  ,
  drop = FALSE
]


print(
  rank_table,
  row.names = FALSE
)


cat("\n")
cat(
  "Monotonic method-seed paths: ",
  sum(
    monotonic_df$monotone_non_worsening
  ),
  "/",
  nrow(monotonic_df),
  "\n",
  sep = ""
)


cat("\n")
cat("Results written to:\n  ")
cat(out_dir)
cat("\n\n")


cat(
  "Interpretation rules:\n",
  "- Lower spatial Xie-Beni is better.\n",
  "- Compare methods at the SAME max_nfe only.\n",
  "- Five seeds are a pilot sample, not the final inferential benchmark.\n",
  "- Use budget-improvement and rank stability to choose the final NFE budget.\n",
  "- Do not declare a final best optimizer until the planned 30-run benchmark.\n",
  sep = ""
)


if (!overall_complete) {

  cat(
    "\nNOTE: Pilot is incomplete or one technical gate failed. ",
    "Inspect the CSV outputs before formal interpretation.\n",
    sep = ""
  )
}


invisible(
  list(
    config = config_df,
    runs = design_rows,
    summary = summary_df,
    seed_ranks = seed_ranks_df,
    rank_summary = rank_summary_df,
    budget_improvement = improvement_df,
    monotonicity = monotonic_df,
    gate = gate_df,
    overall_pass = overall_complete
  )
)
