# =============================================================================
# soviclust — Algorithm Correctness Patch v2
# Objective Harmonization for FGWC Metaheuristic Optimizers
# =============================================================================
#
# Run this script ONCE from the root of the soviclust source project:
#
#   source("apply_patch_v2.R")
#
# The script:
#   1. creates backups under validation/patch_backups/;
#   2. adds a single common optimizer fitness helper to fgwc.R;
#   3. harmonizes ABC/FPA/GSA/HHO/IFA/PSO/TLBO/GWO/WOA to that fitness;
#   4. fixes legacy convergence recording order;
#   5. adds `fitness_type = "jfgwcv"` and `spatial_obj` to optimizer outputs;
#   6. fixes two algorithmic bugs found during audit:
#        - IFA movement expressions were not assigned back to the firefly;
#        - ABC trial counters did not increment when no food improved;
#        - GSA calculated updated velocity `v1` but returned stale `v`, and
#          zero particle mass could create undefined acceleration.
#
# Common optimization fitness:
#
#   J(V) = sum_i sum_k u_ik(V)^m ||x_i - v_k||^2
#
# implemented through jfgwcv().
#
# Spatially adjusted membership is still used to project candidates and to
# produce the final FGWC partition. Its objective is retained separately as
# `spatial_obj` for diagnostics, NOT as the cross-optimizer fitness.
# =============================================================================

root <- normalizePath(".", winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(root, "DESCRIPTION")) ||
    !dir.exists(file.path(root, "inst", "app", "R", "shared", "function"))) {
  stop(
    "Run apply_patch_v2.R from the root directory of the soviclust project.",
    call. = FALSE
  )
}

func_dir <- file.path(root, "inst", "app", "R", "shared", "function")

optimizer_files <- c(
  "abcfgwc.R",
  "fpafgwc.R",
  "gsafgwc.R",
  "hhofgwc.R",
  "ifafgwc.R",
  "psofgwc.R",
  "tlbofgwc.R",
  "gwofgwc.R",
  "woafgwc.R"
)

required_files <- c("fgwc.R", optimizer_files)
missing_files <- required_files[
  !file.exists(file.path(func_dir, required_files))
]

if (length(missing_files) > 0L) {
  stop(
    "Required source file(s) not found: ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# Backup
# -----------------------------------------------------------------------------

stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
backup_dir <- file.path(
  root, "validation", "patch_backups", paste0("v2-", stamp)
)
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

for (f in required_files) {
  ok <- file.copy(
    file.path(func_dir, f),
    file.path(backup_dir, f),
    overwrite = TRUE
  )
  if (!isTRUE(ok)) {
    stop("Failed to back up: ", f, call. = FALSE)
  }
}

message("Backup created: ", backup_dir)


read_src <- function(path) {
  paste(
    readLines(path, encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )
}

write_src <- function(path, text) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(strsplit(text, "\n", fixed = TRUE)[[1]], con, useBytes = TRUE)
}


regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}


count_regex <- function(text, pattern) {
  hits <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (identical(hits[1], -1L)) 0L else length(hits)
}


# -----------------------------------------------------------------------------
# 1. Add common objective helpers to fgwc.R
# -----------------------------------------------------------------------------

fgwc_path <- file.path(func_dir, "fgwc.R")
fgwc_txt <- read_src(fgwc_path)

if (!grepl("optimizer_fitness\\s*<-\\s*function", fgwc_txt, perl = TRUE)) {
  helper_code <- '

# =============================================================================
# Common fitness/evaluation helpers for FGWC metaheuristic optimizers
# =============================================================================

# Common cross-optimizer fitness.
#
# Extra arguments are deliberately accepted and ignored so legacy helper calls
# that previously supplied spatial parameters can be migrated safely to this
# common criterion without changing their public signatures.
optimizer_fitness <- function(data, centers, m,
                              distance = "euclidean", order = 2, ...) {
  jfgwcv(data, centers, m, distance, order)
}


# Diagnostic objective evaluated on the FINAL spatially adjusted membership
# and its associated centroid matrix. This value is intentionally separated
# from optimizer_fitness() because it is not used for cross-optimizer ranking.
optimizer_spatial_objective <- function(data, membership, centers, m,
                                        distance = "euclidean", order = 2) {
  fgwc_objective(
    data = data,
    uij = membership,
    centers = centers,
    m = m,
    distance = distance,
    order = order
  )
}
'
  fgwc_txt <- paste0(fgwc_txt, helper_code)
  write_src(fgwc_path, fgwc_txt)
  message("Patched fgwc.R: added common optimizer helpers.")
} else {
  message("fgwc.R already contains optimizer_fitness(); skipped helper insertion.")
}


# -----------------------------------------------------------------------------
# 2. Replace mixed jfgwcv/jfgwcv2 calls in ALL optimizer files
# -----------------------------------------------------------------------------
#
# optimizer_fitness() accepts `...`, so old jfgwcv2() calls can keep their
# spatial arguments while the actual fitness calculation is now jfgwcv().
# -----------------------------------------------------------------------------

for (f in optimizer_files) {
  path <- file.path(func_dir, f)
  txt <- read_src(path)

  # Replace longer name first.
  txt <- gsub(
    "jfgwcv2\\(",
    "optimizer_fitness(",
    txt,
    perl = TRUE
  )
  txt <- gsub(
    "jfgwcv\\(",
    "optimizer_fitness(",
    txt,
    perl = TRUE
  )

  write_src(path, txt)
}

message("Harmonized legacy jfgwcv/jfgwcv2 calls to optimizer_fitness().")


# -----------------------------------------------------------------------------
# 3. GWO/WOA: replace spatial objective used as SEARCH FITNESS
# -----------------------------------------------------------------------------

patch_search_fitness <- function(file, membership_expr, centroid_expr) {
  path <- file.path(func_dir, file)
  txt <- read_src(path)

  pattern <- paste0(
    "fgwc_objective\\(\\s*",
    "data,\\s*",
    regex_escape(membership_expr), ",\\s*",
    regex_escape(centroid_expr), ",\\s*",
    "m,\\s*distance,\\s*order\\s*\\)"
  )

  n_before <- count_regex(txt, pattern)

  if (n_before == 0L) {
    # If patch was already applied, do not fail.
    already <- grepl(
      paste0(
        "optimizer_fitness\\(\\s*data,\\s*",
        regex_escape(centroid_expr)
      ),
      txt,
      perl = TRUE
    )
    if (!already) {
      stop(
        "Could not locate expected search-fitness block in ", file,
        ". The source may differ from the audited GitHub version.",
        call. = FALSE
      )
    }
    return(invisible(FALSE))
  }

  replacement <- paste0(
    "optimizer_fitness(data, ",
    centroid_expr,
    ", m, distance, order)"
  )

  txt <- gsub(pattern, replacement, txt, perl = TRUE)
  write_src(path, txt)

  message(
    "Patched ", file, ": replaced ",
    n_before, " spatial search-fitness call(s)."
  )

  invisible(TRUE)
}

patch_search_fitness(
  "gwofgwc.R",
  "wolf.other[[i]]",
  "wolf.swarm[[i]]"
)

patch_search_fitness(
  "woafgwc.R",
  "wh.other[[i]]",
  "wh.swarm[[i]]"
)


# -----------------------------------------------------------------------------
# 4. Legacy global-best convergence history
# -----------------------------------------------------------------------------
#
# Previous pattern:
#   record OLD best -> check stagnation -> update global best
#
# New pattern:
#   update global best -> record UPDATED best -> check stagnation
# -----------------------------------------------------------------------------

find_closing_brace <- function(lines, start) {
  balance <- 0L

  for (i in seq.int(start, length(lines))) {
    chars <- strsplit(lines[i], "", fixed = TRUE)[[1]]
    balance <- balance +
      sum(chars == "{") -
      sum(chars == "}")

    if (balance == 0L && i >= start) {
      return(i)
    }
  }

  stop("Unable to locate closing brace during patch.", call. = FALSE)
}


patch_convergence <- function(file, final_fit, current_fit,
                              counter = "iter",
                              increment_inside = TRUE) {
  path <- file.path(func_dir, file)
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  trimmed <- trimws(lines)

  # If new length-based convergence code is already present, treat as patched.
  new_marker <- paste0(
    "conv <- c(conv, ", final_fit, ")"
  )

  if (any(grepl(
    "conv\\[length\\(conv\\)\\].*conv\\[length\\(conv\\) - 1L\\]",
    lines,
    perl = TRUE
  ))) {
    message(file, ": convergence ordering appears already patched; skipped.")
    return(invisible(FALSE))
  }

  conv_re <- paste0(
    "^conv\\s*<-\\s*c\\(conv,\\s*",
    regex_escape(final_fit),
    "\\s*\\)$"
  )

  i_conv <- grep(conv_re, trimmed, perl = TRUE)

  if (length(i_conv) != 1L) {
    stop(
      "Expected exactly one legacy convergence append in ", file,
      "; found ", length(i_conv), ".",
      call. = FALSE
    )
  }

  i_conv <- i_conv[1]

  update_re <- paste0(
    "^if\\s*\\(\\s*",
    regex_escape(current_fit),
    "\\s*<=\\s*",
    regex_escape(final_fit),
    "\\s*\\)\\s*\\{"
  )

  candidates <- grep(update_re, trimmed, perl = TRUE)
  candidates <- candidates[candidates > i_conv]

  if (length(candidates) < 1L) {
    stop("Could not locate global-best update block in ", file, call. = FALSE)
  }

  i_update <- candidates[1]
  i_close <- find_closing_brace(lines, i_update)

  # Preserve the original best-update assignments exactly.
  update_block <- lines[i_update:i_close]

  indent <- sub("^(\\s*).*", "\\1", lines[i_conv], perl = TRUE)

  new_block <- c(
    update_block,
    if (increment_inside) {
      paste0(indent, counter, " <- ", counter, " + 1L")
    } else {
      character(0)
    },
    paste0(indent, "conv <- c(conv, ", final_fit, ")"),
    paste0(
      indent,
      "if (abs(conv[length(conv)] - conv[length(conv) - 1L]) < error) ",
      "same <- same + 1L"
    ),
    paste0(indent, "else same <- 0L")
  )

  before <- if (i_conv > 1L) lines[seq_len(i_conv - 1L)] else character(0)
  after <- if (i_close < length(lines)) {
    lines[(i_close + 1L):length(lines)]
  } else {
    character(0)
  }

  lines <- c(before, new_block, after)

  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, useBytes = TRUE)

  message(file, ": global-best convergence order corrected.")
  invisible(TRUE)
}


patch_convergence(
  "abcfgwc.R",
  "food.fit.finalbest",
  "food.fit.curbest"
)

patch_convergence(
  "fpafgwc.R",
  "flow.fit.finalbest",
  "flow.fit.curbest"
)

patch_convergence(
  "gsafgwc.R",
  "par.fit.finalbest",
  "par.fit.curbest"
)

patch_convergence(
  "hhofgwc.R",
  "hh.fit.finalbest",
  "hh.fit.curbest"
)

patch_convergence(
  "psofgwc.R",
  "par.fit.finalbest",
  "par.fit.curbest"
)

patch_convergence(
  "tlbofgwc.R",
  "stud.fit.finalbest",
  "stud.fit.curbest"
)

# IFA has its generation increment AFTER the old convergence/update block.
patch_convergence(
  "ifafgwc.R",
  "inten.finalbest",
  "inten.curbest",
  counter = "gen",
  increment_inside = FALSE
)


# -----------------------------------------------------------------------------
# 5. IFA iteration count and movement assignment bug
# -----------------------------------------------------------------------------

ifa_path <- file.path(func_dir, "ifafgwc.R")
ifa_lines <- readLines(ifa_path, encoding = "UTF-8", warn = FALSE)

# max.iter should mean exactly max.iter generations.
gen_line <- grep("^\\s*gen\\s*=\\s*1\\s*$", ifa_lines, perl = TRUE)
if (length(gen_line) == 1L) {
  ifa_lines[gen_line] <- sub(
    "gen\\s*=\\s*1",
    "gen=0",
    ifa_lines[gen_line],
    perl = TRUE
  )
}

# Critical legacy bug: movement expressions were evaluated but not assigned.
ifa_lines <- gsub(
  "^(\\s*)ffly\\[\\[j\\]\\]\\+beta\\*",
  "\\1ffly[[j]] <- ffly[[j]] + beta*",
  ifa_lines,
  perl = TRUE
)

ifa_lines <- gsub(
  "^(\\s*)ffly\\[\\[j\\]\\]\\+\\(ff\\.alpha\\*ei\\)",
  "\\1ffly[[j]] <- ffly[[j]] + (ff.alpha*ei)",
  ifa_lines,
  perl = TRUE
)

con <- file(ifa_path, open = "w", encoding = "UTF-8")
writeLines(ifa_lines, con, useBytes = TRUE)
close(con)

message("ifafgwc.R: corrected generation count and movement assignment.")


# -----------------------------------------------------------------------------
# 6. GSA velocity/mass correctness
# -----------------------------------------------------------------------------
#
# Legacy force_v() calculated a corrected velocity object `v1` but returned
# the original `v`, so the GSA velocity update was effectively discarded.
# It also allowed zero particle mass, which can create 0/0 acceleration.
# -----------------------------------------------------------------------------

gsa_path <- file.path(func_dir, "gsafgwc.R")
gsa_lines <- readLines(gsa_path, encoding = "UTF-8", warn = FALSE)

mass_line <- which(
  grepl(
    "^\\s*mass\\s*<-\\s*\\(par\\$I-max\\(par\\$I\\)\\)/\\(min\\(par\\$I\\)-max\\(par\\$I\\)\\)\\s*$",
    gsa_lines,
    perl = TRUE
  )
)

if (length(mass_line) == 1L) {
  i <- mass_line
  indent <- sub("^(\\s*).*", "\\1", gsa_lines[i], perl = TRUE)

  # The following line is expected to be: Mass <- mass/sum(mass)
  remove_idx <- c(i, i + 1L)

  robust_mass <- c(
    paste0(indent, "fit_min <- min(par$I)"),
    paste0(indent, "fit_max <- max(par$I)"),
    paste0(indent, "fit_range <- fit_max - fit_min"),
    paste0(indent, "if (!is.finite(fit_range) || fit_range <= .Machine$double.eps) {"),
    paste0(indent, "  Mass <- rep(1 / length(par$I), length(par$I))"),
    paste0(indent, "} else {"),
    paste0(indent, "  mass <- (fit_max - par$I) / fit_range"),
    paste0(indent, "  mass <- pmax(mass, .Machine$double.eps)"),
    paste0(indent, "  Mass <- mass / sum(mass)"),
    paste0(indent, "}")
  )

  gsa_lines <- c(
    if (i > 1L) gsa_lines[seq_len(i - 1L)] else character(0),
    robust_mass,
    if (i + 1L < length(gsa_lines)) gsa_lines[(i + 2L):length(gsa_lines)] else character(0)
  )
}

return_v <- which(trimws(gsa_lines) == "return(v)")
if (length(return_v) == 1L) {
  gsa_lines[return_v] <- sub(
    "return\\(v\\)",
    "return(v1)",
    gsa_lines[return_v],
    perl = TRUE
  )
}

con <- file(gsa_path, open = "w", encoding = "UTF-8")
writeLines(gsa_lines, con, useBytes = TRUE)
close(con)

message("gsafgwc.R: corrected mass stability and velocity return value.")


# -----------------------------------------------------------------------------
# 7. ABC trial-counter/probability robustness
# -----------------------------------------------------------------------------

abc_path <- file.path(func_dir, "abcfgwc.R")
abc_lines <- readLines(abc_path, encoding = "UTF-8", warn = FALSE)

trial_idx <- which(
  trimws(abc_lines) == "t[-ind] <- t[-ind]+1"
)

if (length(trial_idx) == 1L) {
  i <- trial_idx
  indent <- sub("^(\\s*).*", "\\1", abc_lines[i], perl = TRUE)

  replacement <- c(
    paste0(indent, "not_improved <- setdiff(seq_along(t), ind)"),
    paste0(indent, "if (length(not_improved) > 0L) {"),
    paste0(indent, "  t[not_improved] <- t[not_improved] + 1"),
    paste0(indent, "}")
  )

  abc_lines <- append(
    abc_lines[-i],
    replacement,
    after = i - 1L
  )
}

abc_lines <- gsub(
  "which\\(t==limit\\)",
  "which(t >= limit)",
  abc_lines,
  perl = TRUE
)

prob_idx <- which(
  grepl(
    "^\\s*prob\\s*<-\\s*\\(1/obj\\)/sum\\(1/obj\\)\\s*$",
    abc_lines,
    perl = TRUE
  )
)

if (length(prob_idx) == 1L) {
  i <- prob_idx
  indent <- sub("^(\\s*).*", "\\1", abc_lines[i], perl = TRUE)

  repl <- c(
    paste0(indent, "obj_safe <- pmax(obj, .Machine$double.eps)"),
    paste0(indent, "prob <- (1 / obj_safe) / sum(1 / obj_safe)")
  )

  abc_lines <- append(
    abc_lines[-i],
    repl,
    after = i - 1L
  )
}

# Ensure ABC returns the same class as all other FGWC optimizers.
if (!any(grepl("class\\(abc\\)\\s*<-\\s*['\"]fgwc['\"]", abc_lines))) {
  ret_idx <- which(trimws(abc_lines) == "return(abc)")
  if (length(ret_idx) == 1L) {
    abc_lines <- append(
      abc_lines,
      "  class(abc) <- 'fgwc'",
      after = ret_idx - 1L
    )
  }
}

con <- file(abc_path, open = "w", encoding = "UTF-8")
writeLines(abc_lines, con, useBytes = TRUE)
close(con)

message("abcfgwc.R: corrected trial-counter handling and robustness.")


# -----------------------------------------------------------------------------
# 8. Harmonize stopping comparisons and remove debug print()
# -----------------------------------------------------------------------------

for (f in optimizer_files) {
  path <- file.path(func_dir, f)
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

  lines <- gsub(
    "iter==max\\.iter",
    "iter>=max.iter",
    lines,
    perl = TRUE
  )
  lines <- gsub(
    "gen==max\\.iter",
    "gen>=max.iter",
    lines,
    perl = TRUE
  )

  # `same == threshold` and `same >= threshold` are equivalent under unit
  # increments, but >= is robust and consistent with GWO/WOA.
  lines <- gsub(
    "same==([A-Za-z0-9_.]+)",
    "same>=\\1",
    lines,
    perl = TRUE
  )

  # Remove legacy console debug output such as:
  # print(c(order, ncluster,m, randomN))
  drop <- grepl(
    "^\\s*print\\s*\\(\\s*c\\(\\s*order\\s*,\\s*ncluster\\s*,\\s*m\\s*,\\s*randomN\\s*\\)\\s*\\)\\s*$",
    lines,
    perl = TRUE
  )

  lines <- lines[!drop]

  con <- file(path, open = "w", encoding = "UTF-8")
  writeLines(lines, con, useBytes = TRUE)
  close(con)
}


# -----------------------------------------------------------------------------
# 9. Add explicit fitness metadata + final spatial diagnostic objective
# -----------------------------------------------------------------------------

result_map <- list(
  abcfgwc.R  = c("food.finalpos.other", "food.finalpos"),
  fpafgwc.R  = c("flow.finalpos.other", "flow.finalpos"),
  gsafgwc.R  = c("par.finalpos.other",  "par.finalpos"),
  hhofgwc.R  = c("hh.finalpos.other",   "hh.finalpos"),
  ifafgwc.R  = c("new_uij",             "vi"),
  psofgwc.R  = c("par.finalpos.other",  "par.finalpos"),
  tlbofgwc.R = c("stud.finalpos.other", "stud.finalpos"),
  gwofgwc.R  = c("alpha_other",         "alpha_pos"),
  woafgwc.R  = c("wh.finalpos.other",   "wh.finalpos")
)


add_result_diagnostics <- function(file, membership_expr, centroid_expr) {
  path <- file.path(func_dir, file)
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

  if (any(grepl("fitness_type\\s*=", lines)) ||
      any(grepl('"fitness_type"\\s*=', lines))) {
    message(file, ": result diagnostics already present; skipped.")
    return(invisible(FALSE))
  }

  # Legacy files generally have f_obj and membership on the same list line.
  same_line <- which(
    grepl("f_obj", lines) &
      grepl("membership", lines)
  )

  diagnostic_expr <- paste0(
    'optimizer_spatial_objective(data, ',
    membership_expr, ", ", centroid_expr,
    ", m, distance, order)"
  )

  if (length(same_line) == 1L) {
    i <- same_line
    lines[i] <- sub(
      '"membership"\\s*=',
      paste0(
        '"fitness_type"="jfgwcv",',
        '"spatial_obj"=', diagnostic_expr, ",",
        '"membership"='
      ),
      lines[i],
      perl = TRUE
    )

  } else {
    # GWO/WOA use multiline named lists.
    membership_line <- which(
      grepl("^\\s*membership\\s*=", lines, perl = TRUE)
    )

    if (length(membership_line) != 1L) {
      stop(
        "Could not locate result membership field in ", file,
        call. = FALSE
      )
    }

    i <- membership_line
    indent <- sub("^(\\s*).*", "\\1", lines[i], perl = TRUE)

    additions <- c(
      paste0(indent, 'fitness_type = "jfgwcv",'),
      paste0(indent, "spatial_obj = ", diagnostic_expr, ",")
    )

    lines <- append(lines, additions, after = i - 1L)
  }

  con <- file(path, open = "w", encoding = "UTF-8")
  writeLines(lines, con, useBytes = TRUE)
  close(con)

  message(file, ": added fitness_type and spatial_obj.")
  invisible(TRUE)
}


for (f in names(result_map)) {
  add_result_diagnostics(
    f,
    result_map[[f]][1],
    result_map[[f]][2]
  )
}


# -----------------------------------------------------------------------------
# 10. Static validation
# -----------------------------------------------------------------------------

issues <- character(0)

for (f in optimizer_files) {
  txt <- read_src(file.path(func_dir, f))

  if (grepl("jfgwcv2\\(", txt, perl = TRUE)) {
    issues <- c(issues, paste0(f, ": still contains jfgwcv2()."))
  }

  if (grepl("jfgwcv\\(", txt, perl = TRUE)) {
    issues <- c(issues, paste0(f, ": still contains direct jfgwcv()."))
  }

  if (!grepl("optimizer_fitness\\(", txt, perl = TRUE)) {
    issues <- c(issues, paste0(f, ": optimizer_fitness() not found."))
  }

  if (!grepl("fitness_type", txt, fixed = TRUE)) {
    issues <- c(issues, paste0(f, ": fitness_type metadata missing."))
  }

  if (!grepl("spatial_obj", txt, fixed = TRUE)) {
    issues <- c(issues, paste0(f, ": spatial_obj diagnostic missing."))
  }
}

gwo_txt <- read_src(file.path(func_dir, "gwofgwc.R"))
woa_txt <- read_src(file.path(func_dir, "woafgwc.R"))

# fgwc_objective() is allowed in result diagnostics but should not be the
# repeated population fitness anymore.
if (grepl(
  "fgwc_objective\\(\\s*data,\\s*wolf\\.other\\[\\[i\\]\\]",
  gwo_txt,
  perl = TRUE
)) {
  issues <- c(issues, "gwofgwc.R: spatial objective still used as search fitness.")
}

if (grepl(
  "fgwc_objective\\(\\s*data,\\s*wh\\.other\\[\\[i\\]\\]",
  woa_txt,
  perl = TRUE
)) {
  issues <- c(issues, "woafgwc.R: spatial objective still used as search fitness.")
}

if (length(issues) > 0L) {
  cat("\nPATCH APPLIED, BUT STATIC VALIDATION FOUND ISSUES:\n")
  cat(paste0(" - ", issues, collapse = "\n"), "\n")

  stop(
    "Patch v2 requires manual inspection. Restore from backup if needed: ",
    backup_dir,
    call. = FALSE
  )
}


cat("\n")
cat("============================================================\n")
cat("soviclust PATCH v2 APPLIED SUCCESSFULLY\n")
cat("============================================================\n")
cat("Common optimizer fitness : jfgwcv via optimizer_fitness()\n")
cat("Final spatial diagnostic : spatial_obj\n")
cat("Optimizer files patched  : 9\n")
cat("Backup                    : ", backup_dir, "\n", sep = "")
cat("\nNext commands:\n")
cat("  devtools::load_all()\n")
cat("  devtools::test()\n")
cat("  devtools::check()\n")
cat("\nDo NOT run the 30-run benchmark until tests/check are clean.\n")
