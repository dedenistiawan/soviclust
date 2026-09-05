# =============================================================================
# verify_patch_v2.R
# Lightweight static verification after applying Patch v2.
# Run from the soviclust project root.
# =============================================================================

func_dir <- file.path("inst", "app", "R", "shared", "function")

files <- c(
  "abcfgwc.R", "fpafgwc.R", "gsafgwc.R",
  "gwofgwc.R", "hhofgwc.R", "ifafgwc.R",
  "psofgwc.R", "tlbofgwc.R", "woafgwc.R"
)

cat("Patch v2 static verification\n")
cat("============================\n")

for (f in files) {
  txt <- paste(
    readLines(file.path(func_dir, f), warn = FALSE),
    collapse = "\n"
  )

  mixed_v2 <- grepl("jfgwcv2\\(", txt, perl = TRUE)
  direct_v <- grepl("jfgwcv\\(", txt, perl = TRUE)
  common <- grepl("optimizer_fitness\\(", txt, perl = TRUE)
  metadata <- grepl("fitness_type", txt, fixed = TRUE)
  spatial <- grepl("spatial_obj", txt, fixed = TRUE)

  cat(
    sprintf(
      "%-12s common=%-5s direct_jfgwcv=%-5s jfgwcv2=%-5s metadata=%-5s spatial=%-5s\n",
      f, common, direct_v, mixed_v2, metadata, spatial
    )
  )
}

cat("\nExpected for every file:\n")
cat(" common=TRUE, direct_jfgwcv=FALSE, jfgwcv2=FALSE, metadata=TRUE, spatial=TRUE\n")
