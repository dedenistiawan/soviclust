# =============================================================================
# R/shared/stability_test.R
# Fungsi generik untuk Multiple Independent Runs — Stability Analysis
#
# Merespons catatan reviewer:
#   "Because GWO is a stochastic metaheuristic, reporting a single clustering
#    result for each configuration is insufficient to establish superiority or
#    robustness. The authors should conduct multiple independent runs with
#    different random seeds and report mean, standard deviation, best, worst,
#    and median validity-index values."
#
# Berlaku untuk SEMUA algoritma stokastik di FGWC, LFGWC, dan ALFGWC:
#   abc, fpa, gsa, gwo, hho, ifa, pso, tlbo, woa
#   (termasuk classic karena inisialisasi centroid juga acak)
#
# PENGGUNAAN:
#   stability_result <- run_stability_test(
#     run_fn     = run_fgwc_shiny,   # atau run_lfgwc_shiny / run_alfgwc_shiny
#     base_args  = list(...),        # semua argumen kecuali seed
#     n_runs     = 30,
#     seed_start = 1,
#     module     = "fgwc"            # "fgwc" | "lfgwc" | "alfgwc"
#   )
#
# CATATAN: seed diinjeksi melalui:
#   FGWC   -> base_args$fgwc_params$randomN
#   LFGWC  -> base_args$lfgwc_params$randomN
#   ALFGWC -> base_args$randomN (argumen langsung)
# =============================================================================


# =============================================================================
# HELPER: Ekstrak validity index dari satu run result
# =============================================================================

#' Ekstrak nilai validity index dari satu hasil run
#'
#' @param result  Output dari run_fgwc_shiny / run_lfgwc_shiny / run_alfgwc_shiny
#' @return Named numeric vector: PC, CE, SC, XB, IFV, Kwon, Silhouette, F_obj
.extract_validity <- function(result) {
  if (is.null(result)) {
    return(c(PC = NA, CE = NA, SC = NA, XB = NA,
             IFV = NA, Kwon = NA, Silhouette = NA, F_obj = NA))
  }

  # val_df bisa berisi kolom "Nilai" (FGWC) atau "Value" (LFGWC/ALFGWC)
  val   <- result$val_df
  vcol  <- if ("Nilai" %in% names(val)) "Nilai" else "Value"
  vcol  <- if (vcol %in% names(val)) vcol else names(val)[2]  # fallback kolom ke-2

  # Cari index berdasarkan nama
  idx_col <- if ("Indeks" %in% names(val)) "Indeks" else "Index"

  get_val <- function(pattern) {
    rows <- grep(pattern, val[[idx_col]], ignore.case = TRUE)
    if (length(rows) == 0) return(NA_real_)
    as.numeric(val[[vcol]][rows[1]])
  }

  c(
    PC         = get_val("PC"),
    CE         = get_val("CE"),
    SC         = get_val("SC"),
    XB         = get_val("XB"),
    IFV        = get_val("IFV"),
    Kwon       = get_val("Kwon"),
    Silhouette = if (!is.null(result$sil_mean)) as.numeric(result$sil_mean) else NA_real_,
    F_obj      = if (!is.null(result$f_obj))    as.numeric(result$f_obj)    else NA_real_
  )
}


# =============================================================================
# HELPER: Injeksi seed ke argumen berdasarkan modul
# =============================================================================

#' Injeksikan seed ke dalam argumen run berdasarkan modul
#'
#' @param base_args  List argumen dasar
#' @param seed       Nilai seed yang akan diinjeksi
#' @param module     "fgwc" | "lfgwc" | "alfgwc"
#' @return base_args yang sudah dimodifikasi
.inject_seed <- function(base_args, seed, module) {
  args <- base_args

  if (module == "fgwc") {
    # run_fgwc_shiny: seed ada di fgwc_params$randomN
    if (is.null(args$fgwc_params)) args$fgwc_params <- list()
    args$fgwc_params$randomN <- seed

  } else if (module == "lfgwc") {
    # run_lfgwc_shiny: seed ada di lfgwc_params$randomN
    if (is.null(args$lfgwc_params)) args$lfgwc_params <- list()
    args$lfgwc_params$randomN <- seed

  } else if (module == "alfgwc") {
    # run_alfgwc_shiny: seed sebagai argumen langsung
    args$randomN <- seed
  }

  return(args)
}


# =============================================================================
# FUNGSI UTAMA: run_stability_test()
# =============================================================================

#' Jalankan multiple independent runs untuk stability analysis
#'
#' Menjalankan fungsi clustering N kali dengan seed berbeda dan
#' melaporkan statistik deskriptif untuk setiap validity index.
#'
#' @param run_fn     Fungsi run: run_fgwc_shiny | run_lfgwc_shiny | run_alfgwc_shiny
#' @param base_args  List semua argumen untuk run_fn (kecuali seed)
#' @param n_runs     Jumlah independent runs (default 30)
#' @param seed_start Seed awal; run ke-i menggunakan seed = seed_start + i - 1
#' @param module     "fgwc" | "lfgwc" | "alfgwc"
#' @param progress   Shiny progress object (opsional, untuk update progress bar)
#'
#' @return List berisi:
#'   - summary_df   : data.frame statistik (Index × Mean/SD/Best/Worst/Median)
#'   - detail_df    : data.frame nilai per run (Run × Seed × semua index)
#'   - n_success    : jumlah run yang berhasil
#'   - n_failed     : jumlah run yang gagal
#'   - elapsed_sec  : total waktu (detik)
run_stability_test <- function(run_fn,
                               base_args,
                               n_runs     = 30,
                               seed_start = 1,
                               module     = "fgwc",
                               progress   = NULL) {

  stopifnot(is.function(run_fn))
  stopifnot(module %in% c("fgwc", "lfgwc", "alfgwc"))
  n_runs     <- max(2L, as.integer(n_runs))
  seed_start <- as.integer(seed_start)

  message(sprintf("[stability_test] Starting %d runs for %s (seeds %d–%d)...",
                  n_runs, toupper(module), seed_start, seed_start + n_runs - 1))

  t_start   <- proc.time()
  all_vals  <- vector("list", n_runs)
  n_success <- 0L
  n_failed  <- 0L

  for (i in seq_len(n_runs)) {
    seed_i <- seed_start + i - 1L

    # Update progress bar jika tersedia
    if (!is.null(progress)) {
      progress$inc(
        amount  = 1 / n_runs,
        detail  = sprintf("Run %d / %d (seed = %d)...", i, n_runs, seed_i)
      )
    }

    message(sprintf("[stability_test] Run %d/%d — seed = %d", i, n_runs, seed_i))

    # Injeksi seed ke argumen
    args_i <- .inject_seed(base_args, seed_i, module)

    # Jalankan dan tangkap error per run
    result_i <- tryCatch(
      do.call(run_fn, args_i),
      error = function(e) {
        message(sprintf("[stability_test] Run %d failed: %s", i, e$message))
        NULL
      }
    )

    if (is.null(result_i)) {
      n_failed  <- n_failed  + 1L
    } else {
      n_success <- n_success + 1L
    }

    all_vals[[i]] <- c(
      Run  = i,
      Seed = seed_i,
      .extract_validity(result_i)
    )
  }

  elapsed <- (proc.time() - t_start)[["elapsed"]]
  message(sprintf("[stability_test] Done. Success=%d, Failed=%d, Elapsed=%.1fs",
                  n_success, n_failed, elapsed))

  # ── Susun detail_df ────────────────────────────────────────────────────────
  detail_df <- as.data.frame(do.call(rbind, all_vals))
  detail_df[] <- lapply(detail_df, as.numeric)
  detail_df$Run  <- as.integer(detail_df$Run)
  detail_df$Seed <- as.integer(detail_df$Seed)

  # ── Hitung summary statistik per index ────────────────────────────────────
  index_names <- c("PC", "CE", "SC", "XB", "IFV", "Kwon", "Silhouette", "F_obj")
  best_dir    <- c("max", "min", "min", "min", "max", "min", "max", "min")
  # PC/IFV/Sil: lebih besar lebih baik → Best = max
  # CE/SC/XB/Kwon/F_obj: lebih kecil lebih baik → Best = min

  summary_rows <- lapply(seq_along(index_names), function(j) {
    idx <- index_names[j]
    dir <- best_dir[j]
    vals <- detail_df[[idx]]
    vals_ok <- vals[!is.na(vals)]

    if (length(vals_ok) == 0) {
      return(data.frame(
        Index  = idx,
        Direction = paste0("(", dir, ")"),
        Mean   = NA, SD = NA, Best = NA, Worst = NA, Median = NA,
        stringsAsFactors = FALSE
      ))
    }

    best  <- if (dir == "max") max(vals_ok)  else min(vals_ok)
    worst <- if (dir == "max") min(vals_ok)  else max(vals_ok)

    data.frame(
      Index     = idx,
      Direction = ifelse(dir == "max", "\u2191 higher is better", "\u2193 lower is better"),
      Mean      = round(mean(vals_ok),   6),
      SD        = round(sd(vals_ok),     6),
      Best      = round(best,            6),
      Worst     = round(worst,           6),
      Median    = round(median(vals_ok), 6),
      stringsAsFactors = FALSE
    )
  })

  summary_df <- do.call(rbind, summary_rows)

  return(list(
    summary_df  = summary_df,
    detail_df   = detail_df,
    n_success   = n_success,
    n_failed    = n_failed,
    elapsed_sec = round(elapsed, 1)
  ))
}


# =============================================================================
# HELPER UI: Buat boxplot distribusi validity index dari hasil stability test
# =============================================================================

#' Buat boxplot validity index dari hasil run_stability_test()
#'
#' @param stability_result  Output dari run_stability_test()
#' @param title_prefix      String prefix untuk judul plot
#' @return ggplot object
plot_stability_boxplot <- function(stability_result,
                                   title_prefix = "Stability Analysis") {
  detail <- stability_result$detail_df
  indices <- c("PC", "CE", "SC", "XB", "IFV", "Kwon", "Silhouette", "F_obj")
  indices <- intersect(indices, names(detail))

  # Ubah ke format long untuk ggplot
  long_df <- do.call(rbind, lapply(indices, function(idx) {
    data.frame(
      Index = idx,
      Value = detail[[idx]],
      stringsAsFactors = FALSE
    )
  }))
  long_df <- long_df[!is.na(long_df$Value), ]

  ggplot2::ggplot(long_df, ggplot2::aes(x = Index, y = Value, fill = Index)) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.shape = 21,
                          outlier.size = 1.5, show.legend = FALSE) +
    ggplot2::facet_wrap(~ Index, scales = "free", nrow = 2) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title    = paste(title_prefix, "\u2014 Validity Index Distribution"),
      subtitle = sprintf("%d independent runs (seeds %s\u2013%s)",
                         nrow(detail),
                         detail$Seed[1],
                         detail$Seed[nrow(detail)]),
      x = NULL, y = "Value"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text       = ggplot2::element_text(face = "bold"),
      axis.text.x      = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

message("[stability_test.R] Stability analysis functions loaded.")
