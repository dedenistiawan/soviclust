# =============================================================================
# R/method_comparison/method_comparison_server.R
# Server Logic Tab Method Comparison
#
# DIPANGGIL DARI : server.R via method_comparison_server(input, output, session, rv)
# DEPENDENSI     :
#   - R/core/analysis_core.R  (run_3way_comparison)
#   - R/core/helpers.R        (VULN_CLASSES, VULN_PAL, normalize_01)
#   - Package: ggplot2, patchwork, tidyr, dplyr
# =============================================================================

method_comparison_server <- function(input, output, session, rv) {
  
  # ==========================================================================
  # OUTPUT: Progress box (kosong saat awal)
  # ==========================================================================
  
  output$comparison_progress <- renderUI({ NULL })
  
  # ==========================================================================
  # OBSERVER: Tombol Run Comparison
  # Menjalankan SoVI dengan 3 direction method sekaligus dan membandingkan
  # ==========================================================================
  
  observeEvent(input$run_comparison, {
    req(rv$data, rv$vars_ok, rv$sovi_vars)
    
    output$comparison_progress <- renderUI({
      div(class = "progress-box", style = "background:#fff3cd;",
          icon("spinner", class = "fa-spin"),
          " Menjalankan 3-way comparison...")
    })
    
    withProgress(message = "Running 3-Way Comparison...", value = 0, {
      incProgress(0.2, detail = "Menyiapkan data...")
      
      result <- tryCatch({
        run_3way_comparison(
          data              = rv$data,
          sovi_vars         = rv$sovi_vars,
          neg_vars          = rv$neg_vars,
          loading_threshold = input$comp_threshold,
          id_col            = input$id_col,
          name_col          = input$name_col
        )
      }, error = function(e) {
        showNotification(paste("Error comparison:", e$message),
                         type = "error", duration = 10)
        NULL
      })
      
      incProgress(0.8, detail = "Selesai.")
      rv$comp_results <- result
    })
    
    output$comparison_progress <- renderUI({
      if (!is.null(rv$comp_results))
        div(class = "progress-box status-ok",
            icon("check"), " Comparison selesai!")
      else
        div(class = "progress-box status-err",
            icon("times"), " Gagal. Periksa data.")
    })
  })
  
  # ==========================================================================
  # TAB 0: SEMUA METRIK EVALUASI (tampil pertama)
  # Menampilkan 6 metrik dalam satu tampilan ringkas dan komprehensif
  # ==========================================================================
  
  output$comp_metrics_ui <- renderUI({
    req(rv$comp_results)
    
    m <- rv$comp_results$metrics
    n <- m$theory_vs_loading$n
    
    # ── Helper: warna berdasarkan nilai metrik ──────────────────────────────
    color_rho   <- function(v) if(v>=0.90) "#27ae60" else if(v>=0.80) "#f39c12" else "#e74c3c"
    color_tau   <- function(v) if(v>=0.80) "#27ae60" else if(v>=0.70) "#f39c12" else "#e74c3c"
    color_agree <- function(v) if(v>=80)   "#27ae60" else if(v>=60)   "#f39c12" else "#e74c3c"
    color_kappa <- function(v) {
      if(is.na(v)) return("#adb5bd")
      if(v>=0.80) "#27ae60" else if(v>=0.60) "#f39c12" else "#e74c3c"
    }
    color_mard  <- function(v, n) {
      pct <- v/n*100
      if(pct<=5) "#27ae60" else if(pct<=10) "#f39c12" else "#e74c3c"
    }
    color_rmsd  <- function(v) if(v<=0.05) "#27ae60" else if(v<=0.10) "#f39c12" else "#e74c3c"
    
    # ── Helper: render satu baris metrik ───────────────────────────────────
    metric_row <- function(label, val_1A, val_1B, val_AB,
                           color_fn, fmt_fn = function(x) x,
                           note = NULL) {
      tags$tr(
        tags$td(style = "padding:7px 8px; font-weight:600; font-size:13px;",
                label,
                if(!is.null(note)) tags$span(style="color:#78909c;font-weight:400;
                                                     font-size:11px;display:block;",
                                             note)),
        tags$td(style = paste0("padding:7px 8px; text-align:center;
                                font-weight:700; font-size:13px; color:",
                               color_fn(val_1A), ";"),
                fmt_fn(val_1A)),
        tags$td(style = paste0("padding:7px 8px; text-align:center;
                                font-weight:700; font-size:13px; color:",
                               color_fn(val_1B), ";"),
                fmt_fn(val_1B)),
        tags$td(style = paste0("padding:7px 8px; text-align:center;
                                font-weight:700; font-size:13px; color:",
                               color_fn(val_AB), ";"),
                fmt_fn(val_AB))
      )
    }
    
    div(
      
      # ── Judul & info ─────────────────────────────────────────────────────
      div(style = "margin-bottom:14px;",
          tags$h4(style = "color:#1a73c1; font-weight:700; margin-bottom:4px;",
                  icon("ruler-combined"), " Ringkasan Metrik Evaluasi"),
          tags$p(style = "font-size:12.5px; color:#78909c; margin:0;",
                 "n = ", n, " unit wilayah  |  ",
                 "Threshold loading = ", input$comp_threshold, "  |  ",
                 "Hijau = konsisten  |  Kuning = moderat  |  Merah = berbeda signifikan")
      ),
      
      # ── Tabel utama semua metrik ──────────────────────────────────────────
      div(style = "overflow-x:auto;",
          tags$table(
            style = "width:100%; border-collapse:collapse; font-family:inherit;",
            
            # Header
            tags$thead(
              tags$tr(style = "background:#1a73c1; color:#fff;",
                      tags$th(style = "padding:10px 8px; text-align:left; font-size:13px;
                               border-radius:6px 0 0 0;", "Metrik"),
                      tags$th(style = "padding:10px 8px; text-align:center; font-size:13px;",
                              "Theory vs Loading"),
                      tags$th(style = "padding:10px 8px; text-align:center; font-size:13px;",
                              "Theory vs Cutter"),
                      tags$th(style = "padding:10px 8px; text-align:center; font-size:13px;
                               border-radius:0 6px 0 0;", "Loading vs Cutter")
              )
            ),
            
            tags$tbody(
              
              # ── Kelompok 1: Konsistensi Ranking ────────────────────────────
              tags$tr(style = "background:#e3f2fd;",
                      tags$td(colspan = "4",
                              style = "padding:5px 8px; font-size:11.5px; font-weight:700;
                               color:#1565c0; letter-spacing:0.5px;",
                              "\u25B6 KONSISTENSI RANKING")
              ),
              
              metric_row(
                "Spearman \u03c1",
                m$theory_vs_loading$spearman_r,
                m$theory_vs_cutter$spearman_r,
                m$loading_vs_cutter$spearman_r,
                color_rho,
                note = "\u2265 0.90 kuat | 0.80\u20130.89 moderat | < 0.80 lemah"
              ),
              
              metric_row(
                "Kendall \u03c4",
                m$theory_vs_loading$kendall_r,
                m$theory_vs_cutter$kendall_r,
                m$loading_vs_cutter$kendall_r,
                color_tau,
                note = "\u2265 0.80 kuat | 0.70\u20130.79 moderat | < 0.70 lemah"
              ),
              
              metric_row(
                "MARD",
                m$theory_vs_loading$mard,
                m$theory_vs_cutter$mard,
                m$loading_vs_cutter$mard,
                function(v) color_mard(v, n),
                fmt_fn = function(v) paste0(v, " posisi"),
                note   = paste0("Rata-rata pergeseran ranking (\u2264",
                                round(n*0.05), " = konsisten | \u2264",
                                round(n*0.10), " = moderat)")
              ),
              
              metric_row(
                "RMSD",
                m$theory_vs_loading$rmsd,
                m$theory_vs_cutter$rmsd,
                m$loading_vs_cutter$rmsd,
                color_rmsd,
                note = "\u2264 0.05 kuat | 0.05\u20130.10 moderat | > 0.10 lemah"
              ),
              
              # ── Kelompok 2: Kesepakatan Klasifikasi ────────────────────────
              tags$tr(style = "background:#e8f5e9;",
                      tags$td(colspan = "4",
                              style = "padding:5px 8px; font-size:11.5px; font-weight:700;
                               color:#1b5e20; letter-spacing:0.5px;",
                              "\u25B6 KESEPAKATAN KLASIFIKASI")
              ),
              
              metric_row(
                "Cohen's \u03ba",
                m$theory_vs_loading$kappa,
                m$theory_vs_cutter$kappa,
                m$loading_vs_cutter$kappa,
                color_kappa,
                fmt_fn = function(v) if(is.na(v)) "N/A" else v,
                note   = "\u2265 0.80 hampir sempurna | 0.60\u20130.79 substansial | < 0.60 moderat"
              ),
              
              # ── Kelompok 3: Relevansi Kebijakan ────────────────────────────
              tags$tr(style = "background:#fff8e1;",
                      tags$td(colspan = "4",
                              style = "padding:5px 8px; font-size:11.5px; font-weight:700;
                               color:#e65100; letter-spacing:0.5px;",
                              "\u25B6 RELEVANSI KEBIJAKAN (20% Distrik Teratas & Terbawah)")
              ),
              
              metric_row(
                paste0("Top-20% Agreement"),
                m$theory_vs_loading$top_agree,
                m$theory_vs_cutter$top_agree,
                m$loading_vs_cutter$top_agree,
                color_agree,
                fmt_fn = function(v) paste0(v, "%"),
                note   = paste0("Distrik paling rentan yang sama (n=",
                                m$theory_vs_loading$k_top, ") | \u2265 80% konsisten")
              ),
              
              metric_row(
                paste0("Bottom-20% Agreement"),
                m$theory_vs_loading$bot_agree,
                m$theory_vs_cutter$bot_agree,
                m$loading_vs_cutter$bot_agree,
                color_agree,
                fmt_fn = function(v) paste0(v, "%"),
                note   = paste0("Distrik paling tidak rentan yang sama (n=",
                                m$theory_vs_loading$k_top, ") | \u2265 80% konsisten")
              )
            ) # end tbody
          )   # end table
      ),    # end div overflow
      
      tags$br(),
      
      # ── Panduan interpretasi ───────────────────────────────────────────────
      shinydashboard::box(
        title       = tags$span(icon("book-open"), " Panduan Interpretasi Metrik"),
        status      = "info",
        solidHeader = FALSE,
        width       = 12,
        collapsible = TRUE,
        collapsed   = TRUE,
        
        tags$table(
          style = "width:100%; font-size:12.5px; border-collapse:collapse;",
          tags$thead(tags$tr(
            tags$th(style="padding:6px;background:#f8f9fa;border-bottom:2px solid #dee2e6;",
                    "Metrik"),
            tags$th(style="padding:6px;background:#f8f9fa;border-bottom:2px solid #dee2e6;",
                    "Yang Diukur"),
            tags$th(style="padding:6px;background:#f8f9fa;border-bottom:2px solid #dee2e6;",
                    "Threshold Hijau"),
            tags$th(style="padding:6px;background:#f8f9fa;border-bottom:2px solid #dee2e6;",
                    "Keunggulan")
          )),
          tags$tbody(
            lapply(list(
              list("Spearman \u03c1", "Konsistensi ranking global",
                   "\u2265 0.90", "Standar, mudah dibandingkan antar studi"),
              list("Kendall \u03c4", "Konsistensi ranking lokal",
                   "\u2265 0.80", "Lebih robust untuk dataset kecil, konfirmasi Spearman"),
              list("MARD", "Rata-rata pergeseran ranking (posisi)",
                   "\u2264 5% dari n", "Sangat intuitif untuk pelaporan"),
              list("RMSD", "Magnitude perbedaan skor [0\u20131]",
                   "\u2264 0.05", "Sensitif terhadap outlier skor"),
              list("Cohen's \u03ba", "Kesepakatan kelas kerentanan",
                   "\u2265 0.80", "Koreksi kesepakatan kebetulan"),
              list("Top/Bottom-20%", "Distrik prioritas yang sama",
                   "\u2265 80%", "Paling relevan untuk pengambilan keputusan kebijakan")
            ), function(row) {
              tags$tr(style = "border-bottom:1px solid #f0f0f0;",
                      tags$td(style="padding:6px;font-weight:600;", row[[1]]),
                      tags$td(style="padding:6px;", row[[2]]),
                      tags$td(style="padding:6px;color:#27ae60;font-weight:600;", row[[3]]),
                      tags$td(style="padding:6px;color:#546e7a;", row[[4]])
              )
            })
          )
        )
      )
    )
  })
  
  output$comp_spearman <- DT::renderDT({
    req(rv$comp_results)
    corr           <- round(rv$comp_results$corr_mat, 4)
    colnames(corr) <- rownames(corr) <- c(
      "Theory-Based (PM)", "Loading Sign", "Cutter's Method"
    )
    DT::datatable(as.data.frame(corr),
                  options  = list(dom = "t"),
                  rownames = TRUE,
                  caption  = "Spearman Correlation Matrix")
  })
  
  # Interpretasi teks korelasi
  output$comp_interpretation <- renderPrint({
    req(rv$comp_results)
    corr  <- rv$comp_results$corr_mat
    pairs <- list(
      list(c("theory","loading"), c("Theory-Based (PM)", "Loading Sign")),
      list(c("theory","cutter"),  c("Theory-Based (PM)", "Cutter's Method")),
      list(c("loading","cutter"), c("Loading Sign",       "Cutter's Method"))
    )
    for (p in pairs) {
      rho    <- corr[p[[1]][1], p[[1]][2]]
      interp <- dplyr::case_when(
        rho >= 0.90 ~ "HIGHLY CORRELATED",
        rho >= 0.70 ~ "MODERATELY CORRELATED",
        TRUE        ~ "SUBSTANTIALLY DIFFERENT"
      )
      cat(sprintf("%s vs %s : \u03c1 = %.4f \u2192 %s\n",
                  p[[2]][1], p[[2]][2], rho, interp))
    }
  })
  
  # ==========================================================================
  # TAB 2: SCATTER PLOT
  # ==========================================================================
  
  output$comp_scatter <- renderPlot({
    req(rv$comp_results)
    df   <- rv$comp_results$scores_df
    corr <- rv$comp_results$corr_mat
    
    # Fungsi helper buat satu panel scatter
    make_panel <- function(x_var, y_var, x_lab, y_lab, col, rho_val) {
      ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]],
                                       y = .data[[y_var]])) +
        ggplot2::geom_point(alpha = 0.4, color = col, size = 0.9) +
        ggplot2::geom_abline(linetype = "dashed", color = "grey60") +
        ggplot2::geom_smooth(method = "lm", se = TRUE,
                             color = "#d6604d", linewidth = 0.8) +
        ggplot2::annotate("text", x = 0.1, y = 0.92,
                          label = paste0("\u03c1 = ", round(rho_val, 3)),
                          size = 4, hjust = 0, color = "grey30") +
        ggplot2::labs(title = paste(x_lab, "vs", y_lab),
                      x = x_lab, y = y_lab) +
        ggplot2::theme_minimal(base_size = 11)
    }
    
    p1 <- make_panel("theory",  "loading", "Theory-Based (PM)", "Loading Sign",
                     "#2166ac", corr["theory","loading"])
    p2 <- make_panel("theory",  "cutter",  "Theory-Based (PM)", "Cutter's Method",
                     "#4dac26", corr["theory","cutter"])
    p3 <- make_panel("loading", "cutter",  "Loading Sign",       "Cutter's Method",
                     "#b2abd2", corr["loading","cutter"])
    
    p1 + p2 + p3 +
      patchwork::plot_annotation(
        title = "3-Way SoVI Score Comparison: Direction Method",
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 13, face = "bold")
        )
      )
  })
  
  # ==========================================================================
  # TAB 3: DISTRIBUSI KELAS
  # ==========================================================================
  
  output$comp_class_dist <- renderPlot({
    req(rv$comp_results)
    r      <- rv$comp_results$results
    all_df <- data.frame()
    
    method_labels <- c(
      theory  = "Theory-Based (PM)",
      loading = "Loading Sign",
      cutter  = "Cutter's Method"
    )
    
    for (m in c("theory", "loading", "cutter")) {
      tbl        <- as.data.frame(table(Class = r[[m]]$sovi_df$vuln_class))
      tbl$Pct    <- round(tbl$Freq / sum(tbl$Freq) * 100, 1)
      tbl$Method <- method_labels[m]
      all_df     <- rbind(all_df, tbl)
    }
    
    all_df$Class  <- factor(all_df$Class,  levels = VULN_CLASSES)
    all_df$Method <- factor(all_df$Method,
                            levels = unname(method_labels))
    
    ggplot2::ggplot(all_df,
                    ggplot2::aes(x = Class, y = Freq, fill = Class)) +
      ggplot2::geom_bar(stat = "identity", width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Freq, "\n(", Pct, "%)")),
                         vjust = -0.2, size = 2.8) +
      ggplot2::scale_fill_manual(values = unname(VULN_PAL)) +
      ggplot2::facet_wrap(~Method, ncol = 3) +
      ggplot2::labs(
        title = "Distribusi Kelas Kerentanan per Method",
        x     = NULL,
        y     = "Jumlah Distrik"
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1, size = 8)
      )
  })
  
  # ==========================================================================
  # TAB 4: COMPONENT PROFILE HEATMAP
  # ==========================================================================
  
  output$comp_profile <- renderPlot({
    req(rv$comp_results)
    r       <- rv$comp_results$results
    rc_cols <- grep("^RC", names(r$theory$sovi_df), value = TRUE)
    
    method_labels <- c(
      theory  = "Theory-Based (PM)",
      loading = "Loading Sign",
      cutter  = "Cutter's Method"
    )
    
    all_df <- data.frame()
    for (m in c("theory", "loading", "cutter")) {
      sovi_df <- r[[m]]$sovi_df
      rc_norm <- as.data.frame(
        lapply(sovi_df[, rc_cols, drop = FALSE], normalize_01)
      )
      rc_norm$vuln_class <- sovi_df$vuln_class
      
      prof <- rc_norm |>
        dplyr::group_by(vuln_class) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(rc_cols),
                        \(x) mean(x, na.rm = TRUE)),
          .groups = "drop"
        )
      
      long        <- tidyr::pivot_longer(prof,
                                         cols      = dplyr::all_of(rc_cols),
                                         names_to  = "Component",
                                         values_to = "Mean_Score")
      long$Method <- method_labels[m]
      all_df      <- rbind(all_df, long)
    }
    
    all_df$vuln_class <- factor(all_df$vuln_class, levels = VULN_CLASSES)
    all_df$Method     <- factor(all_df$Method, levels = unname(method_labels))
    
    ggplot2::ggplot(all_df,
                    ggplot2::aes(x = Component, y = vuln_class,
                                 fill = Mean_Score)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = round(Mean_Score, 2)),
                         size = 2.8, color = "black") +
      ggplot2::scale_fill_distiller(palette   = "RdYlGn",
                                    direction = -1,
                                    limits    = c(0, 1),
                                    name      = "Mean\nScore") +
      ggplot2::facet_wrap(~Method, ncol = 3) +
      ggplot2::labs(
        title = "Component Profile \u2014 3-Way Comparison",
        x     = "RC Dimension",
        y     = "Vulnerability Class"
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 8),
        strip.text  = ggplot2::element_text(face = "bold")
      )
  })
  
  # ==========================================================================
  # TAB 6: REKOMENDASI DINAMIS
  #
  # PRINSIP REKOMENDASI:
  #   Theory-Based (PM) SELALU direkomendasikan karena alasan teoritik.
  #   Level konfirmasi ditentukan dari RATA-RATA semua metrik evaluasi,
  #   bukan hanya Spearman ρ saja.
  #
  # LOGIKA SKOR:
  #   Setiap metrik Theory vs Loading dan Theory vs Cutter menghasilkan
  #   skor 0-2 (0=lemah, 1=moderat, 2=kuat). Total skor max = 10.
  #   Skor total → level konfirmasi.
  # ==========================================================================
  
  output$comp_recommendation <- renderUI({
    req(rv$comp_results)
    
    m    <- rv$comp_results$metrics
    corr <- rv$comp_results$corr_mat
    n    <- m$theory_vs_loading$n
    
    # ── Shorthand metrik Theory vs Loading dan Theory vs Cutter ─────────────
    tl <- m$theory_vs_loading
    tc <- m$theory_vs_cutter
    
    # ── Fungsi scoring per metrik ────────────────────────────────────────────
    # Setiap metrik diberi skor 0 (lemah), 1 (moderat), atau 2 (kuat)
    score_rho   <- function(v) if(v >= 0.90) 2 else if(v >= 0.80) 1 else 0
    score_tau   <- function(v) if(v >= 0.80) 2 else if(v >= 0.70) 1 else 0
    score_agree <- function(v) if(v >= 80)   2 else if(v >= 60)   1 else 0
    score_kappa <- function(v) {
      if(is.na(v)) return(1)   # NA = tidak bisa dihitung = neutral
      if(v >= 0.80) 2 else if(v >= 0.60) 1 else 0
    }
    score_mard  <- function(v, n) {
      pct <- v / n * 100
      if(pct <= 5) 2 else if(pct <= 10) 1 else 0
    }
    
    # ── Hitung skor untuk masing-masing pasangan TL dan TC ───────────────────
    scores_tl <- c(
      score_rho(tl$spearman_r),
      score_tau(tl$kendall_r),
      score_agree(tl$top_agree),
      score_agree(tl$bot_agree),
      score_kappa(tl$kappa),
      score_mard(tl$mard, n)
    )
    scores_tc <- c(
      score_rho(tc$spearman_r),
      score_tau(tc$kendall_r),
      score_agree(tc$top_agree),
      score_agree(tc$bot_agree),
      score_kappa(tc$kappa),
      score_mard(tc$mard, n)
    )
    
    # Total skor gabungan (max = 24 dari 6 metrik × 2 pasangan × skor maks 2)
    total_score <- sum(scores_tl) + sum(scores_tc)
    max_score   <- 24
    
    # ── Tentukan level konfirmasi dari skor total ─────────────────────────────
    pct_score <- total_score / max_score * 100
    
    if (pct_score >= 75) {
      conf_status <- "success"
      conf_label  <- "Sangat Dikonfirmasi oleh Data"
      conf_color  <- "#27ae60"
      conf_icon   <- icon("check-circle")
      warning_box <- NULL
      
    } else if (pct_score >= 50) {
      conf_status <- "primary"
      conf_label  <- "Dikonfirmasi oleh Data"
      conf_color  <- "#1a73c1"
      conf_icon   <- icon("check-circle")
      warning_box <- div(
        class = "progress-box",
        style = "background:#e3f2fd; border-left-color:#1a73c1;
                 font-size:12.5px; margin-top:10px;",
        icon("info-circle"),
        " Beberapa metrik menunjukkan perbedaan moderat. Periksa tab ",
        tags$strong("Scatter"), " dan ", tags$strong("Metrik Evaluasi"),
        " untuk detail distrik yang bergeser ranking."
      )
      
    } else if (pct_score >= 30) {
      conf_status <- "warning"
      conf_label  <- "Dikonfirmasi dengan Catatan"
      conf_color  <- "#f39c12"
      conf_icon   <- icon("exclamation-circle")
      warning_box <- div(
        class = "progress-box",
        style = "background:#fff8e1; border-left-color:#f39c12;
                 font-size:12.5px; margin-top:10px;",
        icon("exclamation-triangle"),
        tags$strong(" Saran:"),
        " Verifikasi ulang arah (+/-) variabel di tab Variable Config.",
        " Pastikan setiap variabel sudah sesuai literatur kerentanan sosial.",
        " Perbedaan ini tidak mengubah rekomendasi — cukup didokumentasikan dalam laporan."
      )
      
    } else {
      conf_status <- "warning"
      conf_label  <- "Dikonfirmasi \u2014 Perlu Investigasi Arah Variabel"
      conf_color  <- "#e74c3c"
      conf_icon   <- icon("exclamation-triangle")
      warning_box <- div(
        class = "progress-box",
        style = "background:#fdf0ed; border-left-color:#e74c3c;
                 font-size:12.5px; margin-top:10px;",
        icon("exclamation-triangle", style = "color:#e74c3c;"),
        tags$strong(" Tindakan Diperlukan:"),
        " Kembali ke tab Variable Config dan periksa ulang arah (+/-) setiap variabel.",
        " Pastikan variabel protektif sudah ditandai negatif (-).",
        " Ketidakkonsistenan ini BUKAN alasan beralih ke method lain."
      )
    }
    
    # ── Buat skor scorecard per metrik ────────────────────────────────────────
    score_icon <- function(s) {
      if      (s == 2) tags$span(style="color:#27ae60;font-size:14px;", "\u2713\u2713")
      else if (s == 1) tags$span(style="color:#f39c12;font-size:14px;", "\u2713")
      else             tags$span(style="color:#e74c3c;font-size:14px;", "\u2717")
    }
    
    # ── Helper: warna level ───────────────────────────────────────────────────
    lv_rho   <- function(v) if(v>=0.90) "#27ae60" else if(v>=0.80) "#f39c12" else "#e74c3c"
    lv_tau   <- function(v) if(v>=0.80) "#27ae60" else if(v>=0.70) "#f39c12" else "#e74c3c"
    lv_agree <- function(v) if(v>=80)   "#27ae60" else if(v>=60)   "#f39c12" else "#e74c3c"
    lv_kappa <- function(v) {
      if(is.na(v)) "#adb5bd"
      else if(v>=0.80) "#27ae60" else if(v>=0.60) "#f39c12" else "#e74c3c"
    }
    lv_mard  <- function(v, n) {
      pct <- v/n*100
      if(pct<=5) "#27ae60" else if(pct<=10) "#f39c12" else "#e74c3c"
    }
    
    div(
      shinydashboard::box(
        title       = tags$span(conf_icon, " Rekomendasi Berdasarkan Data"),
        status      = conf_status,
        solidHeader = TRUE,
        width       = 12,
        
        # ── Rekomendasi tetap ─────────────────────────────────────────────
        div(style = "background:#eafaf1; border:1px solid #27ae60;
                     border-radius:8px; padding:14px 18px; margin-bottom:14px;",
            div(style = "font-size:15px; font-weight:700; color:#27ae60; margin-bottom:4px;",
                icon("star"), " Method yang Direkomendasikan: Theory-Based (PM)"),
            div(style = "font-size:12.5px; color:#37474f;",
                "Theory-Based (PM) direkomendasikan secara konsisten dalam literatur SoVI ",
                "(Cutter et al., 2003). Direction variabel ditetapkan berdasarkan teori, ",
                "bukan pola matematis PCA, sehingga lebih reproducible dan defensible ",
                "untuk penelitian dan kebijakan.")
        ),
        
        # ── Level konfirmasi dari semua metrik ────────────────────────────
        div(style = paste0("font-size:14px; font-weight:700; color:", conf_color,
                           "; margin-bottom:10px;"),
            conf_icon, " Level Konfirmasi: ", conf_label,
            tags$span(style = "font-size:12px; font-weight:400; color:#78909c;
                               margin-left:10px;",
                      paste0("(Skor: ", total_score, "/", max_score, " = ",
                             round(pct_score), "%)"))
        ),
        warning_box,
        
        tags$hr(),
        
        # ── Scorecard per metrik ──────────────────────────────────────────
        # CATATAN: score_icon() mengembalikan tags$span (HTML object).
        # Tidak boleh di-paste0() karena akan muncul sebagai teks mentah.
        # Solusi: pisahkan nilai dan ikon menjadi elemen terpisah dalam td.
        tags$p(tags$strong("Detail Skor per Metrik (Theory-Based sebagai referensi):")),
        tags$p(style = "font-size:12px; color:#78909c; margin-bottom:10px;",
               "\u2713\u2713 = kuat (skor 2)  |  \u2713 = moderat (skor 1)  |",
               "  \u2717 = lemah (skor 0)"),
        
        tags$table(
          style = "width:100%; font-size:13px; border-collapse:collapse;",
          
          tags$thead(tags$tr(
            tags$th(style = "padding:6px; background:#f8f9fa;
                             border-bottom:2px solid #dee2e6;",
                    "Metrik"),
            tags$th(style = "padding:6px; background:#f8f9fa;
                             border-bottom:2px solid #dee2e6; text-align:center;",
                    "Theory vs Loading"),
            tags$th(style = "padding:6px; background:#f8f9fa;
                             border-bottom:2px solid #dee2e6; text-align:center;",
                    "Theory vs Cutter")
          )),
          
          tags$tbody(
            
            # ── Helper render satu td nilai + ikon ──────────────────────────
            # Nilai dan ikon dijadikan dua elemen terpisah dalam tags$td
            # agar HTML tags$span tidak di-coerce menjadi string
            
            # Baris 1: Spearman ρ
            tags$tr(style = "border-bottom:1px solid #f0f0f0;",
                    tags$td(style = "padding:6px; font-weight:600;",
                            "Spearman \u03c1"),
                    tags$td(style = paste0("padding:6px; text-align:center;"),
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_rho(tl$spearman_r), ";"),
                                      tl$spearman_r),
                            " ", score_icon(scores_tl[1])),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_rho(tc$spearman_r), ";"),
                                      tc$spearman_r),
                            " ", score_icon(scores_tc[1]))
            ),
            
            # Baris 2: Kendall τ
            tags$tr(style = "border-bottom:1px solid #f0f0f0;",
                    tags$td(style = "padding:6px; font-weight:600;",
                            "Kendall \u03c4"),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_tau(tl$kendall_r), ";"),
                                      tl$kendall_r),
                            " ", score_icon(scores_tl[2])),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_tau(tc$kendall_r), ";"),
                                      tc$kendall_r),
                            " ", score_icon(scores_tc[2]))
            ),
            
            # Baris 3: Top-20% Agreement
            tags$tr(style = "border-bottom:1px solid #f0f0f0;",
                    tags$td(style = "padding:6px; font-weight:600;",
                            "Top-20% Agreement"),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_agree(tl$top_agree), ";"),
                                      paste0(tl$top_agree, "%")),
                            " ", score_icon(scores_tl[3])),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_agree(tc$top_agree), ";"),
                                      paste0(tc$top_agree, "%")),
                            " ", score_icon(scores_tc[3]))
            ),
            
            # Baris 4: Bottom-20% Agreement
            tags$tr(style = "border-bottom:1px solid #f0f0f0;",
                    tags$td(style = "padding:6px; font-weight:600;",
                            "Bottom-20% Agreement"),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_agree(tl$bot_agree), ";"),
                                      paste0(tl$bot_agree, "%")),
                            " ", score_icon(scores_tl[4])),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_agree(tc$bot_agree), ";"),
                                      paste0(tc$bot_agree, "%")),
                            " ", score_icon(scores_tc[4]))
            ),
            
            # Baris 5: Cohen's κ
            tags$tr(style = "border-bottom:1px solid #f0f0f0;",
                    tags$td(style = "padding:6px; font-weight:600;",
                            "Cohen's \u03ba"),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_kappa(tl$kappa), ";"),
                                      if (is.na(tl$kappa)) "N/A" else tl$kappa),
                            " ", score_icon(scores_tl[5])),
                    tags$td(style = "padding:6px; text-align:center;",
                            tags$span(style = paste0("font-weight:700; color:",
                                                     lv_kappa(tc$kappa), ";"),
                                      if (is.na(tc$kappa)) "N/A" else tc$kappa),
                            " ", score_icon(scores_tc[5]))
            ),
            
            # Baris 6: MARD
            tags$tr(
              tags$td(style = "padding:6px; font-weight:600;",
                      "MARD (posisi)"),
              tags$td(style = "padding:6px; text-align:center;",
                      tags$span(style = paste0("font-weight:700; color:",
                                               lv_mard(tl$mard, n), ";"),
                                paste0(tl$mard, " posisi")),
                      " ", score_icon(scores_tl[6])),
              tags$td(style = "padding:6px; text-align:center;",
                      tags$span(style = paste0("font-weight:700; color:",
                                               lv_mard(tc$mard, n), ";"),
                                paste0(tc$mard, " posisi")),
                      " ", score_icon(scores_tc[6]))
            )
          ) # end tbody
        ),  # end table
        
        tags$hr(),
        
        # ── Ringkasan skor total ──────────────────────────────────────────
        div(style = "background:#f8f9fa; border-radius:6px; padding:12px 16px;",
            fluidRow(
              column(4,
                     div(style = "text-align:center;",
                         div(style = paste0("font-size:28px; font-weight:700; color:",
                                            conf_color, ";"),
                             paste0(sum(scores_tl), "/12")),
                         div(style = "font-size:12px; color:#78909c;",
                             "Skor Theory vs Loading")
                     )
              ),
              column(4,
                     div(style = "text-align:center;",
                         div(style = paste0("font-size:28px; font-weight:700; color:",
                                            conf_color, ";"),
                             paste0(sum(scores_tc), "/12")),
                         div(style = "font-size:12px; color:#78909c;",
                             "Skor Theory vs Cutter")
                     )
              ),
              column(4,
                     div(style = "text-align:center;",
                         div(style = paste0("font-size:28px; font-weight:700; color:",
                                            conf_color, ";"),
                             paste0(total_score, "/24")),
                         div(style = "font-size:12px; color:#78909c;",
                             paste0("Total (", round(pct_score), "%)")
                         )
                     )
              )
            )
        ),
        
        tags$br(),
        div(class = "progress-box",
            style = "background:#e3f2fd; border-left-color:#1a73c1; font-size:12.5px;",
            icon("arrow-right"),
            " Setelah menentukan method, lanjut ke ",
            tags$strong("SoVI Computation"), ".")
      )
    )
  })
  
} # end method_comparison_server()