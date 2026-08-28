# =============================================================================
# R/extended_analysis/extended_analysis_server.R
# Server Logic Tab Extended Analysis
#
# DIPANGGIL DARI : server.R via extended_analysis_server(input, output, session, rv)
# DEPENDENSI     :
#   - R/core/analysis_core.R (run_dominant_component, run_component_profile,
#                             run_moran_lisa, run_sensitivity,
#                             run_cutter_comparison)
#   - R/core/map_core.R      (build_leaflet_lisa)
#   - R/core/helpers.R       (VULN_CLASSES, VULN_PAL, normalize_01)
# =============================================================================

extended_analysis_server <- function(input, output, session, rv) {
  
  # ==========================================================================
  # OUTPUT: Progress box (kosong saat awal)
  # ==========================================================================
  
  output$analysis_progress <- renderUI({ NULL })
  
  # ==========================================================================
  # OBSERVER: Tombol Run Extended Analysis
  # Menjalankan analisis yang dipilih secara berurutan
  # ==========================================================================
  
  observeEvent(input$run_analysis, {
    req(rv$sovi_result, rv$sovi_ok)
    req(length(input$selected_analyses) > 0)
    
    sovi_df <- rv$sovi_result$sovi_df
    pca_out <- rv$sovi_result$pca_out
    rc_cols <- grep("^RC", names(sovi_df), value = TRUE)
    n_sel   <- length(input$selected_analyses)
    errors  <- character(0)
    
    output$analysis_progress <- renderUI({
      div(class = "progress-box", style = "background:#d4edda;",
          icon("spinner", class = "fa-spin"), " Menjalankan analisis...")
    })
    
    withProgress(message = "Running Extended Analysis...", value = 0, {
      
      # ── 1. Dominant Component ─────────────────────────────────────────────
      if ("dominant" %in% input$selected_analyses) {
        incProgress(1/n_sel, detail = "Dominant Component...")
        tryCatch({
          rv$dominant_df <- run_dominant_component(sovi_df, rc_cols)
        }, error = function(e) {
          errors <<- c(errors, paste("Dominant:", e$message))
        })
      }
      
      # ── 2. Component Profile ──────────────────────────────────────────────
      if ("profile" %in% input$selected_analyses) {
        incProgress(1/n_sel, detail = "Component Profile...")
        tryCatch({
          rv$profile_df <- run_component_profile(sovi_df, rc_cols)
        }, error = function(e) {
          errors <<- c(errors, paste("Profile:", e$message))
        })
      }
      
      # ── 3. Moran's I + LISA ───────────────────────────────────────────────
      if ("moran" %in% input$selected_analyses) {
        incProgress(1/n_sel, detail = "Moran's I + LISA...")
        tryCatch({
          req(rv$shp)
          rv$moran_res <- run_moran_lisa(
            sovi_df  = sovi_df,
            shp      = rv$shp,
            join_shp = input$join_shp,
            join_df  = input$id_col
          )
        }, error = function(e) {
          errors <<- c(errors, paste("Moran:", e$message))
        })
      }
      
      # ── 4. Sensitivity Analysis ───────────────────────────────────────────
      if ("sensitivity" %in% input$selected_analyses) {
        incProgress(1/n_sel, detail = "Sensitivity Analysis...")
        tryCatch({
          rv$sensitivity <- run_sensitivity(
            data             = rv$data,
            sovi_vars        = rv$sovi_vars,
            neg_vars         = rv$neg_vars,
            direction_method = input$direction_method,
            id_col           = input$id_col,
            name_col         = input$name_col
          )
        }, error = function(e) {
          errors <<- c(errors, paste("Sensitivity:", e$message))
        })
      }
      
      # ── 5. Cutter Comparison ──────────────────────────────────────────────
      if ("cutter_comp" %in% input$selected_analyses) {
        incProgress(1/n_sel, detail = "Cutter Comparison...")
        tryCatch({
          rv$cutter_df <- run_cutter_comparison(
            sovi_df_proposed = sovi_df,
            pca_out          = pca_out,
            id_col           = input$id_col,
            name_col         = input$name_col
          )
        }, error = function(e) {
          errors <<- c(errors, paste("Cutter:", e$message))
        })
      }
      
      rv$analysis_ok <- TRUE
    })
    
    # Tampilkan peringatan jika ada analisis yang gagal
    if (length(errors) > 0) {
      showNotification(
        paste("Beberapa analisis gagal:", paste(errors, collapse = " | ")),
        type = "warning", duration = 10
      )
    }
    
    output$analysis_progress <- renderUI({
      div(class = "progress-box status-ok",
          icon("check"),
          paste0(" Analisis selesai! (",
                 length(input$selected_analyses), " analisis dijalankan)"))
    })
  })
  
  # ==========================================================================
  # TAB 1: DOMINANT COMPONENT
  # ==========================================================================
  
  output$plot_dominant <- renderPlot({
    req(rv$dominant_df)
    df  <- rv$dominant_df
    tbl <- as.data.frame(table(Component = df$dom_comp))
    tbl$Pct <- round(tbl$Freq / nrow(df) * 100, 1)
    
    ggplot2::ggplot(tbl,
                    ggplot2::aes(x = Component, y = Freq, fill = Component)) +
      ggplot2::geom_bar(stat = "identity", width = 0.6) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(Freq, "\n(", Pct, "%)")),
        vjust = -0.3, size = 3.5
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::labs(
        title = "Dominant Component Distribution per District",
        x     = "Component",
        y     = "Number of Districts"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
  })
  
  output$table_dominant <- DT::renderDT({
    req(rv$dominant_df)
    show_df            <- rv$dominant_df[,
                                         c(input$id_col, input$name_col,
                                           "sovi_score", "vuln_class", "dom_comp"),
                                         drop = FALSE
    ]
    show_df$sovi_score <- round(show_df$sovi_score, 4)
    DT::datatable(show_df,
                  filter  = "top",
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ==========================================================================
  # TAB 2: COMPONENT PROFILE
  # ==========================================================================
  
  output$plot_heatmap <- renderPlot({
    req(rv$profile_df)
    profile <- rv$profile_df
    rc_cols <- grep("^RC", names(profile), value = TRUE)
    
    long <- tidyr::pivot_longer(profile,
                                cols      = dplyr::all_of(rc_cols),
                                names_to  = "Component",
                                values_to = "Mean_Score")
    long$vuln_class <- factor(long$vuln_class, levels = VULN_CLASSES)
    long$Component  <- factor(long$Component,  levels = rc_cols)
    
    ggplot2::ggplot(long,
                    ggplot2::aes(x = Component, y = vuln_class,
                                 fill = Mean_Score)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = round(Mean_Score, 2)),
                         size = 3.5, color = "black") +
      ggplot2::scale_fill_distiller(palette   = "RdYlGn",
                                    direction = -1,
                                    limits    = c(0, 1),
                                    name      = "Mean Score") +
      ggplot2::labs(
        title    = "Component Profile per Vulnerability Class",
        subtitle = "Rata-rata skor RC ternormalisasi (0=rendah, 1=tinggi)",
        x        = "Dimensi RC",
        y        = "Kelas Kerentanan"
      ) +
      ggplot2::theme_minimal(base_size = 11)
  })
  
  output$plot_radar <- renderPlot({
    req(rv$profile_df)
    profile <- rv$profile_df
    rc_cols <- grep("^RC", names(profile), value = TRUE)
    pal_rad <- c("#1a9641", "#a6d96a", "#fee08b", "#fdae61", "#d7191c")
    cls     <- VULN_CLASSES
    
    par(mfrow = c(2, 3), mar = c(1, 1, 2, 1))
    
    for (i in seq_along(cls)) {
      row <- profile[profile$vuln_class == cls[i], rc_cols, drop = FALSE]
      if (nrow(row) == 0) next
      rdf <- as.data.frame(rbind(
        rep(1, length(rc_cols)),
        rep(0, length(rc_cols)),
        as.numeric(row)
      ))
      colnames(rdf) <- rc_cols
      fmsb::radarchart(rdf, axistype = 1,
                       pcol        = pal_rad[i],
                       pfcol       = adjustcolor(pal_rad[i], alpha.f = 0.3),
                       plwd        = 2, cglcol = "grey70", cglty = 1,
                       vlcex       = 0.8, title = cls[i],
                       caxislabels = c("0","0.25","0.5","0.75","1"))
    }
    
    plot.new()
    legend("center", legend = cls, col = pal_rad, lwd = 2,
           bty = "n", title = "Kelas Kerentanan", cex = 0.9)
  })
  
  # ==========================================================================
  # TAB 3: MORAN'S I + LISA
  # ==========================================================================
  
  output$moran_global <- renderPrint({
    req(rv$moran_res)
    mg <- rv$moran_res$moran_global
    cat("=== Global Moran's I ===\n")
    cat("I statistic :", round(mg$estimate["Moran I statistic"], 4), "\n")
    cat("Expectation :", round(mg$estimate["Expectation"],       4), "\n")
    cat("Variance    :", round(mg$estimate["Variance"],          6), "\n")
    cat("p-value     :", format(mg$p.value, scientific = TRUE, digits = 4), "\n")
    cat("Islands (KNN hybrid):", rv$moran_res$n_islands, "\n\n")
    
    moran_i <- mg$estimate["Moran I statistic"]
    if      (mg$p.value < 0.05 && moran_i > 0)
      cat("\u2192 POSITIF SIGNIFIKAN \u2014 kerentanan tinggi mengelompok spasial \u2713")
    else if (mg$p.value < 0.05 && moran_i < 0)
      cat("\u2192 NEGATIF SIGNIFIKAN \u2014 pola dispersal spasial")
    else
      cat("\u2192 TIDAK SIGNIFIKAN \u2014 pola acak spasial")
  })
  
  output$table_lisa <- DT::renderDT({
    req(rv$moran_res)
    DT::datatable(rv$moran_res$lisa_table,
                  options  = list(dom = "t"),
                  rownames = FALSE,
                  caption  = "Klasifikasi LISA")
  })
  
  output$map_lisa <- leaflet::renderLeaflet({
    req(rv$moran_res)
    build_leaflet_lisa(
      peta        = rv$moran_res$peta,
      name_col    = input$name_col,
      lisa_colors = rv$moran_res$lisa_colors
    )
  })
  
  # ==========================================================================
  # TAB 4: SENSITIVITY ANALYSIS
  # ==========================================================================
  
  output$sensitivity_result <- renderPrint({
    req(rv$sensitivity)
    s    <- rv$sensitivity
    corr <- s$corr_mat
    
    cat("=== Sensitivity Analysis \u2014 Loading Threshold ===\n\n")
    
    for (key in names(s$sens_info)) {
      info <- s$sens_info[[key]]
      if (!is.null(info$error)) {
        cat(sprintf("\u03c4 = %.1f : GAGAL\n", info$threshold))
      } else {
        cat(sprintf("\u03c4 = %.1f : %d variabel masuk, %d keluar\n",
                    info$threshold, info$n_assigned, info$n_unassigned))
        if (info$n_unassigned > 0)
          cat("         Keluar:", paste(info$unassigned, collapse = ", "), "\n")
      }
    }
    
    cat("\nSpearman Correlation antar Threshold:\n")
    print(round(corr, 4))
    
    min_r <- min(corr[upper.tri(corr)], na.rm = TRUE)
    cat("\nInterpretasi: ")
    if      (min_r >= 0.90) cat("ROBUST \u2014 hasil konsisten antar threshold \u2713")
    else if (min_r >= 0.70) cat("MODERATELY ROBUST")
    else                    cat("SENSITIVE \u2014 justifikasi threshold diperlukan")
  })
  
  output$plot_sensitivity <- renderPlot({
    req(rv$sensitivity)
    df   <- rv$sensitivity$scores_df
    corr <- rv$sensitivity$corr_mat
    
    ggplot2::ggplot(df) +
      ggplot2::geom_point(
        ggplot2::aes(x = thr_0.5, y = thr_0.6,
                     color = "\u03c4=0.5 vs \u03c4=0.6"),
        alpha = 0.4, size = 0.9
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = thr_0.5, y = thr_0.7,
                     color = "\u03c4=0.5 vs \u03c4=0.7"),
        alpha = 0.4, size = 0.9
      ) +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                           linetype = "dashed", color = "grey60") +
      ggplot2::scale_color_manual(
        values = c("\u03c4=0.5 vs \u03c4=0.6" = "#1f78b4",
                   "\u03c4=0.5 vs \u03c4=0.7" = "#e31a1c")
      ) +
      ggplot2::annotate("text", x = 0.05, y = 0.93,
                        label = paste0(
                          "\u03c1(0.5,0.6) = ", round(corr["thr_0.5","thr_0.6"], 3),
                          "\n\u03c1(0.5,0.7) = ", round(corr["thr_0.5","thr_0.7"], 3)
                        ),
                        size = 3.5, hjust = 0, color = "grey30"
      ) +
      ggplot2::labs(
        title    = "Sensitivity Analysis: SoVI Score by Loading Threshold",
        subtitle = "Titik di diagonal = hasil konsisten",
        x        = "SoVI Score (\u03c4 = 0.5)",
        y        = "SoVI Score (\u03c4 = 0.6 / 0.7)",
        color    = "Perbandingan"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom")
  })
  
  # ==========================================================================
  # TAB 5: CUTTER COMPARISON
  # ==========================================================================
  
  output$cutter_result <- renderPrint({
    req(rv$cutter_df)
    cat("=== Perbandingan dengan Metode Cutter (2003) ===\n\n")
    cat("Spearman \u03c1 =", rv$cutter_df$spearman_r, "\n")
    cat("p-value    =",
        format(rv$cutter_df$spearman_p, scientific = TRUE, digits = 4), "\n\n")
    rho <- rv$cutter_df$spearman_r
    if      (abs(rho) >= 0.90)
      cat("\u2192 HIGHLY CORRELATED \u2014 ranking sangat mirip dengan Cutter original")
    else if (abs(rho) >= 0.70)
      cat("\u2192 MODERATELY CORRELATED \u2014 terdapat perbedaan ranking bermakna")
    else
      cat("\u2192 SUBSTANTIALLY DIFFERENT \u2014 perbedaan signifikan dengan Cutter")
  })
  
  output$plot_cutter <- renderPlot({
    req(rv$cutter_df)
    df  <- rv$cutter_df$comparison_df
    rho <- rv$cutter_df$spearman_r
    
    ggplot2::ggplot(df,
                    ggplot2::aes(x = sovi_cutter, y = sovi_proposed)) +
      ggplot2::geom_point(alpha = 0.4, size = 0.9, color = "#2166ac") +
      ggplot2::geom_abline(linetype = "dashed", color = "grey60") +
      ggplot2::geom_smooth(method = "lm", se = TRUE,
                           color = "#d6604d", linewidth = 0.8) +
      ggplot2::annotate("text", x = 0.05, y = 0.92,
                        label = paste0("Spearman \u03c1 = ", rho),
                        size = 4, hjust = 0, color = "grey30") +
      ggplot2::labs(
        title    = "Metode yang Diusulkan vs. Cutter (2003)",
        subtitle = "Garis putus-putus = kesepakatan sempurna",
        x        = "SoVI Score \u2014 Cutter (2003)",
        y        = "SoVI Score \u2014 Metode Diusulkan"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  output$table_cutter_diff <- DT::renderDT({
    req(rv$cutter_df)
    df <- rv$cutter_df$comparison_df |>
      dplyr::arrange(dplyr::desc(abs(rank_diff))) |>
      head(20)
    df$sovi_proposed <- round(df$sovi_proposed, 4)
    df$sovi_cutter   <- round(df$sovi_cutter,   4)
    DT::datatable(df,
                  options  = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  caption  = "Top 20 Distrik \u2014 Perbedaan Ranking Terbesar")
  })
  
} # end extended_analysis_server()