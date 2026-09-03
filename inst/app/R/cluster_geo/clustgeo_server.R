# =============================================================================
# R/cluster_geo/clustgeo_server.R
# Server Logic Tab ClustGeo Advanced
#
# DIPANGGIL DARI : server.R via clustgeo_server(input, output, session, rv)
# DEPENDENSI     :
#   - R/core/helpers.R      (normalize_id)
#   - R/core/cluster_core.R (run_clustgeo_advanced, build_leaflet_clustgeo_adv)
# =============================================================================

clustgeo_server <- function(input, output, session, rv) {
  
  # ==========================================================================
  # OUTPUT: Var Selector (muncul jika sumber raw/raw_norm/standardized)
  # ==========================================================================
  
  output$cga_var_selector <- renderUI({
    req(rv$data, rv$sovi_vars)
    
    num_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    src      <- if (is.null(input$cga_data_source)) "raw" else input$cga_data_source
    
    # Catatan info sesuai sumber data
    note_text <- switch(src,
                        "raw" = div(
                          class = "progress-box",
                          style = "background:#fff8e1; border-left-color:#f39c12;
                 font-size:11.5px; margin-bottom:6px;",
                          icon("exclamation-triangle"),
                          " Raw values used directly without any transformation."
                        ),
                        "raw_norm" = div(
                          class = "progress-box",
                          style = "background:#fff8e1; border-left-color:#f39c12;
                 font-size:11.5px; margin-bottom:6px;",
                          icon("info-circle"),
                          " Original data will be min-max normalized 0\u20131 before clustering."
                        ),
                        "standardized" = div(
                          class = "progress-box",
                          style = "background:#e3f2fd; border-left-color:#1a73c1;
                 font-size:11.5px; margin-bottom:6px;",
                          icon("info-circle"),
                          " Z-scores derived from SoVI standardization process (mean=0, SD=1)."
                        ),
                        NULL
    )
    
    tagList(
      note_text,
      checkboxGroupInput("cga_selected_vars", NULL,
                         choices  = num_cols,
                         selected = rv$sovi_vars)
    )
  })
  
  # ==========================================================================
  # OUTPUT: Data Source Info (sovi/rc)
  # ==========================================================================
  
  output$cga_datasource_info <- renderUI({
    src <- input$cga_data_source
    if (is.null(src)) return(NULL)
    
    info <- switch(src,
                   "sovi" = "Using single SoVI Score (0\u20131) as clustering feature.",
                   "rc"   = "Using RC component scores (PCA Varimax, normalized 0\u20131).",
                   NULL
    )
    if (is.null(info)) return(NULL)
    
    div(class = "progress-box",
        style = "background:#e3f2fd; border-left-color:#1a73c1;
                 font-size:12px; margin-top:6px;",
        icon("info-circle"), " ", info)
  })
  
  # ==========================================================================
  # OUTPUT: Progress box (initially empty)
  # ==========================================================================
  
  output$cga_progress <- renderUI({ NULL })
  
  # ==========================================================================
  # OBSERVER: Run ClustGeo Button
  # ==========================================================================
  
  observeEvent(input$run_clustgeo_adv, {
    req(rv$sovi_result, rv$sovi_ok, rv$shp)
    
    # Show progress spinner
    output$cga_progress <- renderUI({
      div(class = "progress-box", style = "background:#cce5ff;",
          icon("spinner", class = "fa-spin"),
          " Running ClustGeo Advanced...")
    })
    
    # Get selected variables (only for raw/standardized sources)
    sel_vars <- if (input$cga_data_source %in% c("raw", "raw_norm", "standardized"))
      input$cga_selected_vars else NULL
    
    withProgress(message = "ClustGeo Advanced...", value = 0, {
      
      incProgress(0.2, detail = "Building distance matrix...")
      
      result <- tryCatch({
        run_clustgeo_advanced(
          data_source   = input$cga_data_source,
          raw_data      = rv$data,
          sovi_result   = rv$sovi_result,
          shp           = rv$shp,
          selected_vars = sel_vars,
          k_mode        = input$cga_k_mode,
          k             = input$cga_k,
          k_max         = input$cga_k_max,
          alpha_mode    = input$cga_alpha_mode,
          alpha         = input$cga_alpha,
          id_col        = input$id_col,
          name_col      = input$name_col
        )
      }, error = function(e) {
        showNotification(paste("Error ClustGeo:", e$message),
                         type = "error", duration = 12)
        NULL
      })
      
      incProgress(0.8, detail = "Done.")
      rv$cga_result <- result
    })
    
    # Update progress box according to result
    output$cga_progress <- renderUI({
      if (!is.null(rv$cga_result))
        div(class = "progress-box status-ok",
            icon("check"),
            paste0(" Complete! k=", rv$cga_result$k,
                   ", \u03b1=", round(rv$cga_result$alpha, 3),
                   ", Silhouette=", rv$cga_result$sil_mean))
      else
        div(class = "progress-box status-err",
            icon("times"), " Failed. Check data & configuration.")
    })
  })
  
  # ==========================================================================
  # TAB 1: PARAMETER SUMMARY
  # ==========================================================================
  
  output$cga_summary_params <- renderUI({
    res <- rv$cga_result
    
    # Not yet run
    if (is.null(res)) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12;",
                 icon("exclamation-triangle"),
                 " Run ClustGeo first using the button in the left panel."))
    }
    
    # Data source label
    src_label <- switch(res$data_source,
                        "raw"          = "Original Data (no transformation)",
                        "raw_norm"     = "Normalized Data (min-max 0-1)",
                        "standardized" = "Standardized Data (Z-score)",
                        "sovi"         = "SoVI Score",
                        "rc"           = "RC Scores (PCA Components)",
                        res$data_source
    )
    
    k_label     <- if (res$k_mode == "auto")
      paste0(res$k, " (automatic)") else as.character(res$k)
    
    alpha_label <- if (res$alpha_mode == "auto")
      paste0(round(res$alpha, 3), " (automatic)")
    else
      as.character(round(res$alpha, 3))
    
    # Silhouette color based on quality
    sil_color <- if (res$sil_mean >= 0.5) "color:#27ae60;font-weight:700;"
    else if (res$sil_mean >= 0.25) "color:#f39c12;font-weight:700;"
    else "color:#e74c3c;font-weight:700;"
    
    div(
      fluidRow(
        # Card: Data Source
        column(4, div(class = "info-card",
                      tags$h4(icon("database"), " Data Source"),
                      tags$p(style = "font-size:13px;", src_label),
                      tags$p(style = "font-size:12px; color:#78909c;",
                             "Features: ", length(res$feat_cols), " dimensions")
        )),
        
        # Card: Clustering Results
        column(4, div(class = "info-card",
                      tags$h4(icon("object-group"), " Clustering Results"),
                      tags$p(style = "font-size:13px;",
                             tags$strong("k = "), k_label),
                      tags$p(style = "font-size:13px;",
                             tags$strong("\u03b1 = "), alpha_label),
                      tags$p(style = "font-size:13px;",
                             tags$strong("Mean Silhouette = "),
                             tags$span(style = sil_color, res$sil_mean))
        )),
        
        # Card: Cluster Distribution
        column(4, div(class = "info-card",
                      tags$h4(icon("chart-bar"), " Cluster Distribution"),
                      tags$table(
                        style = "width:100%; font-size:12.5px;",
                        tags$thead(tags$tr(
                          tags$th("Cluster"), tags$th("n"), tags$th("Sil. Width")
                        )),
                        tags$tbody(
                          lapply(seq_len(nrow(res$sil_df)), function(i) {
                            tags$tr(
                              tags$td(paste0("Cluster ", res$sil_df$cluster[i])),
                              tags$td(as.character(res$profile$n[i])),
                              tags$td(round(res$sil_df$avg_sil_width[i], 3))
                            )
                          })
                        )
                      )
        ))
      ),
      
      # Card: Silhouette Interpretation
      fluidRow(column(12,
                      div(class = "info-card",
                          style = "border-left-color:#27ae60; margin-top:4px;",
                          tags$h4(icon("lightbulb"), " Silhouette Interpretation"),
                          tags$ul(style = "font-size:12.5px; margin:0;",
                                  tags$li(tags$span(style="color:#27ae60;font-weight:600;", "\u2265 0.50"),
                                          " \u2014 Strong cluster structure"),
                                  tags$li(tags$span(style="color:#f39c12;font-weight:600;", "0.25\u20130.49"),
                                          " \u2014 Moderate cluster structure"),
                                  tags$li(tags$span(style="color:#e74c3c;font-weight:600;", "< 0.25"),
                                          " \u2014 Weak cluster structure, consider different k")
                          )
                      )
      ))
    )
  })
  
  # ==========================================================================
  # TAB 2: INTERACTIVE MAP
  # ==========================================================================
  
  output$cga_map <- leaflet::renderLeaflet({
    req(rv$cga_result, rv$shp)
    res <- rv$cga_result
    build_leaflet_clustgeo_adv(
      result_df = res$result_df,
      shp       = rv$shp,
      join_shp  = input$join_shp,
      join_df   = res$id_col,
      name_col  = res$name_col,
      k         = res$k
    )
  })
  
  # ==========================================================================
  # TAB 3: SILHOUETTE
  # ==========================================================================
  
  output$cga_plot_silhouette <- renderPlot({
    req(rv$cga_result)
    res   <- rv$cga_result
    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set2")[seq_len(k)]
    plot(res$sil_obj,
         main = paste0("Silhouette \u2014 k=", k,
                       ", \u03b1=", round(res$alpha, 3),
                       "\nMean = ", res$sil_mean),
         col  = pal_c)
  })
  
  output$cga_table_silhouette <- DT::renderDT({
    req(rv$cga_result)
    df                <- rv$cga_result$sil_df
    df$avg_sil_width  <- round(df$avg_sil_width, 4)
    DT::datatable(df, options = list(dom = "t"), rownames = FALSE,
                  caption = "Avg. Silhouette Width per Cluster")
  })
  
  output$cga_silhouette_interp <- renderUI({
    req(rv$cga_result)
    s     <- rv$cga_result$sil_mean
    cls   <- if (s >= 0.50) "status-ok" else if (s >= 0.25) "status-warn" else "status-err"
    label <- if (s >= 0.50) "Strong Structure \u2713"
    else if (s >= 0.25) "Moderate Structure"
    else "Weak Structure \u2717"
    div(class = paste("progress-box", cls),
        icon("tachometer-alt"),
        paste0(" Mean Silhouette = ", s, " \u2014 ", label))
  })
  
  output$cga_plot_kopt <- renderPlot({
    req(rv$cga_result)
    k_info <- rv$cga_result$k_info
    if (is.null(k_info)) return(NULL)
    
    df <- k_info$sil_df
    ggplot2::ggplot(df, ggplot2::aes(x = k, y = mean_silhouette)) +
      ggplot2::geom_line(color = "#1a73c1", linewidth = 1.1) +
      ggplot2::geom_point(color = "#1a73c1", size = 3) +
      ggplot2::geom_point(
        data  = df[df$k == rv$cga_result$k, ],
        ggplot2::aes(x = k, y = mean_silhouette),
        color = "#e74c3c", size = 5, shape = 18
      ) +
      ggplot2::annotate("text",
                        x     = rv$cga_result$k,
                        y     = df$mean_silhouette[df$k == rv$cga_result$k],
                        label = paste0("k=", rv$cga_result$k, " (optimal)"),
                        vjust = -1, color = "#e74c3c", fontface = "bold", size = 3.5
      ) +
      ggplot2::scale_x_continuous(breaks = df$k) +
      ggplot2::labs(
        title = "Optimal k Search via Mean Silhouette",
        x     = "Number of Clusters (k)",
        y     = "Mean Silhouette Width"
      ) +
      ggplot2::theme_minimal(base_size = 11)
  })
  
  # ==========================================================================
  # TAB 4: ALPHA TRADE-OFF
  # ==========================================================================
  
  output$cga_alpha_info <- renderUI({
    req(rv$cga_result)
    alpha_info <- rv$cga_result$alpha_info
    
    if (is.null(alpha_info)) {
      return(div(class = "progress-box",
                 style = "background:#f8f9fa; border-left-color:#adb5bd;
                          font-size:12.5px;",
                 icon("info-circle"),
                 " Mode alpha: Manual \u2014 \u03b1 = ", input$cga_alpha,
                 ". Enable Automatic mode to view Q1/Q2 trade-off."))
    }
    
    div(class = "progress-box status-ok",
        icon("check"),
        paste0(" Alpha optimal = ", alpha_info$alpha_opt,
               " (nearest Q1/Q2 trade-off point)"))
  })
  
  output$cga_plot_alpha <- renderPlot({
    req(rv$cga_result)
    alpha_info <- rv$cga_result$alpha_info
    
    if (is.null(alpha_info)) {
      plot.new()
      text(0.5, 0.5,
           "Enable Automatic Alpha mode\nto view Q1/Q2 trade-off chart.",
           cex = 1.1, col = "grey50")
      return()
    }
    
    q_df <- alpha_info$q_df
    long <- tidyr::pivot_longer(
      q_df[, c("alpha", "Q1_attribute", "Q2_spatial")],
      cols      = c("Q1_attribute", "Q2_spatial"),
      names_to  = "Criteria",
      values_to = "Q"
    )
    
    ggplot2::ggplot(long, ggplot2::aes(x = alpha, y = Q,
                                       color = Criteria,
                                       linetype = Criteria)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::geom_vline(xintercept = alpha_info$alpha_opt,
                          linetype = "dashed",
                          color = "#e74c3c", linewidth = 0.8) +
      ggplot2::annotate("text",
                        x     = alpha_info$alpha_opt,
                        y     = max(long$Q, na.rm = TRUE) * 0.98,
                        label = paste0("\u03b1*=", alpha_info$alpha_opt),
                        hjust = -0.1, color = "#e74c3c", fontface = "bold", size = 3.5
      ) +
      ggplot2::scale_color_manual(
        values = c("Q1_attribute" = "#1a73c1", "Q2_spatial" = "#27ae60")
      ) +
      ggplot2::labs(
        title    = "Attribute Homogeneity (Q1) vs Spatial (Q2) Trade-off",
        subtitle = "Optimal alpha selected at minimum |Q1-Q2| point",
        x        = "Alpha (\u03b1)",
        y        = "Q (Normalized Within-Cluster Inertia)",
        color    = "Criteria",
        linetype = "Criteria"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")
  })
  
  # ==========================================================================
  # TAB 5: CLUSTER PROFILE
  # ==========================================================================
  
  output$cga_table_profile <- DT::renderDT({
    req(rv$cga_result)
    df       <- rv$cga_result$profile
    num_cols <- setdiff(names(df), "cluster")
    df[, num_cols] <- round(df[, num_cols, drop = FALSE], 3)
    DT::datatable(df,
                  options  = list(dom = "t", scrollX = TRUE),
                  rownames = FALSE,
                  caption  = "Cluster Profile: Mean Feature per Cluster")
  })
  
  output$cga_plot_heatmap <- renderPlot({
    req(rv$cga_result)
    profile   <- rv$cga_result$profile
    feat_cols <- rv$cga_result$feat_cols
    
    long <- tidyr::pivot_longer(profile,
                                cols      = dplyr::all_of(feat_cols),
                                names_to  = "Feature",
                                values_to = "Mean_Score")
    long$cluster <- factor(long$cluster)
    
    ggplot2::ggplot(long, ggplot2::aes(x = Feature, y = cluster,
                                       fill = Mean_Score)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.6) +
      ggplot2::geom_text(ggplot2::aes(label = round(Mean_Score, 2)),
                         size = 3.2, color = "black") +
      ggplot2::scale_fill_distiller(palette   = "RdYlBu",
                                    direction = -1,
                                    name      = "Mean\nScore") +
      ggplot2::labs(
        title    = "Cluster Profile Heatmap",
        subtitle = "Mean feature values per cluster",
        x        = "Feature / Dimension",
        y        = "Cluster"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9)
      )
  })
  
  output$cga_plot_radar <- renderPlot({
    req(rv$cga_result)
    profile   <- rv$cga_result$profile
    feat_cols <- rv$cga_result$feat_cols
    k         <- rv$cga_result$k
    
    if (length(feat_cols) < 3) {
      plot.new()
      text(0.5, 0.5, "Radar chart requires at least 3 features/dimensions.",
           cex = 1.1, col = "grey50")
      return()
    }
    
    pal_rad        <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set2")[seq_len(k)]
    cluster_labels <- paste0("Cluster ", seq_len(k))
    
    # Normalize profile to [0, 1] for radar
    feat_mat  <- as.matrix(profile[, feat_cols, drop = FALSE])
    row_mins  <- apply(feat_mat, 2, min)
    row_maxs  <- apply(feat_mat, 2, max)
    feat_norm <- sweep(feat_mat, 2, row_mins, "-")
    denom     <- pmax(row_maxs - row_mins, 1e-9)
    feat_norm <- sweep(feat_norm, 2, denom, "/")
    
    n_col_plot <- min(k, 3)
    n_row_plot <- ceiling(k / n_col_plot)
    par(mfrow = c(n_row_plot, n_col_plot + 1),
        mar   = c(1, 1, 2.5, 1),
        oma   = c(0, 0, 2, 0))
    
    for (i in seq_len(k)) {
      row_data <- as.numeric(feat_norm[i, ])
      rdf      <- as.data.frame(rbind(
        rep(1, length(feat_cols)),
        rep(0, length(feat_cols)),
        row_data
      ))
      colnames(rdf) <- feat_cols
      fmsb::radarchart(
        rdf, axistype = 1,
        pcol        = pal_rad[i],
        pfcol       = adjustcolor(pal_rad[i], alpha.f = 0.30),
        plwd        = 2.2, cglcol = "grey70", cglty = 1,
        vlcex       = 0.75, title = cluster_labels[i],
        caxislabels = c("0", "0.25", "0.5", "0.75", "1"),
        calcex      = 0.6
      )
      mtext(paste0("n = ", profile$n[i]), side = 1, line = 0.2, cex = 0.7)
    }
    
    plot.new()
    legend("center", legend = cluster_labels, col = pal_rad,
           lwd = 3, bty = "n", title = "Cluster", cex = 0.85)
    mtext("Radar Profile per Cluster (normalized values 0\u20131)",
          outer = TRUE, line = 0.5, cex = 1.0, font = 2)
  })
  
  # ==========================================================================
  # TAB 6: CLUSTER DATA
  # ==========================================================================
  
  output$cga_table_result <- DT::renderDT({
    req(rv$cga_result)
    res      <- rv$cga_result
    df       <- res$result_df
    id_col   <- res$id_col
    name_col <- res$name_col
    
    show_cols <- unique(c(
      id_col, name_col, "cluster",
      if ("sovi_score" %in% names(df)) "sovi_score",
      if ("vuln_class" %in% names(df)) "vuln_class",
      res$feat_cols
    ))
    show_cols <- show_cols[show_cols %in% names(df)]
    show_df   <- df[, show_cols, drop = FALSE]
    
    num_cols <- show_cols[sapply(show_df, is.numeric)]
    show_df[, num_cols] <- round(show_df[, num_cols, drop = FALSE], 4)
    
    DT::datatable(show_df,
                  filter   = "top",
                  options  = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE,
                  caption  = paste0("ClustGeo Clustering Results \u2014 k=",
                                    res$k, ", \u03b1=", round(res$alpha, 3)))
  })
  
  # ==========================================================================
  # DOWNLOADS
  # ==========================================================================
  
  output$dl_cga_csv <- downloadHandler(
    filename = function() paste0("clustgeo_adv_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$cga_result)
      res      <- rv$cga_result
      df       <- res$result_df
      id_col   <- res$id_col
      name_col <- res$name_col
      
      keep_cols <- unique(c(
        id_col, name_col, "cluster",
        if ("sovi_score" %in% names(df)) "sovi_score",
        if ("vuln_class" %in% names(df)) "vuln_class",
        res$feat_cols
      ))
      keep_cols <- keep_cols[keep_cols %in% names(df)]
      write.csv(df[, keep_cols], file, row.names = FALSE)
    }
  )
  
  output$dl_cga_map_png <- downloadHandler(
    filename = function() paste0("map_clustgeo_adv_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$cga_result, rv$shp)
      res       <- rv$cga_result
      result_df <- res$result_df
      shp       <- rv$shp
      k         <- res$k
      
      result_df[[res$id_col]] <- normalize_id(result_df[[res$id_col]])
      shp[[input$join_shp]]   <- normalize_id(shp[[input$join_shp]])
      
      peta         <- dplyr::left_join(shp, result_df,
                                       by = setNames(res$id_col, input$join_shp))
      peta$cluster <- as.factor(peta$cluster)
      pal_c        <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set2")[seq_len(k)]
      
      tmap::tmap_mode("plot")
      m <- tmap::tm_shape(peta) +
        tmap::tm_polygons(
          fill        = "cluster",
          fill.scale  = tmap::tm_scale_categorical(values = pal_c),
          fill.legend = tmap::tm_legend(title = "Cluster"),
          col         = "grey40", lwd = 0.3
        ) +
        tmap::tm_title(paste0("ClustGeo Advanced \u2014 k=", k,
                              ", \u03b1=", round(res$alpha, 2))) +
        tmap::tm_compass(type = "arrow",
                         position = c("left", "bottom"), size = 1.5) +
        tmap::tm_scalebar(position = c("left", "bottom"), text.size = 0.6) +
        tmap::tm_layout(legend.outside          = TRUE,
                        legend.outside.position = "right")
      
      tmap::tmap_save(m, filename = file, width = 3000, height = 2400, dpi = 300)
      tmap::tmap_mode("view")
    }
  )
  
} # end clustgeo_server()