# =============================================================================
# R/dbscan/dbscan_server.R
# Server Logic Tab DBSCAN Clustering
#
# DIPANGGIL DARI : server.R via dbscan_server(input, output, session, rv)
# DEPENDENSI     :
#   - rv$sovi_result (SoVI score + RC columns)
#   - rv$shp, input$id_col, input$join_shp
#   - Package: dbscan, ggplot2, leaflet, dplyr
# =============================================================================

dbscan_server <- function(input, output, session, rv) {

  output$dbs_progress <- renderUI({ NULL })

  # ==========================================================================
  # OUTPUT: Variable selector (untuk raw/raw_norm/standardized)
  # ==========================================================================
  output$dbs_var_selector <- renderUI({
    req(rv$data, rv$sovi_vars)
    src      <- input$dbs_data_source %||% "rc"
    num_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    note <- switch(src,
      "raw"          = div(class = "progress-box",
                           style = "background:#fff8e1;border-left-color:#f39c12;font-size:11px;",
                           icon("exclamation-triangle"), " Raw values without transformation."),
      "raw_norm"     = div(class = "progress-box",
                           style = "background:#fff8e1;border-left-color:#f39c12;font-size:11px;",
                           icon("info-circle"), " Will be min-max normalized 0\u20131."),
      "standardized" = div(class = "progress-box",
                           style = "background:#e3f2fd;border-left-color:#1a73c1;font-size:11px;",
                           icon("info-circle"), " Z-scores from SoVI process."),
      NULL
    )
    tagList(note, checkboxGroupInput("dbs_selected_vars", NULL,
                                     choices  = num_cols,
                                     selected = rv$sovi_vars))
  })

  # ==========================================================================
  # OUTPUT: Info sumber data (sovi/rc)
  # ==========================================================================
  output$dbs_datasource_info <- renderUI({
    src <- input$dbs_data_source
    if (is.null(src)) return(NULL)
    info <- switch(src,
      "sovi" = "Using single SoVI Score (0\u20131) as clustering feature.",
      "rc"   = "Using RC component scores (PCA Varimax, normalized 0\u20131).",
      NULL
    )
    if (is.null(info)) return(NULL)
    div(class = "progress-box",
        style = "background:#e3f2fd;border-left-color:#1a73c1;font-size:12px;",
        icon("info-circle"), " ", info)
  })

  # ==========================================================================
  # HELPER: Siapkan data input untuk DBSCAN (pakai build_fgwc_feature_matrix)
  # ==========================================================================
  dbs_input_data <- reactive({
    src <- input$dbs_data_source %||% "rc"

    if (src %in% c("sovi", "rc", "standardized")) req(rv$sovi_result)
    else                                           req(rv$data)

    sel_vars <- if (src %in% c("raw", "raw_norm", "standardized"))
      input$dbs_selected_vars else NULL

    X <- tryCatch(
      build_fgwc_feature_matrix(
        data_source   = src,
        raw_data      = rv$data,
        sovi_result   = rv$sovi_result,
        selected_vars = sel_vars
      ),
      error = function(e) {
        showNotification(paste("Error data:", e$message), type = "error")
        NULL
      }
    )
    req(X)

    # Standardisasi tambahan opsional (Z-score)
    if (isTRUE(input$dbs_scale)) {
      X[] <- lapply(X, function(v) as.numeric(scale(v)))
    }
    as.matrix(X)
  })

  # ==========================================================================
  # OUTPUT: k-NN Distance Plot (interaktif, muncul tanpa perlu run)
  # ==========================================================================
  output$dbs_knn_plot <- renderPlot({
    req(rv$sovi_result)
    X <- dbs_input_data()
    k <- min(input$dbs_knn_k, nrow(X) - 1)

    knn_dists <- dbscan::kNNdist(X, k = k)
    sorted_d  <- sort(knn_dists[, k], decreasing = FALSE)

    df <- data.frame(
      Rank     = seq_along(sorted_d),
      Distance = sorted_d
    )

    ggplot2::ggplot(df, ggplot2::aes(x = Rank, y = Distance)) +
      ggplot2::geom_line(color = "#e67e22", linewidth = 1) +
      ggplot2::geom_hline(yintercept = input$dbs_eps,
                          linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
      ggplot2::annotate("text",
                        x     = nrow(df) * 0.05,
                        y     = input$dbs_eps * 1.05,
                        label = paste0("\u03b5 = ", input$dbs_eps),
                        color = "#e74c3c", hjust = 0, size = 4) +
      ggplot2::labs(
        title    = paste0("k-NN Distance Plot (k = ", k, ")"),
        subtitle = "The knee/elbow point indicates the optimal eps value",
        x        = "Observations (sorted)",
        y        = paste0(k, "-NN Distance")
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", color = "#e67e22"),
        plot.subtitle = ggplot2::element_text(color = "#78909c")
      )
  })

  # ==========================================================================
  # OBSERVER: Tombol Run DBSCAN
  # ==========================================================================
  observeEvent(input$run_dbscan, {
    req(rv$sovi_result)

    output$dbs_progress <- renderUI({
      div(class = "progress-box", style = "background:#fff3cd;",
          icon("spinner", class = "fa-spin"),
          " Running DBSCAN (eps=", input$dbs_eps,
          ", minPts=", input$dbs_minpts, ")...")
    })

    X <- dbs_input_data()

    withProgress(message = "DBSCAN Clustering...", value = 0, {

      incProgress(0.3, detail = "Running DBSCAN...")
      dbs_result <- tryCatch(
        dbscan::dbscan(X, eps = input$dbs_eps, minPts = input$dbs_minpts),
        error = function(e) {
          showNotification(paste("Error DBSCAN:", e$message),
                           type = "error", duration = 10)
          NULL
        }
      )

      req(dbs_result)

      incProgress(0.7, detail = "Summarizing results...")
      sovi_df             <- rv$sovi_result$sovi_df
      sovi_df$dbs_cluster <- dbs_result$cluster  # 0 = noise

      n_cluster <- length(unique(dbs_result$cluster[dbs_result$cluster > 0]))
      n_noise   <- sum(dbs_result$cluster == 0)

      rv$dbs_result <- list(
        dbs       = dbs_result,
        n_cluster = n_cluster,
        n_noise   = n_noise,
        sovi_df   = sovi_df,
        eps       = input$dbs_eps,
        minpts    = input$dbs_minpts,
        X         = X
      )

      incProgress(1.0)
    })

    output$dbs_progress <- renderUI({
      if (!is.null(rv$dbs_result)) {
        div(class = "progress-box status-ok",
            icon("check"),
            " DBSCAN complete! Clusters: ", rv$dbs_result$n_cluster,
            " | Noise: ", rv$dbs_result$n_noise, " regions")
      } else {
        div(class = "progress-box status-err",
            icon("times"), " Failed. Try adjusting eps or minPts.")
      }
    })
  })

  # ==========================================================================
  # OUTPUT: Ringkasan
  # ==========================================================================
  output$dbs_summary_table <- DT::renderDT({
    req(rv$dbs_result)
    sovi_df <- rv$dbs_result$sovi_df
    tbl <- as.data.frame(table(Cluster = ifelse(
      sovi_df$dbs_cluster == 0, "Noise (0)",
      paste0("Cluster ", sovi_df$dbs_cluster)
    )))
    tbl$Percent <- round(tbl$Freq / nrow(sovi_df) * 100, 1)
    DT::datatable(tbl,
                  options  = list(dom = "t"),
                  rownames = FALSE)
  })

  output$dbs_params_info <- renderUI({
    req(rv$dbs_result)
    res <- rv$dbs_result
    div(class = "info-card", style = "padding:10px 14px;",
        tags$p(icon("ruler"), tags$strong(" Epsilon (\u03b5): "), res$eps),
        tags$p(icon("users"), tags$strong(" MinPts: "),           res$minpts),
        tags$p(icon("object-group"), tags$strong(" Number of Clusters: "),
               res$n_cluster, " (excluding noise)"),
        tags$p(icon("times-circle"), tags$strong(" Noise Points: "),
               res$n_noise, " regions",
               tags$span(style = "color:#78909c; font-size:12px;",
                         " (", round(res$n_noise / nrow(res$sovi_df) * 100, 1), "%)"))
    )
  })

  output$dbs_detail_table <- DT::renderDT({
    req(rv$dbs_result)
    sovi_df  <- rv$dbs_result$sovi_df
    id_col   <- isolate(input$id_col)
    name_col <- isolate(input$name_col)
    sel_cols <- c(id_col, name_col, "sovi_score", "vuln_class", "dbs_cluster")
    sel_cols <- sel_cols[sel_cols %in% names(sovi_df)]
    show_df  <- sovi_df[, sel_cols, drop = FALSE]
    show_df$dbs_cluster <- ifelse(show_df$dbs_cluster == 0, "Noise",
                                  paste0("Cluster ", show_df$dbs_cluster))
    DT::datatable(show_df,
                  filter   = "top",
                  options  = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE)
  })

  # ==========================================================================
  # OUTPUT: Profil Klaster
  # ==========================================================================
  output$dbs_boxplot <- renderPlot({
    req(rv$dbs_result)
    sovi_df <- rv$dbs_result$sovi_df
    sovi_df$Cluster <- ifelse(sovi_df$dbs_cluster == 0, "Noise",
                              paste0("Cluster ", sovi_df$dbs_cluster))
    n_cls <- length(unique(sovi_df$Klaster))
    pal   <- if (n_cls <= 8) {
      RColorBrewer::brewer.pal(max(3, n_cls), "Set2")[seq_len(n_cls)]
    } else {
      grDevices::rainbow(n_cls)
    }

    ggplot2::ggplot(sovi_df,
                    ggplot2::aes(x = Klaster, y = sovi_score, fill = Klaster)) +
      ggplot2::geom_boxplot(alpha = 0.75, outlier.shape = 21) +
      ggplot2::scale_fill_manual(values = pal) +
      ggplot2::labs(title = "SoVI Score Distribution per DBSCAN Cluster",
                    x = NULL, y = "SoVI Score") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(face = "bold",
                                                        color = "#e67e22"))
  })

  output$dbs_scatter <- renderPlot({
    req(rv$dbs_result)
    sovi_df  <- rv$dbs_result$sovi_df
    rc_cols  <- grep("^RC", names(sovi_df), value = TRUE)
    x_var    <- if (length(rc_cols) > 0) rc_cols[1] else "sovi_score"
    y_var    <- if (length(rc_cols) > 1) rc_cols[2] else "sovi_score"

    sovi_df$Cluster <- ifelse(sovi_df$dbs_cluster == 0, "Noise",
                              paste0("Cluster ", sovi_df$dbs_cluster))

    ggplot2::ggplot(sovi_df,
                    ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]],
                                 color = Klaster,
                                 shape = ifelse(Cluster == "Noise", 4, 16))) +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::scale_color_brewer(palette = "Set2") +
      ggplot2::scale_shape_identity() +
      ggplot2::labs(
        title = "Scatter Plot — DBSCAN Clusters",
        x     = x_var,
        y     = y_var,
        color = "Cluster"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                        color = "#e67e22"))
  })

  # ==========================================================================
  # OUTPUT: Peta DBSCAN
  # ==========================================================================
  output$dbs_map <- leaflet::renderLeaflet({
    req(rv$dbs_result, rv$shp)
    sovi_df  <- rv$dbs_result$sovi_df
    id_col   <- isolate(input$id_col)
    join_shp <- isolate(input$join_shp)
    name_col <- isolate(input$name_col)

    sovi_df[[id_col]] <- normalize_id(sovi_df[[id_col]])
    shp               <- rv$shp
    shp[[join_shp]]   <- normalize_id(shp[[join_shp]])

    peta <- dplyr::left_join(shp, sovi_df,
                             by = setNames(id_col, join_shp))
    peta$klaster_label <- ifelse(peta$dbs_cluster == 0, "Noise",
                                 paste0("Cluster ", peta$dbs_cluster))
    peta$klaster_label <- as.factor(peta$klaster_label)

    n_cls  <- length(levels(peta$klaster_label))
    # Noise selalu abu-abu
    lvls   <- levels(peta$klaster_label)
    n_real <- sum(lvls != "Noise")
    pal_colors <- c(
      if (n_real > 0) RColorBrewer::brewer.pal(max(3, n_real), "Set2")[seq_len(n_real)],
      if ("Noise" %in% lvls) "#aaaaaa"
    )
    names(pal_colors) <- c(lvls[lvls != "Noise"],
                           if ("Noise" %in% lvls) "Noise")
    pal_colors <- pal_colors[lvls]

    pal <- leaflet::colorFactor(pal_colors, domain = lvls)

    nm <- if (name_col %in% names(peta)) peta[[name_col]] else ""
    popup <- paste0(
      "<b>", nm, "</b><br>",
      "Cluster: <b>", peta$klaster_label, "</b><br>",
      "SoVI Score: ", round(peta$sovi_score, 4), "<br>",
      "Kelas: ", peta$vuln_class
    )

    leaflet::leaflet(peta) |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
      leaflet::addPolygons(
        fillColor   = ~pal(klaster_label),
        fillOpacity = 0.75,
        color       = "#fff",
        weight      = 0.5,
        popup       = popup,
        label       = ~paste0(cluster_label, ": ", nm)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ~klaster_label,
        title    = paste0("DBSCAN (\u03b5=", rv$dbs_result$eps, ")")
      )
  })

} # end dbscan_server()
