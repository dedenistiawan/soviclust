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
  # HELPER: Siapkan data input untuk DBSCAN
  # ==========================================================================
  dbs_input_data <- reactive({
    req(rv$sovi_result)
    sovi_df <- rv$sovi_result$sovi_df
    rc_cols  <- grep("^RC", names(sovi_df), value = TRUE)

    X <- switch(input$dbs_input,
      "sovi"    = sovi_df[, "sovi_score", drop = FALSE],
      "rc"      = if (length(rc_cols) > 0) sovi_df[, rc_cols, drop = FALSE]
                  else sovi_df[, "sovi_score", drop = FALSE],
      "sovi_rc" = if (length(rc_cols) > 0)
                    sovi_df[, c("sovi_score", rc_cols), drop = FALSE]
                  else sovi_df[, "sovi_score", drop = FALSE]
    )

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
        subtitle = "Titik siku (knee) menunjukkan nilai eps yang optimal",
        x        = "Observasi (diurutkan)",
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
          " Menjalankan DBSCAN (eps=", input$dbs_eps,
          ", minPts=", input$dbs_minpts, ")...")
    })

    X <- dbs_input_data()

    withProgress(message = "DBSCAN Clustering...", value = 0, {

      incProgress(0.3, detail = "Menjalankan DBSCAN...")
      dbs_result <- tryCatch(
        dbscan::dbscan(X, eps = input$dbs_eps, minPts = input$dbs_minpts),
        error = function(e) {
          showNotification(paste("Error DBSCAN:", e$message),
                           type = "error", duration = 10)
          NULL
        }
      )

      req(dbs_result)

      incProgress(0.7, detail = "Merangkum hasil...")
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
            " DBSCAN selesai! Klaster: ", rv$dbs_result$n_cluster,
            " | Noise: ", rv$dbs_result$n_noise, " wilayah")
      } else {
        div(class = "progress-box status-err",
            icon("times"), " Gagal. Coba sesuaikan eps atau minPts.")
      }
    })
  })

  # ==========================================================================
  # OUTPUT: Ringkasan
  # ==========================================================================
  output$dbs_summary_table <- DT::renderDT({
    req(rv$dbs_result)
    sovi_df <- rv$dbs_result$sovi_df
    tbl <- as.data.frame(table(Klaster = ifelse(
      sovi_df$dbs_cluster == 0, "Noise (0)",
      paste0("Klaster ", sovi_df$dbs_cluster)
    )))
    tbl$Persen <- round(tbl$Freq / nrow(sovi_df) * 100, 1)
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
        tags$p(icon("object-group"), tags$strong(" Jumlah Klaster: "),
               res$n_cluster, " (tidak termasuk noise)"),
        tags$p(icon("times-circle"), tags$strong(" Noise Points: "),
               res$n_noise, " wilayah",
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
                                  paste0("Klaster ", show_df$dbs_cluster))
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
    sovi_df$Klaster <- ifelse(sovi_df$dbs_cluster == 0, "Noise",
                              paste0("Klaster ", sovi_df$dbs_cluster))
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
      ggplot2::labs(title = "Distribusi SoVI Score per Klaster DBSCAN",
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

    sovi_df$Klaster <- ifelse(sovi_df$dbs_cluster == 0, "Noise",
                              paste0("Klaster ", sovi_df$dbs_cluster))

    ggplot2::ggplot(sovi_df,
                    ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]],
                                 color = Klaster,
                                 shape = ifelse(Klaster == "Noise", 4, 16))) +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::scale_color_brewer(palette = "Set2") +
      ggplot2::scale_shape_identity() +
      ggplot2::labs(
        title = "Scatter Plot — Klaster DBSCAN",
        x     = x_var,
        y     = y_var,
        color = "Klaster"
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
                                 paste0("Klaster ", peta$dbs_cluster))
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
      "Klaster: <b>", peta$klaster_label, "</b><br>",
      "SoVI Score: ", round(peta$sovi_score, 4), "<br>",
      "Kelas: ", peta$vuln_class
    )

    leaflet::leaflet(peta) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        fillColor   = ~pal(klaster_label),
        fillOpacity = 0.75,
        color       = "#fff",
        weight      = 0.5,
        popup       = popup,
        label       = ~paste0(klaster_label, ": ", nm)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ~klaster_label,
        title    = paste0("DBSCAN (\u03b5=", rv$dbs_result$eps, ")")
      )
  })

} # end dbscan_server()
