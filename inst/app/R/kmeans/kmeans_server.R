# =============================================================================
# R/kmeans/kmeans_server.R
# Server Logic Tab K-Means Clustering
#
# DIPANGGIL DARI : server.R via kmeans_server(input, output, session, rv)
# DEPENDENSI     :
#   - rv$sovi_result (SoVI score + RC columns)
#   - rv$shp, input$id_col, input$join_shp
#   - Package: cluster (silhouette), ggplot2, leaflet
# =============================================================================

kmeans_server <- function(input, output, session, rv) {

  output$km_progress <- renderUI({ NULL })

  # ==========================================================================
  # OUTPUT: Variable selector (untuk raw/raw_norm/standardized)
  # ==========================================================================
  output$km_var_selector <- renderUI({
    req(rv$data, rv$sovi_vars)
    src      <- input$km_data_source %||% "rc"
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
    tagList(note, checkboxGroupInput("km_selected_vars", NULL,
                                     choices  = num_cols,
                                     selected = rv$sovi_vars))
  })

  # ==========================================================================
  # OUTPUT: Info sumber data (sovi/rc)
  # ==========================================================================
  output$km_datasource_info <- renderUI({
    src <- input$km_data_source
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
  # HELPER: Siapkan data input untuk K-Means (pakai build_fgwc_feature_matrix)
  # ==========================================================================
  km_input_data <- reactive({
    src <- input$km_data_source %||% "rc"

    if (src %in% c("sovi", "rc", "standardized")) req(rv$sovi_result)
    else                                           req(rv$data)

    sel_vars <- if (src %in% c("raw", "raw_norm", "standardized"))
      input$km_selected_vars else NULL

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
    as.matrix(X)
  })

  # ==========================================================================
  # OBSERVER: Tombol Run K-Means
  # ==========================================================================
  observeEvent(input$run_kmeans, {
    req(rv$sovi_result)

    output$km_progress <- renderUI({
      div(class = "progress-box", style = "background:#cce5ff;",
          icon("spinner", class = "fa-spin"), " Running K-Means...")
    })

    X <- km_input_data()

    withProgress(message = "K-Means Clustering...", value = 0, {

      incProgress(0.2, detail = "Preparing data...")

      # ── Tentukan k ─────────────────────────────────────────────────────────
      incProgress(0.4, detail = "Determining k...")
      if (input$km_k_mode == "auto") {
        k_max <- input$km_k_max
        wss   <- sapply(2:k_max, function(k) {
          km <- kmeans(X, centers = k, nstart = 10, iter.max = 100)
          km$tot.withinss
        })
        # Elbow: titik dengan penurunan WSS terbesar
        diffs   <- diff(wss)
        diffs2  <- diff(diffs)
        k_opt   <- which.min(diffs2) + 2L
        k_use   <- max(2L, min(k_opt, k_max))
      } else {
        k_use <- input$km_k
      }

      # ── Jalankan K-Means ────────────────────────────────────────────────────
      incProgress(0.6, detail = paste0("Clustering k=", k_use, "..."))
      set.seed(42)
      km_result <- kmeans(X,
                          centers   = k_use,
                          nstart    = input$km_nstart,
                          iter.max  = 100,
                          algorithm = input$km_algorithm)

      # ── Hitung Silhouette ────────────────────────────────────────────────────
      incProgress(0.8, detail = "Computing silhouette...")
      sil <- tryCatch(
        cluster::silhouette(km_result$cluster, dist(X)),
        error = function(e) NULL
      )
      avg_sil <- if (!is.null(sil)) round(mean(sil[, 3]), 3) else NA

      # ── Simpan hasil ────────────────────────────────────────────────────────
      sovi_df           <- rv$sovi_result$sovi_df
      sovi_df$km_cluster <- km_result$cluster

      rv$km_result <- list(
        km          = km_result,
        k           = k_use,
        sovi_df     = sovi_df,
        sil         = sil,
        avg_sil     = avg_sil,
        input_type  = input$km_input,
        X           = X
      )

      incProgress(1.0)
    })

    output$km_progress <- renderUI({
      if (!is.null(rv$km_result))
        div(class = "progress-box status-ok",
            icon("check"), " K-Means selesai! k =", rv$km_result$k,
            " | Silhouette =", rv$km_result$avg_sil)
      else
        div(class = "progress-box status-err",
            icon("times"), " Failed. Check data.")
    })
  })

  # ==========================================================================
  # OUTPUT: Ringkasan Klaster
  # ==========================================================================
  output$km_summary_table <- DT::renderDT({
    req(rv$km_result)
    sovi_df <- rv$km_result$sovi_df
    tbl <- as.data.frame(table(Cluster = paste0("Cluster ", sovi_df$km_cluster)))
    tbl$Persen <- round(tbl$Freq / nrow(sovi_df) * 100, 1)
    DT::datatable(tbl,
                  options  = list(dom = "t"),
                  rownames = FALSE,
                  caption  = paste0("k = ", rv$km_result$k))
  })

  output$km_stats_table <- DT::renderDT({
    req(rv$km_result)
    km <- rv$km_result$km
    df <- data.frame(
      Cluster    = paste0("Cluster ", seq_len(rv$km_result$k)),
      Size       = km$size,
      WithinSS   = round(km$withinss, 3),
      Pct_TotWSS = round(km$withinss / km$tot.withinss * 100, 1)
    )
    DT::datatable(df, options = list(dom = "t"), rownames = FALSE)
  })

  output$km_detail_table <- DT::renderDT({
    req(rv$km_result)
    sovi_df  <- rv$km_result$sovi_df
    id_col   <- isolate(input$id_col)
    name_col <- isolate(input$name_col)
    sel_cols <- c(id_col, name_col, "sovi_score", "vuln_class", "km_cluster")
    sel_cols <- sel_cols[sel_cols %in% names(sovi_df)]

    top_rows <- sovi_df |>
      dplyr::group_by(km_cluster) |>
      dplyr::slice_max(order_by = sovi_score, n = 5) |>
      dplyr::ungroup() |>
      dplyr::arrange(km_cluster, dplyr::desc(sovi_score)) |>
      dplyr::select(dplyr::all_of(sel_cols))

    DT::datatable(top_rows,
                  options  = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE)
  })

  # ==========================================================================
  # OUTPUT: Elbow & Silhouette Plots
  # ==========================================================================
  output$km_elbow_plot <- renderPlot({
    req(rv$km_result)
    X     <- rv$km_result$X
    k_max <- max(10, rv$km_result$k + 2)
    k_max <- min(k_max, nrow(X) - 1)
    wss   <- sapply(2:k_max, function(k) {
      kmeans(X, centers = k, nstart = 10, iter.max = 50)$tot.withinss
    })
    df <- data.frame(k = 2:k_max, WSS = wss)
    ggplot2::ggplot(df, ggplot2::aes(x = k, y = WSS)) +
      ggplot2::geom_line(color = "#1a73c1", linewidth = 1) +
      ggplot2::geom_point(color = "#1a73c1", size = 3) +
      ggplot2::geom_vline(xintercept = rv$km_result$k,
                          linetype = "dashed", color = "#e74c3c") +
      ggplot2::annotate("text", x = rv$km_result$k + 0.2,
                        y = max(wss) * 0.95,
                        label = paste0("k=", rv$km_result$k),
                        color = "#e74c3c", hjust = 0) +
      ggplot2::scale_x_continuous(breaks = 2:k_max) +
      ggplot2::labs(title = "Elbow Method — Within-Cluster SS",
                    x = "Number of Clusters (k)", y = "Total Within-SS") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                        color = "#1a73c1"))
  })

  output$km_sil_plot <- renderPlot({
    req(rv$km_result)
    X     <- rv$km_result$X
    k_max <- max(10, rv$km_result$k + 2)
    k_max <- min(k_max, nrow(X) - 1)
    sil_scores <- sapply(2:k_max, function(k) {
      km  <- kmeans(X, centers = k, nstart = 10, iter.max = 50)
      sil <- tryCatch(cluster::silhouette(km$cluster, dist(X)),
                      error = function(e) NULL)
      if (is.null(sil)) NA else mean(sil[, 3])
    })
    df <- data.frame(k = 2:k_max, Silhouette = sil_scores)
    ggplot2::ggplot(df, ggplot2::aes(x = k, y = Silhouette)) +
      ggplot2::geom_line(color = "#27ae60", linewidth = 1) +
      ggplot2::geom_point(color = "#27ae60", size = 3) +
      ggplot2::geom_vline(xintercept = rv$km_result$k,
                          linetype = "dashed", color = "#e74c3c") +
      ggplot2::scale_x_continuous(breaks = 2:k_max) +
      ggplot2::labs(title = "Silhouette Score per k",
                    x = "Number of Clusters (k)", y = "Avg. Silhouette") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                        color = "#27ae60"))
  })

  # ==========================================================================
  # OUTPUT: Profil Klaster
  # ==========================================================================
  output$km_boxplot <- renderPlot({
    req(rv$km_result)
    sovi_df <- rv$km_result$sovi_df
    sovi_df$Cluster <- paste0("Cluster ", sovi_df$km_cluster)
    ggplot2::ggplot(sovi_df,
                    ggplot2::aes(x = Klaster, y = sovi_score, fill = Klaster)) +
      ggplot2::geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2) +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::labs(title = "SoVI Score Distribution per K-Means Cluster",
                    x = NULL, y = "SoVI Score") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(face = "bold",
                                                        color = "#1a73c1"))
  })

  output$km_heatmap <- renderPlot({
    req(rv$km_result)
    sovi_df  <- rv$km_result$sovi_df
    rc_cols  <- grep("^RC", names(sovi_df), value = TRUE)
    show_vars <- c("sovi_score", rc_cols)[c("sovi_score", rc_cols) %in% names(sovi_df)]
    if (length(show_vars) == 0) return()

    # Rata-rata per klaster
    means_df <- sovi_df |>
      dplyr::group_by(Cluster = paste0("Cluster ", km_cluster)) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(show_vars), mean, na.rm = TRUE),
                       .groups = "drop") |>
      tidyr::pivot_longer(-Cluster, names_to = "Variable", values_to = "Value")

    ggplot2::ggplot(means_df,
                    ggplot2::aes(x = Variabel, y = Klaster, fill = Nilai)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = round(Nilai, 3)),
                         size = 3.5, color = "grey10") +
      ggplot2::scale_fill_gradient2(low = "#1a9641", mid = "#ffffbf",
                                    high = "#d7191c", midpoint = 0.5) +
      ggplot2::labs(title = "Mean Variable Values per Cluster",
                    x = NULL, y = NULL, fill = "Nilai") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title     = ggplot2::element_text(face = "bold",
                                                             color = "#1a73c1"),
                     axis.text.x    = ggplot2::element_text(angle = 30, hjust = 1),
                     panel.grid     = ggplot2::element_blank())
  })

  # ==========================================================================
  # OUTPUT: Peta Klaster K-Means
  # ==========================================================================
  output$km_map <- leaflet::renderLeaflet({
    req(rv$km_result, rv$shp)
    sovi_df  <- rv$km_result$sovi_df
    k        <- rv$km_result$k
    id_col   <- isolate(input$id_col)
    join_shp <- isolate(input$join_shp)
    name_col <- isolate(input$name_col)

    sovi_df[[id_col]]     <- normalize_id(sovi_df[[id_col]])
    shp                   <- rv$shp
    shp[[join_shp]]       <- normalize_id(shp[[join_shp]])

    peta <- dplyr::left_join(shp, sovi_df,
                             by = setNames(id_col, join_shp))
    peta$km_cluster <- as.factor(peta$km_cluster)

    pal_k <- RColorBrewer::brewer.pal(max(k, 3), "Set2")[seq_len(k)]
    pal   <- leaflet::colorFactor(pal_k, domain = levels(peta$km_cluster))

    nm <- if (name_col %in% names(peta)) peta[[name_col]] else ""
    popup <- paste0(
      "<b>", nm, "</b><br>",
      "Cluster: <b>Cluster ", peta$km_cluster, "</b><br>",
      "SoVI Score: ", round(peta$sovi_score, 4), "<br>",
      "Kelas: ", peta$vuln_class
    )

    leaflet::leaflet(peta) |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
      leaflet::addPolygons(
        fillColor   = ~pal(km_cluster),
        fillOpacity = 0.75,
        color       = "#fff",
        weight      = 0.5,
        popup       = popup,
        label       = ~paste0("Cluster ", km_cluster, ": ", nm)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ~km_cluster,
        title    = paste0("K-Means Cluster (k=", k, ")")
      )
  })

} # end kmeans_server()
