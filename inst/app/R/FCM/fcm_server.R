# =============================================================================
# R/FCM/fcm_server.R
# Server Logic Tab FCM (Fuzzy C-Means)
#
# DIPANGGIL DARI : server.R via fcm_server(input, output, session, rv)
# DEPENDENSI     :
#   - R/FGWC/fgwc_wrapper.R  (build_fgwc_feature_matrix, build_leaflet_fgwc)
#   - R/core/helpers.R        (normalize_id)
#   - Package: ppclust, cluster, dplyr, ggplot2, leaflet, fmsb, MASS, Rtsne, uwot
# =============================================================================

fcm_server <- function(input, output, session, rv) {

  output$fcm_progress <- renderUI({ NULL })

  # ==========================================================================
  # OUTPUT: Variable selector (untuk raw/raw_norm/standardized)
  # ==========================================================================
  output$fcm_var_selector <- renderUI({
    req(rv$data, rv$sovi_vars)
    src      <- input$fcm_data_source %||% "rc"
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
    tagList(note, checkboxGroupInput("fcm_selected_vars", NULL,
                                     choices  = num_cols,
                                     selected = rv$sovi_vars))
  })

  # ==========================================================================
  # OUTPUT: Info sumber data (sovi/rc)
  # ==========================================================================
  output$fcm_datasource_info <- renderUI({
    src <- input$fcm_data_source
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
  # HELPER: Siapkan matriks fitur input FCM
  # ==========================================================================
  fcm_input_data <- reactive({
    src <- input$fcm_data_source %||% "rc"

    if (src %in% c("sovi", "rc", "standardized")) req(rv$sovi_result)
    else                                           req(rv$data)

    sel_vars <- if (src %in% c("raw", "raw_norm", "standardized"))
      input$fcm_selected_vars else NULL

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

    if (isTRUE(input$fcm_scale)) {
      X[] <- lapply(X, function(v) as.numeric(scale(v)))
    }
    as.matrix(X)
  })

  # ==========================================================================
  # OBSERVER: Tombol Run FCM
  # ==========================================================================
  observeEvent(input$run_fcm, {

    if (is.null(rv$data)) {
      showNotification("Upload dataset first!", type = "warning"); return()
    }
    if (input$fcm_data_source %in% c("sovi", "rc", "standardized") &&
        is.null(rv$sovi_result)) {
      showNotification("Run SoVI Computation first!", type = "warning"); return()
    }

    output$fcm_progress <- renderUI({
      div(class = "progress-box", style = "background:#cce5ff;",
          icon("spinner", class = "fa-spin"),
          paste0(" Running FCM (k=", input$fcm_ncluster,
                 ", m=", input$fcm_m, ")..."))
    })

    X        <- fcm_input_data()
    k        <- input$fcm_ncluster
    m        <- input$fcm_m
    maxiter  <- input$fcm_maxiter %||% 500
    seed_val <- input$fcm_seed    %||% 0

    withProgress(message = "Running FCM...", value = 0, {
      incProgress(0.2, detail = "Building feature matrix...")

      result <- tryCatch({
        set.seed(seed_val)
        fcm_res <- ppclust::fcm(
          x        = X,
          centers  = k,
          m        = m,
          maxit    = maxiter,
          con.val  = 1e-6
        )

        incProgress(0.5, detail = "Computing silhouette & profile...")

        # Cluster assignment (hard clustering = argmax membership)
        cluster_vec <- fcm_res$cluster

        # Silhouette
        D0       <- dist(X, method = "euclidean")
        sil_obj  <- tryCatch(cluster::silhouette(cluster_vec, D0), error = function(e) NULL)
        sil_mean <- if (!is.null(sil_obj)) round(mean(sil_obj[, 3]), 3) else NA

        # Validation indices (dari ppclust)
        val_pc  <- round(ppclust::pc(fcm_res$u), 4)
        val_mpc <- round(ppclust::mpc(fcm_res$u), 4)
        val_ce  <- round(ppclust::ce(fcm_res$u), 4)

        # Indeks tambahan via cluster package
        val_si  <- if (!is.null(sil_obj)) round(mean(sil_obj[, 3]), 4) else NA

        val_df <- data.frame(
          Index       = c("PC (Partition Coefficient)",
                          "MPC (Modified Partition Coefficient)",
                          "CE (Classification Entropy)",
                          "SI (Silhouette Index)"),
          Value       = c(val_pc, val_mpc, val_ce, val_si),
          Preference  = c("higher is better", "higher is better",
                          "lower is better",  "higher is better"),
          stringsAsFactors = FALSE
        )

        # Feature matrix (untuk dim-reduction)
        feat_df  <- as.data.frame(X)
        feat_cols <- colnames(feat_df)

        # Profil cluster
        feat_df2          <- feat_df
        feat_df2$cluster  <- as.factor(cluster_vec)
        profile <- feat_df2 |>
          dplyr::group_by(cluster) |>
          dplyr::summarise(
            n = dplyr::n(),
            dplyr::across(dplyr::all_of(feat_cols),
                          \(x) round(mean(x, na.rm = TRUE), 3)),
            .groups = "drop"
          )

        # result_df — gabung dengan sovi_df / raw data
        src_used <- input$fcm_data_source
        if (!is.null(rv$sovi_result) && src_used != "raw") {
          result_df             <- rv$sovi_result$sovi_df
          result_df$fcm_cluster <- as.factor(cluster_vec)
        } else {
          result_df <- data.frame(
            id_col  = rv$data[[input$id_col]],
            nm_col  = rv$data[[input$name_col]],
            fcm_cluster = as.factor(cluster_vec),
            stringsAsFactors = FALSE
          )
          names(result_df)[1:2] <- c(input$id_col, input$name_col)
          result_df <- cbind(result_df, feat_df)
        }

        # Mean SoVI per cluster (jika tersedia)
        if (!is.null(rv$sovi_result) && "sovi_score" %in% names(result_df)) {
          sovi_means <- result_df |>
            dplyr::group_by(fcm_cluster) |>
            dplyr::summarise(
              mean_sovi = round(mean(sovi_score, na.rm = TRUE), 3),
              .groups   = "drop"
            )
          profile <- dplyr::left_join(profile, sovi_means, by = c("cluster" = "fcm_cluster"))
        }

        # Convergence: ppclust::fcm menyimpan di $iter.val
        conv_vec <- if (!is.null(fcm_res$iter.val)) fcm_res$iter.val else fcm_res$func.val

        list(
          fcm_obj     = fcm_res,
          result_df   = result_df,
          feat_df     = feat_df,
          profile     = profile,
          sil_obj     = sil_obj,
          sil_mean    = sil_mean,
          val_df      = val_df,
          conv        = conv_vec,
          k           = k,
          m           = m,
          feat_cols   = feat_cols,
          data_source = src_used,
          id_col      = input$id_col,
          name_col    = input$name_col
        )
      }, error = function(e) {
        showNotification(paste("Error FCM:", e$message),
                         type = "error", duration = 15)
        NULL
      })

      incProgress(0.9, detail = "Done.")
      rv$fcm_result <- result
    })

    output$fcm_progress <- renderUI({
      res <- rv$fcm_result
      if (!is.null(res))
        div(class = "progress-box status-ok", icon("check"),
            paste0(" Complete! k=", res$k,
                   ", m=", res$m,
                   ", Silhouette=", res$sil_mean))
      else
        div(class = "progress-box status-err",
            icon("times"), " Failed. Check data & parameters.")
    })
  })

  # ==========================================================================
  # TAB 1: SUMMARY
  # ==========================================================================
  output$fcm_summary <- renderUI({
    res <- rv$fcm_result
    if (is.null(res)) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12;",
                 icon("exclamation-triangle"),
                 " Run FCM using the button in the left panel."))
    }

    src_label <- switch(res$data_source,
                        "raw"          = "Original Data (no transformation)",
                        "raw_norm"     = "Normalized Data (0-1)",
                        "standardized" = "Z-score Data",
                        "sovi"         = "SoVI Score",
                        "rc"           = "RC Scores (PCA Components)",
                        res$data_source)

    sil_color <- if (!is.na(res$sil_mean) && res$sil_mean >= 0.5)
      "color:#27ae60;font-weight:700;"
    else if (!is.na(res$sil_mean) && res$sil_mean >= 0.25)
      "color:#f39c12;font-weight:700;"
    else
      "color:#e74c3c;font-weight:700;"

    # Validation summary
    val_pc  <- res$val_df$Value[res$val_df$Index == "PC (Partition Coefficient)"]
    val_mpc <- res$val_df$Value[res$val_df$Index == "MPC (Modified Partition Coefficient)"]
    val_ce  <- res$val_df$Value[res$val_df$Index == "CE (Classification Entropy)"]

    div(
      fluidRow(
        column(4, div(class = "info-card",
                      tags$h4(icon("layer-group"), " FCM Configuration"),
                      tags$p(style = "font-size:13px; font-weight:700; color:#1a73c1;",
                             "Fuzzy C-Means (ppclust)"),
                      tags$p(style = "font-size:12px; color:#78909c;",
                             "Data source: ", src_label),
                      tags$p(style = "font-size:12px; color:#78909c;",
                             "Features: ", length(res$feat_cols), " dimensions"),
                      tags$p(style = "font-size:12px; color:#78909c;",
                             "Fuzzifier (m): ", res$m)
        )),
        column(4, div(class = "info-card",
                      tags$h4(icon("object-group"), " Clustering Results"),
                      tags$p(style = "font-size:13px;",
                             tags$strong("k = "), res$k),
                      tags$p(style = "font-size:13px;",
                             tags$strong("PC = "), val_pc,
                             tags$span(style = "font-size:11px; color:#78909c;",
                                       " (higher is better)")),
                      tags$p(style = "font-size:13px;",
                             tags$strong("Mean Silhouette = "),
                             tags$span(style = sil_color,
                                       if (is.na(res$sil_mean)) "N/A" else res$sil_mean))
        )),
        column(4, div(class = "info-card",
                      tags$h4(icon("chart-bar"), " Cluster Distribution"),
                      tags$table(style = "width:100%; font-size:12.5px;",
                                 tags$thead(tags$tr(
                                   tags$th("Cluster"), tags$th("n"), tags$th("Sil. Width")
                                 )),
                                 tags$tbody(lapply(seq_len(nrow(res$profile)), function(i) {
                                   sw <- if (!is.null(res$sil_obj) && !is.na(res$sil_mean)) {
                                     round(summary(res$sil_obj)$clus.avg.widths[i], 3)
                                   } else "N/A"
                                   tags$tr(
                                     tags$td(paste0("Cluster ", i)),
                                     tags$td(as.character(res$profile$n[i])),
                                     tags$td(as.character(sw))
                                   )
                                 }))
                      )
        ))
      ),
      fluidRow(column(12,
                      div(class = "info-card",
                          style = "border-left-color:#27ae60; margin-top:4px;",
                          tags$h4(icon("lightbulb"), " Silhouette Interpretation"),
                          tags$ul(style = "font-size:12.5px; margin:0;",
                                  tags$li(tags$span(style = "color:#27ae60;font-weight:600;", "\u2265 0.50"),
                                          " \u2014 Strong cluster structure"),
                                  tags$li(tags$span(style = "color:#f39c12;font-weight:600;", "0.25\u20130.49"),
                                          " \u2014 Moderate cluster structure"),
                                  tags$li(tags$span(style = "color:#e74c3c;font-weight:600;", "< 0.25"),
                                          " \u2014 Weak cluster structure, consider different k")
                          )
                      )
      ))
    )
  })

  # ==========================================================================
  # TAB 2: VALIDASI + KONVERGENSI
  # ==========================================================================
  output$fcm_val_table <- DT::renderDT({
    req(rv$fcm_result)
    DT::datatable(rv$fcm_result$val_df,
                  options  = list(dom = "t", pageLength = 10),
                  rownames = FALSE,
                  caption  = "FCM Cluster Validation Index")
  })

  output$fcm_conv_plot <- renderPlot({
    req(rv$fcm_result)
    conv <- rv$fcm_result$conv
    if (is.null(conv) || length(conv) == 0) {
      plot.new()
      text(0.5, 0.5, "Convergence data not available.", cex = 1.1, col = "grey50")
      return()
    }
    df <- data.frame(Iteration = seq_along(conv), Obj = conv)
    ggplot2::ggplot(df, ggplot2::aes(x = Iteration, y = Obj)) +
      ggplot2::geom_line(color = "#1a73c1", linewidth = 1.0) +
      ggplot2::geom_point(color = "#1a73c1", size = 1.5, alpha = 0.5) +
      ggplot2::labs(
        title = "Objective Function Convergence \u2014 FCM",
        x = "Iteration", y = "Objective Function"
      ) +
      ggplot2::theme_minimal(base_size = 11)
  })

  # ==========================================================================
  # TAB 3: PETA INTERAKTIF
  # ==========================================================================
  output$fcm_map <- leaflet::renderLeaflet({
    req(rv$fcm_result, rv$shp)
    res <- rv$fcm_result
    build_leaflet_fgwc(
      result_df   = res$result_df,
      shp         = rv$shp,
      join_shp    = input$join_shp,
      join_df     = res$id_col,
      name_col    = res$name_col,
      k           = res$k,
      cluster_col = "fcm_cluster"
    )
  })

  # ==========================================================================
  # TAB 4: SILHOUETTE
  # ==========================================================================
  output$fcm_sil_plot <- renderPlot({
    req(rv$fcm_result)
    res <- rv$fcm_result
    if (is.null(res$sil_obj)) {
      plot.new()
      text(0.5, 0.5, "Silhouette not available.", cex = 1.1, col = "grey50")
      return()
    }
    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set1")[seq_len(k)]
    plot(res$sil_obj,
         main = paste0("Silhouette \u2014 FCM, k=", k, "\nMean = ", res$sil_mean),
         col  = pal_c)
  })

  output$fcm_sil_table <- DT::renderDT({
    req(rv$fcm_result)
    res <- rv$fcm_result
    if (is.null(res$sil_obj))
      return(DT::datatable(data.frame(Info = "N/A")))
    sil_sum <- summary(res$sil_obj)$clus.avg.widths
    df <- data.frame(cluster       = factor(seq_along(sil_sum)),
                     avg_sil_width = round(sil_sum, 4))
    DT::datatable(df, options = list(dom = "t"), rownames = FALSE,
                  caption = "Avg. Silhouette Width per Cluster")
  })

  output$fcm_sil_interp <- renderUI({
    req(rv$fcm_result)
    s <- rv$fcm_result$sil_mean
    if (is.na(s)) return(NULL)
    cls   <- if (s >= 0.50) "status-ok" else if (s >= 0.25) "status-warn" else "status-err"
    label <- if (s >= 0.50) "Strong Structure \u2713"
             else if (s >= 0.25) "Moderate Structure"
             else "Weak Structure \u2717"
    div(class = paste("progress-box", cls),
        icon("tachometer-alt"),
        paste0(" Mean Silhouette = ", s, " \u2014 ", label))
  })

  # ==========================================================================
  # TAB 5: PROFIL CLUSTER
  # ==========================================================================
  output$fcm_profile_table <- DT::renderDT({
    req(rv$fcm_result)
    df       <- rv$fcm_result$profile
    num_cols <- setdiff(names(df), "cluster")
    df[, num_cols] <- round(df[, num_cols, drop = FALSE], 3)
    DT::datatable(df, options = list(dom = "t", scrollX = TRUE),
                  rownames = FALSE,
                  caption  = "Cluster Profile: Mean Feature per Cluster")
  })

  output$fcm_heatmap <- renderPlot({
    req(rv$fcm_result)
    res       <- rv$fcm_result
    feat_cols <- res$feat_cols
    long <- tidyr::pivot_longer(res$profile,
                                cols      = dplyr::all_of(feat_cols),
                                names_to  = "Feature",
                                values_to = "Mean_Score")
    long$cluster <- factor(long$cluster)
    ggplot2::ggplot(long, ggplot2::aes(x = Feature, y = cluster,
                                       fill = Mean_Score)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.6) +
      ggplot2::geom_text(ggplot2::aes(label = round(Mean_Score, 2)),
                         size = 3.0, color = "black") +
      ggplot2::scale_fill_distiller(palette = "RdYlBu", direction = -1,
                                    name = "Mean\nScore") +
      ggplot2::labs(
        title = "Cluster Profile Heatmap \u2014 FCM",
        x = "Feature / Dimension", y = "Cluster"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9))
  })

  output$fcm_radar <- renderPlot({
    req(rv$fcm_result)
    res       <- rv$fcm_result
    feat_cols <- res$feat_cols
    k         <- res$k
    if (length(feat_cols) < 3) {
      plot.new()
      text(0.5, 0.5, "Radar chart requires at least 3 features.", cex = 1.1, col = "grey50")
      return()
    }
    pal_rad        <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set1")[seq_len(k)]
    cluster_labels <- paste0("Cluster ", seq_len(k))
    feat_mat       <- as.matrix(res$profile[, feat_cols, drop = FALSE])
    row_mins       <- apply(feat_mat, 2, min)
    row_maxs       <- apply(feat_mat, 2, max)
    feat_norm      <- sweep(sweep(feat_mat, 2, row_mins, "-"),
                            2, pmax(row_maxs - row_mins, 1e-9), "/")
    n_col_plot <- min(k, 3)
    n_row_plot <- ceiling(k / n_col_plot)
    par(mfrow = c(n_row_plot, n_col_plot + 1),
        mar   = c(1, 1, 2.5, 1), oma = c(0, 0, 2, 0))
    for (i in seq_len(k)) {
      rdf <- as.data.frame(rbind(rep(1, length(feat_cols)),
                                 rep(0, length(feat_cols)),
                                 as.numeric(feat_norm[i, ])))
      colnames(rdf) <- feat_cols
      fmsb::radarchart(rdf, axistype = 1,
                       pcol  = pal_rad[i],
                       pfcol = adjustcolor(pal_rad[i], alpha.f = 0.30),
                       plwd  = 2.2, cglcol = "grey70", cglty = 1,
                       vlcex = 0.75, title = cluster_labels[i],
                       caxislabels = c("0","0.25","0.5","0.75","1"),
                       calcex = 0.6)
      mtext(paste0("n = ", res$profile$n[i]), side = 1, line = 0.2, cex = 0.7)
    }
    plot.new()
    legend("center", legend = cluster_labels, col = pal_rad,
           lwd = 3, bty = "n", title = "Cluster", cex = 0.85)
    mtext("FCM Radar Profile \u2014 normalized values 0\u20131",
          outer = TRUE, line = 0.5, cex = 1.0, font = 2)
  })

  # ==========================================================================
  # TAB 6: DATA CLUSTER
  # ==========================================================================
  output$fcm_result_table <- DT::renderDT({
    req(rv$fcm_result)
    res  <- rv$fcm_result
    df   <- res$result_df
    show_cols <- unique(c(res$id_col, res$name_col, "fcm_cluster",
                          if ("sovi_score" %in% names(df)) "sovi_score",
                          if ("vuln_class" %in% names(df)) "vuln_class"))
    show_cols <- show_cols[show_cols %in% names(df)]
    show_df   <- df[, show_cols, drop = FALSE]
    num_cols  <- show_cols[sapply(show_df, is.numeric)]
    show_df[, num_cols] <- round(show_df[, num_cols, drop = FALSE], 4)
    DT::datatable(show_df, filter = "top",
                  options  = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE,
                  caption  = paste0("FCM Results \u2014 k=", res$k, ", m=", res$m))
  })

  # ==========================================================================
  # DOWNLOADS
  # ==========================================================================
  output$dl_fcm_csv <- downloadHandler(
    filename = function() paste0("fcm_k", input$fcm_ncluster, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$fcm_result)
      res       <- rv$fcm_result
      df        <- res$result_df
      keep_cols <- unique(c(res$id_col, res$name_col, "fcm_cluster",
                            if ("sovi_score" %in% names(df)) "sovi_score",
                            if ("vuln_class" %in% names(df)) "vuln_class"))
      keep_cols <- keep_cols[keep_cols %in% names(df)]
      write.csv(df[, keep_cols], file, row.names = FALSE)
    }
  )

  output$dl_fcm_map_png <- downloadHandler(
    filename = function() paste0("map_fcm_k", input$fcm_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$fcm_result, rv$shp)
      res       <- rv$fcm_result
      result_df <- res$result_df
      shp       <- rv$shp
      k         <- res$k
      result_df[[res$id_col]] <- normalize_id(result_df[[res$id_col]])
      shp[[input$join_shp]]   <- normalize_id(shp[[input$join_shp]])
      peta              <- dplyr::left_join(shp, result_df,
                                            by = setNames(res$id_col, input$join_shp))
      peta$fcm_cluster  <- as.factor(peta$fcm_cluster)
      pal_c             <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set1")[seq_len(k)]
      tmap::tmap_mode("plot")
      m <- tmap::tm_shape(peta) +
        tmap::tm_polygons(fill = "fcm_cluster",
                          fill.scale  = tmap::tm_scale_categorical(values = pal_c),
                          fill.legend = tmap::tm_legend(title = "FCM Cluster"),
                          col = "grey40", lwd = 0.3) +
        tmap::tm_title(paste0("FCM \u2014 k=", k, ", m=", res$m)) +
        tmap::tm_compass(type = "arrow", position = c("left","bottom"), size = 1.5) +
        tmap::tm_scalebar(position = c("left","bottom"), text.size = 0.6) +
        tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "right")
      tmap::tmap_save(m, filename = file, width = 3000, height = 2400, dpi = 300)
      tmap::tmap_mode("view")
    }
  )

  output$dl_fcm_heatmap <- downloadHandler(
    filename = function() paste0("fcm_heatmap_k", input$fcm_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$fcm_result)
      res       <- rv$fcm_result
      feat_cols <- res$feat_cols
      long <- tidyr::pivot_longer(res$profile,
                                  cols      = dplyr::all_of(feat_cols),
                                  names_to  = "Feature",
                                  values_to = "Mean_Score")
      long$cluster <- factor(long$cluster)
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Feature, y = cluster,
                                              fill = Mean_Score)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = round(Mean_Score, 2)),
                           size = 3.0, color = "black") +
        ggplot2::scale_fill_distiller(palette = "RdYlBu", direction = -1,
                                      name = "Mean\nScore") +
        ggplot2::labs(title = "Cluster Profile Heatmap \u2014 FCM",
                      x = "Feature / Dimension", y = "Cluster") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9))
      ggplot2::ggsave(file, plot = p, width = 10, height = 5, dpi = 300, bg = "white")
    }
  )

  output$dl_fcm_radar <- downloadHandler(
    filename = function() paste0("fcm_radar_k", input$fcm_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$fcm_result)
      res       <- rv$fcm_result
      feat_cols <- res$feat_cols
      k         <- res$k
      if (length(feat_cols) < 3) return()
      pal_rad        <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set1")[seq_len(k)]
      cluster_labels <- paste0("Cluster ", seq_len(k))
      feat_mat       <- as.matrix(res$profile[, feat_cols, drop = FALSE])
      row_mins       <- apply(feat_mat, 2, min)
      row_maxs       <- apply(feat_mat, 2, max)
      feat_norm      <- sweep(sweep(feat_mat, 2, row_mins, "-"),
                              2, pmax(row_maxs - row_mins, 1e-9), "/")
      n_col_plot <- min(k, 3)
      n_row_plot <- ceiling(k / n_col_plot)
      grDevices::png(file, width = 3000, height = 2400, res = 300)
      par(mfrow = c(n_row_plot, n_col_plot + 1),
          mar   = c(1, 1, 2.5, 1), oma = c(0, 0, 2, 0))
      for (i in seq_len(k)) {
        rdf <- as.data.frame(rbind(rep(1, length(feat_cols)),
                                   rep(0, length(feat_cols)),
                                   as.numeric(feat_norm[i, ])))
        colnames(rdf) <- feat_cols
        fmsb::radarchart(rdf, axistype = 1, pcol = pal_rad[i],
                         pfcol = adjustcolor(pal_rad[i], alpha.f = 0.30),
                         plwd = 2.2, cglcol = "grey70", cglty = 1,
                         vlcex = 0.75, title = cluster_labels[i],
                         caxislabels = c("0","0.25","0.5","0.75","1"), calcex = 0.6)
        mtext(paste0("n = ", res$profile$n[i]), side = 1, line = 0.2, cex = 0.7)
      }
      plot.new()
      legend("center", legend = cluster_labels, col = pal_rad,
             lwd = 3, bty = "n", title = "Cluster", cex = 0.85)
      mtext("FCM Radar Profile \u2014 normalized values 0\u20131",
            outer = TRUE, line = 0.5, cex = 1.0, font = 2)
      grDevices::dev.off()
    }
  )

  # ==========================================================================
  # HELPER INTERNAL: Buat plot reduksi dimensi (Sammon / t-SNE / UMAP)
  # Sama persis dengan FGWC — menggunakan pola iso-membership + centroid
  # ==========================================================================
  .fcm_dim_reduction_plot <- function(df_plot, k, pal_c, pt_sz,
                                      title_str, subtitle_str, xlab, ylab) {
    df_centers <- df_plot |>
      dplyr::group_by(Cluster) |>
      dplyr::summarise(Dim1 = mean(Dim1, na.rm = TRUE),
                       Dim2 = mean(Dim2, na.rm = TRUE),
                       .groups = "drop")

    pad_x   <- (max(df_plot$Dim1) - min(df_plot$Dim1)) * 0.1
    pad_y   <- (max(df_plot$Dim2) - min(df_plot$Dim2)) * 0.1
    x_seq   <- seq(min(df_plot$Dim1) - pad_x, max(df_plot$Dim1) + pad_x, length.out = 150)
    y_seq   <- seq(min(df_plot$Dim2) - pad_y, max(df_plot$Dim2) + pad_y, length.out = 150)
    grid_df <- expand.grid(Dim1 = x_seq, Dim2 = y_seq)

    centers_mat <- as.matrix(df_centers[, c("Dim1", "Dim2")])
    k_eff       <- nrow(centers_mat)

    D <- matrix(0, nrow = nrow(grid_df), ncol = k_eff)
    for (i in seq_len(k_eff)) {
      D[, i] <- sqrt((grid_df$Dim1 - centers_mat[i, 1])^2 +
                     (grid_df$Dim2 - centers_mat[i, 2])^2)
    }
    D[D == 0] <- 1e-10
    tmp    <- D^(-2)
    U_grid <- tmp / rowSums(tmp)

    grid_single <- data.frame(
      Dim1       = grid_df$Dim1,
      Dim2       = grid_df$Dim2,
      Membership = apply(U_grid, 1, max)
    )

    df_legend <- data.frame(
      x = c(NA, NA), y = c(NA, NA),
      Type = factor(c("Data point", "Cluster center"),
                    levels = c("Data point", "Cluster center"))
    )

    ggplot2::ggplot() +
      ggplot2::geom_contour(data = grid_single,
                            mapping = ggplot2::aes(x = Dim1, y = Dim2,
                                                   z = Membership,
                                                   color = ggplot2::after_stat(level)),
                            bins = 12, linewidth = 0.5, show.legend = FALSE) +
      ggplot2::scale_color_viridis_c(option = "turbo", direction = 1, limits = c(0, 1)) +
      ggplot2::geom_point(data = df_plot,
                          mapping = ggplot2::aes(x = Dim1, y = Dim2),
                          color = "#3949ab", size = pt_sz, alpha = 0.8, shape = 16) +
      ggplot2::geom_point(data = df_centers,
                          mapping = ggplot2::aes(x = Dim1, y = Dim2),
                          color = "red", size = pt_sz * 1.5, stroke = 2, shape = 4) +
      ggplot2::geom_point(data = df_legend,
                          mapping = ggplot2::aes(x = x, y = y, shape = Type),
                          na.rm = TRUE) +
      ggplot2::scale_shape_manual(
        name = NULL,
        values = c("Data point" = 16, "Cluster center" = 4),
        guide = ggplot2::guide_legend(
          override.aes = list(
            color  = c("#3949ab", "red"),
            size   = c(pt_sz + 1, pt_sz * 1.5 + 1),
            stroke = c(0, 1.5)
          )
        )
      ) +
      ggplot2::labs(title = title_str, subtitle = subtitle_str,
                    x = xlab, y = ylab) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        panel.grid      = ggplot2::element_blank(),
        plot.title      = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle   = ggplot2::element_text(color = "#546e7a", size = 10, hjust = 0.5),
        legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.5),
        legend.key      = ggplot2::element_blank(),
        legend.margin   = ggplot2::margin(4, 12, 4, 8),
        legend.text     = ggplot2::element_text(size = 11)
      )
  }

  # ==========================================================================
  # TAB 7: SAMMON MAPPING
  # ==========================================================================
  output$fcm_sammon_plot <- renderPlot({
    req(rv$fcm_result)
    res      <- rv$fcm_result
    feat_mat <- as.matrix(res$feat_df)

    if (is.null(feat_mat) || nrow(feat_mat) < 3) {
      plot.new()
      text(0.5, 0.5, "Insufficient data for Sammon Mapping.", cex = 1.1, col = "grey50")
      return()
    }

    feat_mat <- unique(feat_mat)
    dist_mat <- dist(feat_mat)

    set.seed(42)
    sm <- tryCatch(
      MASS::sammon(dist_mat, k = 2,
                   niter = input$fcm_sammon_iter  %||% 500,
                   magic = input$fcm_sammon_magic %||% 0.2,
                   trace = FALSE),
      error = function(e) {
        showNotification(paste("Sammon error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(sm)) return()

    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
    pt_sz <- input$fcm_sammon_pt %||% 2

    cluster_vec <- as.integer(as.character(res$result_df$fcm_cluster))
    if (length(cluster_vec) > nrow(sm$points))
      cluster_vec <- cluster_vec[seq_len(nrow(sm$points))]

    df_plot <- data.frame(
      Dim1    = sm$points[, 1],
      Dim2    = sm$points[, 2],
      Cluster = factor(cluster_vec, levels = seq_len(k))
    )

    .fcm_dim_reduction_plot(
      df_plot      = df_plot, k = k, pal_c = pal_c, pt_sz = pt_sz,
      title_str    = paste0("FCM \u2014 Sammon Mapping | k=", k),
      subtitle_str = paste0("Stress = ", round(sm$stress, 5),
                            "  |  Iterations = ", input$fcm_sammon_iter %||% 500),
      xlab = "Dim 1", ylab = "Dim 2"
    )
  })

  # ==========================================================================
  # TAB 8: t-SNE
  # ==========================================================================
  output$fcm_tsne_plot <- renderPlot({
    req(rv$fcm_result)
    res      <- rv$fcm_result
    feat_mat <- as.matrix(res$feat_df)

    if (is.null(feat_mat) || nrow(feat_mat) < 3) {
      plot.new()
      text(0.5, 0.5, "Insufficient data for t-SNE.", cex = 1.1, col = "grey50")
      return()
    }

    perp <- min(input$fcm_tsne_perp %||% 15, floor((nrow(feat_mat) - 1) / 3))
    perp <- max(perp, 2)

    set.seed(42)
    tsne_res <- tryCatch(
      Rtsne::Rtsne(feat_mat, dims = 2, perplexity = perp,
                   max_iter         = input$fcm_tsne_iter %||% 1000,
                   check_duplicates = FALSE, verbose = FALSE),
      error = function(e) {
        showNotification(paste("t-SNE error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(tsne_res)) return()

    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
    pt_sz <- input$fcm_tsne_pt %||% 2

    df_plot <- data.frame(
      Dim1    = tsne_res$Y[, 1],
      Dim2    = tsne_res$Y[, 2],
      Cluster = factor(as.integer(as.character(res$result_df$fcm_cluster)),
                       levels = seq_len(k))
    )

    .fcm_dim_reduction_plot(
      df_plot      = df_plot, k = k, pal_c = pal_c, pt_sz = pt_sz,
      title_str    = paste0("FCM \u2014 t-SNE | k=", k),
      subtitle_str = paste0("Perplexity = ", perp,
                            "  |  Iterations = ", input$fcm_tsne_iter %||% 1000),
      xlab = "t-SNE Dim 1", ylab = "t-SNE Dim 2"
    )
  })

  # ==========================================================================
  # TAB 9: UMAP
  # ==========================================================================
  output$fcm_umap_plot <- renderPlot({
    req(rv$fcm_result)
    res      <- rv$fcm_result
    feat_mat <- as.matrix(res$feat_df)

    if (is.null(feat_mat) || nrow(feat_mat) < 3) {
      plot.new()
      text(0.5, 0.5, "Insufficient data for UMAP.", cex = 1.1, col = "grey50")
      return()
    }

    nn <- min(input$fcm_umap_nn %||% 15, nrow(feat_mat) - 1)
    nn <- max(nn, 2)
    md <- input$fcm_umap_md %||% 0.1

    set.seed(42)
    umap_res <- tryCatch(
      uwot::umap(feat_mat, n_neighbors = nn, min_dist = md,
                 n_components = 2, verbose = FALSE),
      error = function(e) {
        showNotification(paste("UMAP error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(umap_res)) return()

    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
    pt_sz <- input$fcm_umap_pt %||% 2

    df_plot <- data.frame(
      Dim1    = umap_res[, 1],
      Dim2    = umap_res[, 2],
      Cluster = factor(as.integer(as.character(res$result_df$fcm_cluster)),
                       levels = seq_len(k))
    )

    .fcm_dim_reduction_plot(
      df_plot      = df_plot, k = k, pal_c = pal_c, pt_sz = pt_sz,
      title_str    = paste0("FCM \u2014 UMAP | k=", k),
      subtitle_str = paste0("n_neighbors = ", nn, "  |  min_dist = ", md),
      xlab = "UMAP Dim 1", ylab = "UMAP Dim 2"
    )
  })

  # Downloads Sammon / t-SNE / UMAP
  output$dl_fcm_sammon <- downloadHandler(
    filename = function() paste0("fcm_sammon_k", input$fcm_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$fcm_result)
      res      <- rv$fcm_result
      feat_mat <- unique(as.matrix(res$feat_df))
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      set.seed(42)
      sm <- tryCatch(
        MASS::sammon(dist(feat_mat), k = 2,
                     niter = input$fcm_sammon_iter  %||% 500,
                     magic = input$fcm_sammon_magic %||% 0.2, trace = FALSE),
        error = function(e) NULL
      )
      if (is.null(sm)) return()
      k   <- res$k
      pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
      cv  <- as.integer(as.character(res$result_df$fcm_cluster))
      if (length(cv) > nrow(sm$points)) cv <- cv[seq_len(nrow(sm$points))]
      df_plot <- data.frame(Dim1 = sm$points[, 1], Dim2 = sm$points[, 2],
                            Cluster = factor(cv, levels = seq_len(k)))
      p <- .fcm_dim_reduction_plot(df_plot, k, pal_c, input$fcm_sammon_pt %||% 2,
                                   paste0("FCM \u2014 Sammon Mapping | k=", k),
                                   paste0("Stress = ", round(sm$stress, 5)),
                                   "Dim 1", "Dim 2")
      ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )

  output$dl_fcm_tsne <- downloadHandler(
    filename = function() paste0("fcm_tsne_k", input$fcm_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$fcm_result)
      res      <- rv$fcm_result
      feat_mat <- as.matrix(res$feat_df)
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      perp <- max(min(input$fcm_tsne_perp %||% 15, floor((nrow(feat_mat) - 1) / 3)), 2)
      set.seed(42)
      tsne_res <- tryCatch(
        Rtsne::Rtsne(feat_mat, dims = 2, perplexity = perp,
                     max_iter = input$fcm_tsne_iter %||% 1000,
                     check_duplicates = FALSE, verbose = FALSE),
        error = function(e) NULL
      )
      if (is.null(tsne_res)) return()
      k     <- res$k
      pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
      df_plot <- data.frame(Dim1 = tsne_res$Y[, 1], Dim2 = tsne_res$Y[, 2],
                            Cluster = factor(as.integer(as.character(res$result_df$fcm_cluster)),
                                             levels = seq_len(k)))
      p <- .fcm_dim_reduction_plot(df_plot, k, pal_c, input$fcm_tsne_pt %||% 2,
                                   paste0("FCM \u2014 t-SNE | k=", k),
                                   paste0("Perplexity = ", perp),
                                   "t-SNE Dim 1", "t-SNE Dim 2")
      ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )

  output$dl_fcm_umap <- downloadHandler(
    filename = function() paste0("fcm_umap_k", input$fcm_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$fcm_result)
      res      <- rv$fcm_result
      feat_mat <- as.matrix(res$feat_df)
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      nn <- max(min(input$fcm_umap_nn %||% 15, nrow(feat_mat) - 1), 2)
      md <- input$fcm_umap_md %||% 0.1
      set.seed(42)
      umap_res <- tryCatch(
        uwot::umap(feat_mat, n_neighbors = nn, min_dist = md,
                   n_components = 2, verbose = FALSE),
        error = function(e) NULL
      )
      if (is.null(umap_res)) return()
      k     <- res$k
      pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
      df_plot <- data.frame(Dim1 = umap_res[, 1], Dim2 = umap_res[, 2],
                            Cluster = factor(as.integer(as.character(res$result_df$fcm_cluster)),
                                             levels = seq_len(k)))
      p <- .fcm_dim_reduction_plot(df_plot, k, pal_c, input$fcm_umap_pt %||% 2,
                                   paste0("FCM \u2014 UMAP | k=", k),
                                   paste0("n_neighbors = ", nn, "  |  min_dist = ", md),
                                   "UMAP Dim 1", "UMAP Dim 2")
      ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )

} # end fcm_server()
