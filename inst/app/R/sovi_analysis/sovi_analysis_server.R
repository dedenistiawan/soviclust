# =============================================================================
# R/sovi_analysis/sovi_analysis_server.R
# Server logic untuk menu SoVI Analysis
# Dipanggil dari dalam server() di server.R
# =============================================================================

sovi_analysis_server <- function(input, output, session, rv) {
  
  # ==========================================================================
  # REACTIVE: state internal SoVI Analysis
  # ==========================================================================
  sa <- reactiveValues(
    cls1      = NULL,   # hasil classify_variable_jenks untuk var1
    cls2      = NULL,   # hasil classify_variable_jenks untuk var2
    map1_ready = FALSE,
    map2_ready = FALSE,
    gvf_info  = NULL    # hasil find_optimal_k_gvf
  )
  
  # ==========================================================================
  # REACTIVE: daftar variabel sesuai sumber data
  # ==========================================================================
  sa_var_choices <- reactive({
    req(rv$data)
    src <- input$sa_data_source
    if (is.null(src)) return(character(0))
    
    get_variable_options(
      data_source  = src,
      raw_data     = rv$data,
      sovi_result  = rv$sovi_result,
      sovi_vars    = rv$sovi_vars
    )
  })
  
  # ==========================================================================
  # UPDATE: dropdown variabel saat sumber data berubah
  # ==========================================================================
  observeEvent(sa_var_choices(), {
    choices <- sa_var_choices()
    
    updateSelectInput(session, "sa_var1",
                      choices  = choices,
                      selected = if (length(choices) > 0) choices[1] else NULL)
    
    updateSelectInput(session, "sa_var2",
                      choices  = choices,
                      selected = if (length(choices) > 1) choices[2] else choices[1])
    
    # Reset state saat sumber data berubah
    sa$cls1       <- NULL
    sa$cls2       <- NULL
    sa$map1_ready <- FALSE
    sa$map2_ready <- FALSE
    sa$gvf_info   <- NULL
  })
  
  # ==========================================================================
  # OUTPUT: info box sumber data
  # ==========================================================================
  output$sa_source_info <- renderUI({
    src <- input$sa_data_source
    if (is.null(src)) return(NULL)
    
    info <- switch(src,
                   "raw"          = list(bg = "#fff8e1", bc = "#f39c12",
                                         ic = "exclamation-triangle",
                                         tx = "Raw values used directly."),
                   "raw_norm"     = list(bg = "#fff8e1", bc = "#f39c12",
                                         ic = "info-circle",
                                         tx = "Original data min-max normalized to [0,1]."),
                   "standardized" = list(bg = "#e3f2fd", bc = "#1a73c1",
                                         ic = "info-circle",
                                         tx = "Z-scores from SoVI standardization process."),
                   "sovi"         = list(bg = "#e8f5e9", bc = "#27ae60",
                                         ic = "check-circle",
                                         tx = "Single SoVI Score from SoVI computation."),
                   "rc"           = list(bg = "#e3f2fd", bc = "#1a73c1",
                                         ic = "info-circle",
                                         tx = "RC Scores (PCA components, normalized 0-1).")
    )
    
    div(class = "progress-box",
        style = paste0("background:", info$bg, "; border-left-color:", info$bc,
                       "; font-size:12px; margin-top:8px;"),
        icon(info$ic), " ", info$tx)
  })
  
  # ==========================================================================
  # OUTPUT: progress
  # ==========================================================================
  output$sa_progress <- renderUI({ NULL })
  
  # ==========================================================================
  # OBSERVER: Auto GVF — update slider ke k optimal
  # ==========================================================================
  observeEvent(input$sa_auto_gvf, {
    req(rv$data, input$sa_var1)
    
    tryCatch({
      x <- extract_variable_vector(
        var_name    = input$sa_var1,
        data_source = input$sa_data_source,
        raw_data    = rv$data,
        sovi_result = rv$sovi_result
      )
      
      gvf_res   <- find_optimal_k_gvf(x, gvf_threshold = input$sa_gvf_threshold)
      sa$gvf_info <- gvf_res
      
      updateSliderInput(session, "sa_n_classes", value = gvf_res$k_opt)
      
      showNotification(
        paste0("Auto GVF: k optimal = ", gvf_res$k_opt,
               " (GVF = ", gvf_res$gvf_df$gvf[gvf_res$gvf_df$k == gvf_res$k_opt], ")"),
        type = "message", duration = 5
      )
      
    }, error = function(e) {
      showNotification(paste("GVF error:", e$message), type = "error", duration = 8)
    })
  })
  
  # Output GVF result box
  output$sa_gvf_result <- renderUI({
    if (is.null(sa$gvf_info)) return(NULL)
    gvf_row <- sa$gvf_info$gvf_df[sa$gvf_info$gvf_df$k == sa$gvf_info$k_opt, ]
    gvf_val <- if (nrow(gvf_row) > 0) gvf_row$gvf[1] else NA
    
    div(class = "progress-box status-ok",
        style  = "font-size:12px; margin-top:6px;",
        icon("check"),
        paste0(" k optimal = ", sa$gvf_info$k_opt,
               " | GVF = ", ifelse(is.na(gvf_val), "N/A", gvf_val)))
  })
  
  # ==========================================================================
  # OBSERVER: Tombol Tampilkan Peta — Run klasifikasi
  # ==========================================================================
  observeEvent(input$sa_run, {
    req(rv$data, rv$sovi_result, rv$shp, input$sa_var1, input$sa_var2)
    
    output$sa_progress <- renderUI({
      div(class = "progress-box",
          style = "background:#cce5ff;",
          icon("spinner", class = "fa-spin"),
          " Computing classification and rendering map...")
    })
    
    tryCatch({
      k   <- input$sa_n_classes
      src <- input$sa_data_source
      
      # ── Klasifikasi Variabel 1 ──────────────────────────────────────────
      x1 <- extract_variable_vector(input$sa_var1, src, rv$data, rv$sovi_result)
      sa$cls1 <- classify_variable_jenks(x1, k = k, var_label = input$sa_var1)
      
      # ── Klasifikasi Variabel 2 ──────────────────────────────────────────
      x2 <- extract_variable_vector(input$sa_var2, src, rv$data, rv$sovi_result)
      sa$cls2 <- classify_variable_jenks(x2, k = k, var_label = input$sa_var2)
      
      sa$map1_ready <- TRUE
      sa$map2_ready <- TRUE
      
      output$sa_progress <- renderUI({
        div(class = "progress-box status-ok",
            icon("check"),
            paste0(" Peta berhasil dibuat! k = ", k,
                   " | Variabel: ", input$sa_var1, " & ", input$sa_var2))
      })
      
    }, error = function(e) {
      sa$map1_ready <- FALSE
      sa$map2_ready <- FALSE
      output$sa_progress <- renderUI({
        div(class = "progress-box status-err",
            icon("times"), " Error: ", e$message)
      })
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # ==========================================================================
  # OUTPUT: Info bar — Peta Tunggal
  # OUTPUT: Info bar — Single Map
  # ==========================================================================
  output$sa_single_infobar <- renderUI({
    if (!sa$map1_ready || is.null(sa$cls1)) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12; font-size:13px;",
                 icon("exclamation-triangle"),
                 " Select data source, variable, number of classes, then click ",
                 tags$strong("Generate Map.")))
    }
    div(class = "progress-box status-ok",
        style  = "font-size:13px;",
        icon("map"), " ",
        tags$strong("Variable: "), input$sa_var1, " | ",
        tags$strong("Source: "), get_source_label(input$sa_data_source), " | ",
        tags$strong("k = "), sa$cls1$k
    )
  })
  
  # ==========================================================================
  # OUTPUT: Single Map (Leaflet)
  # ==========================================================================
  output$sa_map_single <- leaflet::renderLeaflet({
    req(sa$map1_ready, !is.null(sa$cls1), rv$shp)
    
    build_leaflet_sovi_analysis(
      shp           = rv$shp,
      cls_result    = sa$cls1,
      join_shp      = input$join_shp,
      sovi_df       = if (input$sa_show_centroid) rv$sovi_result$sovi_df else NULL,
      join_df       = if (input$sa_show_centroid) input$id_col else NULL,
      name_col      = input$name_col,
      show_centroid = input$sa_show_centroid,
      var_label     = input$sa_var1
    )
  })
  
  # ==========================================================================
  # OUTPUT: Info bar — Side-by-Side
  # ==========================================================================
  output$sa_sidebyside_infobar <- renderUI({
    if (!sa$map1_ready || !sa$map2_ready) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12; font-size:13px;",
                 icon("exclamation-triangle"),
                 " Click ", tags$strong("Generate Map"), " first."))
    }
    div(class = "progress-box status-ok",
        style  = "font-size:13px;",
        icon("arrow-up"), " ",
        tags$strong("Top: "), input$sa_var1, " | ",
        tags$strong("Bottom: "), input$sa_var2, " | ",
        tags$strong("Source: "), get_source_label(input$sa_data_source), " | ",
        tags$strong("k = "), input$sa_n_classes
    )
  })
  
  # ==========================================================================
  # OUTPUT: Top-Bottom — two full-width synchronized leaflet maps
  # ==========================================================================
  output$sa_map_sidebyside_ui <- renderUI({
    if (!sa$map1_ready || !sa$map2_ready) return(NULL)
    
    # Layout top-bottom: each column(12) full width
    tagList(
      # ── Top Map ────────────────────────────────────────────────────────
      fluidRow(
        column(12,
               div(style = "font-weight:700; color:#1a73c1; margin-bottom:4px;
                       font-size:13px; border-left:4px solid #1a73c1;
                       padding-left:8px;",
                   icon("map"), " Top Map \u2014 ", input$sa_var1),
               leaflet::leafletOutput("sa_map_left", height = "440px")
        )
      ),
      tags$div(style = "height:10px;"),
      # ── Bottom Map ───────────────────────────────────────────────────────
      fluidRow(
        column(12,
               div(style = "font-weight:700; color:#27ae60; margin-bottom:4px;
                       font-size:13px; border-left:4px solid #27ae60;
                       padding-left:8px;",
                   icon("map"), " Bottom Map \u2014 ", input$sa_var2),
               leaflet::leafletOutput("sa_map_right", height = "440px")
        )
      )
    )
  })
  
  output$sa_map_left <- leaflet::renderLeaflet({
    req(sa$map1_ready, !is.null(sa$cls1), rv$shp)
    
    build_leaflet_sovi_analysis(
      shp           = rv$shp,
      cls_result    = sa$cls1,
      join_shp      = input$join_shp,
      sovi_df       = if (input$sa_show_centroid_left) rv$sovi_result$sovi_df else NULL,
      join_df       = if (input$sa_show_centroid_left) input$id_col else NULL,
      name_col      = input$name_col,
      show_centroid = input$sa_show_centroid_left,
      var_label     = input$sa_var1
    )
  })
  
  output$sa_map_right <- leaflet::renderLeaflet({
    req(sa$map2_ready, !is.null(sa$cls2), rv$shp)
    
    build_leaflet_sovi_analysis(
      shp           = rv$shp,
      cls_result    = sa$cls2,
      join_shp      = input$join_shp,
      sovi_df       = if (input$sa_show_centroid_right) rv$sovi_result$sovi_df else NULL,
      join_df       = if (input$sa_show_centroid_right) input$id_col else NULL,
      name_col      = input$name_col,
      show_centroid = input$sa_show_centroid_right,
      var_label     = input$sa_var2
    )
  })
  
  # Zoom & Pan for each map are independent (not synchronized)
  
  # ==========================================================================
  # OUTPUT: Summary Tab — header
  # ==========================================================================
  output$sa_summary_header <- renderUI({
    if (!sa$map1_ready) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12;",
                 icon("exclamation-triangle"),
                 " Click Generate Map to view the summary."))
    }
    div(class = "progress-box status-ok",
        icon("table"),
        paste0(" Jenks classification summary \u2014 k = ", input$sa_n_classes,
               " | Source: ", get_source_label(input$sa_data_source)))
  })
  
  output$sa_summary_title1 <- renderUI({
    if (is.null(sa$cls1)) return(NULL)
    tags$span(icon("map"), " Variable 1: ", input$sa_var1)
  })
  
  output$sa_summary_title2 <- renderUI({
    if (is.null(sa$cls2)) return(NULL)
    tags$span(icon("map"), " Variable 2: ", input$sa_var2)
  })
  
  # ==========================================================================
  # OUTPUT: Summary table Variable 1
  # ==========================================================================
  output$sa_summary_table1 <- DT::renderDT({
    req(!is.null(sa$cls1))
    df <- build_summary_table(sa$cls1)
    names(df) <- c("No", "Range", "Min", "Max", "n", "Percent (%)")
    DT::datatable(df,
                  options  = list(dom = "t", pageLength = 12),
                  rownames = FALSE,
                  caption  = paste0("Distribution per Class \u2014 ", input$sa_var1))
  })
  
  # ==========================================================================
  # OUTPUT: Summary table Variable 2
  # ==========================================================================
  output$sa_summary_table2 <- DT::renderDT({
    req(!is.null(sa$cls2))
    df <- build_summary_table(sa$cls2)
    names(df) <- c("No", "Range", "Min", "Max", "n", "Percent (%)")
    DT::datatable(df,
                  options  = list(dom = "t", pageLength = 12),
                  rownames = FALSE,
                  caption  = paste0("Distribution per Class \u2014 ", input$sa_var2))
  })
  
  # ==========================================================================
  # OUTPUT: GVF Plot Variable 1
  # ==========================================================================
  output$sa_gvf_plot1 <- renderPlot({
    req(!is.null(sa$cls1), rv$data, input$sa_var1)
    
    x <- extract_variable_vector(
      input$sa_var1, input$sa_data_source, rv$data, rv$sovi_result
    )
    gvf_df <- find_optimal_k_gvf(x, gvf_threshold = input$sa_gvf_threshold)$gvf_df
    k_used <- sa$cls1$k
    
    ggplot2::ggplot(gvf_df, ggplot2::aes(x = k, y = gvf)) +
      ggplot2::geom_line(color = "#1a73c1", linewidth = 1.0) +
      ggplot2::geom_point(color = "#1a73c1", size = 2.5) +
      ggplot2::geom_point(
        data  = gvf_df[gvf_df$k == k_used, ],
        ggplot2::aes(x = k, y = gvf),
        color = "#e74c3c", size = 5, shape = 18
      ) +
      ggplot2::geom_hline(
        yintercept = input$sa_gvf_threshold,
        linetype   = "dashed",
        color      = "#f39c12",
        linewidth  = 0.8
      ) +
      ggplot2::annotate("text",
                        x     = min(gvf_df$k) + 0.2,
                        y     = input$sa_gvf_threshold + 0.01,
                        label = paste0("Threshold = ", input$sa_gvf_threshold),
                        hjust = 0, size = 3.2, color = "#f39c12"
      ) +
      ggplot2::scale_x_continuous(breaks = gvf_df$k) +
      ggplot2::labs(
        title = paste0("GVF — ", input$sa_var1),
        x     = "Number of Classes (k)",
        y     = "GVF"
      ) +
      ggplot2::theme_minimal(base_size = 10)
  })
  
  # ==========================================================================
  # OUTPUT: GVF Plot Variabel 2
  # ==========================================================================
  output$sa_gvf_plot2 <- renderPlot({
    req(!is.null(sa$cls2), rv$data, input$sa_var2)
    
    x <- extract_variable_vector(
      input$sa_var2, input$sa_data_source, rv$data, rv$sovi_result
    )
    gvf_df <- find_optimal_k_gvf(x, gvf_threshold = input$sa_gvf_threshold)$gvf_df
    k_used <- sa$cls2$k
    
    ggplot2::ggplot(gvf_df, ggplot2::aes(x = k, y = gvf)) +
      ggplot2::geom_line(color = "#27ae60", linewidth = 1.0) +
      ggplot2::geom_point(color = "#27ae60", size = 2.5) +
      ggplot2::geom_point(
        data  = gvf_df[gvf_df$k == k_used, ],
        ggplot2::aes(x = k, y = gvf),
        color = "#e74c3c", size = 5, shape = 18
      ) +
      ggplot2::geom_hline(
        yintercept = input$sa_gvf_threshold,
        linetype   = "dashed",
        color      = "#f39c12",
        linewidth  = 0.8
      ) +
      ggplot2::annotate("text",
                        x     = min(gvf_df$k) + 0.2,
                        y     = input$sa_gvf_threshold + 0.01,
                        label = paste0("Threshold = ", input$sa_gvf_threshold),
                        hjust = 0, size = 3.2, color = "#f39c12"
      ) +
      ggplot2::scale_x_continuous(breaks = gvf_df$k) +
      ggplot2::labs(
        title = paste0("GVF — ", input$sa_var2),
        x     = "Number of Classes (k)",
        y     = "GVF"
      ) +
      ggplot2::theme_minimal(base_size = 10)
  })
  
} # end sovi_analysis_server()