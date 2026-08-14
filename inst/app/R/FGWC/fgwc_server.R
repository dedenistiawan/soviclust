# =============================================================================
# R/FGWC/fgwc_server.R
# Server Logic Tab FGWC
#
# DIPANGGIL DARI : server.R via fgwc_server(input, output, session, rv)
# DEPENDENSI     :
#   - R/FGWC/fgwc_wrapper.R  (run_fgwc_shiny, build_leaflet_fgwc,
#                              read_uploaded_file, parse_distance_matrix,
#                              parse_population, haversine_matrix, %||%)
#   - R/core/helpers.R        (normalize_id)
# =============================================================================

fgwc_server <- function(input, output, session, rv) {

  rv_fgwc_sample_dist <- reactiveVal(NULL)
  rv_fgwc_sample_pop  <- reactiveVal(NULL)

  observeEvent(input$fgwc_load_sample, {
    withProgress(message = "Memuat data sampel FGWC...", value = 0, {
      extdata <- system.file("extdata", package = "soviclust")
      mode <- input$fgwc_dist_mode %||% "matrix"
      incProgress(0.3)
      if (mode == "lonlat") {
        df_c <- as.data.frame(readxl::read_excel(file.path(extdata, "Koordinat.xlsx")))
        cols <- tolower(names(df_c))
        lon_col <- names(df_c)[which(cols %in% c("longitude","lon","long","x"))[1]]
        lat_col <- names(df_c)[which(cols %in% c("latitude","lat","y"))[1]]
        id_col  <- names(df_c)[which(cols %in% c("districtcode","id","kode","code"))[1]]
        mat <- haversine_matrix(as.numeric(df_c[[lon_col]]), as.numeric(df_c[[lat_col]]))
        if (!is.na(id_col)) { rownames(mat) <- df_c[[id_col]]; colnames(mat) <- df_c[[id_col]] }
        attr(mat, "dist_mode") <- "lonlat"; attr(mat, "dist_unit") <- "kilometer"
      } else {
        df_d <- as.data.frame(readxl::read_excel(file.path(extdata, "Distance_matrix_514.xlsx")))
        mat  <- parse_distance_matrix(df_d)
        attr(mat, "dist_mode") <- "matrix"; attr(mat, "dist_unit") <- "unit asli"
      }
      rv_fgwc_sample_dist(mat)
      incProgress(0.7)
      rv_pop <- as.data.frame(readxl::read_excel(file.path(extdata, "sovi_data_pop_514.xlsx")))
      rv_fgwc_sample_pop(parse_population(rv_pop))
      incProgress(1.0)
    })
    showNotification(paste0("✓ Data sampel FGWC dimuat: ", nrow(rv_fgwc_sample_dist()), "x", ncol(rv_fgwc_sample_dist()), " | pop ", length(rv_fgwc_sample_pop())), type = "message", duration = 5)
  })

  
  # ==========================================================================
  # REACTIVE: Baca matriks jarak dari upload
  # Mendukung 2 mode: upload matrix langsung ATAU hitung dari lon/lat
  # ==========================================================================
  
  rv_fgwc_dist <- reactive({
    if (!is.null(rv_fgwc_sample_dist())) return(rv_fgwc_sample_dist())

    mode <- input$fgwc_dist_mode %||% "matrix"
    
    if (mode == "matrix") {
      # â”€â”€ Mode 1: Upload matriks nÃ—n langsung â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      req(input$fgwc_file_dist)
      tryCatch({
        df  <- read_uploaded_file(input$fgwc_file_dist)
        mat <- parse_distance_matrix(df)
        attr(mat, "dist_mode") <- "matrix"
        attr(mat, "dist_unit") <- "unit asli"
        mat
      }, error = function(e) {
        showNotification(paste("Error matriks jarak:", e$message),
                         type = "error", duration = 8)
        NULL
      })
      
    } else {
      # â”€â”€ Mode 2: Hitung Haversine dari lon/lat â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      req(input$fgwc_file_lonlat)
      tryCatch({
        df   <- read_uploaded_file(input$fgwc_file_lonlat)
        cols <- tolower(names(df))
        
        # Deteksi kolom lon/lat secara fleksibel
        lon_col <- names(df)[which(cols %in% c("longitude","lon","long","x"))[1]]
        lat_col <- names(df)[which(cols %in% c("latitude","lat","y"))[1]]
        id_col  <- names(df)[which(cols %in% c("districtcode","id","kode","code"))[1]]
        
        if (is.na(lon_col)) stop("Kolom longitude tidak ditemukan.")
        if (is.na(lat_col)) stop("Kolom latitude tidak ditemukan.")
        
        lon <- as.numeric(df[[lon_col]])
        lat <- as.numeric(df[[lat_col]])
        
        if (any(is.na(lon)) || any(is.na(lat)))
          stop("Terdapat nilai NA pada kolom longitude/latitude.")
        if (any(lon < -180 | lon > 180))
          stop("Nilai longitude di luar rentang -180 hingga 180.")
        if (any(lat < -90 | lat > 90))
          stop("Nilai latitude di luar rentang -90 hingga 90.")
        
        mat <- haversine_matrix(lon, lat)
        
        # Beri nama baris/kolom dari ID jika tersedia
        if (!is.na(id_col)) {
          rownames(mat) <- df[[id_col]]
          colnames(mat) <- df[[id_col]]
        }
        
        attr(mat, "dist_mode") <- "lonlat"
        attr(mat, "dist_unit") <- "kilometer"
        mat
        
      }, error = function(e) {
        showNotification(paste("Error lon/lat:", e$message),
                         type = "error", duration = 8)
        NULL
      })
    }
  })
  
  # ==========================================================================
  # REACTIVE: Baca vektor populasi dari upload
  # ==========================================================================
  
  rv_fgwc_pop <- reactive({
    if (!is.null(rv_fgwc_sample_pop())) return(rv_fgwc_sample_pop())

    req(input$fgwc_file_pop)
    tryCatch({
      df  <- read_uploaded_file(input$fgwc_file_pop)
      pop <- parse_population(df)
      pop
    }, error = function(e) {
      showNotification(paste("Error data populasi:", e$message),
                       type = "error", duration = 8)
      NULL
    })
  })
  
  # ==========================================================================
  # OUTPUT: Status upload matriks jarak
  # ==========================================================================
  
  output$fgwc_dist_status <- renderUI({
    mat <- rv_fgwc_dist()
    if (is.null(mat)) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12;
                          font-size:12px;",
                 icon("exclamation-triangle"),
                 " Belum diupload atau error."))
    }
    mode <- attr(mat, "dist_mode") %||% "matrix"
    unit <- attr(mat, "dist_unit") %||% "unit asli"
    
    if (mode == "lonlat") {
      div(class = "progress-box status-ok", style = "font-size:12px;",
          icon("check"),
          paste0(" Haversine distance dihitung: ", nrow(mat), " \u00d7 ", ncol(mat)),
          tags$br(),
          tags$span(style = "color:#27ae60;", icon("ruler"), " Satuan: kilometer"))
    } else {
      div(class = "progress-box status-ok", style = "font-size:12px;",
          icon("check"),
          paste0(" Matriks dimuat: ", nrow(mat), " \u00d7 ", ncol(mat)),
          tags$br(),
          tags$span(style = "color:#78909c;", icon("ruler"), " Satuan: ", unit))
    }
  })
  
  # ==========================================================================
  # OUTPUT: Status upload populasi
  # ==========================================================================
  
  output$fgwc_pop_status <- renderUI({
    pop <- rv_fgwc_pop()
    if (is.null(pop)) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12;
                          font-size:12px;",
                 icon("exclamation-triangle"), " Belum diupload atau error."))
    }
    div(class = "progress-box status-ok", style = "font-size:12px;",
        icon("check"), paste0(" Populasi dimuat: ", length(pop), " unit"))
  })
  
  # ==========================================================================
  # OUTPUT: Var selector dinamis (raw/raw_norm/standardized)
  # ==========================================================================
  
  output$fgwc_var_selector <- renderUI({
    req(rv$data, rv$sovi_vars)
    num_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    src      <- input$fgwc_data_source %||% "raw"
    
    note <- switch(src,
                   "raw" = div(class = "progress-box",
                               style = "background:#fff8e1;border-left-color:#f39c12;
                           font-size:11px;margin-bottom:6px;",
                               icon("exclamation-triangle"), " Nilai mentah tanpa transformasi."),
                   "raw_norm" = div(class = "progress-box",
                                    style = "background:#fff8e1;border-left-color:#f39c12;
                                font-size:11px;margin-bottom:6px;",
                                    icon("info-circle"), " Akan dinormalisasi min-max 0\u20131."),
                   "standardized" = div(class = "progress-box",
                                        style = "background:#e3f2fd;border-left-color:#1a73c1;
                                    font-size:11px;margin-bottom:6px;",
                                        icon("info-circle"), " Z-score dari proses SoVI."),
                   NULL
    )
    
    tagList(
      note,
      checkboxGroupInput("fgwc_selected_vars", NULL,
                         choices  = num_cols,
                         selected = rv$sovi_vars)
    )
  })
  
  # ==========================================================================
  # OUTPUT: Info sumber data (sovi/rc)
  # ==========================================================================
  
  output$fgwc_datasource_info <- renderUI({
    src  <- input$fgwc_data_source
    if (is.null(src)) return(NULL)
    info <- switch(src,
                   "sovi" = "Menggunakan SoVI Score tunggal (0\u20131) sebagai fitur clustering.",
                   "rc"   = "Menggunakan skor komponen RC (PCA Varimax, ternormalisasi 0\u20131).",
                   NULL
    )
    if (is.null(info)) return(NULL)
    div(class = "progress-box",
        style = "background:#e3f2fd; border-left-color:#1a73c1;
                 font-size:12px; margin-top:6px;",
        icon("info-circle"), " ", info)
  })
  
  # ==========================================================================
  # OUTPUT: Progress box (kosong saat awal)
  # ==========================================================================
  
  output$fgwc_progress <- renderUI({ NULL })
  
  # ==========================================================================
  # OBSERVER: Tombol Run FGWC
  # ==========================================================================
  
  observeEvent(input$run_fgwc, {
    
    # â”€â”€ Validasi prasyarat â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    if (is.null(rv$data)) {
      showNotification("Upload dataset terlebih dahulu!", type = "warning")
      return()
    }
    if (is.null(rv_fgwc_dist())) {
      showNotification("Upload matriks jarak terlebih dahulu!", type = "warning")
      return()
    }
    if (is.null(rv_fgwc_pop())) {
      showNotification("Upload data populasi terlebih dahulu!", type = "warning")
      return()
    }
    if (input$fgwc_data_source %in% c("sovi","rc","standardized") &&
        is.null(rv$sovi_result)) {
      showNotification("Jalankan SoVI Computation terlebih dahulu!", type = "warning")
      return()
    }
    
    output$fgwc_progress <- renderUI({
      div(class = "progress-box", style = "background:#cce5ff;",
          icon("spinner", class = "fa-spin"),
          paste0(" Menjalankan FGWC (", toupper(input$fgwc_algorithm), ")..."))
    })
    
    # â”€â”€ Kumpulkan variabel yang dipilih â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    sel_vars <- if (input$fgwc_data_source %in% c("raw","raw_norm","standardized"))
      input$fgwc_selected_vars else NULL
    
    # â”€â”€ Kumpulkan parameter FGWC â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    fgwc_params <- list(
      m        = input$fgwc_m,
      distance = "euclidean",
      order    = 3,
      alpha    = input$fgwc_alpha,
      a        = input$fgwc_a,
      b        = input$fgwc_b,
      max.iter = input$fgwc_maxiter,
      error    = 1e-6,
      randomN  = input$fgwc_seed
    )
    
    # â”€â”€ Kumpulkan parameter algoritma â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    algo       <- input$fgwc_algorithm
    opt_params <- list(
      npar       = input$fgwc_npar,
      same       = input$fgwc_same,
      vi_dist    = input$fgwc_vi_dist,
      n_onlooker = input$fgwc_abc_onlooker,
      limit      = input$fgwc_abc_limit,
      p          = input$fgwc_fpa_p,
      gamma      = input$fgwc_fpa_gamma,
      lambda     = input$fgwc_fpa_lambda,
      ei_distr   = input$fgwc_fpa_ei,
      chaos      = 3,
      G          = input$fgwc_gsa_G,
      vmax       = input$fgwc_gsa_vmax,
      new        = input$fgwc_gsa_new,
      hho_algo   = input$fgwc_hho_algo,
      a1         = input$fgwc_hho_a1,
      a2         = input$fgwc_hho_a2,
      a3         = input$fgwc_hho_a3,
      par_no     = input$fgwc_ifa_parno,
      beta       = input$fgwc_ifa_beta,
      vmax       = input$fgwc_pso_vmax,
      c1         = input$fgwc_pso_c1,
      c2         = input$fgwc_pso_c2,
      type       = input$fgwc_pso_type,
      wmax       = input$fgwc_pso_wmax,
      wmin       = input$fgwc_pso_wmin,
      map        = 0.3,
      nselection = input$fgwc_tlbo_nselect,
      elitism    = input$fgwc_tlbo_elitism,
      n_elite    = input$fgwc_tlbo_nelite,
      woa_b      = input$fgwc_woa_b
    )
    
    withProgress(message = paste("Running FGWC", toupper(algo), "..."), value = 0, {
      incProgress(0.2, detail = "Membangun matriks fitur...")
      
      result <- tryCatch({
        run_fgwc_shiny(
          data_source   = input$fgwc_data_source,
          raw_data      = rv$data,
          sovi_result   = rv$sovi_result,
          selected_vars = sel_vars,
          pop_vec       = rv_fgwc_pop(),
          dist_mat      = rv_fgwc_dist(),
          algorithm     = algo,
          ncluster      = input$fgwc_ncluster,
          fgwc_params   = fgwc_params,
          opt_params    = opt_params,
          id_col        = input$id_col,
          name_col      = input$name_col
        )
      }, error = function(e) {
        showNotification(paste("Error FGWC:", e$message),
                         type = "error", duration = 15)
        NULL
      })
      
      incProgress(0.8, detail = "Selesai.")
      rv$cga_result_fgwc <- result
    })
    
    output$fgwc_progress <- renderUI({
      res <- rv$cga_result_fgwc
      if (!is.null(res))
        div(class = "progress-box status-ok", icon("check"),
            paste0(" Selesai! k=", res$k,
                   ", Algo=", toupper(res$algorithm),
                   ", Silhouette=", res$sil_mean,
                   ", f_obj=", round(res$f_obj, 4),
                   ", Iter=", res$iteration))
      else
        div(class = "progress-box status-err",
            icon("times"), " Gagal. Periksa data & parameter.")
    })
  })
  
  # ==========================================================================
  # TAB 1: RINGKASAN
  # ==========================================================================
  
  output$fgwc_summary <- renderUI({
    res <- rv$cga_result_fgwc
    if (is.null(res)) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12;",
                 icon("exclamation-triangle"),
                 " Jalankan FGWC dengan tombol di panel kiri."))
    }
    
    src_label <- switch(res$data_source,
                        "raw"          = "Data Asli (tanpa transformasi)",
                        "raw_norm"     = "Data Asli Ternormalisasi (0-1)",
                        "standardized" = "Data Z-score",
                        "sovi"         = "SoVI Score",
                        "rc"           = "Skor RC (komponen PCA)",
                        res$data_source
    )
    
    algo_labels <- c(
      classic = "Classic FGWC",
      abc     = "ABC \u2014 Artificial Bee Colony",
      fpa     = "FPA \u2014 Flower Pollination Algorithm",
      gsa     = "GSA \u2014 Gravitational Search Algorithm",
      gwo     = "GWO \u2014 Grey Wolf Optimizer",
      hho     = "HHO \u2014 Harris-Hawk Optimization",
      ifa     = "IFA \u2014 Intelligent Firefly Algorithm",
      pso     = "PSO \u2014 Particle Swarm Optimization",
      tlbo    = "TLBO \u2014 Teaching-Learning Based Optimization",
      woa     = "WOA \u2014 Whale Optimization Algorithm"
    )
    
    sil_color <- if (!is.na(res$sil_mean) && res$sil_mean >= 0.5)
      "color:#27ae60;font-weight:700;"
    else if (!is.na(res$sil_mean) && res$sil_mean >= 0.25)
      "color:#f39c12;font-weight:700;"
    else
      "color:#e74c3c;font-weight:700;"
    
    div(
      fluidRow(
        column(4, div(class = "info-card",
                      tags$h4(icon("microchip"), " Algoritma"),
                      tags$p(style = "font-size:13px; font-weight:700; color:#1a73c1;",
                             algo_labels[res$algorithm]),
                      tags$p(style = "font-size:12px; color:#78909c;",
                             "Sumber data: ", src_label),
                      tags$p(style = "font-size:12px; color:#78909c;",
                             "Fitur: ", length(res$feat_cols), " dimensi"),
                      tags$p(style = "font-size:12px; color:#78909c;",
                             "Iterasi: ", res$iteration)
        )),
        column(4, div(class = "info-card",
                      tags$h4(icon("object-group"), " Hasil Clustering"),
                      tags$p(style = "font-size:13px;",
                             tags$strong("k = "), res$k),
                      tags$p(style = "font-size:13px;",
                             tags$strong("Objective Function = "),
                             tags$span(style = "font-weight:700; color:#1a73c1;",
                                       round(res$f_obj, 4))),
                      tags$p(style = "font-size:13px;",
                             tags$strong("Mean Silhouette = "),
                             tags$span(style = sil_color,
                                       if (is.na(res$sil_mean)) "N/A" else res$sil_mean))
        )),
        column(4, div(class = "info-card",
                      tags$h4(icon("chart-bar"), " Distribusi Cluster"),
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
                          tags$h4(icon("lightbulb"), " Interpretasi Silhouette"),
                          tags$ul(style = "font-size:12.5px; margin:0;",
                                  tags$li(tags$span(style="color:#27ae60;font-weight:600;","\u2265 0.50"),
                                          " \u2014 Struktur cluster kuat"),
                                  tags$li(tags$span(style="color:#f39c12;font-weight:600;","0.25\u20130.49"),
                                          " \u2014 Struktur cluster moderat"),
                                  tags$li(tags$span(style="color:#e74c3c;font-weight:600;","< 0.25"),
                                          " \u2014 Struktur cluster lemah, pertimbangkan k lain")
                          )
                      )
      ))
    )
  })
  
  # ==========================================================================
  # TAB 2: VALIDASI + KONVERGENSI
  # ==========================================================================
  
  output$fgwc_val_table <- DT::renderDT({
    req(rv$cga_result_fgwc)
    DT::datatable(rv$cga_result_fgwc$val_df,
                  options  = list(dom = "t", pageLength = 10),
                  rownames = FALSE,
                  caption  = "Indeks Validasi Cluster FGWC")
  })
  
  output$fgwc_conv_plot <- renderPlot({
    req(rv$cga_result_fgwc)
    conv <- rv$cga_result_fgwc$conv
    df   <- data.frame(Iterasi = seq_along(conv), Obj = conv)
    ggplot2::ggplot(df, ggplot2::aes(x = Iterasi, y = Obj)) +
      ggplot2::geom_line(color = "#1a73c1", linewidth = 1.0) +
      ggplot2::geom_point(color = "#1a73c1", size = 1.5, alpha = 0.5) +
      ggplot2::labs(
        title = paste("Konvergensi Objective Function \u2014",
                      toupper(rv$cga_result_fgwc$algorithm)),
        x = "Iterasi", y = "Objective Function"
      ) +
      ggplot2::theme_minimal(base_size = 11)
  })
  
  # ==========================================================================
  # TAB 3: PETA INTERAKTIF
  # ==========================================================================
  
  output$fgwc_map <- leaflet::renderLeaflet({
    req(rv$cga_result_fgwc, rv$shp)
    res <- rv$cga_result_fgwc
    build_leaflet_fgwc(
      result_df   = res$result_df,
      shp         = rv$shp,
      join_shp    = input$join_shp,
      join_df     = res$id_col,
      name_col    = res$name_col,
      k           = res$k,
      cluster_col = "fgwc_cluster"
    )
  })
  
  # ==========================================================================
  # TAB 4: SILHOUETTE
  # ==========================================================================
  
  output$fgwc_sil_plot <- renderPlot({
    req(rv$cga_result_fgwc)
    res <- rv$cga_result_fgwc
    if (is.null(res$sil_obj)) {
      plot.new()
      text(0.5, 0.5, "Silhouette tidak tersedia.", cex = 1.1, col = "grey50")
      return()
    }
    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set1")[seq_len(k)]
    plot(res$sil_obj,
         main = paste0("Silhouette \u2014 ", toupper(res$algorithm),
                       ", k=", k, "\nMean = ", res$sil_mean),
         col  = pal_c)
  })
  
  output$fgwc_sil_table <- DT::renderDT({
    req(rv$cga_result_fgwc)
    res <- rv$cga_result_fgwc
    if (is.null(res$sil_obj))
      return(DT::datatable(data.frame(Info = "N/A")))
    sil_sum <- summary(res$sil_obj)$clus.avg.widths
    df <- data.frame(cluster       = factor(seq_along(sil_sum)),
                     avg_sil_width = round(sil_sum, 4))
    DT::datatable(df, options = list(dom = "t"), rownames = FALSE,
                  caption = "Avg. Silhouette Width per Cluster")
  })
  
  output$fgwc_sil_interp <- renderUI({
    req(rv$cga_result_fgwc)
    s <- rv$cga_result_fgwc$sil_mean
    if (is.na(s)) return(NULL)
    cls   <- if (s >= 0.50) "status-ok" else if (s >= 0.25) "status-warn" else "status-err"
    label <- if (s >= 0.50) "Struktur Kuat \u2713"
    else if (s >= 0.25) "Struktur Moderat"
    else "Struktur Lemah \u2717"
    div(class = paste("progress-box", cls),
        icon("tachometer-alt"),
        paste0(" Mean Silhouette = ", s, " \u2014 ", label))
  })
  
  # ==========================================================================
  # TAB 5: PROFIL CLUSTER
  # ==========================================================================
  
  output$fgwc_profile_table <- DT::renderDT({
    req(rv$cga_result_fgwc)
    df       <- rv$cga_result_fgwc$profile
    num_cols <- setdiff(names(df), "cluster")
    df[, num_cols] <- round(df[, num_cols, drop = FALSE], 3)
    DT::datatable(df, options = list(dom = "t", scrollX = TRUE),
                  rownames = FALSE,
                  caption  = "Profil Cluster: Mean Fitur per Cluster")
  })
  
  output$fgwc_heatmap <- renderPlot({
    req(rv$cga_result_fgwc)
    res       <- rv$cga_result_fgwc
    feat_cols <- res$feat_cols
    long      <- tidyr::pivot_longer(res$profile,
                                     cols      = dplyr::all_of(feat_cols),
                                     names_to  = "Fitur",
                                     values_to = "Mean_Score")
    long$cluster <- factor(long$cluster)
    ggplot2::ggplot(long, ggplot2::aes(x = Fitur, y = cluster,
                                       fill = Mean_Score)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.6) +
      ggplot2::geom_text(ggplot2::aes(label = round(Mean_Score, 2)),
                         size = 3.0, color = "black") +
      ggplot2::scale_fill_distiller(palette = "RdYlBu", direction = -1,
                                    name = "Mean\nScore") +
      ggplot2::labs(
        title = paste("Heatmap Profil Cluster \u2014", toupper(res$algorithm)),
        x = "Fitur / Dimensi", y = "Cluster"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9))
  })
  
  output$fgwc_radar <- renderPlot({
    req(rv$cga_result_fgwc)
    res       <- rv$cga_result_fgwc
    feat_cols <- res$feat_cols
    k         <- res$k
    if (length(feat_cols) < 3) {
      plot.new()
      text(0.5, 0.5, "Radar chart membutuhkan minimal 3 fitur.",
           cex = 1.1, col = "grey50")
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
      mtext(paste0("n = ", res$profile$n[i]),
            side = 1, line = 0.2, cex = 0.7)
    }
    plot.new()
    legend("center", legend = cluster_labels, col = pal_rad,
           lwd = 3, bty = "n", title = "Cluster", cex = 0.85)
    mtext(paste0("Profil Radar FGWC (", toupper(res$algorithm),
                 ") \u2014 nilai ternormalisasi 0\u20131"),
          outer = TRUE, line = 0.5, cex = 1.0, font = 2)
  })
  
  # ==========================================================================
  # TAB 6: DATA CLUSTER
  # ==========================================================================
  
  output$fgwc_result_table <- DT::renderDT({
    req(rv$cga_result_fgwc)
    res  <- rv$cga_result_fgwc
    df   <- res$result_df
    show_cols <- unique(c(res$id_col, res$name_col, "fgwc_cluster",
                          if ("sovi_score" %in% names(df)) "sovi_score",
                          if ("vuln_class" %in% names(df)) "vuln_class"))
    show_cols <- show_cols[show_cols %in% names(df)]
    show_df   <- df[, show_cols, drop = FALSE]
    num_cols  <- show_cols[sapply(show_df, is.numeric)]
    show_df[, num_cols] <- round(show_df[, num_cols, drop = FALSE], 4)
    DT::datatable(show_df, filter = "top",
                  options  = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE,
                  caption  = paste0("Hasil FGWC \u2014 Algoritma: ",
                                    toupper(res$algorithm), " | k=", res$k))
  })

  # ==========================================================================
  # DOWNLOADS
  # ==========================================================================

  output$dl_fgwc_csv <- downloadHandler(
    filename = function() paste0("fgwc_", input$fgwc_algorithm, "_k", input$fgwc_ncluster, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$cga_result_fgwc)
      res       <- rv$cga_result_fgwc
      df        <- res$result_df
      keep_cols <- unique(c(res$id_col, res$name_col, "fgwc_cluster",
                            if ("sovi_score" %in% names(df)) "sovi_score",
                            if ("vuln_class" %in% names(df)) "vuln_class"))
      keep_cols <- keep_cols[keep_cols %in% names(df)]
      write.csv(df[, keep_cols], file, row.names = FALSE)
    }
  )

  output$dl_fgwc_map_png <- downloadHandler(
    filename = function() paste0("map_fgwc_", input$fgwc_algorithm, "_k", input$fgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_fgwc, rv$shp)
      res       <- rv$cga_result_fgwc
      result_df <- res$result_df
      shp       <- rv$shp
      k         <- res$k
      result_df[[res$id_col]] <- normalize_id(result_df[[res$id_col]])
      shp[[input$join_shp]]   <- normalize_id(shp[[input$join_shp]])
      peta              <- dplyr::left_join(shp, result_df, by = setNames(res$id_col, input$join_shp))
      peta$fgwc_cluster <- as.factor(peta$fgwc_cluster)
      pal_c             <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set1")[seq_len(k)]
      tmap::tmap_mode("plot")
      m <- tmap::tm_shape(peta) +
        tmap::tm_polygons(fill = "fgwc_cluster", fill.scale = tmap::tm_scale_categorical(values = pal_c),
                          fill.legend = tmap::tm_legend(title = "FGWC Cluster"), col = "grey40", lwd = 0.3) +
        tmap::tm_title(paste0("FGWC \u2014 ", toupper(res$algorithm), " | k=", k)) +
        tmap::tm_compass(type = "arrow", position = c("left","bottom"), size = 1.5) +
        tmap::tm_scalebar(position = c("left","bottom"), text.size = 0.6) +
        tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "right")
      tmap::tmap_save(m, filename = file, width = 3000, height = 2400, dpi = 300)
      tmap::tmap_mode("view")
    }
  )

  output$dl_fgwc_heatmap <- downloadHandler(
    filename = function() paste0("fgwc_heatmap_", input$fgwc_algorithm, "_k", input$fgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_fgwc)
      res       <- rv$cga_result_fgwc
      feat_cols <- res$feat_cols
      long      <- tidyr::pivot_longer(res$profile, cols = dplyr::all_of(feat_cols), names_to = "Fitur", values_to = "Mean_Score")
      long$cluster <- factor(long$cluster)
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Fitur, y = cluster, fill = Mean_Score)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = round(Mean_Score, 2)), size = 3.0, color = "black") +
        ggplot2::scale_fill_distiller(palette = "RdYlBu", direction = -1, name = "Mean\nScore") +
        ggplot2::labs(title = paste("Heatmap Profil Cluster \u2014", toupper(res$algorithm)), x = "Fitur / Dimensi", y = "Cluster") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9))
      ggplot2::ggsave(file, plot = p, width = 10, height = 5, dpi = 300, bg = "white")
    }
  )

  output$dl_fgwc_radar <- downloadHandler(
    filename = function() paste0("fgwc_radar_", input$fgwc_algorithm, "_k", input$fgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_fgwc)
      res       <- rv$cga_result_fgwc
      feat_cols <- res$feat_cols
      k         <- res$k
      if (length(feat_cols) < 3) return()
      pal_rad        <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Set1")[seq_len(k)]
      cluster_labels <- paste0("Cluster ", seq_len(k))
      feat_mat       <- as.matrix(res$profile[, feat_cols, drop = FALSE])
      row_mins       <- apply(feat_mat, 2, min)
      row_maxs       <- apply(feat_mat, 2, max)
      feat_norm      <- sweep(sweep(feat_mat, 2, row_mins, "-"), 2, pmax(row_maxs - row_mins, 1e-9), "/")
      n_col_plot <- min(k, 3)
      n_row_plot <- ceiling(k / n_col_plot)
      grDevices::png(file, width = 3000, height = 2400, res = 300)
      par(mfrow = c(n_row_plot, n_col_plot + 1), mar = c(1, 1, 2.5, 1), oma = c(0, 0, 2, 0))
      for (i in seq_len(k)) {
        rdf <- as.data.frame(rbind(rep(1, length(feat_cols)), rep(0, length(feat_cols)), as.numeric(feat_norm[i, ])))
        colnames(rdf) <- feat_cols
        fmsb::radarchart(rdf, axistype = 1, pcol = pal_rad[i], pfcol = adjustcolor(pal_rad[i], alpha.f = 0.30),
                         plwd = 2.2, cglcol = "grey70", cglty = 1, vlcex = 0.75, title = cluster_labels[i],
                         caxislabels = c("0","0.25","0.5","0.75","1"), calcex = 0.6)
        mtext(paste0("n = ", res$profile$n[i]), side = 1, line = 0.2, cex = 0.7)
      }
      plot.new()
      legend("center", legend = cluster_labels, col = pal_rad, lwd = 3, bty = "n", title = "Cluster", cex = 0.85)
      mtext(paste0("Profil Radar FGWC (", toupper(res$algorithm), ") \u2014 nilai ternormalisasi 0\u20131"),
            outer = TRUE, line = 0.5, cex = 1.0, font = 2)
      grDevices::dev.off()
    }
  )

  output$dl_fgwc_sammon <- downloadHandler(
    filename = function() paste0("fgwc_sammon_", input$fgwc_algorithm, "_k", input$fgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_fgwc)
      res      <- rv$cga_result_fgwc
      feat_mat <- as.matrix(res$feat_df)
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      feat_mat <- unique(feat_mat)
      dist_mat <- dist(feat_mat)
      set.seed(42)
      sm <- tryCatch(
        MASS::sammon(dist_mat, k = 2, niter = input$fgwc_sammon_iter %||% 500,
                     magic = input$fgwc_sammon_magic %||% 0.2, trace = FALSE),
        error = function(e) NULL
      )
      if (is.null(sm)) return()
      k     <- res$k
      pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
      pt_sz <- input$fgwc_sammon_pt %||% 3
      cluster_vec <- as.integer(as.character(res$result_df$fgwc_cluster))
      if (length(cluster_vec) > nrow(sm$points)) cluster_vec <- cluster_vec[seq_len(nrow(sm$points))]
      df_plot <- data.frame(Dim1 = sm$points[, 1], Dim2 = sm$points[, 2], Cluster = factor(cluster_vec, levels = seq_len(k)))
      p <- .dim_reduction_plot(
        df_plot = df_plot, k = k, pal_c = pal_c, pt_sz = pt_sz,
        title_str = paste0("FGWC \u2014 Sammon Mapping (", toupper(res$algorithm), ") | k=", k),
        subtitle_str = paste0("Stress = ", round(sm$stress, 5), "  |  Iterasi = ", input$fgwc_sammon_iter %||% 500),
        xlab = "Dim 1", ylab = "Dim 2"
      )
      ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )

  output$dl_fgwc_tsne <- downloadHandler(
    filename = function() paste0("fgwc_tsne_", input$fgwc_algorithm, "_k", input$fgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_fgwc)
      res      <- rv$cga_result_fgwc
      feat_mat <- as.matrix(res$feat_df)
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      perp <- min(input$fgwc_tsne_perp %||% 15, floor((nrow(feat_mat) - 1) / 3))
      perp <- max(perp, 2)
      set.seed(42)
      tsne_res <- tryCatch(
        Rtsne::Rtsne(feat_mat, dims = 2, perplexity = perp, max_iter = input$fgwc_tsne_iter %||% 1000,
                     check_duplicates = FALSE, verbose = FALSE),
        error = function(e) NULL
      )
      if (is.null(tsne_res)) return()
      k     <- res$k
      pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
      pt_sz <- input$fgwc_tsne_pt %||% 3
      df_plot <- data.frame(Dim1 = tsne_res$Y[, 1], Dim2 = tsne_res$Y[, 2],
                            Cluster = factor(as.integer(as.character(res$result_df$fgwc_cluster)), levels = seq_len(k)))
      p <- .dim_reduction_plot(
        df_plot = df_plot, k = k, pal_c = pal_c, pt_sz = pt_sz,
        title_str = paste0("FGWC \u2014 t-SNE (", toupper(res$algorithm), ") | k=", k),
        subtitle_str = paste0("Perplexity = ", perp, "  |  Iterasi = ", input$fgwc_tsne_iter %||% 1000),
        xlab = "t-SNE Dim 1", ylab = "t-SNE Dim 2"
      )
      ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )

  output$dl_fgwc_umap <- downloadHandler(
    filename = function() paste0("fgwc_umap_", input$fgwc_algorithm, "_k", input$fgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_fgwc)
      res      <- rv$cga_result_fgwc
      feat_mat <- as.matrix(res$feat_df)
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      nn <- min(input$fgwc_umap_nn %||% 15, nrow(feat_mat) - 1)
      nn <- max(nn, 2)
      md <- input$fgwc_umap_md %||% 0.1
      set.seed(42)
      umap_res <- tryCatch(
        uwot::umap(feat_mat, n_neighbors = nn, min_dist = md, n_components = 2, verbose = FALSE),
        error = function(e) NULL
      )
      if (is.null(umap_res)) return()
      k     <- res$k
      pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
      pt_sz <- input$fgwc_umap_pt %||% 3
      df_plot <- data.frame(Dim1 = umap_res[, 1], Dim2 = umap_res[, 2],
                            Cluster = factor(as.integer(as.character(res$result_df$fgwc_cluster)), levels = seq_len(k)))
      p <- .dim_reduction_plot(
        df_plot = df_plot, k = k, pal_c = pal_c, pt_sz = pt_sz,
        title_str = paste0("FGWC \u2014 UMAP (", toupper(res$algorithm), ") | k=", k),
        subtitle_str = paste0("n_neighbors = ", nn, "  |  min_dist = ", md),
        xlab = "UMAP Dim 1", ylab = "UMAP Dim 2"
      )
      ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )
  # ==========================================================================
  # HELPER INTERNAL: Buat plot reduksi dimensi bergaya kontur + centroid
  # (Dipakai oleh Sammon, t-SNE, UMAP)
  # ==========================================================================

  .dim_reduction_plot <- function(df_plot, k, pal_c, pt_sz,
                                  title_str, subtitle_str,
                                  xlab, ylab) {

    # 1. Hitung pusat cluster dalam ruang 2D (rata-rata koordinat per klaster)
    df_centers <- df_plot |>
      dplyr::group_by(Cluster) |>
      dplyr::summarise(Dim1 = mean(Dim1, na.rm = TRUE),
                       Dim2 = mean(Dim2, na.rm = TRUE),
                       .groups = "drop")

    # 2. Buat grid 2D untuk menghitung iso-membership lines secara matematis
    pad_x <- (max(df_plot$Dim1) - min(df_plot$Dim1)) * 0.1
    pad_y <- (max(df_plot$Dim2) - min(df_plot$Dim2)) * 0.1
    x_seq <- seq(min(df_plot$Dim1) - pad_x, max(df_plot$Dim1) + pad_x, length.out = 150)
    y_seq <- seq(min(df_plot$Dim2) - pad_y, max(df_plot$Dim2) + pad_y, length.out = 150)
    grid_df <- expand.grid(Dim1 = x_seq, Dim2 = y_seq)

    centers_mat <- as.matrix(df_centers[, c("Dim1", "Dim2")])
    k_eff <- nrow(centers_mat)

    # Hitung jarak Euclidean dari tiap titik grid ke setiap pusat klaster yang terisi
    D <- matrix(0, nrow = nrow(grid_df), ncol = k_eff)
    for (i in seq_len(k_eff)) {
      D[, i] <- sqrt((grid_df$Dim1 - centers_mat[i, 1])^2 + (grid_df$Dim2 - centers_mat[i, 2])^2)
    }
    D[D == 0] <- 1e-10

    # Hitung FCM Membership (m = 2) untuk seluruh titik di grid
    tmp <- D^(-2)
    U_grid <- tmp / rowSums(tmp)

    # Ambil nilai keanggotaan tertinggi (maksimal) di setiap titik grid
    # Ini akan menghasilkan SATU permukaan topografi yang rapi tanpa garis bertabrakan
    grid_single <- data.frame(
      Dim1       = grid_df$Dim1,
      Dim2       = grid_df$Dim2,
      Membership = apply(U_grid, 1, max)
    )

    # 3. Data frame dummy untuk legend manual
    df_legend <- data.frame(
      x = c(NA, NA), y = c(NA, NA),
      Type = factor(c("Data point", "Cluster center"),
                    levels = c("Data point", "Cluster center"))
    )

    p <- ggplot2::ggplot() +
      # Layer 1: Iso-membership contours (Pasti kuning/merah di pusat!)
      ggplot2::geom_contour(data = grid_single,
                            mapping = ggplot2::aes(x = Dim1, y = Dim2, z = Membership, color = ggplot2::after_stat(level)),
                            bins = 12, linewidth = 0.5, show.legend = FALSE) +
      # Paksa batas warna dari 0 (biru) sampai 1 (merah/kuning)
      ggplot2::scale_color_viridis_c(option = "turbo", direction = 1, limits = c(0, 1)) +

      # Layer 2: Titik data (Biru, seperti referensi)
      ggplot2::geom_point(data = df_plot,
                          mapping = ggplot2::aes(x = Dim1, y = Dim2),
                          color = "#3949ab", size = pt_sz, alpha = 0.8, shape = 16) +

      # Layer 3: Pusat cluster (Silang merah)
      ggplot2::geom_point(data = df_centers,
                          mapping = ggplot2::aes(x = Dim1, y = Dim2),
                          color = "red", size = pt_sz * 1.5, stroke = 2, shape = 4) +

      # Dummy layer untuk legend
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

      # Labels & Tema
      ggplot2::labs(
        title    = title_str,
        subtitle = subtitle_str,
        x = xlab, y = ylab
      ) +
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

    return(p)
  }

  # ==========================================================================
  # TAB 8: SAMMON MAPPING
  # ==========================================================================

  output$fgwc_sammon_plot <- renderPlot({
    req(rv$cga_result_fgwc)
    res       <- rv$cga_result_fgwc
    feat_mat  <- as.matrix(res$feat_df)

    if (is.null(feat_mat) || nrow(feat_mat) < 3) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk Sammon Mapping.", cex = 1.1, col = "grey50")
      return()
    }

    # Jaga duplikat agar dist matrix tidak ada nol
    feat_mat <- unique(feat_mat)
    dist_mat <- dist(feat_mat)

    set.seed(42)
    sm <- tryCatch(
      MASS::sammon(dist_mat,
                   k     = 2,
                   niter = input$fgwc_sammon_iter  %||% 500,
                   magic = input$fgwc_sammon_magic %||% 0.2,
                   trace = FALSE),
      error = function(e) {
        showNotification(paste("Sammon error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(sm)) return()

    k   <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
    pt_sz <- input$fgwc_sammon_pt %||% 3

    cluster_vec <- as.integer(as.character(res$result_df$fgwc_cluster))
    if (length(cluster_vec) > nrow(sm$points))
      cluster_vec <- cluster_vec[seq_len(nrow(sm$points))]

    df_plot <- data.frame(
      Dim1    = sm$points[, 1],
      Dim2    = sm$points[, 2],
      Cluster = factor(cluster_vec, levels = seq_len(k))
    )

    .dim_reduction_plot(
      df_plot      = df_plot,
      k            = k,
      pal_c        = pal_c,
      pt_sz        = pt_sz,
      title_str    = paste0("LFGWC â€” Sammon Mapping (",
                            toupper(res$algorithm), ") | k=", k),
      subtitle_str = paste0("Stress = ", round(sm$stress, 5),
                            "  |  Iterasi Sammon = ",
                            input$fgwc_sammon_iter %||% 500),
      xlab         = "Dim 1",
      ylab         = "Dim 2"
    )
  })

  # ==========================================================================
  # TAB 9: t-SNE
  # ==========================================================================

  output$fgwc_tsne_plot <- renderPlot({
    req(rv$cga_result_fgwc)
    res      <- rv$cga_result_fgwc
    feat_mat <- as.matrix(res$feat_df)

    if (is.null(feat_mat) || nrow(feat_mat) < 3) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk t-SNE.", cex = 1.1, col = "grey50")
      return()
    }

    perp <- min(input$fgwc_tsne_perp %||% 15,
                floor((nrow(feat_mat) - 1) / 3))
    perp <- max(perp, 2)

    set.seed(42)
    tsne_res <- tryCatch(
      Rtsne::Rtsne(feat_mat,
                   dims             = 2,
                   perplexity       = perp,
                   max_iter         = input$fgwc_tsne_iter %||% 1000,
                   check_duplicates = FALSE,
                   verbose          = FALSE),
      error = function(e) {
        showNotification(paste("t-SNE error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(tsne_res)) return()

    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
    pt_sz <- input$fgwc_tsne_pt %||% 3

    df_plot <- data.frame(
      Dim1    = tsne_res$Y[, 1],
      Dim2    = tsne_res$Y[, 2],
      Cluster = factor(as.integer(as.character(res$result_df$fgwc_cluster)),
                       levels = seq_len(k))
    )

    .dim_reduction_plot(
      df_plot      = df_plot,
      k            = k,
      pal_c        = pal_c,
      pt_sz        = pt_sz,
      title_str    = paste0("LFGWC â€” t-SNE (",
                            toupper(res$algorithm), ") | k=", k),
      subtitle_str = paste0("Perplexity = ", perp,
                            "  |  Iterasi = ",
                            input$fgwc_tsne_iter %||% 1000),
      xlab         = "t-SNE Dim 1",
      ylab         = "t-SNE Dim 2"
    )
  })

  # ==========================================================================
  # TAB 10: UMAP
  # ==========================================================================

  output$fgwc_umap_plot <- renderPlot({
    req(rv$cga_result_fgwc)
    res      <- rv$cga_result_fgwc
    feat_mat <- as.matrix(res$feat_df)

    if (is.null(feat_mat) || nrow(feat_mat) < 3) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk UMAP.", cex = 1.1, col = "grey50")
      return()
    }

    nn <- min(input$fgwc_umap_nn %||% 15, nrow(feat_mat) - 1)
    nn <- max(nn, 2)
    md <- input$fgwc_umap_md %||% 0.1

    set.seed(42)
    umap_res <- tryCatch(
      uwot::umap(feat_mat,
                 n_neighbors  = nn,
                 min_dist     = md,
                 n_components = 2,
                 verbose      = FALSE),
      error = function(e) {
        showNotification(paste("UMAP error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(umap_res)) return()

    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), "Dark2")[seq_len(k)]
    pt_sz <- input$fgwc_umap_pt %||% 3

    df_plot <- data.frame(
      Dim1    = umap_res[, 1],
      Dim2    = umap_res[, 2],
      Cluster = factor(as.integer(as.character(res$result_df$fgwc_cluster)),
                       levels = seq_len(k))
    )

    .dim_reduction_plot(
      df_plot      = df_plot,
      k            = k,
      pal_c        = pal_c,
      pt_sz        = pt_sz,
      title_str    = paste0("LFGWC â€” UMAP (",
                            toupper(res$algorithm), ") | k=", k),
      subtitle_str = paste0("n_neighbors = ", nn,
                            "  |  min_dist = ", md),
      xlab         = "UMAP Dim 1",
      ylab         = "UMAP Dim 2"
    )
  })


} # end fgwc_server()
