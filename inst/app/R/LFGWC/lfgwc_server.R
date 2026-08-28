# =============================================================================
# lfgwc_server.R — Server Logic Tab LFGWC untuk Shiny SoVI App
# Letakkan di: R/LFGWC/lfgwc_server.R
# Di-source dari global.R, dipanggil dari server.R
# =============================================================================

lfgwc_server <- function(input, output, session, rv) {

  rv_lfgwc_sample_dist <- reactiveVal(NULL)
  rv_lfgwc_sample_pop  <- reactiveVal(NULL)

  observeEvent(input$lfgwc_load_sample, {
    withProgress(message = "Memuat data sampel LFGWC...", value = 0, {
      extdata <- system.file("extdata", package = "soviclust")
      mode <- input$lfgwc_dist_mode %||% "matrix"
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
      rv_lfgwc_sample_dist(mat)
      incProgress(0.7)
      rv_pop <- as.data.frame(readxl::read_excel(file.path(extdata, "sovi_data_pop_514.xlsx")))
      rv_lfgwc_sample_pop(parse_population(rv_pop))
      incProgress(1.0)
    })
    showNotification(paste0("✓ Data sampel LFGWC dimuat: ", nrow(rv_lfgwc_sample_dist()), "x", ncol(rv_lfgwc_sample_dist()), " | pop ", length(rv_lfgwc_sample_pop())), type = "message", duration = 5)
  })

  
  # ==========================================================================
  # REACTIVE: Baca matriks jarak dari upload
  # (Sama dengan FGWC — reuse fungsi parse_distance_matrix & parse_population
  #  yang sudah ada di fgwc_wrapper.R)
  # ==========================================================================
  
  
  # ==========================================================================
  # REACTIVE: Baca matriks jarak — dari upload matrix ATAU dari lon/lat
  # ==========================================================================
  
  rv_lfgwc_dist <- reactive({
    if (!is.null(rv_lfgwc_sample_dist())) return(rv_lfgwc_sample_dist())

    mode <- input$lfgwc_dist_mode %||% "matrix"
    
    if (mode == "matrix") {
      # ── Mode 1: Upload matriks n×n langsung ──────────────────────────
      req(input$lfgwc_file_dist)
      tryCatch({
        df  <- read_uploaded_file(input$lfgwc_file_dist)
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
      # ── Mode 2: Hitung dari lon/lat via Haversine ─────────────────────
      req(input$lfgwc_file_lonlat)
      tryCatch({
        df <- read_uploaded_file(input$lfgwc_file_lonlat)
        
        # Deteksi kolom lon/lat secara fleksibel
        cols     <- tolower(names(df))
        lon_col  <- names(df)[which(cols %in% c("longitude", "lon", "long", "x"))[1]]
        lat_col  <- names(df)[which(cols %in% c("latitude",  "lat", "y"))[1]]
        id_col   <- names(df)[which(cols %in% c("districtcode", "id", "kode",
                                                "code", "regions"))[1]]
        
        if (is.na(lon_col)) stop("Kolom longitude tidak ditemukan.")
        if (is.na(lat_col)) stop("Kolom latitude tidak ditemukan.")
        
        lon <- as.numeric(df[[lon_col]])
        lat <- as.numeric(df[[lat_col]])
        
        if (any(is.na(lon)) || any(is.na(lat)))
          stop("Terdapat nilai NA pada kolom longitude/latitude.")
        if (any(lon < -180 | lon > 180))
          stop("Nilai longitude di luar rentang -180 hingga 180.")
        if (any(lat < -90  | lat > 90))
          stop("Nilai latitude di luar rentang -90 hingga 90.")
        
        # Hitung Haversine distance matrix
        mat <- haversine_matrix(lon, lat)
        
        # Beri nama baris/kolom dari ID jika tersedia
        if (!is.na(id_col)) {
          rownames(mat) <- df[[id_col]]
          colnames(mat) <- df[[id_col]]
        }
        
        attr(mat, "dist_mode") <- "lonlat"
        attr(mat, "dist_unit") <- "kilometer"
        attr(mat, "n_unit")    <- nrow(df)
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
  
  rv_lfgwc_pop <- reactive({
    if (!is.null(rv_lfgwc_sample_pop())) return(rv_lfgwc_sample_pop())

    req(input$lfgwc_file_pop)
    tryCatch({
      df  <- read_uploaded_file(input$lfgwc_file_pop)
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
  
  output$lfgwc_dist_status <- renderUI({
    mat <- rv_lfgwc_dist()
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
          paste0(" Haversine distance dihitung: ", nrow(mat), " × ", ncol(mat)),
          tags$br(),
          tags$span(style = "color:#27ae60;",
                    icon("ruler"), " Satuan: kilometer"))
    } else {
      div(class = "progress-box status-ok", style = "font-size:12px;",
          icon("check"),
          paste0(" Matriks dimuat: ", nrow(mat), " × ", ncol(mat)),
          tags$br(),
          tags$span(style = "color:#78909c;",
                    icon("ruler"), " Satuan: ", unit))
    }
  })
  
  # ==========================================================================
  # OUTPUT: Status upload populasi
  # ==========================================================================
  
  output$lfgwc_pop_status <- renderUI({
    pop <- rv_lfgwc_pop()
    if (is.null(pop)) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12;
                          font-size:12px;",
                 icon("exclamation-triangle"),
                 " Belum diupload atau error."))
    }
    div(class = "progress-box status-ok", style = "font-size:12px;",
        icon("check"),
        paste0(" Populasi dimuat: ", length(pop), " unit"))
  })
  
  # ==========================================================================
  # OUTPUT: Info dthr — tampilkan max_dist dari matriks jarak
  # ==========================================================================
  
  output$lfgwc_dthr_info <- renderUI({
    mat <- rv_lfgwc_dist()
    if (is.null(mat)) return(NULL)
    
    # Hitung jarak minimum agar tiap unit punya ≥1 tetangga
    dist_aux       <- mat
    diag(dist_aux) <- NA
    col_mins       <- apply(dist_aux, 2, min, na.rm = TRUE)
    max_dist       <- round(max(col_mins), 2)
    
    mode <- attr(mat, "dist_mode") %||% "matrix"
    unit <- if (mode == "lonlat") "km" else "unit asli"
    
    div(class = "progress-box",
        style = "background:#e3f2fd; border-left-color:#1a73c1;
                 font-size:11.5px; margin-bottom:6px;",
        icon("info-circle"),
        tags$strong(sprintf(" Min. dthr aman: %.2f %s", max_dist, unit)),
        sprintf(" (jarak minimum agar setiap unit punya ≥1 tetangga)."),
        tags$br(),
        "Set -99 untuk mode Global (semua unit).")
  })
  
  # ==========================================================================
  # OUTPUT: Pilihan variabel dinamis sesuai sumber data
  # ==========================================================================
  
  output$lfgwc_var_selector <- renderUI({
    req(rv$data, rv$sovi_vars)
    num_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    src      <- input$lfgwc_data_source %||% "raw"
    
    note <- switch(src,
                   "raw"          = div(class = "progress-box",
                                        style = "background:#fff8e1;border-left-color:#f39c12;
                                    font-size:11px;margin-bottom:6px;",
                                        icon("exclamation-triangle"),
                                       " Nilai mentah tanpa transformasi."),
                   "raw_norm"     = div(class = "progress-box",
                                        style = "background:#fff8e1;border-left-color:#f39c12;
                                    font-size:11px;margin-bottom:6px;",
                                        icon("info-circle"),
                                        " Akan dinormalisasi min-max 0–1."),
                   "standardized" = div(class = "progress-box",
                                        style = "background:#e3f2fd;border-left-color:#1a73c1;
                                    font-size:11px;margin-bottom:6px;",
                                        icon("info-circle"),
                                        " Z-score dari SoVI. LFGWC akan normalisasi ke [0,1]."),
                   NULL
    )
    
    tagList(
      note,
      checkboxGroupInput("lfgwc_selected_vars", NULL,
                         choices  = num_cols,
                         selected = rv$sovi_vars)
    )
  })
  
  # ==========================================================================
  # OUTPUT: Info sumber data (sovi / rc)
  # ==========================================================================
  
  output$lfgwc_datasource_info <- renderUI({
    src <- input$lfgwc_data_source
    if (is.null(src)) return(NULL)
    
    info <- switch(src,
                   "sovi" = "SoVI Score tunggal (0–1) sebagai fitur clustering.",
                   "rc"   = "Skor RC (komponen PCA Varimax, ternormalisasi 0–1).",
                   NULL
    )
    if (is.null(info)) return(NULL)
    
    div(class = "progress-box",
        style = "background:#e3f2fd; border-left-color:#1a73c1;
                 font-size:12px; margin-top:6px;",
        icon("info-circle"), " ", info)
  })
  
  # ==========================================================================
  # OUTPUT: Progress bar
  # ==========================================================================
  
  output$lfgwc_progress <- renderUI({ NULL })
  
  # ==========================================================================
  # OBSERVER: Run LFGWC
  # ==========================================================================
  
  observeEvent(input$run_lfgwc, {
    
    # ── Validasi prasyarat ───────────────────────────────────────────────────
    if (is.null(rv$data)) {
      showNotification("Upload dataset terlebih dahulu!", type = "warning")
      return()
    }
    if (is.null(rv_lfgwc_dist())) {
      showNotification("Upload matriks jarak terlebih dahulu!", type = "warning")
      return()
    }
    if (is.null(rv_lfgwc_pop())) {
      showNotification("Upload data populasi terlebih dahulu!", type = "warning")
      return()
    }
    if (input$lfgwc_data_source %in% c("sovi", "rc", "standardized") &&
        is.null(rv$sovi_result)) {
      showNotification("Jalankan SoVI Computation terlebih dahulu!",
                       type = "warning")
      return()
    }
    
    output$lfgwc_progress <- renderUI({
      div(class = "progress-box", style = "background:#cce5ff;",
          icon("spinner", class = "fa-spin"),
          paste0(" Running LFGWC (",
                 toupper(input$lfgwc_algorithm), ")..."))
    })
    
    # ── Kumpulkan variabel yang dipilih ─────────────────────────────────────
    sel_vars <- if (input$lfgwc_data_source %in%
                    c("raw", "raw_norm", "standardized"))
      input$lfgwc_selected_vars else NULL
    
    # ── Kumpulkan parameter LFGWC ───────────────────────────────────────────
    lfgwc_params <- list(
      m        = input$lfgwc_m,
      alpha    = input$lfgwc_alpha,
      a        = if (input$lfgwc_si == "TRUE") input$lfgwc_a    else 1,
      b        = if (input$lfgwc_si == "TRUE") input$lfgwc_b    else 1,
      max_iter = input$lfgwc_maxiter,
      error    = input$lfgwc_error,
      randomN  = input$lfgwc_seed,
      dthr     = input$lfgwc_dthr,
      si       = as.logical(input$lfgwc_si),
      exp_d    = if (input$lfgwc_si == "FALSE") input$lfgwc_exp_d else 2
    )
    
    # ── Kumpulkan parameter algoritma optimasi ───────────────────────────────
    algo <- input$lfgwc_algorithm
    opt_params <- list(
      npar       = input$lfgwc_npar,
      same       = input$lfgwc_same,
      vi_dist    = input$lfgwc_vi_dist,
      # ABC
      n_onlooker = input$lfgwc_abc_onlooker,
      limit      = input$lfgwc_abc_limit,
      # FPA
      p          = input$lfgwc_fpa_p,
      gamma      = input$lfgwc_fpa_gamma,
      lambda     = input$lfgwc_fpa_lambda,
      ei_distr   = input$lfgwc_fpa_ei,
      chaos      = 3,
      # GSA
      G          = input$lfgwc_gsa_G,
      vmax       = input$lfgwc_gsa_vmax,
      new        = input$lfgwc_gsa_new,
      # HHO
      hho_algo   = input$lfgwc_hho_algo,
      a1         = input$lfgwc_hho_a1,
      a2         = input$lfgwc_hho_a2,
      a3         = input$lfgwc_hho_a3,
      # IFA
      par_no     = input$lfgwc_ifa_parno,
      gamma      = input$lfgwc_ifa_gamma,
      beta       = input$lfgwc_ifa_beta,
      ei_distr   = input$lfgwc_ifa_ei,
      # PSO
      vmax       = input$lfgwc_pso_vmax,
      c1         = input$lfgwc_pso_c1,
      c2         = input$lfgwc_pso_c2,
      type       = input$lfgwc_pso_type,
      wmax       = input$lfgwc_pso_wmax,
      wmin       = input$lfgwc_pso_wmin,
      map        = 0.3,
      # TLBO
      nselection = input$lfgwc_tlbo_nselect,
      elitism    = input$lfgwc_tlbo_elitism,
      n_elite    = input$lfgwc_tlbo_nelite,
      # WOA
      woa_b      = input$lfgwc_woa_b
    )
    
    # ── Jalankan LFGWC ──────────────────────────────────────────────────────
    withProgress(message = paste("Running LFGWC", toupper(algo), "..."),
                 value = 0, {
                   
                   incProgress(0.2, detail = "Membangun spatial weights matrix...")
                   
                   result <- tryCatch({
                     run_lfgwc_shiny(
                       data_source   = input$lfgwc_data_source,
                       raw_data      = rv$data,
                       sovi_result   = rv$sovi_result,
                       selected_vars = sel_vars,
                       pop_vec       = rv_lfgwc_pop(),
                       dist_mat      = rv_lfgwc_dist(),
                       algorithm     = algo,
                       ncluster      = input$lfgwc_ncluster,
                       lfgwc_params  = lfgwc_params,
                       opt_params    = opt_params,
                       id_col        = input$id_col,
                       name_col      = input$name_col
                     )
                   }, error = function(e) {
                     showNotification(paste("Error LFGWC:", e$message),
                                      type = "error", duration = 15)
                     NULL
                   })
                   
                   incProgress(0.8, detail = "Selesai.")
                   rv$cga_result_lfgwc <- result
                 })
    
    # ── Update progress box ──────────────────────────────────────────────────
    output$lfgwc_progress <- renderUI({
      res <- rv$cga_result_lfgwc
      if (!is.null(res))
        div(class = "progress-box status-ok",
            icon("check"),
            paste0(" Selesai! k=", res$k,
                   " | Mode: ", res$mode_label,
                   " | Sil=", res$sil_mean,
                   " | J=", round(res$f_obj, 4),
                   " | Iter=", res$iteration))
      else
        div(class = "progress-box status-err",
            icon("times"), " Gagal. Periksa data & parameter.")
    })
  })
  
  # ==========================================================================
  # TAB 1: RINGKASAN PARAMETER
  # ==========================================================================
  
  output$lfgwc_summary <- renderUI({
    res <- rv$cga_result_lfgwc
    if (is.null(res)) {
      return(div(class = "progress-box",
                 style = "background:#fff3cd; border-left-color:#f39c12;",
                 icon("exclamation-triangle"),
                 " Jalankan LFGWC dengan tombol di panel kiri."))
    }
    
    src_label <- switch(res$data_source,
                        "raw"          = "Data Asli (tanpa transformasi)",
                        "raw_norm"     = "Data Asli Ternormalisasi (0-1)",
                        "standardized" = "Data Z-score → Normalisasi [0,1]",
                        "sovi"         = "SoVI Score",
                        "rc"           = "Skor RC (komponen PCA)",
                        res$data_source
    )
    
    algo_labels <- c(
      classic = "Classic LFGWC",
      abc     = "ABC — Artificial Bee Colony",
      fpa     = "FPA — Flower Pollination Algorithm",
      gsa     = "GSA — Gravitational Search Algorithm",
      hho     = "HHO — Harris-Hawk Optimization",
      ifa     = "IFA — Intelligent Firefly Algorithm",
      pso     = "PSO — DLFGWC-PSO (Grekousis 2020)",
      tlbo    = "TLBO — Teaching-Learning Based Optimization",
      woa     = "WOA — Whale Optimization Algorithm"
    )
    
    div(
      fluidRow(
        column(4,
               div(class = "info-card",
                   tags$h4(icon("microchip"), " Configuration"),
                   tags$p(style = "font-size:13px; font-weight:700; color:#1a73c1;",
                          algo_labels[res$algorithm]),
                   tags$p(style = "font-size:12px; color:#78909c;",
                          "Sumber data: ", src_label),
                   tags$p(style = "font-size:12px; color:#78909c;",
                          "Fitur: ", length(res$feat_cols), " dimensi"),
                   tags$p(style = "font-size:12px; color:#78909c;",
                          "Iterasi: ", res$iteration)
               )
        ),
        column(4,
               div(class = "info-card",
                   tags$h4(icon("map-marker-alt"), " Parameter Spasial LFGWC"),
                   tags$p(style = "font-size:13px;",
                          tags$strong("Mode: "), res$mode_label),
                   tags$p(style = "font-size:13px;",
                          tags$strong("dthr = "),
                          if (res$dthr == -99) "Global (semua unit)"
                          else paste0(res$dthr, " (neighborhood)")),
                   tags$p(style = "font-size:13px;",
                          tags$strong("exp = "), res$exp_d),
                   tags$p(style = "font-size:12px; color:#78909c;",
                          "Unit tanpa tetangga (KNN fallback): ",
                          res$n_isolated),
                   tags$p(style = "font-size:12px; color:#78909c;",
                          "Max dist aman: ",
                          round(res$max_dist, 2))
               )
        ),
        column(4,
               div(class = "info-card",
                   tags$h4(icon("object-group"), " Clustering Results"),
                   tags$p(style = "font-size:13px;",
                          tags$strong("k = "), res$k),
                   tags$p(style = "font-size:13px;",
                          tags$strong("Objective Function J = "),
                          tags$span(style = "font-weight:700; color:#1a73c1;",
                                    round(res$f_obj, 4))),
                   tags$p(style = "font-size:13px;",
                          tags$strong("Mean Silhouette = "),
                          tags$span(
                            style = if (!is.na(res$sil_mean) &&
                                        res$sil_mean >= 0.5)
                              "color:#27ae60;font-weight:700;"
                            else if (!is.na(res$sil_mean) &&
                                     res$sil_mean >= 0.25)
                              "color:#f39c12;font-weight:700;"
                            else
                              "color:#e74c3c;font-weight:700;",
                            if (is.na(res$sil_mean)) "N/A" else res$sil_mean))
               )
        )
      ),
      fluidRow(column(12,
                      div(class = "info-card",
                          style = "border-left-color:#27ae60; margin-top:4px;",
                          tags$h4(icon("book-open"), " Referensi"),
                          tags$p(style = "font-size:12px; margin:0;",
                                 tags$strong("Grekousis, G. (2020)."),
                                 " Local fuzzy geographically weighted clustering: a new method",
                                 " for geodemographic segmentation.",
                                 tags$em(" International Journal of Geographical Information Science."),
                                 " DOI: 10.1080/13658816.2020.1808221")
                      )
      ))
    )
  })
  
  # ==========================================================================
  # TAB 2: VALIDASI + KONVERGENSI
  # ==========================================================================
  
  output$lfgwc_val_table <- DT::renderDT({
    req(rv$cga_result_lfgwc)
    df <- rv$cga_result_lfgwc$val_df
    DT::datatable(df,
                  options  = list(dom = "t", pageLength = 10),
                  rownames = FALSE,
                  caption  = "Indeks Validasi Cluster LFGWC (Grekousis 2020)")
  })
  
  output$lfgwc_conv_plot <- renderPlot({
    req(rv$cga_result_lfgwc)
    conv <- rv$cga_result_lfgwc$conv
    df   <- data.frame(Iterasi = seq_along(conv), J = conv)
    
    ggplot2::ggplot(df, ggplot2::aes(x = Iterasi, y = J)) +
      ggplot2::geom_line(color = "#1a73c1", linewidth = 1.0) +
      ggplot2::geom_point(color = "#1a73c1", size = 1.5, alpha = 0.6) +
      ggplot2::labs(
        title = paste("Konvergensi Objective Function J —",
                      toupper(rv$cga_result_lfgwc$algorithm)),
        x = "Iterasi", y = "J (Objective Function)"
      ) +
      ggplot2::theme_minimal(base_size = 11)
  })
  
  # ==========================================================================
  # TAB 3: PETA CLUSTER
  # ==========================================================================
  
  output$lfgwc_map <- leaflet::renderLeaflet({
    req(rv$cga_result_lfgwc, rv$shp)
    res <- rv$cga_result_lfgwc
    
    # Gabungkan max_membership ke result_df jika belum ada
    result_df <- res$result_df
    if (!is.null(res$memb_df) && !"max_membership" %in% names(result_df)) {
      result_df$max_membership <- res$memb_df$max_membership
    }
    
    build_leaflet_lfgwc(
      result_df   = result_df,
      shp         = rv$shp,
      join_shp    = input$join_shp,
      join_df     = res$id_col,
      name_col    = res$name_col,
      k           = res$k,
      cluster_col = "lfgwc_cluster",
      palette_name = input$lfgwc_palette %||% "Dark2"
    )
  })
  
  # ==========================================================================
  # TAB 4: PETA MAX MEMBERSHIP (Figure 5b dari paper)
  # ==========================================================================
  
  output$lfgwc_map_membership <- leaflet::renderLeaflet({
    req(rv$cga_result_lfgwc, rv$shp)
    res <- rv$cga_result_lfgwc
    
    result_df <- res$result_df
    if (!is.null(res$memb_df)) {
      result_df$max_membership <- res$memb_df$max_membership
    }
    
    build_leaflet_lfgwc_membership(
      result_df   = result_df,
      shp         = rv$shp,
      join_shp    = input$join_shp,
      join_df     = res$id_col,
      name_col    = res$name_col,
      cluster_col = "lfgwc_cluster"
    )
  })
  
  # ==========================================================================
  # TAB 5: SILHOUETTE
  # ==========================================================================
  
  output$lfgwc_sil_plot <- renderPlot({
    req(rv$cga_result_lfgwc)
    res <- rv$cga_result_lfgwc
    if (is.null(res$sil_obj)) {
      plot.new()
      text(0.5, 0.5, "Silhouette tidak tersedia.",
           cex = 1.1, col = "grey50")
      return()
    }
    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), (input$lfgwc_palette %||% "Dark2"))[seq_len(k)]
    plot(res$sil_obj,
         main = paste0("Silhouette — LFGWC (", toupper(res$algorithm), ")",
                       "\nk=", k, " | Mean = ", res$sil_mean),
         col  = pal_c)
  })
  
  output$lfgwc_sil_table <- DT::renderDT({
    req(rv$cga_result_lfgwc)
    res <- rv$cga_result_lfgwc
    if (is.null(res$sil_obj)) {
      return(DT::datatable(data.frame(Info = "Silhouette tidak tersedia")))
    }
    sil_sum <- summary(res$sil_obj)$clus.avg.widths
    df <- data.frame(
      cluster       = factor(seq_along(sil_sum)),
      avg_sil_width = round(sil_sum, 4)
    )
    DT::datatable(df, options = list(dom = "t"), rownames = FALSE,
                  caption = "Avg. Silhouette Width per Cluster")
  })
  
  output$lfgwc_sil_interp <- renderUI({
    req(rv$cga_result_lfgwc)
    s     <- rv$cga_result_lfgwc$sil_mean
    if (is.na(s)) return(NULL)
    cls   <- if (s >= 0.50) "status-ok"   else
      if (s >= 0.25) "status-warn" else "status-err"
    label <- if (s >= 0.50) "Struktur Kuat ✓" else
      if (s >= 0.25) "Struktur Moderat" else "Struktur Lemah ✗"
    div(class = paste("progress-box", cls),
        icon("tachometer-alt"),
        paste0(" Mean Silhouette = ", s, " — ", label))
  })
  
  # ==========================================================================
  # TAB 6: PROFIL CLUSTER (Tabel + Heatmap + Radar)
  # ==========================================================================
  
  output$lfgwc_profile_table <- DT::renderDT({
    req(rv$cga_result_lfgwc)
    df       <- rv$cga_result_lfgwc$profile
    num_cols <- setdiff(names(df), "cluster")
    df[, num_cols] <- round(df[, num_cols, drop = FALSE], 3)
    DT::datatable(df,
                  options  = list(dom = "t", scrollX = TRUE),
                  rownames = FALSE,
                  caption  = "Cluster Profile: Mean Feature per Cluster")
  })
  
  output$lfgwc_heatmap <- renderPlot({
    req(rv$cga_result_lfgwc)
    res       <- rv$cga_result_lfgwc
    profile   <- res$profile
    feat_cols <- res$feat_cols
    
    long <- tidyr::pivot_longer(profile, cols = dplyr::all_of(feat_cols),
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
        title = paste("Cluster Profile Heatmap — LFGWC",
                      toupper(res$algorithm)),
        x = "Feature / Dimension", y = "Cluster"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9)
      )
  })
  
  output$lfgwc_radar <- renderPlot({
    req(rv$cga_result_lfgwc)
    res       <- rv$cga_result_lfgwc
    profile   <- res$profile
    feat_cols <- res$feat_cols
    k         <- res$k
    
    if (length(feat_cols) < 3) {
      plot.new()
      text(0.5, 0.5, "Radar chart membutuhkan minimal 3 fitur.",
           cex = 1.1, col = "grey50")
      return()
    }
    
    pal_rad        <- RColorBrewer::brewer.pal(min(max(k, 3), 8),
                                               (input$lfgwc_palette %||% "Dark2"))[seq_len(k)]
    cluster_labels <- paste0("Cluster ", seq_len(k))
    
    # Normalisasi profil ke [0,1] untuk radar
    feat_mat  <- as.matrix(profile[, feat_cols, drop = FALSE])
    row_mins  <- apply(feat_mat, 2, min)
    row_maxs  <- apply(feat_mat, 2, max)
    feat_norm <- sweep(feat_mat, 2, row_mins, "-")
    denom     <- pmax(row_maxs - row_mins, 1e-9)
    feat_norm <- sweep(feat_norm, 2, denom, "/")
    
    n_col_plot <- min(k, 3)
    n_row_plot <- ceiling(k / n_col_plot)
    par(mfrow = c(n_row_plot, n_col_plot + 1),
        mar   = c(1, 1, 2.5, 1), oma = c(0, 0, 2, 0))
    
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
        pcol  = pal_rad[i],
        pfcol = adjustcolor(pal_rad[i], alpha.f = 0.30),
        plwd  = 2.2, cglcol = "grey70", cglty = 1,
        vlcex = 0.75, title = cluster_labels[i],
        caxislabels = c("0", "0.25", "0.5", "0.75", "1"),
        calcex = 0.6
      )
      mtext(paste0("n = ", profile$n[i]),
            side = 1, line = 0.2, cex = 0.7)
    }
    
    plot.new()
    legend("center",
           legend = cluster_labels,
           col    = pal_rad,
           lwd    = 3,
           bty    = "n",
           title  = "Cluster",
           cex    = 0.85)
    mtext(paste0("Profil Radar LFGWC (", toupper(res$algorithm),
                 ") — nilai ternormalisasi 0–1"),
          outer = TRUE, line = 0.5, cex = 1.0, font = 2)
  })
  
  # ==========================================================================
  # TAB 7: DATA CLUSTER
  # ==========================================================================
  
  output$lfgwc_result_table <- DT::renderDT({
    req(rv$cga_result_lfgwc)
    res      <- rv$cga_result_lfgwc
    df       <- res$result_df
    id_col   <- res$id_col
    name_col <- res$name_col
    
    show_cols <- unique(c(id_col, name_col, "lfgwc_cluster",
                          if ("sovi_score" %in% names(df)) "sovi_score",
                          if ("vuln_class" %in% names(df)) "vuln_class"))
    show_cols <- show_cols[show_cols %in% names(df)]
    show_df   <- df[, show_cols, drop = FALSE]
    
    num_cols <- show_cols[sapply(show_df, is.numeric)]
    if (length(num_cols) > 0)
      show_df[, num_cols] <- round(show_df[, num_cols, drop = FALSE], 4)
    
    DT::datatable(show_df, filter = "top",
                  options  = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE,
                  caption  = paste0("Hasil LFGWC — Algoritma: ",
                                    toupper(res$algorithm),
                                    " | k=", res$k,
                                    " | Mode: ", res$mode_label))
  })
  
  # ==========================================================================
  # DOWNLOADS
  # ==========================================================================
  
  output$dl_lfgwc_csv <- downloadHandler(
    filename = function()
      paste0("lfgwc_", input$lfgwc_algorithm,
             "_k", input$lfgwc_ncluster, "_",
             Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$cga_result_lfgwc)
      res      <- rv$cga_result_lfgwc
      df       <- res$result_df
      id_col   <- res$id_col
      name_col <- res$name_col
      
      keep_cols <- unique(c(id_col, name_col, "lfgwc_cluster",
                            if ("sovi_score" %in% names(df)) "sovi_score",
                            if ("vuln_class" %in% names(df)) "vuln_class"))
      keep_cols <- keep_cols[keep_cols %in% names(df)]
      write.csv(df[, keep_cols], file, row.names = FALSE)
    }
  )
  
  output$dl_lfgwc_memb_csv <- downloadHandler(
    filename = function()
      paste0("lfgwc_membership_k", input$lfgwc_ncluster,
             "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$cga_result_lfgwc)
      res      <- rv$cga_result_lfgwc
      df       <- res$result_df
      memb_df  <- res$memb_df
      id_col   <- res$id_col
      name_col <- res$name_col
      
      out <- cbind(
        df[, c(id_col, name_col, "lfgwc_cluster"), drop = FALSE],
        memb_df
      )
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_lfgwc_map_png <- downloadHandler(
    filename = function()
      paste0("map_lfgwc_", input$lfgwc_algorithm,
             "_k", input$lfgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_lfgwc, rv$shp)
      res       <- rv$cga_result_lfgwc
      result_df <- res$result_df
      shp       <- rv$shp
      k         <- res$k
      
      result_df[[res$id_col]] <- normalize_id(result_df[[res$id_col]])
      shp[[input$join_shp]]   <- normalize_id(shp[[input$join_shp]])
      
      peta <- dplyr::left_join(shp, result_df,
                               by = setNames(res$id_col, input$join_shp))
      peta$lfgwc_cluster <- as.factor(peta$lfgwc_cluster)
      
      pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8),
                                        (input$lfgwc_palette %||% "Dark2"))[seq_len(k)]
      
      tmap::tmap_mode("plot")
      m <- tmap::tm_shape(peta) +
        tmap::tm_polygons(
          fill        = "lfgwc_cluster",
          fill.scale  = tmap::tm_scale_categorical(values = pal_c),
          fill.legend = tmap::tm_legend(title = "LFGWC Cluster"),
          col  = "grey40", lwd = 0.3
        ) +
        tmap::tm_title(paste0("LFGWC — ", toupper(res$algorithm),
                              " | k=", k,
                              " | Mode: ", res$mode_label)) +
        tmap::tm_compass(type = "arrow", position = c("left", "bottom"),
                         size = 1.5) +
        tmap::tm_scalebar(position = c("left", "bottom"), text.size = 0.6) +
        tmap::tm_layout(legend.outside          = TRUE,
                        legend.outside.position = "right")
      
      tmap::tmap_save(m, filename = file, width = 3000, height = 2400,
                      dpi = 300)
      tmap::tmap_mode("view")
    }
  )

  # --------------------------------------------------------------------------
  # DOWNLOAD: Heatmap Profil (ggplot2)
  # --------------------------------------------------------------------------
  output$dl_lfgwc_heatmap <- downloadHandler(
    filename = function()
      paste0("lfgwc_heatmap_", input$lfgwc_algorithm,
             "_k", input$lfgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_lfgwc)
      res       <- rv$cga_result_lfgwc
      feat_cols <- res$feat_cols
      long      <- tidyr::pivot_longer(res$profile,
                                       cols      = dplyr::all_of(feat_cols),
                                       names_to  = "Fitur",
                                       values_to = "Mean_Score")
      long$cluster <- factor(long$cluster)
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Fitur, y = cluster,
                                               fill = Mean_Score)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = round(Mean_Score, 2)),
                           size = 3.0, color = "black") +
        ggplot2::scale_fill_distiller(palette = "RdYlBu", direction = -1,
                                      name = "Mean\nScore") +
        ggplot2::labs(
          title = paste("Cluster Profile Heatmap \u2014 LFGWC", toupper(res$algorithm)),
          x = "Feature / Dimension", y = "Cluster"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9))
      ggplot2::ggsave(file, plot = p, width = 10, height = 5, dpi = 300, bg = "white")
    }
  )

  # --------------------------------------------------------------------------
  # DOWNLOAD: Radar Chart (base R graphics)
  # --------------------------------------------------------------------------
  output$dl_lfgwc_radar <- downloadHandler(
    filename = function()
      paste0("lfgwc_radar_", input$lfgwc_algorithm,
             "_k", input$lfgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_lfgwc)
      res       <- rv$cga_result_lfgwc
      profile   <- res$profile
      feat_cols <- res$feat_cols
      k         <- res$k
      if (length(feat_cols) < 3) return()
      pal_rad        <- RColorBrewer::brewer.pal(min(max(k, 3), 8), (input$lfgwc_palette %||% "Dark2"))[seq_len(k)]
      cluster_labels <- paste0("Cluster ", seq_len(k))
      feat_mat  <- as.matrix(profile[, feat_cols, drop = FALSE])
      row_mins  <- apply(feat_mat, 2, min)
      row_maxs  <- apply(feat_mat, 2, max)
      feat_norm <- sweep(feat_mat, 2, row_mins, "-")
      denom     <- pmax(row_maxs - row_mins, 1e-9)
      feat_norm <- sweep(feat_norm, 2, denom, "/")
      n_col_plot <- min(k, 3)
      n_row_plot <- ceiling(k / n_col_plot)
      grDevices::png(file, width = 3000, height = 2400, res = 300)
      par(mfrow = c(n_row_plot, n_col_plot + 1), mar = c(1, 1, 2.5, 1), oma = c(0, 0, 2, 0))
      for (i in seq_len(k)) {
        row_data <- as.numeric(feat_norm[i, ])
        rdf      <- as.data.frame(rbind(rep(1, length(feat_cols)), rep(0, length(feat_cols)), row_data))
        colnames(rdf) <- feat_cols
        fmsb::radarchart(rdf, axistype = 1, pcol = pal_rad[i],
                         pfcol = adjustcolor(pal_rad[i], alpha.f = 0.30),
                         plwd = 2.2, cglcol = "grey70", cglty = 1,
                         vlcex = 0.75, title = cluster_labels[i],
                         caxislabels = c("0","0.25","0.5","0.75","1"), calcex = 0.6)
        mtext(paste0("n = ", profile$n[i]), side = 1, line = 0.2, cex = 0.7)
      }
      plot.new()
      legend("center", legend = cluster_labels, col = pal_rad, lwd = 3, bty = "n", title = "Cluster", cex = 0.85)
      mtext(paste0("Profil Radar LFGWC (", toupper(res$algorithm), ") \u2014 nilai ternormalisasi 0\u20131"),
            outer = TRUE, line = 0.5, cex = 1.0, font = 2)
      grDevices::dev.off()
    }
  )

  # --------------------------------------------------------------------------
  # DOWNLOAD: Sammon Mapping
  # --------------------------------------------------------------------------
  output$dl_lfgwc_sammon <- downloadHandler(
    filename = function() paste0("lfgwc_sammon_", input$lfgwc_algorithm, "_k", input$lfgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_lfgwc)
      res <- rv$cga_result_lfgwc; feat_mat <- as.matrix(res$feat_df)
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      feat_mat <- unique(feat_mat); dist_mat <- dist(feat_mat); set.seed(42)
      sm <- tryCatch(MASS::sammon(dist_mat, k=2, niter=input$lfgwc_sammon_iter%||%500, magic=input$lfgwc_sammon_magic%||%0.2, trace=FALSE), error=function(e) NULL)
      if (is.null(sm)) return()
      k <- res$k; pal_c <- RColorBrewer::brewer.pal(min(max(k,3),8),(input$lfgwc_palette %||% "Dark2"))[seq_len(k)]; pt_sz <- input$lfgwc_sammon_pt%||%3
      cv <- as.integer(as.character(res$result_df$lfgwc_cluster))
      if (length(cv) > nrow(sm$points)) cv <- cv[seq_len(nrow(sm$points))]
      df_plot <- data.frame(Dim1=sm$points[,1], Dim2=sm$points[,2], Cluster=factor(cv, levels=seq_len(k)))
      p <- .dim_reduction_plot(df_plot=df_plot, k=k, pal_c=pal_c, pt_sz=pt_sz,
        title_str=paste0("LFGWC \u2014 Sammon Mapping (", toupper(res$algorithm), ") | k=", k),
        subtitle_str=paste0("Stress=", round(sm$stress,5), "  |  Iterasi=", input$lfgwc_sammon_iter%||%500),
        xlab="Dim 1", ylab="Dim 2")
      ggplot2::ggsave(file, plot=p, width=10, height=8, dpi=300, bg="white")
    }
  )

  # --------------------------------------------------------------------------
  # DOWNLOAD: t-SNE
  # --------------------------------------------------------------------------
  output$dl_lfgwc_tsne <- downloadHandler(
    filename = function() paste0("lfgwc_tsne_", input$lfgwc_algorithm, "_k", input$lfgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_lfgwc)
      res <- rv$cga_result_lfgwc; feat_mat <- as.matrix(res$feat_df)
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      perp <- min(input$lfgwc_tsne_perp%||%15, floor((nrow(feat_mat)-1)/3)); perp <- max(perp, 2); set.seed(42)
      tsne_res <- tryCatch(Rtsne::Rtsne(feat_mat, dims=2, perplexity=perp, max_iter=input$lfgwc_tsne_iter%||%1000, check_duplicates=FALSE, verbose=FALSE), error=function(e) NULL)
      if (is.null(tsne_res)) return()
      k <- res$k; pal_c <- RColorBrewer::brewer.pal(min(max(k,3),8),(input$lfgwc_palette %||% "Dark2"))[seq_len(k)]; pt_sz <- input$lfgwc_tsne_pt%||%3
      df_plot <- data.frame(Dim1=tsne_res$Y[,1], Dim2=tsne_res$Y[,2],
                            Cluster=factor(as.integer(as.character(res$result_df$lfgwc_cluster)), levels=seq_len(k)))
      p <- .dim_reduction_plot(df_plot=df_plot, k=k, pal_c=pal_c, pt_sz=pt_sz,
        title_str=paste0("LFGWC \u2014 t-SNE (", toupper(res$algorithm), ") | k=", k),
        subtitle_str=paste0("Perplexity=", perp, "  |  Iterasi=", input$lfgwc_tsne_iter%||%1000),
        xlab="t-SNE Dim 1", ylab="t-SNE Dim 2")
      ggplot2::ggsave(file, plot=p, width=10, height=8, dpi=300, bg="white")
    }
  )

  # --------------------------------------------------------------------------
  # DOWNLOAD: UMAP
  # --------------------------------------------------------------------------
  output$dl_lfgwc_umap <- downloadHandler(
    filename = function() paste0("lfgwc_umap_", input$lfgwc_algorithm, "_k", input$lfgwc_ncluster, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$cga_result_lfgwc)
      res <- rv$cga_result_lfgwc; feat_mat <- as.matrix(res$feat_df)
      if (is.null(feat_mat) || nrow(feat_mat) < 3) return()
      nn <- min(input$lfgwc_umap_nn%||%15, nrow(feat_mat)-1); nn <- max(nn,2); md <- input$lfgwc_umap_md%||%0.1; set.seed(42)
      umap_res <- tryCatch(uwot::umap(feat_mat, n_neighbors=nn, min_dist=md, n_components=2, verbose=FALSE), error=function(e) NULL)
      if (is.null(umap_res)) return()
      k <- res$k; pal_c <- RColorBrewer::brewer.pal(min(max(k,3),8),(input$lfgwc_palette %||% "Dark2"))[seq_len(k)]; pt_sz <- input$lfgwc_umap_pt%||%3
      df_plot <- data.frame(Dim1=umap_res[,1], Dim2=umap_res[,2],
                            Cluster=factor(as.integer(as.character(res$result_df$lfgwc_cluster)), levels=seq_len(k)))
      p <- .dim_reduction_plot(df_plot=df_plot, k=k, pal_c=pal_c, pt_sz=pt_sz,
        title_str=paste0("LFGWC \u2014 UMAP (", toupper(res$algorithm), ") | k=", k),
        subtitle_str=paste0("n_neighbors=", nn, "  |  min_dist=", md),
        xlab="UMAP Dim 1", ylab="UMAP Dim 2")
      ggplot2::ggsave(file, plot=p, width=10, height=8, dpi=300, bg="white")
    }
  )

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

  output$lfgwc_sammon_plot <- renderPlot({
    req(rv$cga_result_lfgwc)
    res       <- rv$cga_result_lfgwc
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
                   niter = input$lfgwc_sammon_iter  %||% 500,
                   magic = input$lfgwc_sammon_magic %||% 0.2,
                   trace = FALSE),
      error = function(e) {
        showNotification(paste("Sammon error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(sm)) return()

    k   <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), (input$lfgwc_palette %||% "Dark2"))[seq_len(k)]
    pt_sz <- input$lfgwc_sammon_pt %||% 3

    cluster_vec <- as.integer(as.character(res$result_df$lfgwc_cluster))
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
      title_str    = paste0("LFGWC — Sammon Mapping (",
                            toupper(res$algorithm), ") | k=", k),
      subtitle_str = paste0("Stress = ", round(sm$stress, 5),
                            "  |  Iterasi Sammon = ",
                            input$lfgwc_sammon_iter %||% 500),
      xlab         = "Dim 1",
      ylab         = "Dim 2"
    )
  })

  # ==========================================================================
  # TAB 9: t-SNE
  # ==========================================================================

  output$lfgwc_tsne_plot <- renderPlot({
    req(rv$cga_result_lfgwc)
    res      <- rv$cga_result_lfgwc
    feat_mat <- as.matrix(res$feat_df)

    if (is.null(feat_mat) || nrow(feat_mat) < 3) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk t-SNE.", cex = 1.1, col = "grey50")
      return()
    }

    perp <- min(input$lfgwc_tsne_perp %||% 15,
                floor((nrow(feat_mat) - 1) / 3))
    perp <- max(perp, 2)

    set.seed(42)
    tsne_res <- tryCatch(
      Rtsne::Rtsne(feat_mat,
                   dims             = 2,
                   perplexity       = perp,
                   max_iter         = input$lfgwc_tsne_iter %||% 1000,
                   check_duplicates = FALSE,
                   verbose          = FALSE),
      error = function(e) {
        showNotification(paste("t-SNE error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(tsne_res)) return()

    k     <- res$k
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), (input$lfgwc_palette %||% "Dark2"))[seq_len(k)]
    pt_sz <- input$lfgwc_tsne_pt %||% 3

    df_plot <- data.frame(
      Dim1    = tsne_res$Y[, 1],
      Dim2    = tsne_res$Y[, 2],
      Cluster = factor(as.integer(as.character(res$result_df$lfgwc_cluster)),
                       levels = seq_len(k))
    )

    .dim_reduction_plot(
      df_plot      = df_plot,
      k            = k,
      pal_c        = pal_c,
      pt_sz        = pt_sz,
      title_str    = paste0("LFGWC — t-SNE (",
                            toupper(res$algorithm), ") | k=", k),
      subtitle_str = paste0("Perplexity = ", perp,
                            "  |  Iterasi = ",
                            input$lfgwc_tsne_iter %||% 1000),
      xlab         = "t-SNE Dim 1",
      ylab         = "t-SNE Dim 2"
    )
  })

  # ==========================================================================
  # TAB 10: UMAP
  # ==========================================================================

  output$lfgwc_umap_plot <- renderPlot({
    req(rv$cga_result_lfgwc)
    res      <- rv$cga_result_lfgwc
    feat_mat <- as.matrix(res$feat_df)

    if (is.null(feat_mat) || nrow(feat_mat) < 3) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk UMAP.", cex = 1.1, col = "grey50")
      return()
    }

    nn <- min(input$lfgwc_umap_nn %||% 15, nrow(feat_mat) - 1)
    nn <- max(nn, 2)
    md <- input$lfgwc_umap_md %||% 0.1

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
    pal_c <- RColorBrewer::brewer.pal(min(max(k, 3), 8), (input$lfgwc_palette %||% "Dark2"))[seq_len(k)]
    pt_sz <- input$lfgwc_umap_pt %||% 3

    df_plot <- data.frame(
      Dim1    = umap_res[, 1],
      Dim2    = umap_res[, 2],
      Cluster = factor(as.integer(as.character(res$result_df$lfgwc_cluster)),
                       levels = seq_len(k))
    )

    .dim_reduction_plot(
      df_plot      = df_plot,
      k            = k,
      pal_c        = pal_c,
      pt_sz        = pt_sz,
      title_str    = paste0("LFGWC — UMAP (",
                            toupper(res$algorithm), ") | k=", k),
      subtitle_str = paste0("n_neighbors = ", nn,
                            "  |  min_dist = ", md),
      xlab         = "UMAP Dim 1",
      ylab         = "UMAP Dim 2"
    )
  })

} # end lfgwc_server()

