# =============================================================================
# server.R — SoVI Shiny Application (Refactored)
#
# File ini hanya berisi:
#   1. Reactive values (shared state seluruh aplikasi)
#   2. Helper lock/unlock tab
#   3. Logic Data Upload (Tab 1)
#   4. Pemanggilan server function setiap modul
#
# CATATAN PENTING — unlock_tab & lock_tab:
#   Kedua fungsi ini didefinisikan di dalam server() dan harus DIPASS
#   secara eksplisit ke setiap modul sebagai parameter.
#   Modul tidak bisa mengakses fungsi lokal server() secara langsung.
# =============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE VALUES — Shared state seluruh aplikasi
  # ============================================================================
  rv <- reactiveValues(
    
    # ── Tab Upload Data ────────────────────────────────────────────────────
    data      = NULL,
    shp       = NULL,
    upload_ok = FALSE,
    
    # ── Tab Variable Config ────────────────────────────────────────────────
    sovi_vars = NULL,
    neg_vars  = NULL,
    vars_ok   = FALSE,
    
    # ── Tab Method Comparison ─────────────────────────────────────────────
    comp_results = NULL,
    
    # ── Tab SoVI Computation ──────────────────────────────────────────────
    sovi_result = NULL,
    sovi_ok     = FALSE,
    
    # ── Tab Extended Analysis ─────────────────────────────────────────────
    dominant_df = NULL,
    profile_df  = NULL,
    cluster_res = NULL,
    moran_res   = NULL,
    sensitivity = NULL,
    cutter_df   = NULL,
    analysis_ok = FALSE,
    
    # ── Tab Cluster Analysis ──────────────────────────────────────────────
    cga_result       = NULL,
    cga_result_fgwc  = NULL,
    cga_result_lfgwc = NULL
  )
  
  # ============================================================================
  # HELPER: Lock / Unlock tab di sidebar
  # Didefinisikan di sini dan di-PASS ke setiap modul sebagai parameter
  # ============================================================================
  
  lock_tab <- function(tabname) {
    shinyjs::runjs(sprintf(
      "var el = document.querySelector(\"a[data-value='%s']\");
       if(el) el.parentElement.classList.add('menu-locked');",
      tabname
    ))
  }
  
  unlock_tab <- function(tabname) {
    shinyjs::runjs(sprintf(
      "var el = document.querySelector(\"a[data-value='%s']\");
       if(el) el.parentElement.classList.remove('menu-locked');",
      tabname
    ))
  }
  
  # ============================================================================
  # INISIALISASI: Kunci semua pipeline tab saat aplikasi pertama dibuka
  # ============================================================================
  observe({
    shinyjs::runjs("
      ['tab_varconfig','tab_comparison','tab_sovi','tab_analysis',
       'tab_clustgeo_adv','tab_fgwc','tab_lfgwc','tab_alfgwc',
       'tab_sovi_analysis','tab_download']
        .forEach(function(t) {
          var el = document.querySelector(\"a[data-value='\" + t + \"']\");
          if (el) el.parentElement.classList.add('menu-locked');
        });
      var clusterMenuLinks = document.querySelectorAll(
        '.sidebar-menu .treeview > a'
      );
      clusterMenuLinks.forEach(function(link) {
        var submenu = link.nextElementSibling;
        if (submenu) {
          var subItems = submenu.querySelectorAll('a[data-value]');
          var allLocked = Array.from(subItems).every(function(si) {
            return ['tab_clustgeo_adv','tab_fgwc','tab_lfgwc','tab_alfgwc']
                     .indexOf(si.getAttribute('data-value')) >= 0;
          });
          if (allLocked && subItems.length > 0) {
            link.classList.add('menu-locked');
          }
        }
      });
    ")
  }) |> bindEvent(session$clientData$url_hostname, once = TRUE)
  
  # Tombol Home → navigasi ke Upload
  observeEvent(input$goto_upload, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "tab_upload")
  })
  
  # ============================================================================
  # TAB 1 — DATA UPLOAD
  # ============================================================================

  # ── Tombol: Muat Data Sampel ──────────────────────────────────────────────
  observeEvent(input$load_sample, {

    withProgress(message = "Memuat data sampel...", value = 0, {

      # 1. Load dataset dari inst/extdata
      incProgress(0.2, detail = "Membaca dataset...")
      extdata <- system.file("extdata", package = "soviclust")
      data_path <- file.path(extdata, "sovi_data_kab_514.RData")
      env <- new.env()
      load(data_path, envir = env)
      df <- as.data.frame(get(ls(env)[1], envir = env))

      # 2. Load shapefile dari inst/app/map
      incProgress(0.4, detail = "Membaca shapefile...")
      shp_path <- system.file("app", "map", "514_kabupaten.shp", package = "soviclust")
      sf::sf_use_s2(FALSE)
      shp <- sf::st_read(shp_path, quiet = TRUE)
      shp <- sf::st_make_valid(shp)
      sf::sf_use_s2(TRUE)

      # 3. Tambah kolom ID dan Nama dari shapefile ke dataset
      #    (dataset 514 baris diurutkan sesuai shapefile)
      incProgress(0.6, detail = "Menggabungkan data...")
      df <- cbind(
        ID    = as.character(shp$idkab),
        Nama  = as.character(shp$nmkab),
        df
      )

      # 4. Simpan ke reactive values
      rv$data      <- df
      rv$shp       <- shp
      rv$upload_ok <- FALSE

      # 5. Update UI inputs
      incProgress(0.8, detail = "Mengisi konfigurasi...")
      cols     <- names(df)
      num_cols <- cols[sapply(df, is.numeric)]

      updateSelectInput(session, "id_col",   choices = cols, selected = "ID")
      updateSelectInput(session, "name_col", choices = cols, selected = "Nama")
      updateSelectInput(session, "join_shp", choices = setdiff(names(shp), attr(shp, "sf_column")),
                        selected = "idkab")
      updateCheckboxGroupInput(session, "sovi_vars",
                               choices  = num_cols,
                               selected = num_cols)

      # 6. Sembunyikan panel upload (tidak perlu upload manual)
      shinyjs::hide("panel_upload")

      incProgress(1.0)
    }) # end withProgress

    showNotification(
      paste0("\u2713 Data sampel dimuat: ", nrow(rv$data), " kabupaten/kota, ",
             sum(sapply(rv$data, is.numeric)), " variabel SoVI."),
      type = "message", duration = 5
    )
  })

  # ── Tombol: Upload Data Sendiri ────────────────────────────────────────────
  observeEvent(input$use_own_data, {
    shinyjs::show("panel_upload")
    # Reset data jika sebelumnya muat sampel
    rv$data      <- NULL
    rv$shp       <- NULL
    rv$upload_ok <- FALSE
    updateSelectInput(session, "id_col",   choices = NULL)
    updateSelectInput(session, "name_col", choices = NULL)
    updateSelectInput(session, "join_shp", choices = NULL)
  })

  # ── Upload file dataset manual ─────────────────────────────────────────────
  observeEvent(input$file_data, {
    req(input$file_data)
    tryCatch({
      path <- input$file_data$datapath
      ext  <- tolower(tools::file_ext(input$file_data$name))

      df <- if (ext == "xlsx") {
        as.data.frame(readxl::read_excel(path))
      } else {
        read.csv(path, stringsAsFactors = FALSE)
      }

      rv$data  <- df
      cols     <- names(df)
      num_cols <- cols[sapply(df, is.numeric)]

      id_guess <- cols[grep("id|code|kode", cols, ignore.case = TRUE)][1]
      if (is.na(id_guess)) id_guess <- cols[1]

      name_guess <- cols[grep("name|nama|kab|city|wilayah",
                              cols, ignore.case = TRUE)][1]
      if (is.na(name_guess)) name_guess <- cols[2]

      updateSelectInput(session, "id_col",   choices = cols, selected = id_guess)
      updateSelectInput(session, "name_col", choices = cols, selected = name_guess)
      updateCheckboxGroupInput(session, "sovi_vars",
                               choices  = num_cols,
                               selected = num_cols)

      rv$upload_ok <- FALSE

    }, error = function(e) {
      showNotification(paste("Error membaca dataset:", e$message),
                       type = "error", duration = 8)
    })
  })

  
  observeEvent(input$file_shp, {
    req(input$file_shp)
    tryCatch({
      shp    <- read_shapefile(input$file_shp)
      rv$shp <- shp
      
      geom_col <- attr(shp, "sf_column")
      shp_cols <- setdiff(names(shp), geom_col)
      id_guess <- shp_cols[grep("id|code|kode", shp_cols, ignore.case = TRUE)][1]
      if (is.na(id_guess)) id_guess <- shp_cols[1]
      
      updateSelectInput(session, "join_shp",
                        choices  = shp_cols,
                        selected = id_guess)
      rv$upload_ok <- FALSE
      
    }, error = function(e) {
      showNotification(paste("Error membaca shapefile:", e$message),
                       type = "error", duration = 8)
    })
  })
  
  output$data_status <- renderPrint({
    if (is.null(rv$data)) { cat("Belum ada dataset yang diupload."); return() }
    num_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    cat("\u2713 Dataset dimuat:", nrow(rv$data), "baris x", ncol(rv$data), "kolom\n")
    cat("Kolom numerik  :", length(num_cols), "kolom\n")
    cat("Kolom          :", paste(names(rv$data), collapse = ", "))
  })
  
  output$shp_status <- renderPrint({
    if (is.null(rv$shp)) { cat("Belum ada shapefile yang diupload."); return() }
    cat("\u2713 Shapefile dimuat:", nrow(rv$shp), "unit spasial\n")
    cat("CRS            :", sf::st_crs(rv$shp)$input, "\n")
    geom_col <- attr(rv$shp, "sf_column")
    cat("Kolom          :", paste(setdiff(names(rv$shp), geom_col), collapse = ", "))
  })
  
  output$preview_data <- DT::renderDT({
    req(rv$data)
    DT::datatable(head(rv$data, 10),
                  options  = list(scrollX = TRUE, pageLength = 10, dom = "t"),
                  rownames = FALSE)
  })
  
  observeEvent(input$confirm_upload, {
    if (is.null(rv$data)) {
      showNotification("Upload dataset terlebih dahulu!", type = "warning"); return()
    }
    if (is.null(rv$shp)) {
      showNotification("Upload shapefile terlebih dahulu!", type = "warning"); return()
    }
    rv$upload_ok <- TRUE
    unlock_tab("tab_varconfig")
    showNotification(
      "\u2713 Data dikonfirmasi. Silakan lanjut ke Variable Config.",
      type = "message", duration = 4
    )
    shinydashboard::updateTabItems(session, "sidebar_menu", "tab_varconfig")
  })
  
  # ============================================================================
  # PANGGIL SERVER MODUL
  # unlock_tab dipass sebagai parameter agar bisa digunakan di dalam modul
  # ============================================================================
  
  # ── Pipeline SoVI ──────────────────────────────────────────────────────────
  var_config_server(input, output, session, rv, unlock_tab)
  method_comparison_server(input, output, session, rv)
  sovi_computation_server(input, output, session, rv, unlock_tab)
  extended_analysis_server(input, output, session, rv)
  
  # ── Cluster Analysis ───────────────────────────────────────────────────────
  clustgeo_server(input, output, session, rv)
  fgwc_server(input, output, session, rv)
  lfgwc_server(input, output, session, rv)
  alfgwc_server(input, output, session, rv)
  
  # ── Analisis Lanjutan & Downloads ──────────────────────────────────────────
  sovi_analysis_server(input, output, session, rv)
  downloads_server(input, output, session, rv)
  
} # end server()