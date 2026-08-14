# =============================================================================
# R/var_config/var_config_server.R
# Server Logic Tab Variable Configuration
#
# DIPANGGIL DARI : server.R via var_config_server(input, output, session, rv)
# FUNGSI UTAMA   :
#   - Select all / deselect all variabel
#   - Render tabel direction (+/-) per variabel secara dinamis
#   - Status konfigurasi real-time
#   - Konfirmasi variabel → unlock tab berikutnya
# =============================================================================

var_config_server <- function(input, output, session, rv, unlock_tab) {
  
  # ==========================================================================
  # OBSERVER: Tombol Pilih Semua
  # ==========================================================================
  
  observeEvent(input$select_all_vars, {
    req(rv$data)
    num_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    updateCheckboxGroupInput(session, "sovi_vars", selected = num_cols)
  })
  
  # ==========================================================================
  # OBSERVER: Tombol Batal Semua
  # ==========================================================================
  
  observeEvent(input$deselect_all_vars, {
    updateCheckboxGroupInput(session, "sovi_vars", selected = character(0))
  })
  
  # ==========================================================================
  # OUTPUT: Tabel direction (+/-) per variabel
  # Dirender ulang setiap kali pilihan variabel berubah
  # ==========================================================================
  
  output$direction_ui <- renderUI({
    req(input$sovi_vars, length(input$sovi_vars) > 0)
    vars <- input$sovi_vars
    
    tagList(
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        
        # ── Header tabel ────────────────────────────────────────────────────
        tags$thead(
          tags$tr(
            tags$th(style = "padding:6px; background:#f8f9fa;
                             border-bottom:2px solid #dee2e6;",
                    "Variabel"),
            tags$th(style = "padding:6px; background:#f8f9fa;
                             border-bottom:2px solid #dee2e6;",
                    "Direction"),
            tags$th(style = "padding:6px; background:#f8f9fa;
                             border-bottom:2px solid #dee2e6;",
                    "Keterangan")
          )
        ),
        
        # ── Baris per variabel ──────────────────────────────────────────────
        tags$tbody(
          lapply(vars, function(v) {
            input_id <- paste0("dir_", v)
            
            tags$tr(
              style = "border-bottom:1px solid #f0f0f0;",
              
              # Nama variabel
              tags$td(style = "padding:5px 8px; font-weight:500;", v),
              
              # Radio button direction
              tags$td(style = "padding:5px 8px;",
                      shinyWidgets::radioGroupButtons(
                        inputId      = input_id,
                        label        = NULL,
                        choiceNames  = list(
                          tags$span(style = "color:#155724; font-weight:600;",
                                    "+ (Positif)"),
                          tags$span(style = "color:#721c24; font-weight:600;",
                                    "- (Negatif)")
                        ),
                        choiceValues = list("pos", "neg"),
                        selected     = "pos",
                        size         = "sm",
                        status       = "default",
                        individual   = TRUE
                      ),
                      
                      # Override warna tombol aktif via CSS inline
                      tags$style(HTML(paste0(
                        "#", input_id,
                        " .btn[data-value='pos'].active {",
                        "  background-color:#28a745 !important;",
                        "  border-color:#1e7e34 !important;",
                        "  color:#fff !important; }",
                        "#", input_id,
                        " .btn[data-value='neg'].active {",
                        "  background-color:#dc3545 !important;",
                        "  border-color:#bd2130 !important;",
                        "  color:#fff !important; }",
                        "#", input_id,
                        " .btn[data-value='pos']:not(.active) {",
                        "  background-color:#f8f9fa;",
                        "  border-color:#ced4da;",
                        "  color:#6c757d; }",
                        "#", input_id,
                        " .btn[data-value='neg']:not(.active) {",
                        "  background-color:#f8f9fa;",
                        "  border-color:#ced4da;",
                        "  color:#6c757d; }"
                      )))
              ),
              
              # Keterangan singkat
              tags$td(style = "padding:5px 8px; font-size:12px; color:#888;",
                      "+ = meningkatkan kerentanan | - = menurunkan kerentanan")
            )
          })
        )
      )
    )
  })
  
  # ==========================================================================
  # OUTPUT: Status konfigurasi (live update real-time)
  # Menampilkan jumlah variabel positif/negatif dan status konfirmasi
  # ==========================================================================
  
  output$varconfig_status <- renderUI({
    vars <- input$sovi_vars
    
    # Belum memilih variabel
    if (is.null(vars) || length(vars) == 0) {
      return(div(
        class = "progress-box",
        style = "background:#f8f9fa; border-left-color:#adb5bd; font-size:13px;",
        icon("info-circle", style = "color:#6c757d;"),
        tags$span(style = "color:#6c757d;", " Pilih variabel terlebih dahulu.")
      ))
    }
    
    # Hitung jumlah variabel negatif dan positif
    neg_count <- sum(vapply(vars, function(v) {
      val <- input[[paste0("dir_", v)]]
      !is.null(val) && val == "neg"
    }, logical(1)))
    pos_count <- length(vars) - neg_count
    
    # Style berdasarkan status konfirmasi
    if (isTRUE(rv$vars_ok)) {
      status_icon  <- icon("check-circle", style = "color:#27ae60;")
      status_label <- tags$span(style = "color:#27ae60; font-weight:700;",
                                " \u2713 Konfigurasi dikonfirmasi")
      box_style    <- "background:#eafaf1; border-left-color:#27ae60;"
    } else {
      status_icon  <- icon("clock", style = "color:#f39c12;")
      status_label <- tags$span(style = "color:#f39c12; font-weight:700;",
                                " Belum dikonfirmasi")
      box_style    <- "background:#fff8e1; border-left-color:#f39c12;"
    }
    
    div(
      class = "progress-box",
      style = paste0(box_style, " font-size:13px; padding:10px 14px;"),
      
      # Baris status
      div(status_icon, status_label),
      tags$hr(style = "margin:6px 0; border-color:rgba(0,0,0,0.08);"),
      
      # Ringkasan variabel
      div(style = "line-height:1.8;",
          tags$span(style = "color:#37474f;",
                    icon("layer-group"),
                    tags$strong(" Total variabel : "),
                    tags$span(style = "font-weight:700; color:#1a73c1;", length(vars))
          ),
          tags$br(),
          tags$span(style = "color:#155724;",
                    icon("plus-circle"),
                    tags$strong(" Positif (+) : "),
                    tags$span(style = "font-weight:700; color:#28a745;", pos_count)
          ),
          tags$br(),
          tags$span(style = "color:#721c24;",
                    icon("minus-circle"),
                    tags$strong(" Negatif (-) : "),
                    tags$span(style = "font-weight:700; color:#dc3545;", neg_count)
          )
      )
    )
  })
  
  # ==========================================================================
  # OBSERVER: Tombol Konfirmasi Konfigurasi
  # Menyimpan pilihan variabel & direction ke rv, lalu unlock tab berikutnya
  # ==========================================================================
  
  observeEvent(input$confirm_vars, {
    
    # Validasi: minimal 2 variabel dipilih
    if (is.null(input$sovi_vars) || length(input$sovi_vars) < 2) {
      showNotification("Pilih minimal 2 variabel!", type = "warning")
      return()
    }
    
    vars     <- input$sovi_vars
    neg_vars <- character(0)
    
    # Kumpulkan variabel yang ditandai negatif
    for (v in vars) {
      val <- input[[paste0("dir_", v)]]
      if (!is.null(val) && val == "neg") {
        neg_vars <- c(neg_vars, v)
      }
    }
    
    # Simpan ke reactive values (shared state)
    rv$sovi_vars <- vars
    rv$neg_vars  <- neg_vars
    rv$vars_ok   <- TRUE
    
    # Unlock tab Method Comparison (opsional) dan SoVI Computation
    unlock_tab("tab_comparison")
    unlock_tab("tab_sovi")
    
    showNotification(
      paste0("\u2713 ", length(vars), " variabel dikonfigurasi (",
             length(neg_vars), " negatif). ",
             "Lanjut ke SoVI Computation."),
      type     = "message",
      duration = 5
    )
    
    # Navigasi otomatis ke SoVI Computation
    shinydashboard::updateTabItems(session, "sidebar_menu", "tab_sovi")
  })
  
} # end var_config_server()