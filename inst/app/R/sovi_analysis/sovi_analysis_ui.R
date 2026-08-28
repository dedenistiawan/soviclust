# =============================================================================
# R/sovi_analysis/sovi_analysis_ui.R
# UI untuk menu SoVI Analysis
# Dipanggil dari ui.R sebagai satu tabItem
# =============================================================================

sovi_analysis_tab_ui <- function() {
  shinydashboard::tabItem("tab_sovi_analysis",
                          
                          fluidRow(
                            
                            # ══════════════════════════════════════════════════════════════════════
                            # PANEL KIRI — Control
                            # ══════════════════════════════════════════════════════════════════════
                            column(3,
                                   
                                   # ── Sumber Data ──────────────────────────────────────────────────
                                   shinydashboard::box(
                                     title       = tags$span(icon("database"), " Data Source"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     div(class = "step-header", "1. Select Data Source"),
                                     radioButtons(
                                       inputId  = "sa_data_source",
                                       label    = NULL,
                                       choices  = c(
                                         "Data Asli (tanpa transformasi)"   = "raw",
                                         "Data Asli Ternormalisasi (0-1)"   = "raw_norm",
                                         "Data Ter-standardisasi (Z-score)" = "standardized",
                                         "SoVI Score"                       = "sovi",
                                         "Skor RC (Komponen PCA)"           = "rc"
                                       ),
                                       selected = "raw"
                                     ),
                                     
                                     # Info box sumber data
                                     uiOutput("sa_source_info")
                                   ),
                                   
                                   # ── Pilih Variabel ───────────────────────────────────────────────
                                   shinydashboard::box(
                                     title       = tags$span(icon("list-ul"), " Variabel"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     div(class = "step-header", "2. Main Variable (Single Map)"),
                                     selectInput(
                                       inputId  = "sa_var1",
                                       label    = NULL,
                                       choices  = NULL,
                                       selected = NULL
                                     ),
                                     
                                     tags$hr(),
                                     
                                     div(class = "step-header", "3. Comparison Variable (Side-by-Side)"),
                                     tags$p(
                                       style = "font-size:12px; color:#78909c; margin-bottom:6px;",
                                       icon("info-circle"),
                                       " Digunakan di Tab Side-by-Side. Sumber data sama."
                                     ),
                                     selectInput(
                                       inputId  = "sa_var2",
                                       label    = NULL,
                                       choices  = NULL,
                                       selected = NULL
                                     )
                                   ),
                                   
                                   # ── Klasifikasi ──────────────────────────────────────────────────
                                   shinydashboard::box(
                                     title       = tags$span(icon("sliders-h"), " Jenks Classification"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     div(class = "step-header", "4. Number of Classes"),
                                     sliderInput(
                                       inputId = "sa_n_classes",
                                       label   = NULL,
                                       min     = 2,
                                       max     = 10,
                                       value   = 5,
                                       step    = 1
                                     ),
                                     
                                     tags$hr(),
                                     
                                     div(class = "step-header", "5. Auto GVF"),
                                     tags$p(
                                       style = "font-size:12px; color:#78909c; margin-bottom:8px;",
                                       "Cari jumlah kelas optimal secara otomatis (GVF \u2265 0.85)."
                                     ),
                                     fluidRow(
                                       column(6,
                                              numericInput(
                                                inputId = "sa_gvf_threshold",
                                                label   = "GVF Threshold",
                                                value   = 0.85,
                                                min     = 0.50,
                                                max     = 0.99,
                                                step    = 0.01
                                              )
                                       ),
                                       column(6,
                                              tags$br(),
                                              actionButton(
                                                inputId = "sa_auto_gvf",
                                                label   = tags$span(icon("magic"), " Auto GVF"),
                                                class   = "btn-warning btn-block"
                                              )
                                       )
                                     ),
                                     uiOutput("sa_gvf_result")
                                   ),
                                   
                                   # ── Overlay & Tampilkan ───────────────────────────────────────────
                                   shinydashboard::box(
                                     title       = tags$span(icon("layer-group"), " Overlay Options"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     div(class = "step-header", "6. Single Map Overlay"),
                                     checkboxInput(
                                       inputId = "sa_show_centroid",
                                       label   = tags$span(
                                         icon("map-marker-alt"),
                                         " Tampilkan centroid (warna = kelas SoVI)"
                                       ),
                                       value   = FALSE
                                     ),
                                     
                                     tags$hr(),
                                     
                                     div(class = "step-header", "7. Side-by-Side Overlay"),
                                     fluidRow(
                                       column(6,
                                              checkboxInput(
                                                inputId = "sa_show_centroid_left",
                                                label   = tags$span(icon("map-marker-alt"), " Peta Atas"),
                                                value   = FALSE
                                              )
                                       ),
                                       column(6,
                                              checkboxInput(
                                                inputId = "sa_show_centroid_right",
                                                label   = tags$span(icon("map-marker-alt"), " Peta Bawah"),
                                                value   = FALSE
                                              )
                                       )
                                     ),
                                     
                                     tags$hr(),
                                     
                                     actionButton(
                                       inputId = "sa_run",
                                       label   = tags$span(icon("map"), " Tampilkan Peta"),
                                       class   = "btn-primary btn-lg btn-block"
                                     ),
                                     tags$br(),
                                     uiOutput("sa_progress")
                                   )
                                   
                            ), # end column kiri
                            
                            # ══════════════════════════════════════════════════════════════════════
                            # PANEL KANAN — Output
                            # ══════════════════════════════════════════════════════════════════════
                            column(9,
                                   
                                   shinydashboard::box(
                                     title       = tags$span(icon("map-marked-alt"), " SoVI Analysis — Visualisasi Spasial"),
                                     status      = "info",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     tabsetPanel(
                                       id = "sa_tabs",
                                       
                                       # ── Tab 1: Peta Tunggal ─────────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("map"), " Peta Tunggal"),
                                         tags$br(),
                                         
                                         # Info bar variabel aktif
                                         uiOutput("sa_single_infobar"),
                                         tags$br(),
                                         
                                         # Peta leaflet tunggal
                                         leaflet::leafletOutput("sa_map_single", height = "520px")
                                       ),
                                       
                                       # ── Tab 2: Side-by-Side ─────────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("columns"), " Side-by-Side"),
                                         tags$br(),
                                         
                                         uiOutput("sa_sidebyside_infobar"),
                                         tags$br(),
                                         
                                         # Dua peta disinkronkan via leafsync
                                         uiOutput("sa_map_sidebyside_ui")
                                       ),
                                       
                                       # ── Tab 3: Ringkasan ────────────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("table"), " Ringkasan"),
                                         tags$br(),
                                         
                                         uiOutput("sa_summary_header"),
                                         tags$br(),
                                         
                                         fluidRow(
                                           # Ringkasan Variabel 1
                                           column(6,
                                                  div(class = "step-header", uiOutput("sa_summary_title1")),
                                                  DT::DTOutput("sa_summary_table1"),
                                                  tags$br(),
                                                  plotOutput("sa_gvf_plot1", height = "220px")
                                           ),
                                           # Ringkasan Variabel 2
                                           column(6,
                                                  div(class = "step-header", uiOutput("sa_summary_title2")),
                                                  DT::DTOutput("sa_summary_table2"),
                                                  tags$br(),
                                                  plotOutput("sa_gvf_plot2", height = "220px")
                                           )
                                         )
                                       )
                                       
                                     ) # end tabsetPanel
                                   ) # end box kanan
                                   
                            ) # end column kanan
                            
                          ) # end fluidRow
  ) # end tabItem
}