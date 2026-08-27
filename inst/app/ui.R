# =============================================================================
# ui.R — SoVI Shiny Application (Refactored)
#
# File ini hanya berisi:
#   1. Sidebar menu
#   2. Tab-tab statis (Home, Info, Data Upload) yang tidak dipisah
#   3. Pemanggilan fungsi UI dari setiap modul
#
# Tab yang SUDAH dipisah ke modul masing-masing:
#   var_config_tab_ui()             — R/var_config/var_config_ui.R
#   method_comparison_tab_ui()      — R/method_comparison/method_comparison_ui.R
#   sovi_computation_tab_ui()       — R/sovi_computation/sovi_computation_ui.R
#   extended_analysis_tab_ui()      — R/extended_analysis/extended_analysis_ui.R
#   clustgeo_tab_ui()               — R/cluster_geo/clustgeo_ui.R
#   fgwc_tab_ui()                   — R/FGWC/fgwc_ui.R
#   lfgwc_tab_ui()                  — R/LFGWC/lfgwc_ui.R
#   sovi_analysis_tab_ui()          — R/sovi_analysis/sovi_analysis_ui.R
#   downloads_tab_ui()              — R/downloads/downloads_ui.R
# =============================================================================

# Tab Home, Info, Data — isi kontennya sama dengan ui.R lama
# Dipindah ke fungsi lokal agar ui.R lebih terstruktur

home_tab_ui         <- function() shinydashboard::tabItem("tab_home",
                                                          fluidRow(column(12, div(class = "home-hero",
                                                                                  fluidRow(
                                                                                    column(9,
                                                                                           tags$h2(icon("shield-alt"), " Social Vulnerability Index (SoVI)"),
                                                                                           tags$p(tags$strong("SoVI Interactive Mapper:"),
                                                                                                  "Platform praktis untuk menganalisis kerentanan sosial dan risiko bencana secara spasial.")
                                                                                    ),
                                                                                    column(3, div(style = "text-align:right; opacity:0.20; font-size:80px;",
                                                                                                  icon("map-marked-alt")))
                                                                                  )
                                                          ))),
                                                          fluidRow(column(12,
                                                                          tags$h4(style = "color:#1a73c1; font-weight:700; margin:0 0 14px 4px;",
                                                                                  icon("star"), " Fitur Utama")
                                                          )),
                                                          fluidRow(lapply(list(
                                                            list("upload",       "Upload Data",       "Dataset .xlsx/.csv dan shapefile wilayah"),
                                                            list("sliders-h",    "Variable Config",   "Pilih variabel & tentukan arah (+/-) kerentanan"),
                                                            list("calculator",   "Hitung SoVI",       "PCA varimax, bobot loading, klasifikasi Jenks"),
                                                            list("map",          "Peta Interaktif",   "Visualisasi hasil SoVI dengan Leaflet"),
                                                            list("object-group", "Cluster Analysis",  "ClustGeo, FGWC & LFGWC: clustering spasial fleksibel"),
                                                            list("download",     "Download Hasil",    "Ekspor CSV dan PNG peta")
                                                          ), function(f) {
                                                            column(2, div(class = "home-feature-card",
                                                                          div(class = "icon-wrap", icon(f[[1]])),
                                                                          tags$h4(f[[2]]), tags$p(f[[3]])
                                                            ))
                                                          })),
                                                          fluidRow(column(12, tags$br(),
                                                                          shinydashboard::box(
                                                                            title = tags$span(icon("rocket"), " Quick Start \u2014 Alur Penggunaan"),
                                                                            status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                                                                            fluidRow(
                                                                              column(6,
                                                                                     div(class="workflow-step", div(class="step-number","1"),
                                                                                         div(class="step-content", tags$h5("Upload Data"),
                                                                                             tags$p("Upload dataset (.xlsx/.csv) dan shapefile wilayah"))),
                                                                                     div(class="workflow-step", div(class="step-number","2"),
                                                                                         div(class="step-content", tags$h5("Variable Config"),
                                                                                             tags$p("Pilih variabel SoVI dan tentukan direction (+/-)"))),
                                                                                     div(class="workflow-step", div(class="step-number","3"),
                                                                                         div(class="step-content",
                                                                                             tags$h5("Method Comparison", tags$span(class="badge-optional","Opsional")),
                                                                                             tags$p("Bandingkan 3 metode penentuan arah variabel")))
                                                                              ),
                                                                              column(6,
                                                                                     div(class="workflow-step", div(class="step-number","4"),
                                                                                         div(class="step-content", tags$h5("SoVI Computation"),
                                                                                             tags$p("Hitung SoVI score, lihat peta & diagnostik PCA"))),
                                                                                     div(class="workflow-step", div(class="step-number","5"),
                                                                                         div(class="step-content",
                                                                                             tags$h5("Cluster Analysis", tags$span(class="badge-optional","Baru")),
                                                                                             tags$p("ClustGeo, FGWC & LFGWC dengan parameter fleksibel"))),
                                                                                     div(class="workflow-step", div(class="step-number","6"),
                                                                                         div(class="step-content",
                                                                                             tags$h5("SoVI Analysis", tags$span(class="badge-optional","Baru")),
                                                                                             tags$p("Visualisasi Peta SoVI Per Variabel"))),
                                                                                     div(class="workflow-step", div(class="step-number","7"),
                                                                                         div(class="step-content", tags$h5("Downloads"),
                                                                                             tags$p("Download CSV dan PNG untuk publikasi")))
                                                                              )
                                                                            ),
                                                                            fluidRow(column(12, div(style="text-align:center; margin-top:12px;",
                                                                                                    actionButton("goto_upload",
                                                                                                                 tags$span(icon("arrow-right"), " Mulai: Upload Data"),
                                                                                                                 class = "btn-primary btn-lg")
                                                                            )))
                                                                          )
                                                          ))
)

upload_tab_ui <- function() shinydashboard::tabItem("tab_upload",

  # ── Banner: Pilihan Data ──────────────────────────────────────────────────
  fluidRow(column(12,
    div(class = "sample-data-banner",
      fluidRow(
        column(8,
          tags$h4(icon("database"), " Gunakan Data Sampel Bawaan",
                  style = "margin:0 0 6px 0; color:#1a73c1; font-weight:700;"),
          tags$p(
            style = "margin:0; color:#546e7a; font-size:13px;",
            icon("info-circle"), " Dataset: ",
            tags$strong("514 Kabupaten/Kota Indonesia"),
            " (15 variabel SoVI, tahun 2015) + Shapefile batas wilayah.",
            tags$br(),
            "Cocok untuk eksplorasi dan demo sebelum menggunakan data Anda sendiri."
          )
        ),
        column(4, div(style = "text-align:right; padding-top:4px;",
          actionButton("load_sample",
            tags$span(icon("play-circle"), " Muat Data Sampel"),
            class = "btn-primary btn-lg"
          ),
          tags$br(), tags$br(),
          actionButton("use_own_data",
            tags$span(icon("upload"), " Upload Data Sendiri"),
            class = "btn-default"
          )
        ))
      )
    )
  )),

  # ── Panel Upload (tersembunyi saat sample data aktif) ─────────────────────
  shinyjs::hidden(
    div(id = "panel_upload",
      fluidRow(
        column(6, shinydashboard::box(
          title = tags$span(icon("file-upload"), " Upload Dataset"),
          status = "primary", solidHeader = TRUE, width = 12,
          div(class = "upload-zone",
              icon("file-excel"),
              tags$p(style="color:#1a73c1;font-weight:600;margin:0;",
                     "Klik untuk upload atau drag & drop"),
              tags$p(style="color:#78909c;font-size:12px;margin:4px 0 0;",
                     "Format: .xlsx atau .csv | Maks 200 MB")),
          fileInput("file_data", NULL, accept = c(".xlsx",".csv"),
                    placeholder = "Belum ada file dipilih"),
          div(class = "step-header", "Konfigurasi Kolom"),
          fluidRow(
            column(6, selectInput("id_col",   "Kolom ID Wilayah",   choices = NULL)),
            column(6, selectInput("name_col", "Kolom Nama Wilayah", choices = NULL))
          ),
          div(class = "step-header", "Status Dataset"),
          verbatimTextOutput("data_status")
        )),
        column(6, shinydashboard::box(
          title = tags$span(icon("map"), " Upload Shapefile"),
          status = "primary", solidHeader = TRUE, width = 12,
          div(class = "upload-zone",
              icon("map-marked-alt"),
              tags$p(style="color:#1a73c1;font-weight:600;margin:0;",
                     "Pilih semua file shapefile sekaligus"),
              tags$p(style="color:#78909c;font-size:12px;margin:4px 0 0;",
                     ".shp + .dbf + .shx + .prj | Maks 200 MB")),
          fileInput("file_shp", NULL, multiple = TRUE,
                    accept = c(".shp",".dbf",".shx",".prj",".cpg"),
                    placeholder = "Belum ada file dipilih"),
          div(class = "step-header", "Konfigurasi Join"),
          selectInput("join_shp", "Kolom ID di Shapefile", choices = NULL),
          div(class = "step-header", "Status Shapefile"),
          verbatimTextOutput("shp_status")
        ))
      )
    ) # end hidden panel_upload
  ),

  # ── Status & Preview (selalu tampil) ─────────────────────────────────────
  fluidRow(column(12,
    div(id = "panel_status",
      shinydashboard::box(
        title = tags$span(icon("info-circle"), " Status Data"),
        status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
        fluidRow(
          column(6,
            div(class = "step-header", "Dataset"),
            verbatimTextOutput("data_status")
          ),
          column(6,
            div(class = "step-header", "Shapefile"),
            verbatimTextOutput("shp_status")
          )
        )
      )
    )
  )),

  fluidRow(column(12, shinydashboard::box(
    title = tags$span(icon("table"), " Preview Dataset"),
    status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
    DT::DTOutput("preview_data")
  ))),

  fluidRow(column(12, div(style="text-align:right; margin-bottom:20px;",
    actionButton("confirm_upload",
                 tags$span(icon("check-circle"), " Konfirmasi & Lanjut \u2192"),
                 class = "btn-success btn-lg")
  )))
)

# =============================================================================
# UI UTAMA
# =============================================================================

ui <- shinydashboard::dashboardPage(
  title = "SoVI Analysis App",
  skin  = "blue",

  # ── Header ──────────────────────────────────────────────────────────────────
  shinydashboard::dashboardHeader(
    title = tags$span(
      icon("map-marked-alt", style = "color:#90caf9;"),
      tags$span(" Vulnerability Mapping", style = "font-weight:700;")
    ),
    titleWidth = 260
  ),
  
  # ── Sidebar ─────────────────────────────────────────────────────────────────
  shinydashboard::dashboardSidebar(
    width = 260,
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      
      # ── Navigasi Utama ──────────────────────────────────────────────────────
      shinydashboard::menuItem(i18n$t("Beranda"), tabName = "tab_home",
                               icon = icon("home")),

      shinydashboard::menuItem("Info", icon = icon("info-circle"),
                               startExpanded = FALSE,
                               shinydashboard::menuSubItem("SoVI Workflow",   tabName = "tab_workflow",
                                                           icon = icon("project-diagram")),
                               shinydashboard::menuSubItem("SoVI Method",     tabName = "tab_sovimethod",
                                                           icon = icon("flask")),
                               shinydashboard::menuSubItem(i18n$t("Format Data"), tabName = "tab_files",
                                                           icon = icon("file-alt")),
                               shinydashboard::menuSubItem(i18n$t("Tim Pengembang"), tabName = "tab_team",
                                                           icon = icon("users"))
      ),

      shinydashboard::menuItem("Data", icon = icon("database"),
                               startExpanded = FALSE,
                               shinydashboard::menuSubItem(i18n$t("Upload Data"), tabName = "tab_upload",
                                                           icon = icon("upload")),
                               shinydashboard::menuSubItem(i18n$t("Informasi Dataset"), tabName = "tab_datainfo",
                                                           icon = icon("table"))
      ),

      tags$li(class = "divider-item"),

      # ── Pipeline SoVI ───────────────────────────────────────────────────────
      shinydashboard::menuItem(i18n$t("Konfigurasi Variabel"),
                               tabName = "tab_varconfig",
                               icon    = icon("sliders-h")),

      shinydashboard::menuItem(i18n$t("Method Comparison"),
                               tabName = "tab_comparison",
                               icon    = icon("balance-scale")),

      shinydashboard::menuItem(i18n$t("Hitung SoVI"),
                               tabName = "tab_sovi",
                               icon    = icon("calculator")),

      shinydashboard::menuItem(i18n$t("Extended Analysis"),
                               tabName = "tab_analysis",
                               icon    = icon("chart-bar")),

      # ── Cluster Analysis ────────────────────────────────────────────────────
      shinydashboard::menuItem(i18n$t("Cluster Analysis"),
                               icon = icon("object-group"), startExpanded = FALSE,
                               shinydashboard::menuSubItem("ClustGeo",  tabName = "tab_clustgeo_adv",
                                                           icon = icon("globe-asia")),
                               shinydashboard::menuSubItem("FGWC",      tabName = "tab_fgwc",
                                                           icon = icon("hubspot")),
                               shinydashboard::menuSubItem("LFGWC",     tabName = "tab_lfgwc",
                                                           icon = icon("map-pin")),
                               shinydashboard::menuSubItem("ALFGWC",    tabName = "tab_alfgwc",
                                                           icon = icon("map-marked-alt")),
                               shinydashboard::menuSubItem("K-Means",   tabName = "tab_kmeans",
                                                           icon = icon("circle-notch")),
                               shinydashboard::menuSubItem("DBSCAN",    tabName = "tab_dbscan",
                                                           icon = icon("project-diagram"))
      ),

      shinydashboard::menuItem(i18n$t("SoVI Analysis"),
                               tabName = "tab_sovi_analysis",
                               icon    = icon("chart-area")),

      shinydashboard::menuItem(i18n$t("Unduh Hasil"),
                               tabName = "tab_download",
                               icon    = icon("download")),
      
      # ── Footer Sidebar ──────────────────────────────────────────────────────
      tags$li(class = "divider-item"),
      tags$li(tags$div(
        class = "sidebar-footer-info",
        icon("user-tie"),
        tags$a(href   = "https://dedenistiawan.netlify.app/",
               target = "_blank",
               style  = "color:rgba(255,255,255,0.65); text-decoration:none;",
               " Deden Istiawan"),
        tags$br(),
        icon("code"),           " Built with R Shiny", tags$br(),
        icon("map-marker-alt"), " Indonesia"
      ))
    )
  ),
  
  # ── Body ────────────────────────────────────────────────────────────────────
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$script(src   = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js",
                  async = NA, id = "MathJax-script")
    ),
    
    shinydashboard::tabItems(
      
      # ── Tab Statis: Home ─────────────────────────────────────────────────
      home_tab_ui(),
      
      # ── Tab Statis: Info (SoVI Workflow, Method, Files, Team) ───────────
      # Konten Info tetap sama persis dengan ui.R lama
      # Disimpan dari ui.R lama tanpa perubahan
      shinydashboard::tabItem("tab_workflow",
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(icon("project-diagram"), " Pipeline SoVI \u2014 7 Fase"),
                                status = "primary", solidHeader = TRUE, width = 12,
                                tags$p(style = "color:#37474f; font-size:13.5px;",
                                       "Pipeline mengikuti ", tags$strong("Cutter et al. (2003)"),
                                       " dengan peningkatan berupa bobot proporsional loading dan ",
                                       "tiga opsi penentuan arah variabel."),
                                fluidRow(lapply(list(
                                  list("1","Load Data","upload","Dataset & shapefile diupload user"),
                                  list("2","Z-score","equals","Standardisasi: mean=0, SD=1"),
                                  list("3","PCA","cogs","KMO, Bartlett, communality, Kaiser, Varimax"),
                                  list("4","Seleksi Var","filter","Threshold |\u03c4|\u22650.5, 1 variabel \u2192 1 komponen"),
                                  list("5","Agregasi","calculator","Bobot proporsional loading, normalisasi 0\u20131"),
                                  list("6","Jenks","tags","Natural Breaks 5 kelas kerentanan"),
                                  list("7","Output","map","Peta Leaflet, LISA, ClustGeo, Sensitivity")
                                ), function(p) {
                                  column(width = 12, div(class = "workflow-step",
                                                         div(class = "step-number", p[[1]]),
                                                         div(class = "step-content",
                                                             tags$h5(icon(p[[3]]), " ", p[[2]]),
                                                             tags$p(p[[4]]))
                                  ))
                                }))
                              )))
      ),
      
      shinydashboard::tabItem("tab_sovimethod",
                              
                              # ── Hero header ────────────────────────────────────────────────────
                              fluidRow(column(12,
                                              div(style = "background:linear-gradient(135deg,#1a73c1,#42a5f5);
                       border-radius:10px; padding:28px 32px; margin-bottom:20px; color:#fff;",
                                                  tags$h2(style = "margin:0 0 6px; font-weight:700;",
                                                          icon("flask"), " SoVI Direction Method"),
                                                  tags$p(style = "margin:0; font-size:14.5px; opacity:.9;",
                                                         "Penjelasan tiga metode penentuan arah (direction) variabel ",
                                                         "dalam pipeline Social Vulnerability Index (SoVI). Setiap metode ",
                                                         "menghasilkan bobot berbeda pada agregasi skor komponen.")
                                              )
                              )),
                              
                              # ── Ringkasan 3 metode ─────────────────────────────────────────────
                              fluidRow(
                                column(4, div(class="info-card", style="border-left-color:#1a73c1; min-height:130px;",
                                              tags$h4(icon("star", style="color:#1a73c1"), " Theory-Based (PM)"),
                                              tags$p(style="font-size:13px; color:#37474f;",
                                                     "Arah variabel ditentukan ", tags$strong("sepenuhnya berdasarkan teori"),
                                                     " kerentanan sosial. Ditetapkan secara a priori sebelum PCA."),
                                              tags$span(class="badge-optional",
                                                        style="background:#e3f2fd;color:#1a73c1;border-color:#90caf9;",
                                                        "Direkomendasikan")
                                )),
                                column(4, div(class="info-card", style="border-left-color:#1976d2; min-height:130px;",
                                              tags$h4(icon("chart-line", style="color:#1976d2"), " Loading Sign"),
                                              tags$p(style="font-size:13px; color:#37474f;",
                                                     "Arah variabel ditentukan ", tags$strong("dari tanda loading PCA"),
                                                     " secara empiris. Variabel mengikuti tanda loading-nya pada komponen tempat ia berada.")
                                )),
                                column(4, div(class="info-card", style="border-left-color:#42a5f5; min-height:130px;",
                                              tags$h4(icon("book", style="color:#42a5f5"), " Cutter's Method"),
                                              tags$p(style="font-size:13px; color:#37474f;",
                                                     "Arah komponen dari ", tags$strong("variabel dominan"),
                                                     " (loading absolut terbesar) per komponen — metode asli Cutter et al. (2003).")
                                ))
                              ),
                              
                              # ── Method 1: Theory-Based ─────────────────────────────────────────
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(
                                  tags$span(style="background:#1a73c1;color:#fff;padding:3px 10px;
                             border-radius:4px;font-size:13px;margin-right:8px;", "01"),
                                  tags$span(style="font-weight:700; font-size:15px;", "Theory-Based (PM)")
                                ),
                                status = "primary", solidHeader = FALSE, width = 12, collapsible = TRUE,
                                fluidRow(
                                  column(6,
                                         div(class="step-header","Konsep Dasar"),
                                         tags$p(style="font-size:13.5px;color:#37474f;line-height:1.8;",
                                                "Berlandaskan premis bahwa arah kontribusi setiap variabel sudah diketahui dari teori ",
                                                "sebelum analisis dimulai. Peneliti menetapkan setiap variabel sebagai ",
                                                tags$strong("+1"), " (meningkatkan) atau ", tags$strong("-1"), " (menurunkan) kerentanan."),
                                         div(class="step-header","Rumus"),
                                         div(style="background:#f8faff;border:1px solid #90caf9;border-radius:8px;padding:14px 18px;",
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Direction (teori):")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ d_i = \\begin{cases} +1 & \\text{meningkatkan kerentanan} \\\\ -1 & \\text{menurunkan kerentanan} \\end{cases} \\]")),
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Bobot proporsional loading:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ w_{ik} = \\frac{|\\lambda_{ik}|}{\\sum_{j \\in C_k} |\\lambda_{jk}|} \\]")),
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Skor komponen & SoVI:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ RC_k = \\sum_{i \\in C_k} w_{ik} \\cdot d_i \\cdot z_i \\quad\\Rightarrow\\quad \\text{SoVI} = \\frac{\\sum RC_k - \\min}{\\max - \\min} \\]"))
                                         )
                                  ),
                                  column(6,
                                         div(class="info-card", style="border-left-color:#27ae60;padding:12px 16px;margin-top:30px;",
                                             tags$h4(style="font-size:13.5px;color:#27ae60;margin-bottom:6px;",
                                                     icon("check-circle"), " Keunggulan"),
                                             tags$ul(style="font-size:13px;margin:0;padding-left:18px;",
                                                     tags$li("Konsisten dengan teori kerentanan sosial"),
                                                     tags$li("Tidak bergantung pada artefak matematis PCA"),
                                                     tags$li("Hasil lebih mudah diinterpretasikan secara substantif"),
                                                     tags$li("Reprodusibel antar dataset yang berbeda")
                                             )
                                         )
                                  )
                                )
                              ))),
                              
                              # ── Method 2: Loading Sign ─────────────────────────────────────────
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(
                                  tags$span(style="background:#1976d2;color:#fff;padding:3px 10px;
                             border-radius:4px;font-size:13px;margin-right:8px;", "02"),
                                  tags$span(style="font-weight:700; font-size:15px;", "Loading Sign")
                                ),
                                status = "info", solidHeader = FALSE, width = 12, collapsible = TRUE,
                                fluidRow(
                                  column(6,
                                         div(class="step-header","Konsep Dasar"),
                                         tags$p(style="font-size:13.5px;color:#37474f;line-height:1.8;",
                                                "Metode ini bersifat ", tags$strong("sepenuhnya empiris"),
                                                ". Arah kontribusi variabel ditentukan dari tanda loading PCA. ",
                                                "Tidak ada penetapan direction a priori — data yang menentukan strukturnya."),
                                         div(class="step-header","Rumus"),
                                         div(style="background:#f8faff;border:1px solid #90caf9;border-radius:8px;padding:14px 18px;",
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Direction dari tanda loading:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ d_i = \\begin{cases} +\\,\\text{sign}(\\lambda_{ik^*}) & i \\notin \\text{neg\\_vars} \\\\ -\\,\\text{sign}(\\lambda_{ik^*}) & i \\in \\text{neg\\_vars} \\end{cases} \\]")),
                                             tags$p(style="font-size:12px;color:#78909c;",
                                                    HTML("dimana \\( k^* = \\arg\\max_k |\\lambda_{ik}| \\) adalah komponen dominan variabel \\( i \\)"))
                                         )
                                  ),
                                  column(6,
                                         div(class="info-card", style="border-left-color:#e74c3c;padding:12px 16px;margin-top:30px;",
                                             tags$h4(style="font-size:13.5px;color:#e74c3c;margin-bottom:6px;",
                                                     icon("exclamation-triangle"), " Perhatian"),
                                             tags$ul(style="font-size:13px;margin:0;padding-left:18px;",
                                                     tags$li("Loading negatif bisa jadi artefak matematis PCA, bukan cerminan teori"),
                                                     tags$li("Hasil dapat berbeda antar dataset karena tergantung struktur korelasi"),
                                                     tags$li("Cocok untuk eksplorasi, namun perlu validasi teoritik")
                                             )
                                         )
                                  )
                                )
                              ))),
                              
                              # ── Method 3: Cutter's Method ──────────────────────────────────────
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(
                                  tags$span(style="background:#42a5f5;color:#fff;padding:3px 10px;
                             border-radius:4px;font-size:13px;margin-right:8px;", "03"),
                                  tags$span(style="font-weight:700; font-size:15px;",
                                            "Cutter's Method",
                                            tags$span(style="font-size:12px;font-weight:400;color:#78909c;margin-left:8px;",
                                                      "Cutter et al., 2003"))
                                ),
                                status = "info", solidHeader = FALSE, width = 12, collapsible = TRUE,
                                fluidRow(
                                  column(6,
                                         div(class="step-header","Konsep Dasar"),
                                         tags$p(style="font-size:13.5px;color:#37474f;line-height:1.8;",
                                                "Metode orisinal Cutter et al. (2003). Arah setiap komponen ditentukan oleh ",
                                                tags$strong("variabel dominan"), " (loading absolut terbesar) di komponen tersebut. ",
                                                "Semua variabel dalam satu komponen mengikuti arah komponen itu."),
                                         div(class="step-header","Rumus"),
                                         div(style="background:#f8faff;border:1px solid #90caf9;border-radius:8px;padding:14px 18px;",
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Variabel dominan per komponen:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ v_k^* = \\arg\\max_{i \\in C_k}\\, |\\lambda_{ik}| \\]")),
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Direction komponen dari variabel dominan:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ D_k = \\text{sign}\\left(\\lambda_{v_k^*, k}\\right) \\quad\\Rightarrow\\quad RC_k = D_k \\cdot F_k \\]")),
                                             tags$p(style="font-size:12px;color:#78909c;",
                                                    HTML("dimana \\( F_k \\) adalah factor score komponen ke-\\(k\\) dari PCA"))
                                         )
                                  ),
                                  column(6,
                                         div(class="info-card", style="border-left-color:#42a5f5;padding:12px 16px;margin-top:30px;",
                                             tags$h4(style="font-size:13.5px;color:#42a5f5;margin-bottom:6px;",
                                                     icon("book-open"), " Referensi Asli"),
                                             tags$p(style="font-size:13px;margin:0;",
                                                    tags$strong("Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003)."),
                                                    " Social vulnerability to environmental hazards. ",
                                                    tags$em("Social Science Quarterly"), ", 84(2), 242\u2013261.",
                                                    tags$br(),
                                                    tags$a(href="https://doi.org/10.1111/1540-6237.8402002",
                                                           target="_blank", style="font-size:12px;",
                                                           icon("external-link-alt"), " doi.org/10.1111/1540-6237.8402002")
                                             )
                                         )
                                  )
                                )
                              ))),
                              
                              # ── Tabel Perbandingan ─────────────────────────────────────────────
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(icon("table"), " Perbandingan Ketiga Metode"),
                                status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                                tags$table(class = "format-table",
                                           tags$thead(tags$tr(
                                             tags$th("Aspek"),
                                             tags$th(icon("star"), " Theory-Based (PM)"),
                                             tags$th(icon("chart-line"), " Loading Sign"),
                                             tags$th(icon("book"), " Cutter's Method")
                                           )),
                                           tags$tbody(
                                             tags$tr(tags$td(tags$strong("Sumber direction")),
                                                     tags$td("Teori (a priori)"),
                                                     tags$td("Tanda loading PCA"),
                                                     tags$td("Variabel dominan per komponen")),
                                             tags$tr(tags$td(tags$strong("Unit direction")),
                                                     tags$td("Per variabel"), tags$td("Per variabel"), tags$td("Per komponen")),
                                             tags$tr(tags$td(tags$strong("Bobot agregasi")),
                                                     tags$td("Proporsional |loading|"),
                                                     tags$td("Proporsional |loading|"),
                                                     tags$td("Factor scores PCA langsung")),
                                             tags$tr(tags$td(tags$strong("Ketergantungan data")),
                                                     tags$td(tags$span(style="color:#27ae60;","Rendah")),
                                                     tags$td(tags$span(style="color:#e74c3c;","Tinggi")),
                                                     tags$td(tags$span(style="color:#f39c12;","Sedang"))),
                                             tags$tr(tags$td(tags$strong("Reprodusibilitas")),
                                                     tags$td(tags$span(style="color:#27ae60;","Tinggi")),
                                                     tags$td(tags$span(style="color:#f39c12;","Sedang")),
                                                     tags$td(tags$span(style="color:#27ae60;","Tinggi"))),
                                             tags$tr(tags$td(tags$strong("Rekomendasi")),
                                                     tags$td(tags$span(style="color:#27ae60;font-weight:700;",
                                                                       icon("check-circle"), " Utama")),
                                                     tags$td(tags$span(style="color:#f39c12;",
                                                                       icon("exclamation-circle"), " Eksplorasi")),
                                                     tags$td(tags$span(style="color:#1a73c1;",
                                                                       icon("book-open"), " Replikasi Cutter")))
                                           )
                                )
                              )))
      ),
      
      shinydashboard::tabItem("tab_files",
                              fluidRow(
                                column(6, shinydashboard::box(
                                  title = tags$span(icon("file-excel"), " Format Dataset"),
                                  status = "primary", solidHeader = TRUE, width = 12,
                                  tags$p(style="font-size:13px; color:#37474f;",
                                         "Upload file ", tags$code(".xlsx"), " atau ", tags$code(".csv"),
                                         " dengan struktur minimal:"),
                                  tags$table(class = "format-table",
                                             tags$thead(tags$tr(tags$th("Kolom"), tags$th("Tipe"), tags$th("Keterangan"))),
                                             tags$tbody(
                                               tags$tr(tags$td(tags$code("DISTRICTCODE")), tags$td("Chr/Num"),
                                                       tags$td("ID unik \u2014 cocok dengan shapefile")),
                                               tags$tr(tags$td(tags$code("KABUPATEN")), tags$td("Karakter"),
                                                       tags$td("Nama wilayah")),
                                               tags$tr(tags$td(tags$code("VAR_1...n")), tags$td("Numerik"),
                                                       tags$td("Variabel SoVI dalam % (0\u2013100)"))
                                             )
                                  )
                                )),
                                column(6, shinydashboard::box(
                                  title = tags$span(icon("map"), " Format Shapefile"),
                                  status = "primary", solidHeader = TRUE, width = 12,
                                  tags$p(style="font-size:13px; color:#37474f;",
                                         "Upload semua file berikut ", tags$strong("sekaligus"), ":"),
                                  tags$table(class = "format-table",
                                             tags$thead(tags$tr(tags$th("Ekstensi"), tags$th("Keterangan"), tags$th("Status"))),
                                             tags$tbody(
                                               tags$tr(tags$td(tags$code(".shp")), tags$td("Geometri wilayah"),
                                                       tags$td(tags$span(style="color:#27ae60;font-weight:bold;","\u2713 Wajib"))),
                                               tags$tr(tags$td(tags$code(".dbf")), tags$td("Atribut data"),
                                                       tags$td(tags$span(style="color:#27ae60;font-weight:bold;","\u2713 Wajib"))),
                                               tags$tr(tags$td(tags$code(".shx")), tags$td("Index geometri"),
                                                       tags$td(tags$span(style="color:#27ae60;font-weight:bold;","\u2713 Wajib"))),
                                               tags$tr(tags$td(tags$code(".prj")), tags$td("Proyeksi CRS"), tags$td("Disarankan")),
                                               tags$tr(tags$td(tags$code(".cpg")), tags$td("Encoding karakter"), tags$td("Opsional"))
                                             )
                                  )
                                ))
                              )
      ),
      
      shinydashboard::tabItem("tab_team",
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(icon("users"), " Tim Pengembang"),
                                status = "primary", solidHeader = TRUE, width = 12,
                                fluidRow(
                                  column(3, div(class="team-card",
                                                div(class="team-avatar", icon("user-tie")),
                                                tags$h5("Deden Istiawan"), tags$p("Ketua Peneliti"),
                                                tags$p(icon("envelope"), " deden.istiawan@itesa.ac.id"))),
                                  column(3, div(class="team-card",
                                                div(class="team-avatar", icon("user-tie")),
                                                tags$h5("Herman Yuliansyah"), tags$p("Ketua TPM"),
                                                tags$p(icon("envelope"), " herman.yuliansyah@tif.uad.ac.id"))),
                                  column(3, div(class="team-card",
                                                div(class="team-avatar", icon("user-tie")),
                                                tags$h5("Rusydi Umar"), tags$p("Anggota TPM"),
                                                tags$p(icon("envelope"), " rusydi@mti.uad.ac.id"))),
                                  column(3, div(class="team-card",
                                                div(class="team-avatar", icon("university")),
                                                tags$h5("Itesa Muhammadiyah"), tags$p("Universitas"),
                                                tags$p(icon("globe"), " www.itesa.ac.id")))
                                ),
                                tags$hr(),
                                div(class = "info-card",
                                    tags$h4(icon("book-open"), " Referensi Utama"),
                                    tags$p(style="font-size:13px;",
                                           tags$strong("Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003)."),
                                           tags$br(), "Social vulnerability to environmental hazards.",
                                           tags$em(" Social Science Quarterly"), ", 84(2), 242\u2013261.",
                                           tags$br(),
                                           tags$a(href="https://doi.org/10.1111/1540-6237.8402002",
                                                  target="_blank", icon("external-link-alt"),
                                                  " doi.org/10.1111/1540-6237.8402002")))
                              )))
      ),
      
      shinydashboard::tabItem("tab_datainfo",
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(icon("table"), " Informasi & Ketentuan Dataset"),
                                status = "info", solidHeader = TRUE, width = 12,
                                fluidRow(
                                  column(6, div(class="info-card",
                                                tags$h4(icon("list-ol"), " Struktur Kolom Wajib"),
                                                tags$table(class="format-table",
                                                           tags$thead(tags$tr(tags$th("No"), tags$th("Kolom"), tags$th("Isi"))),
                                                           tags$tbody(
                                                             tags$tr(tags$td("1"), tags$td(tags$code("DISTRICTCODE")), tags$td("ID unik wilayah")),
                                                             tags$tr(tags$td("2"), tags$td(tags$code("KABUPATEN")),    tags$td("Nama wilayah")),
                                                             tags$tr(tags$td("3+"),tags$td(tags$code("VARIABEL_n")),   tags$td("Variabel numerik SoVI (%)"))
                                                           )
                                                )
                                  )),
                                  column(6, div(class="info-card",
                                                tags$h4(icon("check-circle"), " Ketentuan Data"),
                                                tags$ul(style="font-size:13px;",
                                                        tags$li("Variabel dalam ", tags$strong("satuan persentase (0\u2013100)")),
                                                        tags$li("Tidak ada ", tags$strong("missing value")),
                                                        tags$li("Minimal ", tags$strong("2 variabel"), " untuk PCA"),
                                                        tags$li("Minimal ", tags$strong("50 unit wilayah")),
                                                        tags$li("ID wilayah ", tags$strong("unik & konsisten"), " dengan shapefile")
                                                )
                                  ))
                                )
                              )))
      ),
      
      # ── Tab Upload Data ──────────────────────────────────────────────────
      upload_tab_ui(),
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB MODULAR — Dipanggil dari modul masing-masing
      # ══════════════════════════════════════════════════════════════════════
      
      # Pipeline SoVI
      var_config_tab_ui(),
      method_comparison_tab_ui(),
      sovi_computation_tab_ui(),
      extended_analysis_tab_ui(),
      
      # Cluster Analysis
      clustgeo_tab_ui(),
      fgwc_tab_ui(),
      lfgwc_tab_ui(),
      alfgwc_tab_ui(),
      kmeans_tab_ui(),
      dbscan_tab_ui(),
      
      # Analisis Tambahan
      sovi_analysis_tab_ui(),
      
      # Downloads
      downloads_tab_ui()
      
    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage