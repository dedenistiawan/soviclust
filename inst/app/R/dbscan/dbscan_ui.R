# =============================================================================
# R/dbscan/dbscan_ui.R
# UI Tab DBSCAN Clustering (Density-Based Spatial Clustering)
#
# DIPANGGIL DARI: ui.R via dbscan_tab_ui()
# =============================================================================

dbscan_tab_ui <- function() {

  shinydashboard::tabItem("tab_dbscan",

    fluidRow(

      # ── Kolom Kiri: Parameter ──────────────────────────────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("cog"), " Konfigurasi DBSCAN"),
          status      = "warning",
          solidHeader = TRUE,
          width       = 12,

          # Info DBSCAN
          div(class  = "progress-box",
              style  = "background:#fff8e1; border-left-color:#f39c12;
                        font-size:12px; margin-bottom:10px;",
              icon("info-circle"),
              tags$strong(" DBSCAN"), " mengelompokkan wilayah berdasarkan",
              tags$strong(" densitas"), " — tidak perlu menentukan k.",
              " Titik yang terlalu jauh dari klaster mana pun diklasifikasi",
              " sebagai ", tags$strong("noise (klaster 0)"), "."),

          # 1. Epsilon (radius tetangga)
          div(class = "step-header", "1. Epsilon (\u03b5) — Radius Tetangga"),
          sliderInput("dbs_eps", NULL,
                      min = 0.01, max = 1.0, value = 0.15, step = 0.01),
          div(style = "font-size:12px; color:#78909c; margin-top:-10px;",
              icon("lightbulb"),
              " Gunakan Tab 'k-NN Distance' untuk memilih eps optimal."),

          tags$hr(),

          # 2. MinPts
          div(class = "step-header", "2. MinPts — Minimum Anggota"),
          sliderInput("dbs_minpts", NULL,
                      min = 2, max = 20, value = 5, step = 1),
          div(style = "font-size:12px; color:#78909c; margin-top:-10px;",
              "Aturan: MinPts \u2265 dimensi data + 1."),

          tags$hr(),

          # 3. Variabel Input
          div(class = "step-header", "3. Variabel Input"),
          radioButtons("dbs_input", NULL,
                       choices = c(
                         "SoVI Score saja"        = "sovi",
                         "Skor RC (Komponen PCA)" = "rc",
                         "SoVI + RC"              = "sovi_rc"
                       ),
                       selected = "sovi_rc"),

          tags$hr(),

          # 4. Skala data
          checkboxInput("dbs_scale", "Standardisasi input (Z-score)", value = TRUE),

          tags$hr(),

          actionButton("run_dbscan", "Run DBSCAN",
                       class = "btn-warning btn-lg btn-block",
                       icon  = icon("play")),
          tags$br(), tags$br(),
          uiOutput("dbs_progress")
        )
      ),

      # ── Kolom Kanan: Output ───────────────────────────────────────────────
      column(9,
        shinydashboard::box(
          title       = tags$span(icon("project-diagram"), " Hasil DBSCAN Clustering"),
          status      = "warning",
          solidHeader = TRUE,
          width       = 12,

          tabsetPanel(

            # Tab 1: Ringkasan
            tabPanel(
              title = tags$span(icon("table"), " Ringkasan"),
              tags$br(),
              fluidRow(
                column(6,
                  div(class = "step-header", "Distribusi Klaster (termasuk Noise)"),
                  DT::DTOutput("dbs_summary_table")
                ),
                column(6,
                  div(class = "step-header", "Parameter yang Digunakan"),
                  uiOutput("dbs_params_info")
                )
              ),
              tags$br(),
              div(class = "step-header", "Data per Wilayah"),
              DT::DTOutput("dbs_detail_table")
            ),

            # Tab 2: k-NN Distance (untuk pilih eps)
            tabPanel(
              title = tags$span(icon("chart-line"), " k-NN Distance"),
              tags$br(),
              div(class = "step-header", "k-NN Distance Plot — Bantu Memilih Epsilon"),
              div(style = "font-size:13px; color:#546e7a; margin-bottom:10px;",
                  icon("info-circle"),
                  " Cari titik ", tags$strong("\"siku\" (knee/elbow)"),
                  " pada plot. Nilai jarak di titik siku adalah eps yang baik."),
              plotOutput("dbs_knn_plot", height = "380px"),
              fluidRow(
                column(4,
                  sliderInput("dbs_knn_k", "k untuk k-NN Distance:",
                              min = 2, max = 20, value = 5, step = 1)
                )
              )
            ),

            # Tab 3: Profil Klaster
            tabPanel(
              title = tags$span(icon("chart-bar"), " Profil Klaster"),
              tags$br(),
              div(class = "step-header", "Distribusi SoVI per Klaster"),
              plotOutput("dbs_boxplot", height = "350px"),
              tags$br(),
              div(class = "step-header", "Scatter: SoVI Score vs Klaster"),
              plotOutput("dbs_scatter", height = "300px")
            ),

            # Tab 4: Peta
            tabPanel(
              title = tags$span(icon("map"), " Peta Klaster"),
              tags$br(),
              div(style = "font-size:13px; color:#78909c; margin-bottom:8px;",
                  icon("exclamation-circle"),
                  " Klaster 0 = ", tags$strong("Noise"),
                  " (wilayah yang tidak masuk klaster manapun)"),
              leaflet::leafletOutput("dbs_map", height = "500px")
            )
          )
        )
      )
    )
  )
}
