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
          title       = tags$span(icon("cog"), " DBSCAN Configuration"),
          status      = "warning",
          solidHeader = TRUE,
          width       = 12,

          # Info DBSCAN
          div(class  = "progress-box",
              style  = "background:#fff8e1; border-left-color:#f39c12;
                        font-size:12px; margin-bottom:10px;",
              icon("info-circle"),
              tags$strong(" DBSCAN"), " groups regions based on",
              tags$strong(" densitas"), " — tidak perlu menentukan k.",
              " Points too far from any cluster are classified",
              " sebagai ", tags$strong("noise (cluster 0)"), "."),

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

          # 3. Sumber Data
          div(class = "step-header", "3. Data Source"),
          radioButtons("dbs_data_source", NULL,
                       choices = c(
                         "Data Asli (tanpa transformasi)"   = "raw",
                         "Data Asli Ternormalisasi (0-1)"   = "raw_norm",
                         "Data Ter-standardisasi (Z-score)" = "standardized",
                         "SoVI Score"                       = "sovi",
                         "Skor RC (komponen PCA)"           = "rc"
                       ),
                       selected = "rc"
          ),

          conditionalPanel(
            "input.dbs_data_source == 'raw' ||
             input.dbs_data_source == 'raw_norm' ||
             input.dbs_data_source == 'standardized'",
            div(class = "step-header", "Pilih Variabel"),
            uiOutput("dbs_var_selector")
          ),

          conditionalPanel(
            "input.dbs_data_source == 'sovi' ||
             input.dbs_data_source == 'rc'",
            uiOutput("dbs_datasource_info")
          ),

          tags$hr(),

          # 4. Skala data
          checkboxInput("dbs_scale", "Standardisasi tambahan (Z-score)", value = FALSE),

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
                  div(class = "step-header", "Cluster Distribution (including Noise)"),
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
              title = tags$span(icon("chart-bar"), " Cluster Profile"),
              tags$br(),
              div(class = "step-header", "SoVI Distribution per Cluster"),
              plotOutput("dbs_boxplot", height = "350px"),
              tags$br(),
              div(class = "step-header", "Scatter: SoVI Score vs Cluster"),
              plotOutput("dbs_scatter", height = "300px")
            ),

            # Tab 4: Peta
            tabPanel(
              title = tags$span(icon("map"), " Cluster Map"),
              tags$br(),
              div(style = "font-size:13px; color:#78909c; margin-bottom:8px;",
                  icon("exclamation-circle"),
                  " Cluster 0 = ", tags$strong("Noise"),
                  " (regions that do not belong to any cluster)"),
              leaflet::leafletOutput("dbs_map", height = "500px")
            )
          )
        )
      )
    )
  )
}
