# =============================================================================
# R/kmeans/kmeans_ui.R
# UI Tab K-Means Clustering
#
# DIPANGGIL DARI: ui.R via kmeans_tab_ui()
# =============================================================================

kmeans_tab_ui <- function() {

  shinydashboard::tabItem("tab_kmeans",

    fluidRow(

      # ── Kolom Kiri: Parameter ──────────────────────────────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("cog"), " Konfigurasi K-Means"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,

          # 1. Jumlah Cluster
          div(class = "step-header", "1. Jumlah Cluster (k)"),
          radioButtons("km_k_mode", NULL,
                       choices  = c("Manual" = "manual",
                                    "Otomatis (Elbow)" = "auto"),
                       selected = "manual", inline = TRUE),

          conditionalPanel(
            "input.km_k_mode == 'manual'",
            sliderInput("km_k", "Jumlah Cluster (k)",
                        min = 2, max = 10, value = 4, step = 1)
          ),
          conditionalPanel(
            "input.km_k_mode == 'auto'",
            sliderInput("km_k_max", "k Maksimum untuk Pencarian",
                        min = 3, max = 12, value = 10, step = 1),
            div(class = "progress-box",
                style = "background:#e3f2fd; border-left-color:#1a73c1; font-size:12px;",
                icon("info-circle"),
                " k optimal dipilih dari titik elbow Within-SS.")
          ),

          tags$hr(),

          # 2. Sumber Data
          div(class = "step-header", "2. Sumber Data"),
          radioButtons("km_data_source", NULL,
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
            "input.km_data_source == 'raw' ||
             input.km_data_source == 'raw_norm' ||
             input.km_data_source == 'standardized'",
            div(class = "step-header", "Pilih Variabel"),
            uiOutput("km_var_selector")
          ),

          conditionalPanel(
            "input.km_data_source == 'sovi' ||
             input.km_data_source == 'rc'",
            uiOutput("km_datasource_info")
          ),

          tags$hr(),

          # 3. Algoritma
          div(class = "step-header", "3. Algoritma"),
          selectInput("km_algorithm", NULL,
                      choices  = c("Hartigan-Wong (default)" = "Hartigan-Wong",
                                   "Lloyd (MacQueen)"        = "Lloyd",
                                   "Forgy"                   = "Forgy"),
                      selected = "Hartigan-Wong"),
          sliderInput("km_nstart", "Starts (ulangan random)",
                      min = 1, max = 50, value = 25, step = 1),

          tags$hr(),

          actionButton("run_kmeans", "Run K-Means",
                       class = "btn-primary btn-lg btn-block",
                       icon  = icon("play")),
          tags$br(), tags$br(),
          uiOutput("km_progress")
        )
      ),

      # ── Kolom Kanan: Output ───────────────────────────────────────────────
      column(9,
        shinydashboard::box(
          title       = tags$span(icon("object-group"), " Hasil K-Means Clustering"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,

          tabsetPanel(

            # Tab 1: Ringkasan Klaster
            tabPanel(
              title = tags$span(icon("table"), " Ringkasan"),
              tags$br(),
              fluidRow(
                column(6,
                  div(class = "step-header", "Distribusi Anggota Klaster"),
                  DT::DTOutput("km_summary_table")
                ),
                column(6,
                  div(class = "step-header", "Statistik Within-Cluster SS"),
                  DT::DTOutput("km_stats_table")
                )
              ),
              tags$br(),
              div(class = "step-header", "10 Wilayah Tiap Klaster (SoVI Tertinggi)"),
              DT::DTOutput("km_detail_table")
            ),

            # Tab 2: Elbow Plot
            tabPanel(
              title = tags$span(icon("chart-line"), " Elbow / Silhouette"),
              tags$br(),
              fluidRow(
                column(6,
                  div(class = "step-header", "Within-SS per k (Elbow Method)"),
                  plotOutput("km_elbow_plot", height = "300px")
                ),
                column(6,
                  div(class = "step-header", "Silhouette Score per k"),
                  plotOutput("km_sil_plot", height = "300px")
                )
              )
            ),

            # Tab 3: Profil Klaster
            tabPanel(
              title = tags$span(icon("chart-bar"), " Profil Klaster"),
              tags$br(),
              div(class = "step-header", "Distribusi SoVI Score per Klaster"),
              plotOutput("km_boxplot", height = "350px"),
              tags$br(),
              div(class = "step-header", "Rata-rata Variabel per Klaster"),
              plotOutput("km_heatmap", height = "350px")
            ),

            # Tab 4: Peta
            tabPanel(
              title = tags$span(icon("map"), " Peta Klaster"),
              tags$br(),
              leaflet::leafletOutput("km_map", height = "520px")
            )
          )
        )
      )
    )
  )
}
