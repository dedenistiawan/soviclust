# =============================================================================
# R/FGWC/fgwc_ui.R
# UI Tab FGWC — Submenu Cluster Analysis
#
# DIPANGGIL DARI: ui.R via fgwc_tab_ui()
# BERISI       : definisi tabItem("tab_fgwc", ...)
#
# LAYOUT:
#   Baris 1 (atas)  : Setting Parameter — 4 kolom
#     Kolom 1 (w=3) : Data Pendukung FGWC + Sumber Data
#     Kolom 2 (w=3) : Parameter FGWC
#     Kolom 3 (w=3) : Algoritma Optimasi
#     Kolom 4 (w=3) : Parameter Algoritma + Jalankan
#   Baris 2 (bawah) : Hasil FGWC — full width (w=12)
# =============================================================================

fgwc_tab_ui <- function() {

  shinydashboard::tabItem("tab_fgwc",

    # ══════════════════════════════════════════════════════════════════════════
    # BARIS 1: Setting Parameter (atas) — 4 kolom
    # ══════════════════════════════════════════════════════════════════════════
    fluidRow(

      # ── KOLOM 1 (width=3): Data Pendukung FGWC + Sumber Data ───────────────
      column(3,

        # ── Box: Upload Data Pendukung ────────────────────────────────────────
        shinydashboard::box(
          title       = tags$span(icon("upload"), " FGWC Supporting Data"),
          status      = "warning",
          solidHeader = TRUE,
          width       = 12,

          div(class = "sample-data-banner", style = "padding:10px 14px; margin-bottom:10px;",
            tags$p(style = "margin:0 0 6px 0; font-weight:700; color:#1a73c1; font-size:13px;",
                   icon("database"), " Use FGWC Sample Data"),
            tags$p(style = "margin:0 0 8px 0; font-size:11.5px; color:#546e7a;",
                   "Distance matrix and population data for 514 Indonesian regencies/cities."),
            actionButton("fgwc_load_sample", tags$span(icon("play-circle"), " Load Sample Data"),
              class = "btn-primary btn-sm btn-block")
          ),
          div(class = "step-header", "1. Distance Matrix Input"),
          radioButtons("fgwc_dist_mode", NULL,
                       choices  = c("Upload n\u00d7n Distance Matrix" = "matrix",
                                    "Upload Longitude & Latitude"   = "lonlat"),
                       selected = "matrix"
          ),

          # Mode: Upload matriks jarak langsung
          conditionalPanel(
            "input.fgwc_dist_mode == 'matrix'",
            tags$p(style = "font-size:12px; color:#78909c; margin-bottom:6px;",
                   icon("info-circle"),
                   " File Excel/CSV with distance matrix n\u00d7n regions."),
            fileInput("fgwc_file_dist", NULL,
                      accept = c(".xlsx", ".csv"),
                      placeholder = "No file selected")
          ),

          # Mode: Hitung dari lon/lat
          conditionalPanel(
            "input.fgwc_dist_mode == 'lonlat'",
            tags$p(style = "font-size:12px; color:#78909c; margin-bottom:6px;",
                   icon("info-circle"),
                   " Excel/CSV file with columns: DISTRICTCODE, longitude, latitude."),
            fileInput("fgwc_file_lonlat", NULL,
                      accept = c(".xlsx", ".csv"),
                      placeholder = "No file selected"),
            div(class = "progress-box",
                style = "background:#e8f5e9; border-left-color:#27ae60;
                         font-size:11.5px; margin-bottom:6px;",
                icon("info-circle"),
                " Distance is calculated automatically using",
                tags$strong(" Haversine Distance"),
                " (unit: kilometers).")
          ),

          uiOutput("fgwc_dist_status"),

          tags$hr(),

          div(class = "step-header", "2. Upload Population Data"),
          tags$p(style = "font-size:12px; color:#78909c; margin-bottom:6px;",
                 icon("info-circle"),
                 " Excel/CSV file: 1 population column (n rows)."),
          fileInput("fgwc_file_pop", NULL,
                    accept = c(".xlsx", ".csv"),
                    placeholder = "No file selected"),
          uiOutput("fgwc_pop_status")
        ),

        # ── Box: Sumber Data Fitur ─────────────────────────────────────────────
        shinydashboard::box(
          title       = tags$span(icon("database"), " Data Source"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,

          div(class = "step-header", "3. Feature Data Source"),
          radioButtons("fgwc_data_source", NULL,
                       choices = c(
                         "Original Data (no transformation)" = "raw",
                         "Normalized Data (0-1)"            = "raw_norm",
                         "Standardized Data (Z-score)"      = "standardized",
                         "SoVI Score"                       = "sovi",
                         "RC Scores (PCA components)"       = "rc"
                       ),
                       selected = "rc"
          ),

          conditionalPanel(
            "input.fgwc_data_source == 'raw' ||
             input.fgwc_data_source == 'raw_norm' ||
             input.fgwc_data_source == 'standardized'",
            div(class = "step-header", "Select Variables"),
            uiOutput("fgwc_var_selector")
          ),

          conditionalPanel(
            "input.fgwc_data_source == 'sovi' ||
             input.fgwc_data_source == 'rc'",
            uiOutput("fgwc_datasource_info")
          )
        )
      ),

      # ── KOLOM 2 (width=3): Parameter FGWC ─────────────────────────────────
      column(3,

        # ── Box: Parameter FGWC ───────────────────────────────────────────────
        shinydashboard::box(
          title       = tags$span(icon("cog"), " FGWC Parameters"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,
          collapsible = TRUE,

          div(class = "step-header", "4. Number of Clusters"),
          sliderInput("fgwc_ncluster", NULL,
                      min = 2, max = 10, value = 4, step = 1),

          div(class = "step-header", "Fuzzifier (m)"),
          sliderInput("fgwc_m", NULL,
                      min = 1.1, max = 3.0, value = 2.0, step = 0.1),

          div(class = "step-header", "Spatial Alpha (\u03b1)"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "\u03b1=1 \u2192 pure FCM | \u03b1=0 \u2192 full spatial weight"),
          sliderInput("fgwc_alpha", NULL,
                      min = 0.0, max = 1.0, value = 0.5, step = 0.05),

          fluidRow(
            column(6,
                   div(class = "step-header", "Distance Magnitude (a)"),
                   numericInput("fgwc_a", NULL, value = 1.2, min = 0, step = 0.1)
            ),
            column(6,
                   div(class = "step-header", "Population Magnitude (b)"),
                   numericInput("fgwc_b", NULL, value = 1.2, min = 0, step = 0.1)
            )
          ),

          div(class = "step-header", "Max. Iterations"),
          numericInput("fgwc_maxiter", NULL, value = 500, min = 10, step = 50),

          div(class = "step-header", "Random Seed"),
          numericInput("fgwc_seed", NULL, value = 0, min = 0, step = 1)
        )
      ),

      # ── KOLOM 3 (width=3): Algoritma Optimasi ─────────────────────────────
      column(3,

        # ── Box: Pilih Algoritma ──────────────────────────────────────────────
        shinydashboard::box(
          title       = tags$span(icon("microchip"), " Optimization Algorithm"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,

          div(class = "step-header", "5. Select Algorithm"),
          radioButtons("fgwc_algorithm", NULL,
                       choices = c(
                         "Classic FGWC"                      = "classic",
                         "ABC (Artificial Bee Colony)"  = "abc",
                         "FPA (Flower Pollination)"     = "fpa",
                         "GSA (Gravitational Search)"   = "gsa",
                         "GWO (Grey Wolf Optimizer)"    = "gwo",
                         "HHO (Harris-Hawk Optimization)" = "hho",
                         "IFA (Intelligent Firefly)"    = "ifa",
                         "PSO (Particle Swarm)"         = "pso",
                         "TLBO (Teaching-Learning Based)" = "tlbo",
                         "WOA (Whale Optimization)"     = "woa"
                       ),
                       selected = "classic"
          )
        )
      ),

      # ── KOLOM 4 (width=3): Parameter Algoritma + Jalankan ─────────────────
      column(3,

        # ── Box: Parameter Algoritma ──────────────────────────────────────────
        shinydashboard::box(
          title       = tags$span(icon("sliders-h"), " Algorithm Parameters"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,
          collapsible = TRUE,

          # Universal — semua algoritma selain classic
          conditionalPanel(
            "input.fgwc_algorithm != 'classic'",
            div(class = "step-header", "Number of Particles / Agents"),
            sliderInput("fgwc_npar", NULL,
                        min = 3, max = 30, value = 10, step = 1),
            div(class = "step-header", "Convergence (same)"),
            sliderInput("fgwc_same", NULL,
                        min = 5, max = 30, value = 10, step = 1),
            div(class = "step-header", "Initialization Distribution"),
            selectInput("fgwc_vi_dist", NULL,
                        choices  = c("Uniform" = "uniform", "Normal" = "normal"),
                        selected = "uniform")
          ),

          conditionalPanel(
            "input.fgwc_algorithm == 'classic'",
            div(class = "progress-box",
                style = "background:#f8f9fa; border-left-color:#adb5bd; font-size:12px;",
                icon("info-circle"),
                " Classic FGWC has no additional optimization parameters.")
          ),

          # ABC
          conditionalPanel("input.fgwc_algorithm == 'abc'",
            div(class = "step-header", "Number of Onlooker Bees"),
            sliderInput("fgwc_abc_onlooker", NULL,
                        min = 2, max = 20, value = 5, step = 1),
            div(class = "step-header", "Limit (Scout)"),
            sliderInput("fgwc_abc_limit", NULL,
                        min = 1, max = 20, value = 5, step = 1)
          ),

          # FPA
          conditionalPanel("input.fgwc_algorithm == 'fpa'",
            div(class = "step-header", "Switch Prob. (p)"),
            sliderInput("fgwc_fpa_p", NULL,
                        min = 0.1, max = 0.9, value = 0.7, step = 0.05),
            div(class = "step-header", "Gamma"),
            numericInput("fgwc_fpa_gamma", NULL,
                         value = 1.2, min = 0.1, step = 0.1),
            div(class = "step-header", "Lambda (Levy)"),
            numericInput("fgwc_fpa_lambda", NULL,
                         value = 1.5, min = 0.1, step = 0.1),
            div(class = "step-header", "EI Distribution"),
            selectInput("fgwc_fpa_ei", NULL,
                        choices  = c("logchaotic","normal","uniform",
                                     "kentchaotic","levy"),
                        selected = "logchaotic")
          ),

          # GSA
          conditionalPanel("input.fgwc_algorithm == 'gsa'",
            div(class = "step-header", "Gravitational Constant (G)"),
            numericInput("fgwc_gsa_G", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "Vmax"),
            sliderInput("fgwc_gsa_vmax", NULL,
                        min = 0.1, max = 2.0, value = 0.7, step = 0.1),
            checkboxInput("fgwc_gsa_new",
                          "Use new GSA version (Li & Dong 2017)",
                          value = FALSE)
          ),

          # GWO
          conditionalPanel("input.fgwc_algorithm == 'gwo'",
            div(class = "progress-box",
                style = "background:#e8f5e9; border-left-color:#27ae60; font-size:12px;",
                icon("info-circle"),
                " GWO only uses universal parameters.", tags$br(),
                "Set the number of wolves via ", tags$strong("Number of Particles / Agents"), " above.")
          ),

          # HHO
          conditionalPanel("input.fgwc_algorithm == 'hho'",
            div(class = "step-header", "HHO Algorithm"),
            selectInput("fgwc_hho_algo", NULL,
                        choices  = c("Heidari (2019)" = "heidari",
                                     "Bairathi (2018)" = "bairathi"),
                        selected = "bairathi"),
            fluidRow(
              column(4, div(class="step-header","a1"),
                     numericInput("fgwc_hho_a1", NULL,
                                  value = 3, min = 0, step = 0.5)),
              column(4, div(class="step-header","a2"),
                     numericInput("fgwc_hho_a2", NULL,
                                  value = 1, min = 0, step = 0.5)),
              column(4, div(class="step-header","a3"),
                     numericInput("fgwc_hho_a3", NULL,
                                  value = 0.4, min = 0, step = 0.1))
            )
          ),

          # IFA
          conditionalPanel("input.fgwc_algorithm == 'ifa'",
            div(class = "step-header", "Number of Selected Fireflies"),
            sliderInput("fgwc_ifa_parno", NULL,
                        min = 1, max = 10, value = 3, step = 1),
            div(class = "step-header", "Gamma"),
            numericInput("fgwc_ifa_gamma", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "Beta (Attractiveness)"),
            numericInput("fgwc_ifa_beta", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "EI Distribution"),
            selectInput("fgwc_ifa_ei", NULL,
                        choices  = c("logchaotic","normal","uniform",
                                     "kentchaotic","levy"),
                        selected = "logchaotic")
          ),

          # PSO
          conditionalPanel("input.fgwc_algorithm == 'pso'",
            div(class = "step-header", "Vmax"),
            sliderInput("fgwc_pso_vmax", NULL,
                        min = 0.1, max = 2.0, value = 0.8, step = 0.1),
            fluidRow(
              column(6, div(class="step-header","c1 (cognitive)"),
                     numericInput("fgwc_pso_c1", NULL,
                                  value = 0.7, min = 0, step = 0.1)),
              column(6, div(class="step-header","c2 (social)"),
                     numericInput("fgwc_pso_c2", NULL,
                                  value = 0.6, min = 0, step = 0.1))
            ),
            div(class = "step-header", "Inertia Weight Type"),
            selectInput("fgwc_pso_type", NULL,
                        choices  = c("chaotic","constant","sim.annealing",
                                     "nat.exponent1","nat.exponent2"),
                        selected = "chaotic"),
            fluidRow(
              column(6, div(class="step-header","wmax"),
                     numericInput("fgwc_pso_wmax", NULL,
                                  value = 0.8, min = 0, step = 0.05)),
              column(6, div(class="step-header","wmin"),
                     numericInput("fgwc_pso_wmin", NULL,
                                  value = 0.3, min = 0, step = 0.05))
            )
          ),

          # TLBO
          conditionalPanel("input.fgwc_algorithm == 'tlbo'",
            div(class = "step-header", "Number of Selections"),
            sliderInput("fgwc_tlbo_nselect", NULL,
                        min = 2, max = 20, value = 10, step = 1),
            checkboxInput("fgwc_tlbo_elitism", "Use Elitism",
                          value = FALSE),
            div(class = "step-header", "Number of Elites"),
            numericInput("fgwc_tlbo_nelite", NULL,
                         value = 2, min = 1, step = 1)
          ),

          # WOA
          conditionalPanel("input.fgwc_algorithm == 'woa'",
            div(class = "step-header", "Spiral Constant (b)"),
            numericInput("fgwc_woa_b", NULL,
                         value = 1, min = 0.1, step = 0.1)
          )
        ),

        # ── Box: Tombol Run + Download ─────────────────────────────────────────
        shinydashboard::box(
          title       = tags$span(icon("play"), " Run"),
          status      = "success",
          solidHeader = TRUE,
          width       = 12,

          actionButton("run_fgwc",
                       tags$span(icon("play-circle"), " Run FGWC"),
                       class = "btn-success btn-lg btn-block"),
          tags$br(), tags$br(),
          uiOutput("fgwc_progress"),
          tags$hr(),

          div(class = "step-header", "Download Results"),
          downloadButton("dl_fgwc_csv",
                         tags$span(icon("download"), " Cluster Results (.csv)"),
                         class = "btn-info btn-block"),
          tags$br(),
          downloadButton("dl_fgwc_map_png",
                         tags$span(icon("map"), " FGWC Map (.png)"),
                         class = "btn-success btn-block")
        )
      )
    ),

    # ══════════════════════════════════════════════════════════════════════════
    # BARIS 2: Hasil FGWC (bawah) — full width
    # ══════════════════════════════════════════════════════════════════════════
    fluidRow(
      column(12,

        shinydashboard::box(
          title       = tags$span(icon("project-diagram"), " FGWC Results"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,

          tabsetPanel(

            # Tab 1: Ringkasan
            tabPanel(tags$span(icon("info-circle"), " Summary"),
                     tags$br(),
                     uiOutput("fgwc_summary")),

            # Tab 2: Validasi + Konvergensi
            tabPanel(tags$span(icon("chart-bar"), " Validation"),
                     tags$br(),
                     div(class = "step-header", "Cluster Validation Index"),
                     tags$p(style = "font-size:12px; color:#78909c;",
                            "PC & IFV: higher is better.",
                            " CE, SC, SI, XB, Kwon: lower is better."),
                     DT::DTOutput("fgwc_val_table"),
                     tags$hr(),
                     div(class = "step-header", "Objective Function Convergence"),
                     plotOutput("fgwc_conv_plot", height = "260px")),

            # Tab 3: Peta Interaktif
            tabPanel(tags$span(icon("map"), " Interactive Map"),
                     tags$br(),
                     leaflet::leafletOutput("fgwc_map", height = "500px")),

            # Tab 4: Silhouette
            tabPanel(tags$span(icon("chart-line"), " Silhouette"),
                     tags$br(),
                     fluidRow(
                       column(7,
                              div(class = "step-header", "Silhouette Plot"),
                              plotOutput("fgwc_sil_plot", height = "300px")),
                       column(5,
                              div(class = "step-header", "Avg. Silhouette Width"),
                              DT::DTOutput("fgwc_sil_table"),
                              tags$br(),
                              uiOutput("fgwc_sil_interp"))
                     )),

            # Tab 5: Profil Cluster
            tabPanel(tags$span(icon("th"), " Cluster Profile"),
                     tags$br(),
                     div(class = "step-header", "Cluster Profile Table (Mean per Cluster)"),
                     DT::DTOutput("fgwc_profile_table"),
                     tags$hr(),
                     div(class = "step-header", "Profile Heatmap"),
                     plotOutput("fgwc_heatmap", height = "320px"),
                     div(style = "text-align:right; margin-top:6px;",
                         downloadButton("dl_fgwc_heatmap",
                                        tags$span(icon("download"), " Download Heatmap (.png)"),
                                        class = "btn-sm btn-default")),
                     tags$hr(),
                     div(class = "step-header", "Radar Chart per Cluster"),
                     plotOutput("fgwc_radar", height = "400px"),
                     div(style = "text-align:right; margin-top:6px;",
                         downloadButton("dl_fgwc_radar",
                                        tags$span(icon("download"), " Download Radar Chart (.png)"),
                                        class = "btn-sm btn-default"))),

            # Tab 6: Data Cluster
            tabPanel(tags$span(icon("list-ol"), " Cluster Data"),
                     tags$br(),
                     DT::DTOutput("fgwc_result_table")),

            # Tab 7: Sammon Mapping
            tabPanel(tags$span(icon("project-diagram"), " Sammon Mapping"),
                     tags$br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                tags$strong("Sammon Parameters"),
                                tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
                                numericInput("fgwc_sammon_iter",  "Max. Iterations:",    value = 500,  min = 100,  step = 100),
                                numericInput("fgwc_sammon_magic", "Magic (Step Size):", value = 0.2, min = 0.01, step = 0.05),
                                sliderInput("fgwc_sammon_pt",    "Point Size:",    min = 1, max = 5, value = 2, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_fgwc_sammon",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("fgwc_sammon_plot", height = "600px")
                       )
                     )
            ),

            # Tab 8: t-SNE
            tabPanel(tags$span(icon("braille"), " t-SNE"),
                     tags$br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                tags$strong("t-SNE Parameters"),
                                tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
                                numericInput("fgwc_tsne_perp", "Perplexity:",    value = 15,   min = 2,   step = 1),
                                numericInput("fgwc_tsne_iter", "Max. Iterations:",  value = 1000, min = 500, step = 100),
                                sliderInput("fgwc_tsne_pt",   "Point Size:",  min = 1, max = 5, value = 2, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_fgwc_tsne",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("fgwc_tsne_plot", height = "600px")
                       )
                     )
            ),

            # Tab 9: UMAP
            tabPanel(tags$span(icon("connectdevelop"), " UMAP"),
                     tags$br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                tags$strong("UMAP Parameters"),
                                tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
                                numericInput("fgwc_umap_nn", "n_neighbors:", value = 15,  min = 2,    step = 1),
                                numericInput("fgwc_umap_md", "min_dist:",    value = 0.1, min = 0.01, step = 0.05),
                                sliderInput("fgwc_umap_pt",  "Point Size:", min = 1, max = 5, value = 2, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_fgwc_umap",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("fgwc_umap_plot", height = "600px")
                       )
                     )
            )

          ) # end tabsetPanel
        )   # end box Hasil FGWC
      )     # end column(12)
    )       # end fluidRow baris 2

  )         # end tabItem
}
