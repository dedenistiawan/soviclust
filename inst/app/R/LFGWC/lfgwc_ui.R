# =============================================================================
# R/LFGWC/lfgwc_ui.R
# UI Tab LFGWC — Submenu Cluster Analysis
#
# DIPANGGIL DARI: ui.R via lfgwc_tab_ui()
# BERISI       : definisi tabItem("tab_lfgwc", ...)
#
# LAYOUT:
#   Baris 1 (atas)  : Setting Parameter — 4 kolom
#     Kolom 1 (w=3) : Data Pendukung LFGWC + Sumber Data
#     Kolom 2 (w=3) : Parameter LFGWC
#     Kolom 3 (w=3) : Algoritma Optimasi
#     Kolom 4 (w=3) : Parameter Algoritma + Jalankan
#   Baris 2 (bawah) : Hasil LFGWC — full width (w=12)
# =============================================================================

lfgwc_tab_ui <- function() {

  shinydashboard::tabItem("tab_lfgwc",

    # ══════════════════════════════════════════════════════════════════════════
    # BARIS 1: Setting Parameter (atas) — 4 kolom
    # ══════════════════════════════════════════════════════════════════════════
    fluidRow(

      # ── KOLOM 1 (width=3): Data Pendukung LFGWC + Sumber Data ──────────────
      column(3,

        # ── Box: Upload Data Pendukung ──────────────────────────────────────
        shinydashboard::box(
          title       = tags$span(icon("upload"), " LFGWC Supporting Data"),
          status      = "warning",
          solidHeader = TRUE,
          width       = 12,

          div(class = "sample-data-banner", style = "padding:10px 14px; margin-bottom:10px;",
            tags$p(style = "margin:0 0 6px 0; font-weight:700; color:#1a73c1; font-size:13px;",
                   icon("database"), " Use LFGWC Sample Data"),
            tags$p(style = "margin:0 0 8px 0; font-size:11.5px; color:#546e7a;",
                   "Distance matrix and population data for 514 Indonesian regencies/cities."),
            actionButton("lfgwc_load_sample", tags$span(icon("play-circle"), " Load Sample Data"),
              class = "btn-primary btn-sm btn-block")
          ),
          div(class = "step-header", "1. Distance Matrix Input"),
          radioButtons("lfgwc_dist_mode", NULL,
                       choices  = c("Upload n\u00d7n Distance Matrix" = "matrix",
                                    "Upload Longitude & Latitude"    = "lonlat"),
                       selected = "matrix"
          ),

          conditionalPanel(
            "input.lfgwc_dist_mode == 'matrix'",
            tags$p(style = "font-size:12px; color:#78909c; margin-bottom:6px;",
                   icon("info-circle"),
                   " Excel/CSV file: n\u00d7n distance matrix between n\u00d7n regions."),
            fileInput("lfgwc_file_dist", NULL,
                      accept      = c(".xlsx", ".csv"),
                      placeholder = "No file selected")
          ),

          conditionalPanel(
            "input.lfgwc_dist_mode == 'lonlat'",
            tags$p(style = "font-size:12px; color:#78909c; margin-bottom:6px;",
                   icon("info-circle"),
                   " Excel/CSV file with columns: DISTRICTCODE, longitude, latitude."),
            fileInput("lfgwc_file_lonlat", NULL,
                      accept      = c(".xlsx", ".csv"),
                      placeholder = "No file selected"),
            div(class = "progress-box",
                style = "background:#e8f5e9; border-left-color:#27ae60;
                         font-size:11.5px; margin-bottom:6px;",
                icon("info-circle"),
                " Distance is calculated automatically using",
                tags$strong(" Haversine Distance"),
                " (unit: kilometers).")
          ),

          uiOutput("lfgwc_dist_status"),
          tags$hr(),

          div(class = "step-header", "2. Upload Population Data"),
          tags$p(style = "font-size:12px; color:#78909c; margin-bottom:6px;",
                 icon("info-circle"),
                 " Excel/CSV file: 1 population column (n rows)."),
          fileInput("lfgwc_file_pop", NULL,
                    accept      = c(".xlsx", ".csv"),
                    placeholder = "No file selected"),
          uiOutput("lfgwc_pop_status")
        ),

        # ── Box: Sumber Data Fitur ──────────────────────────────────────────
        shinydashboard::box(
          title  = tags$span(icon("database"), " Data Source"),
          status = "primary",
          solidHeader = TRUE,
          width  = 12,

          div(class = "step-header", "3. Feature Data Source"),
          radioButtons("lfgwc_data_source", NULL,
                       choices = c(
                         "Original Data (no transformation)" = "raw",
                         "Normalized Data (0-1)"            = "raw_norm",
                         "Standardized Data (Z-score)"      = "standardized",
                         "SoVI Score"                       = "sovi",
                         "RC Scores (PCA components)"       = "rc"
                       ),
                       selected = "rc"),

          conditionalPanel(
            "input.lfgwc_data_source == 'raw' ||
             input.lfgwc_data_source == 'raw_norm' ||
             input.lfgwc_data_source == 'standardized'",
            div(class = "step-header", "Select Variables"),
            uiOutput("lfgwc_var_selector")
          ),

          conditionalPanel(
            "input.lfgwc_data_source == 'sovi' ||
             input.lfgwc_data_source == 'rc'",
            uiOutput("lfgwc_datasource_info")
          )
        )
      ),

      # ── KOLOM 2 (width=3): Parameter LFGWC ─────────────────────────────────
      column(3,

        shinydashboard::box(
          title       = tags$span(icon("cog"), " LFGWC Parameters"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,
          collapsible = TRUE,

          div(class = "step-header", "4. Number of Clusters (c)"),
          sliderInput("lfgwc_ncluster", NULL,
                      min = 2, max = 10, value = 4, step = 1),

          div(class = "step-header", "Fuzzifier (m)"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "m=2 default | m>2 more fuzzy | m\u21921 approaches hard clustering"),
          sliderInput("lfgwc_m", NULL,
                      min = 1.1, max = 3.0, value = 2.0, step = 0.1),

          div(class = "step-header", "Alpha (\u03b1) \u2014 Old Membership Weight"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "\u03b1=1 \u2192 pure FCM (no neighbor influence) | \u03b1=0 \u2192 neighbor only"),
          sliderInput("lfgwc_alpha", NULL,
                      min = 0.0, max = 1.0, value = 0.8, step = 0.05),

          tags$hr(),

          div(class = "step-header",
              tags$span(style = "color:#e74c3c;", icon("map-marker-alt"),
                        " LFGWC Spatial Parameters")),

          div(class = "step-header", "Distance Threshold (dthr)"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 4px;",
                 "Neighborhood radius. -99 = Global mode (all units)."),
          uiOutput("lfgwc_dthr_info"),
          numericInput("lfgwc_dthr", NULL,
                       value = 600, min = -99, step = 100),

          div(class = "step-header", "Mode Weighting"),
          radioButtons("lfgwc_si", NULL,
                       choices = c(
                         "Distance Decay: 1/d^exp (DLFGWC)"    = "FALSE",
                         "SIM-PF: (Pi\u00b7Pj)^b / d^a (LFGWC)" = "TRUE"
                       ),
                       selected = "FALSE"),

          conditionalPanel(
            "input.lfgwc_si == 'FALSE'",
            div(class = "step-header", "Distance Decay Exponent (exp)"),
            tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                   "Default = 2 (inverse distance squared)"),
            sliderInput("lfgwc_exp_d", NULL,
                        min = 1, max = 4, value = 2, step = 0.5)
          ),

          conditionalPanel(
            "input.lfgwc_si == 'TRUE'",
            fluidRow(
              column(6,
                     div(class = "step-header", "Distance Magnitude (a)"),
                     numericInput("lfgwc_a", NULL,
                                  value = 1, min = 0, step = 0.1)),
              column(6,
                     div(class = "step-header", "Population Magnitude (b)"),
                     numericInput("lfgwc_b", NULL,
                                  value = 1, min = 0, step = 0.1))
            )
          ),

          tags$hr(),

          div(class = "step-header", "Max. Iterations"),
          numericInput("lfgwc_maxiter", NULL,
                       value = 100, min = 10, step = 10),

          div(class = "step-header", "Convergence Tolerance (\u03b5)"),
          numericInput("lfgwc_error", NULL,
                       value = 0.001, min = 1e-6, step = 0.001),

          div(class = "step-header", "Random Seed"),
          numericInput("lfgwc_seed", NULL,
                       value = 0, min = 0, step = 1)
        )
      ),

      # ── KOLOM 3 (width=3): Algoritma Optimasi ──────────────────────────────
      column(3,

        shinydashboard::box(
          title  = tags$span(icon("microchip"), " Optimization Algorithm"),
          status = "primary",
          solidHeader = TRUE,
          width  = 12,

          div(class = "step-header", "5. Select Algorithm"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "Classic = pure LFGWC. Others = initial centroid optimization."),
          radioButtons("lfgwc_algorithm", NULL,
                       choices = c(
                         "Classic LFGWC"                          = "classic",
                         "ABC (Artificial Bee Colony)"        = "abc",
                         "FPA (Flower Pollination)"           = "fpa",
                         "GSA (Gravitational Search)"         = "gsa",
                         "GWO (Grey Wolf Optimizer)"          = "gwo",
                         "HHO (Harris-Hawk Optimization)"     = "hho",
                         "IFA (Intelligent Firefly)"          = "ifa",
                         "PSO (Particle Swarm) (DLFGWC-PSO)" = "pso",
                         "TLBO (Teaching-Learning Based)"     = "tlbo",
                         "WOA (Whale Optimization)"           = "woa"
                       ),
                       selected = "classic")
        )
      ),

      # ── KOLOM 4 (width=3): Parameter Algoritma + Tombol Run + Download ──────
      column(3,

        shinydashboard::box(
          title       = tags$span(icon("sliders-h"), " Algorithm Parameters"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,
          collapsible = TRUE,

          # Universal
          conditionalPanel(
            "input.lfgwc_algorithm != 'classic'",
            div(class = "step-header", "Number of Particles / Agents"),
            sliderInput("lfgwc_npar", NULL,
                        min = 3, max = 30, value = 10, step = 1),
            div(class = "step-header", "Convergence (same)"),
            sliderInput("lfgwc_same", NULL,
                        min = 5, max = 30, value = 10, step = 1),
            div(class = "step-header", "Initialization Distribution"),
            selectInput("lfgwc_vi_dist", NULL,
                        choices  = c("Uniform" = "uniform", "Normal"  = "normal"),
                        selected = "uniform")
          ),

          conditionalPanel(
            "input.lfgwc_algorithm == 'classic'",
            div(class = "progress-box",
                style = "background:#f8f9fa; border-left-color:#adb5bd; font-size:12px;",
                icon("info-circle"),
                " Classic LFGWC: random centroid initialization (uniform/normal).")
          ),

          # ABC
          conditionalPanel(
            "input.lfgwc_algorithm == 'abc'",
            div(class = "step-header", "Number of Onlooker Bees"),
            sliderInput("lfgwc_abc_onlooker", NULL,
                        min = 2, max = 20, value = 5, step = 1),
            div(class = "step-header", "Limit (Scout)"),
            sliderInput("lfgwc_abc_limit", NULL,
                        min = 1, max = 20, value = 5, step = 1)
          ),

          # FPA
          conditionalPanel(
            "input.lfgwc_algorithm == 'fpa'",
            div(class = "step-header", "Switch Prob. (p)"),
            sliderInput("lfgwc_fpa_p", NULL,
                        min = 0.1, max = 0.9, value = 0.7, step = 0.05),
            div(class = "step-header", "Gamma"),
            numericInput("lfgwc_fpa_gamma", NULL,
                         value = 1.2, min = 0.1, step = 0.1),
            div(class = "step-header", "Lambda (Levy)"),
            numericInput("lfgwc_fpa_lambda", NULL,
                         value = 1.5, min = 0.1, step = 0.1),
            div(class = "step-header", "EI Distribution"),
            selectInput("lfgwc_fpa_ei", NULL,
                        choices  = c("logchaotic","normal","uniform","kentchaotic","levy"),
                        selected = "logchaotic")
          ),

          # GSA
          conditionalPanel(
            "input.lfgwc_algorithm == 'gsa'",
            div(class = "step-header", "Gravitational Constant (G)"),
            numericInput("lfgwc_gsa_G", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "Vmax"),
            sliderInput("lfgwc_gsa_vmax", NULL,
                        min = 0.1, max = 2.0, value = 0.7, step = 0.1),
            checkboxInput("lfgwc_gsa_new",
                          "Use new GSA version (Li & Dong 2017)",
                          value = FALSE)
          ),

          # HHO
          conditionalPanel(
            "input.lfgwc_algorithm == 'hho'",
            div(class = "step-header", "HHO Algorithm"),
            selectInput("lfgwc_hho_algo", NULL,
                        choices  = c("Heidari (2019)" = "heidari",
                                     "Bairathi (2018)" = "bairathi"),
                        selected = "bairathi"),
            fluidRow(
              column(4, div(class = "step-header", "a1"),
                     numericInput("lfgwc_hho_a1", NULL,
                                  value = 3, min = 0, step = 0.5)),
              column(4, div(class = "step-header", "a2"),
                     numericInput("lfgwc_hho_a2", NULL,
                                  value = 1, min = 0, step = 0.5)),
              column(4, div(class = "step-header", "a3"),
                     numericInput("lfgwc_hho_a3", NULL,
                                  value = 0.4, min = 0, step = 0.1))
            )
          ),

          # IFA
          conditionalPanel(
            "input.lfgwc_algorithm == 'ifa'",
            div(class = "step-header", "Number of Selected Fireflies"),
            sliderInput("lfgwc_ifa_parno", NULL,
                        min = 1, max = 10, value = 3, step = 1),
            div(class = "step-header", "Gamma"),
            numericInput("lfgwc_ifa_gamma", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "Beta (Attractiveness)"),
            numericInput("lfgwc_ifa_beta", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "EI Distribution"),
            selectInput("lfgwc_ifa_ei", NULL,
                        choices  = c("logchaotic","normal","uniform","kentchaotic","levy"),
                        selected = "logchaotic")
          ),

          # PSO
          conditionalPanel(
            "input.lfgwc_algorithm == 'pso'",
            div(class = "progress-box",
                style = "background:#e8f5e9; border-left-color:#27ae60;
                         font-size:11.5px; margin-bottom:8px;",
                icon("star", style = "color:#27ae60;"),
                tags$strong(" DLFGWC-PSO"), " \u2014 Best variant per the paper."),
            div(class = "step-header", "Vmax"),
            sliderInput("lfgwc_pso_vmax", NULL,
                        min = 0.1, max = 2.0, value = 0.8, step = 0.1),
            fluidRow(
              column(6, div(class = "step-header", "c1 (cognitive)"),
                     numericInput("lfgwc_pso_c1", NULL,
                                  value = 0.7, min = 0, step = 0.1)),
              column(6, div(class = "step-header", "c2 (social)"),
                     numericInput("lfgwc_pso_c2", NULL,
                                  value = 0.6, min = 0, step = 0.1))
            ),
            div(class = "step-header", "Inertia Weight Type"),
            selectInput("lfgwc_pso_type", NULL,
                        choices  = c("chaotic","constant","sim.annealing",
                                     "nat.exponent1","nat.exponent2"),
                        selected = "chaotic"),
            fluidRow(
              column(6, div(class = "step-header", "wmax"),
                     numericInput("lfgwc_pso_wmax", NULL,
                                  value = 0.8, min = 0, step = 0.05)),
              column(6, div(class = "step-header", "wmin"),
                     numericInput("lfgwc_pso_wmin", NULL,
                                  value = 0.3, min = 0, step = 0.05))
            )
          ),

          # TLBO
          conditionalPanel(
            "input.lfgwc_algorithm == 'tlbo'",
            div(class = "step-header", "Number of Selections"),
            sliderInput("lfgwc_tlbo_nselect", NULL,
                        min = 2, max = 20, value = 10, step = 1),
            checkboxInput("lfgwc_tlbo_elitism",
                          "Use Elitism", value = FALSE),
            div(class = "step-header", "Number of Elites"),
            numericInput("lfgwc_tlbo_nelite", NULL,
                         value = 2, min = 1, step = 1)
          ),

          # WOA
          conditionalPanel(
            "input.lfgwc_algorithm == 'woa'",
            div(class = "step-header", "Spiral Constant (b)"),
            numericInput("lfgwc_woa_b", NULL,
                         value = 1, min = 0.1, step = 0.1)
          )
        ),

        # ── Box: Tombol Run + Download ────────────────────────────────────────
        shinydashboard::box(
          title  = tags$span(icon("play"), " Run LFGWC"),
          status = "success",
          solidHeader = TRUE,
          width  = 12,

          actionButton("run_lfgwc",
                       tags$span(icon("play-circle"), " Run LFGWC"),
                       class = "btn-success btn-lg btn-block"),
          tags$br(), tags$br(),
          uiOutput("lfgwc_progress"),
          tags$hr(),

          div(class = "step-header", "Display Settings"),
          selectInput("lfgwc_palette", "Map & Chart Color Palette",
                      choices = c("Dark2", "Set1", "Set2", "Set3", 
                                  "Pastel1", "Pastel2", "Paired", 
                                  "Accent", "Spectral", "RdYlBu"),
                      selected = "Dark2"),
          tags$hr(),

          div(class = "step-header", "Download Results"),
          downloadButton("dl_lfgwc_csv",
                         tags$span(icon("download"), " Cluster Results (.csv)"),
                         class = "btn-info btn-block"),
          tags$br(),
          downloadButton("dl_lfgwc_memb_csv",
                         tags$span(icon("download"), " Membership Matrix (.csv)"),
                         class = "btn-info btn-block"),
          tags$br(),
          downloadButton("dl_lfgwc_map_png",
                         tags$span(icon("map"), " Cluster Map (.png)"),
                         class = "btn-success btn-block")
        )
      )
    ),

    # ══════════════════════════════════════════════════════════════════════════
    # BARIS 2: Hasil LFGWC (bawah) — full width
    # ══════════════════════════════════════════════════════════════════════════
    fluidRow(
      column(12,

        shinydashboard::box(
          title       = tags$span(icon("project-diagram"), " LFGWC Results"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,

          tabsetPanel(

            # Tab 1: Ringkasan
            tabPanel(tags$span(icon("info-circle"), " Summary"),
                     tags$br(),
                     uiOutput("lfgwc_summary")),

            # Tab 2: Validasi + Konvergensi
            tabPanel(tags$span(icon("chart-bar"), " Validation"),
                     tags$br(),
                     div(class = "step-header", "LFGWC Validation Index"),
                     tags$p(style = "font-size:12px; color:#78909c;",
                            "PC & IFV: higher is better.",
                            " CE & SC: lower is better."),
                     DT::DTOutput("lfgwc_val_table"),
                     tags$hr(),
                     div(class = "step-header", "Objective Function Convergence"),
                     plotOutput("lfgwc_conv_plot", height = "260px")),

            # Tab 3: Peta Cluster
            tabPanel(tags$span(icon("map"), " Cluster Map"),
                     tags$br(),
                     leaflet::leafletOutput("lfgwc_map", height = "500px")),

            # Tab 4: Peta Max Membership
            tabPanel(tags$span(icon("percent"), " Max Membership"),
                     tags$br(),
                     div(class = "progress-box",
                         style = "background:#e3f2fd; border-left-color:#1a73c1;
                                  font-size:12px; margin-bottom:8px;",
                         icon("info-circle"),
                         " Values close to 1 = unit clearly belongs to 1 cluster.",
                         " Low values = boundary units between clusters (fuzzy)."),
                     leaflet::leafletOutput("lfgwc_map_membership", height = "480px")),

            # Tab 5: Silhouette
            tabPanel(tags$span(icon("chart-line"), " Silhouette"),
                     tags$br(),
                     fluidRow(
                       column(7,
                              div(class = "step-header", "Silhouette Plot"),
                              plotOutput("lfgwc_sil_plot", height = "300px")),
                       column(5,
                              div(class = "step-header", "Avg. Silhouette Width"),
                              DT::DTOutput("lfgwc_sil_table"),
                              tags$br(),
                              uiOutput("lfgwc_sil_interp"))
                     )),

            # Tab 6: Profil Cluster
            tabPanel(tags$span(icon("th"), " Cluster Profile"),
                     tags$br(),
                     div(class = "step-header", "Cluster Profile Table (Mean per Cluster)"),
                     DT::DTOutput("lfgwc_profile_table"),
                     tags$hr(),
                     div(class = "step-header", "Profile Heatmap"),
                     plotOutput("lfgwc_heatmap", height = "320px"),
                     div(style = "text-align:right; margin-top:6px;",
                         downloadButton("dl_lfgwc_heatmap",
                                        tags$span(icon("download"), " Download Heatmap (.png)"),
                                        class = "btn-sm btn-default")),
                     tags$hr(),
                     div(class = "step-header", "Radar Chart per Cluster"),
                     plotOutput("lfgwc_radar", height = "400px"),
                     div(style = "text-align:right; margin-top:6px;",
                         downloadButton("dl_lfgwc_radar",
                                        tags$span(icon("download"), " Download Radar Chart (.png)"),
                                        class = "btn-sm btn-default"))),

            # Tab 7: Data Cluster
            tabPanel(tags$span(icon("list-ol"), " Cluster Data"),
                     tags$br(),
                     DT::DTOutput("lfgwc_result_table")),

            # Tab 8: Sammon Mapping
            tabPanel(tags$span(icon("compress-arrows-alt"), " Sammon Mapping"),
                     tags$br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                tags$strong("Sammon Parameters"),
                                tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
                                div(class = "progress-box",
                                    style = "background:#e8f5e9; border-left-color:#27ae60;
                                             font-size:11.5px; margin-bottom:8px;",
                                    icon("info-circle"),
                                    " Projects high-dimensional data to 2D",
                                    " while preserving distance structure."),
                                numericInput("lfgwc_sammon_iter",  "Max. Iterations:",
                                             value = 500, min = 100, step = 100),
                                numericInput("lfgwc_sammon_magic", "Magic (Step Size):",
                                             value = 0.2, min = 0.01, step = 0.05),
                                sliderInput("lfgwc_sammon_pt",    "Point Size:",
                                            min = 1, max = 6, value = 3, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_lfgwc_sammon",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("lfgwc_sammon_plot", height = "600px")
                       )
                     )
            ),

            # Tab 9: t-SNE
            tabPanel(tags$span(icon("dot-circle"), " t-SNE"),
                     tags$br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                tags$strong("t-SNE Parameters"),
                                tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
                                div(class = "progress-box",
                                    style = "background:#e3f2fd; border-left-color:#1a73c1;
                                             font-size:11.5px; margin-bottom:8px;",
                                    icon("info-circle"),
                                    " t-SNE highlights local cluster structure in 2D space."),
                                sliderInput("lfgwc_tsne_perp", "Perplexity:",
                                            min = 5, max = 50, value = 15, step = 5),
                                numericInput("lfgwc_tsne_iter", "Max. Iterations:",
                                             value = 1000, min = 250, step = 250),
                                sliderInput("lfgwc_tsne_pt",   "Point Size:",
                                            min = 1, max = 6, value = 3, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_lfgwc_tsne",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("lfgwc_tsne_plot", height = "600px")
                       )
                     )
            ),

            # Tab 10: UMAP
            tabPanel(tags$span(icon("project-diagram"), " UMAP"),
                     tags$br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                tags$strong("UMAP Parameters"),
                                tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
                                div(class = "progress-box",
                                    style = "background:#fce4ec; border-left-color:#e91e63;
                                             font-size:11.5px; margin-bottom:8px;",
                                    icon("info-circle"),
                                    " UMAP preserves local & global structure, faster than t-SNE."),
                                sliderInput("lfgwc_umap_nn", "n_neighbors:",
                                            min = 2, max = 30, value = 15, step = 1),
                                sliderInput("lfgwc_umap_md", "min_dist:",
                                            min = 0.01, max = 0.99, value = 0.1, step = 0.05),
                                sliderInput("lfgwc_umap_pt", "Point Size:",
                                            min = 1, max = 6, value = 3, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_lfgwc_umap",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("lfgwc_umap_plot", height = "600px")
                       )
                     )
            ),

            # ── Tab 10: Stability Analysis (Multiple Independent Runs) ─────────
            tabPanel(tags$span(icon("sync-alt"), " Stability Analysis"),
                     tags$br(),

                     fluidRow(
                       column(12,
                         shinydashboard::box(
                           title       = tags$span(icon("flask"), " Stability Analysis Settings"),
                           status      = "primary",
                           solidHeader = TRUE,
                           width       = 12,
                           collapsible = TRUE,

                           tags$p(style = "font-size:12.5px; color:#546e7a; margin-bottom:12px;",
                             icon("info-circle"),
                             " Run the clustering algorithm multiple times with different random seeds to ",
                             tags$strong("assess robustness"),
                             ". Reports Mean, SD, Best, Worst, and Median for each validity index.",
                             tags$br(),
                             tags$span(style = "color:#e74c3c; font-weight:600;",
                               icon("exclamation-triangle"),
                               " Run LFGWC (main tab) first before starting stability analysis."
                             )
                           ),

                           fluidRow(
                             column(3,
                               numericInput("lfgwc_nruns",
                                 label = tags$span(icon("redo"), " Number of Runs:"),
                                 value = 30, min = 5, max = 100, step = 5)
                             ),
                             column(3,
                               numericInput("lfgwc_seed_start",
                                 label = tags$span(icon("random"), " Seed Start:"),
                                 value = 1, min = 0, step = 1)
                             ),
                             column(3,
                               tags$br(),
                               actionButton("lfgwc_run_stability",
                                 label = tags$span(icon("play-circle"), " Run Stability Test"),
                                 class = "btn-primary btn-block",
                                 style = "margin-top:6px;"
                               )
                             ),
                             column(3,
                               tags$br(),
                               downloadButton("dl_lfgwc_stability",
                                 label = tags$span(icon("download"), " Download Detail (.csv)"),
                                 class = "btn-default btn-block",
                                 style = "margin-top:6px;"
                               )
                             )
                           ),

                           tags$div(style = "margin-top:10px;",
                             uiOutput("lfgwc_stability_progress")
                           )
                         )
                       )
                     ),

                     fluidRow(
                       column(12,
                         shinydashboard::box(
                           title       = tags$span(icon("table"), " Summary Statistics — Validity Index across Runs"),
                           status      = "success",
                           solidHeader = TRUE,
                           width       = 12,
                           DT::dataTableOutput("lfgwc_stability_summary")
                         )
                       )
                     ),

                     fluidRow(
                       column(12,
                         shinydashboard::box(
                           title       = tags$span(icon("chart-bar"), " Distribution Boxplot across Runs"),
                           status      = "warning",
                           solidHeader = TRUE,
                           width       = 12,
                           plotOutput("lfgwc_stability_boxplot", height = "420px")
                         )
                       )
                     ),

                     fluidRow(
                       column(12,
                         shinydashboard::box(
                           title       = tags$span(icon("list-alt"), " Detail per Run"),
                           status      = "default",
                           solidHeader = FALSE,
                           width       = 12,
                           collapsible = TRUE,
                           collapsed   = TRUE,
                           DT::dataTableOutput("lfgwc_stability_detail")
                         )
                       )
                     )

            ) # end Tab 10

          ) # end tabsetPanel
        )   # end box Hasil LFGWC
      )     # end column(12)
    )       # end fluidRow baris 2

  )         # end tabItem
}
