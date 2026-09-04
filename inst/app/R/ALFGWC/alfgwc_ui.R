# =============================================================================
# R/ALFGWC/alfgwc_ui.R
# UI Tab ALFGWC — Submenu Cluster Analysis
# =============================================================================

alfgwc_tab_ui <- function() {
  shinydashboard::tabItem("tab_alfgwc",
    
    fluidRow(
      # ── KOLOM 1 (width=3): Data Pendukung & Sumber Data ──────────────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("upload"), " ALFGWC Supporting Data"),
          status      = "warning",
          solidHeader = TRUE,
          width       = 12,
          
          div(class = "sample-data-banner", style = "padding:10px 14px; margin-bottom:10px;",
            tags$p(style = "margin:0 0 6px 0; font-weight:700; color:#1a73c1; font-size:13px;",
                   icon("database"), " Use ALFGWC Sample Data"),
            tags$p(style = "margin:0 0 8px 0; font-size:11.5px; color:#546e7a;",
                   "Distance matrix and population data for 514 Indonesian regencies/cities."),
            actionButton("alfgwc_load_sample", tags$span(icon("play-circle"), " Load Sample Data"),
              class = "btn-primary btn-sm btn-block")
          ),
          div(class = "step-header", "1. Distance Matrix Input"),
          radioButtons("alfgwc_dist_mode", NULL,
                       choices  = c("Upload n\u00d7n Distance Matrix" = "matrix",
                                    "Upload Longitude & Latitude"   = "lonlat"),
                       selected = "matrix"),
          
          conditionalPanel(
            "input.alfgwc_dist_mode == 'matrix'",
            fileInput("alfgwc_file_dist", NULL, accept = c(".xlsx", ".csv"))
          ),
          
          conditionalPanel(
            "input.alfgwc_dist_mode == 'lonlat'",
            fileInput("alfgwc_file_lonlat", NULL, accept = c(".xlsx", ".csv"))
          ),
          uiOutput("alfgwc_dist_status"),
          tags$hr(),
          
          div(class = "step-header", "2. Upload Population Data"),
          fileInput("alfgwc_file_pop", NULL, accept = c(".xlsx", ".csv")),
          uiOutput("alfgwc_pop_status")
        ),
        
        shinydashboard::box(
          title  = tags$span(icon("database"), " Data Source"),
          status = "primary",
          solidHeader = TRUE,
          width  = 12,
          
          div(class = "step-header", "3. Feature Data Source"),
          radioButtons("alfgwc_data_source", NULL,
                       choices = c(
                         "Original Data (no transformation)" = "raw",
                         "Normalized Data (0-1)"            = "raw_norm",
                         "Standardized Data (Z-score)"      = "standardized",
                         "SoVI Score"                       = "sovi",
                         "RC Scores (PCA components)"       = "rc"
                       ),
                       selected = "rc"),
          
          conditionalPanel(
            "input.alfgwc_data_source == 'raw' || input.alfgwc_data_source == 'raw_norm' || input.alfgwc_data_source == 'standardized'",
            div(class = "step-header", "Select Variables"),
            uiOutput("alfgwc_var_selector")
          ),
          
          conditionalPanel(
            "input.alfgwc_data_source == 'sovi' || input.alfgwc_data_source == 'rc'",
            div(class = "progress-box", style = "background:#e3f2fd; border-left-color:#1a73c1; font-size:11px; margin-bottom:6px;",
                icon("info-circle"), " Using data from the computation stage.")
          )
        )
      ),
      
      # ── KOLOM 2 (width=3): Parameter ALFGWC ──────────────────────────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("cog"), " ALFGWC Parameters"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,
          collapsible = TRUE,
          
          div(class = "step-header", "4. Number of Clusters (c)"),
          sliderInput("alfgwc_ncluster", NULL, min = 2, max = 10, value = 4, step = 1),
          
          div(class = "step-header", "Fuzzifier (m)"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "m=2 default | m>2 more fuzzy | m\u21921 approaches hard clustering"),
          sliderInput("alfgwc_m", NULL, min = 1.1, max = 3.0, value = 2.0, step = 0.1),
          

          div(class = "step-header", "Contiguity Criterion"),
          selectInput("alfgwc_neighbor_type", NULL, 
                      choices = c(
                        "Queen Contiguity" = "queen",
                        "Rook Contiguity" = "rook",
                        "Bishop Contiguity" = "bishop",
                        "Distance Threshold (dthr)" = "dthr"
                      ),
                      selected = "queen"),
          
          conditionalPanel(
            condition = "input.alfgwc_neighbor_type == 'dthr'",
            div(class = "step-header", "Distance Threshold (dthr)"),
            numericInput("alfgwc_dthr", NULL, value = 1.0, step = 0.1),
            div(class = "progress-box", style = "font-size:11px; margin-bottom:10px;",
                icon("info-circle"), " Neighborhood radius. -99 = Global mode (all units).")
          ),
          div(class = "step-header", "Mode Weighting (TW)"),
          radioButtons("alfgwc_tw", NULL,
                       choices = c(
                         "Distance Decay (DLFGWC)"    = "DISTANCE_DECAY",
                         "Spatial Interaction (LFGWC)" = "SPATIAL_INTERACTION"
                       ),
                       selected = "SPATIAL_INTERACTION"),
          
          conditionalPanel(
            "input.alfgwc_tw == 'DISTANCE_DECAY'",
            div(class = "step-header", "Eksponen Distance Decay (\u03b3)"),
            sliderInput("alfgwc_gamma", NULL, min = 1, max = 4, value = 2, step = 0.5)
          ),
          
          tags$hr(),
          
          div(class = "step-header", "Max. Iterations"),
          numericInput("alfgwc_maxiter", NULL, value = 100, min = 10, step = 10),
          
          div(class = "step-header", "Convergence Tolerance (\u03b5)"),
          numericInput("alfgwc_error", NULL, value = 0.001, min = 1e-6, step = 0.001),
          
          div(class = "step-header", "Random Seed"),
          numericInput("alfgwc_seed", NULL, value = 0, min = 0, step = 1)
        )
      ),
      
      # ── KOLOM 3 (width=3): Parameter Adaptif (Local Moran's I) ───────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("project-diagram"), " Adaptive Mechanism"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,
          
          div(class = "step-header", "Target Variable for Local Moran's I"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "Variable used to compute Spatial Autocorrelation Index (Hotspot/Coldspot)"),
          uiOutput("alfgwc_moran_var_selector"),
          
          tags$hr(),
          
          div(class = "step-header", "Alpha (\u03b1) Rules"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;", "Old membership weights, dynamically adjusted:"),
          
          div(class = "step-header", "Alpha High"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;", "(Hotspot region I>0, p<0.05)"),
          numericInput("alfgwc_alpha_high", NULL, value = 0.8, min = 0, max = 1, step = 0.1),
          
          div(class = "step-header", "Alpha Low"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;", "(Coldspot region I<0, p<0.05)"),
          numericInput("alfgwc_alpha_low", NULL, value = 0.2, min = 0, max = 1, step = 0.1),
          
          div(class = "step-header", "Alpha Mid"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;", "(Not Significant / Other)"),
          numericInput("alfgwc_alpha_mid", NULL, value = 0.5, min = 0, max = 1, step = 0.1)
        )
      ),
      # ── KOLOM 4 (width=3): Algoritma Optimasi ──────────────────────────────
      column(3,
        shinydashboard::box(
          title  = tags$span(icon("microchip"), " Optimization Algorithm"),
          status = "primary",
          solidHeader = TRUE,
          width  = 12,

          div(class = "step-header", "5. Select Algorithm"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "Classic = pure ALFGWC. Others = initial centroid optimization."),
          radioButtons("alfgwc_algorithm", NULL,
                       choices = c(
                         "Classic ALFGWC"                         = "classic",
                         "ABC (Artificial Bee Colony)"       = "abc",
                         "FPA (Flower Pollination)"          = "fpa",
                         "GSA (Gravitational Search)"        = "gsa",
                         "GWO (Grey Wolf Optimizer)"         = "gwo",
                         "HHO (Harris-Hawk Optimization)"    = "hho",
                         "IFA (Intelligent Firefly)"         = "ifa",
                         "PSO (Particle Swarm)"              = "pso",
                         "TLBO (Teaching-Learning Based)"    = "tlbo",
                         "WOA (Whale Optimization)"          = "woa"
                       ),
                       selected = "classic")
        )
      ),

      # ── KOLOM 5 (width=3): Parameter Algoritma ─────────────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("sliders-h"), " Algorithm Parameters"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,
          collapsible = TRUE,

          # Universal
          conditionalPanel(
            "input.alfgwc_algorithm != 'classic'",
            div(class = "step-header", "Number of Particles / Agents"),
            sliderInput("alfgwc_npar", NULL,
                        min = 3, max = 30, value = 10, step = 1),
            div(class = "step-header", "Convergence (same)"),
            sliderInput("alfgwc_same", NULL,
                        min = 5, max = 30, value = 10, step = 1),
            div(class = "step-header", "Initialization Distribution"),
            selectInput("alfgwc_vi_dist", NULL,
                        choices  = c("Uniform" = "uniform", "Normal"  = "normal"),
                        selected = "uniform")
          ),

          conditionalPanel(
            "input.alfgwc_algorithm == 'classic'",
            div(class = "progress-box",
                style = "background:#f8f9fa; border-left-color:#adb5bd; font-size:12px;",
                icon("info-circle"),
                " Classic ALFGWC: random centroid initialization (uniform/normal).")
          ),

          # ABC
          conditionalPanel(
            "input.alfgwc_algorithm == 'abc'",
            div(class = "step-header", "Number of Onlooker Bees"),
            sliderInput("alfgwc_abc_onlooker", NULL,
                        min = 2, max = 20, value = 5, step = 1),
            div(class = "step-header", "Limit (Scout)"),
            sliderInput("alfgwc_abc_limit", NULL,
                        min = 1, max = 20, value = 5, step = 1)
          ),

          # FPA
          conditionalPanel(
            "input.alfgwc_algorithm == 'fpa'",
            div(class = "step-header", "Switch Prob. (p)"),
            sliderInput("alfgwc_fpa_p", NULL,
                        min = 0.1, max = 0.9, value = 0.7, step = 0.05),
            div(class = "step-header", "Gamma"),
            numericInput("alfgwc_fpa_gamma", NULL,
                         value = 1.2, min = 0.1, step = 0.1),
            div(class = "step-header", "Lambda (Levy)"),
            numericInput("alfgwc_fpa_lambda", NULL,
                         value = 1.5, min = 0.1, step = 0.1),
            div(class = "step-header", "EI Distribution"),
            selectInput("alfgwc_fpa_ei", NULL,
                        choices  = c("logchaotic","normal","uniform","kentchaotic","levy"),
                        selected = "logchaotic")
          ),

          # GSA
          conditionalPanel(
            "input.alfgwc_algorithm == 'gsa'",
            div(class = "step-header", "Gravitational Constant (G)"),
            numericInput("alfgwc_gsa_G", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "Vmax"),
            sliderInput("alfgwc_gsa_vmax", NULL,
                        min = 0.1, max = 2.0, value = 0.7, step = 0.1),
            checkboxInput("alfgwc_gsa_new",
                          "Use new GSA version (Li & Dong 2017)",
                          value = FALSE)
          ),

          # HHO
          conditionalPanel(
            "input.alfgwc_algorithm == 'hho'",
            div(class = "step-header", "HHO Algorithm"),
            selectInput("alfgwc_hho_algo", NULL,
                        choices  = c("Heidari (2019)" = "heidari",
                                     "Bairathi (2018)" = "bairathi"),
                        selected = "bairathi"),
            fluidRow(
              column(4, div(class = "step-header", "a1"),
                     numericInput("alfgwc_hho_a1", NULL,
                                  value = 3, min = 0, step = 0.5)),
              column(4, div(class = "step-header", "a2"),
                     numericInput("alfgwc_hho_a2", NULL,
                                  value = 1, min = 0, step = 0.5)),
              column(4, div(class = "step-header", "a3"),
                     numericInput("alfgwc_hho_a3", NULL,
                                  value = 0.4, min = 0, step = 0.1))
            )
          ),

          # IFA
          conditionalPanel(
            "input.alfgwc_algorithm == 'ifa'",
            div(class = "step-header", "Number of Selected Fireflies"),
            sliderInput("alfgwc_ifa_parno", NULL,
                        min = 1, max = 10, value = 3, step = 1),
            div(class = "step-header", "Gamma"),
            numericInput("alfgwc_ifa_gamma", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "Beta (Attractiveness)"),
            numericInput("alfgwc_ifa_beta", NULL,
                         value = 1, min = 0.1, step = 0.1),
            div(class = "step-header", "EI Distribution"),
            selectInput("alfgwc_ifa_ei", NULL,
                        choices  = c("logchaotic","normal","uniform","kentchaotic","levy"),
                        selected = "logchaotic")
          ),

          # PSO
          conditionalPanel(
            "input.alfgwc_algorithm == 'pso'",
            div(class = "step-header", "Vmax"),
            sliderInput("alfgwc_pso_vmax", NULL,
                        min = 0.1, max = 2.0, value = 0.8, step = 0.1),
            fluidRow(
              column(6, div(class = "step-header", "c1 (cognitive)"),
                     numericInput("alfgwc_pso_c1", NULL,
                                  value = 0.7, min = 0, step = 0.1)),
              column(6, div(class = "step-header", "c2 (social)"),
                     numericInput("alfgwc_pso_c2", NULL,
                                  value = 0.6, min = 0, step = 0.1))
            ),
            div(class = "step-header", "Inertia Weight Type"),
            selectInput("alfgwc_pso_type", NULL,
                        choices  = c("chaotic","constant","sim.annealing",
                                     "nat.exponent1","nat.exponent2"),
                        selected = "chaotic"),
            fluidRow(
              column(6, div(class = "step-header", "wmax"),
                     numericInput("alfgwc_pso_wmax", NULL,
                                  value = 0.8, min = 0, step = 0.05)),
              column(6, div(class = "step-header", "wmin"),
                     numericInput("alfgwc_pso_wmin", NULL,
                                  value = 0.3, min = 0, step = 0.05))
            )
          ),

          # TLBO
          conditionalPanel(
            "input.alfgwc_algorithm == 'tlbo'",
            div(class = "step-header", "Number of Selections"),
            sliderInput("alfgwc_tlbo_nselect", NULL,
                        min = 2, max = 20, value = 10, step = 1),
            checkboxInput("alfgwc_tlbo_elitism",
                          "Use Elitism", value = FALSE),
            div(class = "step-header", "Number of Elites"),
            numericInput("alfgwc_tlbo_nelite", NULL,
                         value = 2, min = 1, step = 1)
          ),

          # WOA
          conditionalPanel(
            "input.alfgwc_algorithm == 'woa'",
            div(class = "step-header", "Spiral Constant (b)"),
            numericInput("alfgwc_woa_b", NULL,
                         value = 1, min = 0.1, step = 0.1)
          )
        )
      ),
      
      # ── KOLOM 6 (width=3): Eksekusi + Download ────────────────────────────────────────────────
      column(3,
        shinydashboard::box(
          title = tags$span(icon("play"), " Run ALFGWC"),
          status = "success", solidHeader = TRUE, width = 12,
          
          actionButton("btn_run_alfgwc", "Run ALFGWC",
                       icon = icon("cogs"),
                       class = "btn-primary btn-lg btn-block",
                       style = "margin-bottom:15px; font-weight:bold;"),
          
          uiOutput("alfgwc_run_msg"),
          tags$hr(),
          
          div(class = "step-header", "Display Settings"),
          selectInput("alfgwc_palette", "Map & Chart Color Palette",
                      choices = c("Dark2", "Set1", "Set2", "Set3", 
                                  "Pastel1", "Pastel2", "Paired", 
                                  "Accent", "Spectral", "RdYlBu"),
                      selected = "Dark2"),
          tags$hr(),
          
          div(class = "step-header", "Download Results"),
          downloadButton("dl_alfgwc_csv",
                         tags$span(icon("download"), " Cluster Results (.csv)"),
                         class = "btn-info btn-block"),
          tags$br(),
          downloadButton("dl_alfgwc_memb_csv",
                         tags$span(icon("download"), " Membership Matrix (.csv)"),
                         class = "btn-info btn-block"),
          tags$br(),
          downloadButton("dl_alfgwc_map_png",
                         tags$span(icon("map"), " Cluster Map (.png)"),
                         class = "btn-success btn-block")
        )
      )
    ),
    
    # ── BARIS 2: Hasil ALFGWC ──────────────────────────────────────────────
    fluidRow(
      column(12,
        shinydashboard::box(
          title       = tags$span(icon("project-diagram"), " ALFGWC Results"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,

          tabsetPanel(
            # Tab 1: Ringkasan
            tabPanel(tags$span(icon("info-circle"), " Summary"),
                     tags$br(),
                     uiOutput("alfgwc_summary")),

            # Tab 2: Validasi + Konvergensi
            tabPanel(tags$span(icon("chart-bar"), " Validation"),
                     tags$br(),
                     div(class = "step-header", "ALFGWC Validation Index"),
                     tags$p(style = "font-size:12px; color:#78909c;",
                            "PC & IFV: higher is better.",
                            " CE & SC: lower is better."),
                     DT::DTOutput("alfgwc_val_table"),
                     tags$hr(),
                     div(class = "step-header", "Objective Function Convergence"),
                     plotOutput("alfgwc_conv_plot", height = "260px")),

            # Tab 3: Peta Cluster
            tabPanel(tags$span(icon("map"), " Cluster Map"),
                     tags$br(),
                     leaflet::leafletOutput("alfgwc_map", height = "500px")),

            # Tab 4: Peta Max Membership
            tabPanel(tags$span(icon("percent"), " Max Membership"),
                     tags$br(),
                     div(class = "progress-box",
                         style = "background:#e3f2fd; border-left-color:#1a73c1;
                                  font-size:12px; margin-bottom:8px;",
                         icon("info-circle"),
                         " Values close to 1 = unit clearly belongs to 1 cluster.",
                         " Low values = boundary units between clusters (fuzzy)."),
                     leaflet::leafletOutput("alfgwc_map_membership", height = "480px")),

            # Tab 5: Silhouette
            tabPanel(tags$span(icon("chart-line"), " Silhouette"),
                     tags$br(),
                     fluidRow(
                       column(7,
                              div(class = "step-header", "Silhouette Plot"),
                              plotOutput("alfgwc_sil_plot", height = "300px")),
                       column(5,
                              div(class = "step-header", "Avg. Silhouette Width"),
                              DT::DTOutput("alfgwc_sil_table"),
                              tags$br(),
                              uiOutput("alfgwc_sil_interp"))
                     )),

            # Tab 6: Profil Cluster
            tabPanel(tags$span(icon("th"), " Cluster Profile"),
                     tags$br(),
                     div(class = "step-header", "Cluster Profile Table (Mean per Cluster)"),
                     DT::DTOutput("alfgwc_profile_table"),
                     tags$hr(),
                     div(class = "step-header", "Profile Heatmap"),
                     plotOutput("alfgwc_heatmap", height = "320px"),
                     div(style = "text-align:right; margin-top:6px;",
                         downloadButton("dl_alfgwc_heatmap",
                                        tags$span(icon("download"), " Download Heatmap (.png)"),
                                        class = "btn-sm btn-default")),
                     tags$hr(),
                     div(class = "step-header", "Radar Chart per Cluster"),
                     plotOutput("alfgwc_radar", height = "400px"),
                     div(style = "text-align:right; margin-top:6px;",
                         downloadButton("dl_alfgwc_radar",
                                        tags$span(icon("download"), " Download Radar Chart (.png)"),
                                        class = "btn-sm btn-default"))),

            # Tab 7: Data Cluster
            tabPanel(tags$span(icon("list-ol"), " Cluster Data"),
                     tags$br(),
                     DT::DTOutput("alfgwc_result_table")),

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
                                numericInput("alfgwc_sammon_iter",  "Max. Iterations:",
                                             value = 500, min = 100, step = 100),
                                numericInput("alfgwc_sammon_magic", "Magic (Step Size):",
                                             value = 0.2, min = 0.01, step = 0.05),
                                sliderInput("alfgwc_sammon_pt",    "Point Size:",
                                            min = 1, max = 6, value = 3, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_alfgwc_sammon",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("alfgwc_sammon_plot", height = "600px")
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
                                sliderInput("alfgwc_tsne_perp", "Perplexity:",
                                            min = 5, max = 50, value = 15, step = 5),
                                numericInput("alfgwc_tsne_iter", "Max. Iterations:",
                                             value = 1000, min = 250, step = 250),
                                sliderInput("alfgwc_tsne_pt",   "Point Size:",
                                            min = 1, max = 6, value = 3, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_alfgwc_tsne",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("alfgwc_tsne_plot", height = "600px")
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
                                sliderInput("alfgwc_umap_nn", "n_neighbors:",
                                            min = 2, max = 30, value = 15, step = 1),
                                sliderInput("alfgwc_umap_md", "min_dist:",
                                            min = 0.01, max = 0.99, value = 0.1, step = 0.05),
                                sliderInput("alfgwc_umap_pt", "Point Size:",
                                            min = 1, max = 6, value = 3, step = 0.5),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_alfgwc_umap",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("alfgwc_umap_plot", height = "600px")
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
                               " Run ALFGWC (main tab) first before starting stability analysis."
                             )
                           ),

                           fluidRow(
                             column(3,
                               numericInput("alfgwc_nruns",
                                 label = tags$span(icon("redo"), " Number of Runs:"),
                                 value = 30, min = 5, max = 100, step = 5)
                             ),
                             column(3,
                               numericInput("alfgwc_seed_start",
                                 label = tags$span(icon("random"), " Seed Start:"),
                                 value = 1, min = 0, step = 1)
                             ),
                             column(3,
                               tags$br(),
                               actionButton("alfgwc_run_stability",
                                 label = tags$span(icon("play-circle"), " Run Stability Test"),
                                 class = "btn-primary btn-block",
                                 style = "margin-top:6px;"
                               )
                             ),
                             column(3,
                               tags$br(),
                               downloadButton("dl_alfgwc_stability",
                                 label = tags$span(icon("download"), " Download Detail (.csv)"),
                                 class = "btn-default btn-block",
                                 style = "margin-top:6px;"
                               )
                             )
                           ),

                           tags$div(style = "margin-top:10px;",
                             uiOutput("alfgwc_stability_progress")
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
                           DT::dataTableOutput("alfgwc_stability_summary")
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
                           plotOutput("alfgwc_stability_boxplot", height = "420px")
                         )
                       )
                     ),

                     fluidRow(
                       column(12,
                         shinydashboard::box(
                           title       = tags$span(icon("list-alt"), " Detail per Run"),
                           status      = "info",
                           solidHeader = FALSE,
                           width       = 12,
                           collapsible = TRUE,
                           collapsed   = TRUE,
                           DT::dataTableOutput("alfgwc_stability_detail")
                         )
                       )
                     )

            ) # end Tab 10

          ) # end tabsetPanel
        )   # end box Hasil ALFGWC
      )     # end column(12)
    )       # end fluidRow baris 2
  )         # end tabItem
}
