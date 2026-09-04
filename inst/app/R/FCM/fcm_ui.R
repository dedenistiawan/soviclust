# =============================================================================
# R/FCM/fcm_ui.R
# UI Tab FCM (Fuzzy C-Means) — Submenu Cluster Analysis
#
# DIPANGGIL DARI: ui.R via fcm_tab_ui()
# BERISI       : definisi tabItem("tab_fcm", ...)
#
# LAYOUT:
#   Baris 1 (atas)  : Setting Parameter — 3 kolom
#     Kolom 1 (w=3) : Sumber Data (identik FGWC)
#     Kolom 2 (w=3) : Parameter FCM
#     Kolom 3 (w=3) : Tombol Run + Download
#   Baris 2 (bawah) : Hasil FCM — full width (w=12), 9 tab
# =============================================================================

fcm_tab_ui <- function() {

  shinydashboard::tabItem("tab_fcm",

    # ══════════════════════════════════════════════════════════════════════════
    # BARIS 1: Setting Parameter (atas)
    # ══════════════════════════════════════════════════════════════════════════
    fluidRow(

      # ── KOLOM 1 (width=3): Sumber Data ─────────────────────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("database"), " Data Source"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,

          div(class = "step-header", "1. Feature Data Source"),
          radioButtons("fcm_data_source", NULL,
                       choices = c(
                         "Original Data (no transformation)" = "raw",
                         "Normalized Data (0-1)"             = "raw_norm",
                         "Standardized Data (Z-score)"       = "standardized",
                         "SoVI Score"                        = "sovi",
                         "RC Scores (PCA components)"        = "rc"
                       ),
                       selected = "rc"
          ),

          conditionalPanel(
            "input.fcm_data_source == 'raw' ||
             input.fcm_data_source == 'raw_norm' ||
             input.fcm_data_source == 'standardized'",
            div(class = "step-header", "Select Variables"),
            uiOutput("fcm_var_selector")
          ),

          conditionalPanel(
            "input.fcm_data_source == 'sovi' ||
             input.fcm_data_source == 'rc'",
            uiOutput("fcm_datasource_info")
          ),

          tags$hr(),
          div(class = "progress-box",
              style = "background:#e8f5e9; border-left-color:#27ae60;
                       font-size:11.5px; margin-top:4px;",
              icon("info-circle"),
              tags$strong(" FCM vs FGWC:"),
              tags$br(),
              " FCM is the non-spatial version of FGWC \u2014 no distance matrix
              or population data required. Clustering is based purely on
              attribute similarity.")
        )
      ),

      # ── KOLOM 2 (width=3): Parameter FCM ───────────────────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("cog"), " FCM Parameters"),
          status      = "primary",
          solidHeader = TRUE,
          width       = 12,
          collapsible = TRUE,

          div(class = "step-header", "2. Number of Clusters (k)"),
          sliderInput("fcm_ncluster", NULL,
                      min = 2, max = 10, value = 4, step = 1),

          div(class = "step-header", "Fuzzifier (m)"),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "m = 2 (default). Higher m \u2192 softer cluster boundaries."),
          sliderInput("fcm_m", NULL,
                      min = 1.1, max = 3.0, value = 2.0, step = 0.1),

          div(class = "step-header", "Max. Iterations"),
          numericInput("fcm_maxiter", NULL, value = 500, min = 10, step = 50),

          div(class = "step-header", "Random Seed"),
          numericInput("fcm_seed", NULL, value = 0, min = 0, step = 1),

          tags$hr(),
          div(class = "step-header", "Optional: Standardize Features"),
          checkboxInput("fcm_scale",
                        "Apply Z-score scaling before FCM",
                        value = FALSE),
          tags$p(style = "font-size:11px; color:#78909c; margin:-4px 0 6px;",
                 "Recommended when features have very different scales.")
        )
      ),

      # ── KOLOM 3 (width=3): Visualisasi Sammon / t-SNE / UMAP ─────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("sliders-h"), " Visualization Parameters"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,
          collapsible = TRUE,

          div(class = "step-header", "Sammon Mapping"),
          fluidRow(
            column(6, numericInput("fcm_sammon_iter",  "Max Iterations:", value = 500, min = 100, step = 100)),
            column(6, numericInput("fcm_sammon_magic", "Step Size:",      value = 0.2, min = 0.01, step = 0.05))
          ),
          sliderInput("fcm_sammon_pt", "Point Size:", min = 1, max = 5, value = 2, step = 0.5),

          tags$hr(),
          div(class = "step-header", "t-SNE"),
          fluidRow(
            column(6, numericInput("fcm_tsne_perp", "Perplexity:", value = 15, min = 2, step = 1)),
            column(6, numericInput("fcm_tsne_iter", "Max Iterations:", value = 1000, min = 500, step = 100))
          ),
          sliderInput("fcm_tsne_pt", "Point Size:", min = 1, max = 5, value = 2, step = 0.5),

          tags$hr(),
          div(class = "step-header", "UMAP"),
          fluidRow(
            column(6, numericInput("fcm_umap_nn", "n_neighbors:", value = 15, min = 2, step = 1)),
            column(6, numericInput("fcm_umap_md", "min_dist:",    value = 0.1, min = 0.01, step = 0.05))
          ),
          sliderInput("fcm_umap_pt", "Point Size:", min = 1, max = 5, value = 2, step = 0.5)
        )
      ),

      # ── KOLOM 4 (width=3): Tombol Run + Download ───────────────────────────
      column(3,
        shinydashboard::box(
          title       = tags$span(icon("play"), " Run"),
          status      = "success",
          solidHeader = TRUE,
          width       = 12,

          actionButton("run_fcm",
                       tags$span(icon("play-circle"), " Run FCM"),
                       class = "btn-success btn-lg btn-block"),
          tags$br(), tags$br(),
          uiOutput("fcm_progress"),
          tags$hr(),

          div(class = "step-header", "Download Results"),
          downloadButton("dl_fcm_csv",
                         tags$span(icon("download"), " Cluster Results (.csv)"),
                         class = "btn-info btn-block"),
          tags$br(),
          downloadButton("dl_fcm_map_png",
                         tags$span(icon("map"), " FCM Map (.png)"),
                         class = "btn-success btn-block"),
          tags$br(),
          downloadButton("dl_fcm_heatmap",
                         tags$span(icon("download"), " Heatmap (.png)"),
                         class = "btn-default btn-block"),
          tags$br(),
          downloadButton("dl_fcm_radar",
                         tags$span(icon("download"), " Radar Chart (.png)"),
                         class = "btn-default btn-block")
        )
      )
    ),

    # ══════════════════════════════════════════════════════════════════════════
    # BARIS 2: Hasil FCM (bawah) — full width
    # ══════════════════════════════════════════════════════════════════════════
    fluidRow(
      column(12,
        shinydashboard::box(
          title       = tags$span(icon("project-diagram"), " FCM Results"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,

          tabsetPanel(

            # Tab 1: Summary
            tabPanel(tags$span(icon("info-circle"), " Summary"),
                     tags$br(),
                     uiOutput("fcm_summary")),

            # Tab 2: Validation + Convergence
            tabPanel(tags$span(icon("chart-bar"), " Validation"),
                     tags$br(),
                     div(class = "step-header", "Cluster Validation Index"),
                     tags$p(style = "font-size:12px; color:#78909c;",
                            "PC & MPC: higher is better.  CE: lower is better.
                             SC, SI, XB, Kwon: lower is better."),
                     DT::DTOutput("fcm_val_table"),
                     tags$hr(),
                     div(class = "step-header", "Objective Function Convergence"),
                     plotOutput("fcm_conv_plot", height = "260px")),

            # Tab 3: Interactive Map
            tabPanel(tags$span(icon("map"), " Interactive Map"),
                     tags$br(),
                     leaflet::leafletOutput("fcm_map", height = "500px")),

            # Tab 4: Silhouette
            tabPanel(tags$span(icon("chart-line"), " Silhouette"),
                     tags$br(),
                     fluidRow(
                       column(7,
                              div(class = "step-header", "Silhouette Plot"),
                              plotOutput("fcm_sil_plot", height = "300px")),
                       column(5,
                              div(class = "step-header", "Avg. Silhouette Width"),
                              DT::DTOutput("fcm_sil_table"),
                              tags$br(),
                              uiOutput("fcm_sil_interp"))
                     )),

            # Tab 5: Cluster Profile
            tabPanel(tags$span(icon("th"), " Cluster Profile"),
                     tags$br(),
                     div(class = "step-header", "Cluster Profile Table (Mean per Cluster)"),
                     DT::DTOutput("fcm_profile_table"),
                     tags$hr(),
                     div(class = "step-header", "Profile Heatmap"),
                     plotOutput("fcm_heatmap", height = "320px"),
                     tags$hr(),
                     div(class = "step-header", "Radar Chart per Cluster"),
                     plotOutput("fcm_radar", height = "400px")),

            # Tab 6: Cluster Data
            tabPanel(tags$span(icon("list-ol"), " Cluster Data"),
                     tags$br(),
                     DT::DTOutput("fcm_result_table")),

            # Tab 7: Sammon Mapping
            tabPanel(tags$span(icon("project-diagram"), " Sammon Mapping"),
                     tags$br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                tags$strong("Sammon Parameters"),
                                tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
                                tags$p(style = "font-size:11.5px; color:#78909c;",
                                       "Adjust in the 'Visualization Parameters' box above."),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_fcm_sammon",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("fcm_sammon_plot", height = "600px")
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
                                tags$p(style = "font-size:11.5px; color:#78909c;",
                                       "Adjust in the 'Visualization Parameters' box above."),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_fcm_tsne",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("fcm_tsne_plot", height = "600px")
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
                                tags$p(style = "font-size:11.5px; color:#78909c;",
                                       "Adjust in the 'Visualization Parameters' box above."),
                                tags$hr(style = "margin-top:10px; margin-bottom:8px;"),
                                downloadButton("dl_fcm_umap",
                                               tags$span(icon("download"), " Download (.png)"),
                                               class = "btn-default btn-block btn-sm")
                              )
                       ),
                       column(9,
                              plotOutput("fcm_umap_plot", height = "600px")
                       )
                     )
            )

          ) # end tabsetPanel
        )   # end box Hasil FCM
      )     # end column(12)
    )       # end fluidRow baris 2

  )         # end tabItem
}
