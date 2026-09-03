# =============================================================================
# R/sovi_computation/sovi_computation_ui.R
# UI Tab SoVI Computation
#
# CALLED FROM: ui.R via sovi_computation_tab_ui()
# =============================================================================

sovi_computation_tab_ui <- function() {
  
  shinydashboard::tabItem("tab_sovi",
                          
                          fluidRow(
                            
                            # ── Kolom Kiri: Parameter ─────────────────────────────────────────────
                            column(3,
                                   shinydashboard::box(
                                     title       = tags$span(icon("cog"), " SoVI Parameters"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     # Direction Method
                                     div(class = "step-header", "Direction Method"),
                                     radioButtons("direction_method", NULL,
                                                  choices = c(
                                                    "Theory-Based (PM)" = "theory",
                                                    "Loading Sign"      = "loading",
                                                    "Cutter's Method"   = "cutter"
                                                  ),
                                                  selected = "theory"
                                     ),
                                     
                                     tags$hr(),
                                     
                                     # Rotasi PCA
                                     div(class = "step-header", "PCA Rotation"),
                                     selectInput("pca_rotation", NULL,
                                                 choices = c(
                                                   "Varimax (default)"   = "varimax",
                                                   "Oblimin"             = "oblimin",
                                                   "Promax"              = "promax",
                                                   "Quartimax"           = "quartimax",
                                                   "None (no rotation)"  = "none"
                                                 ),
                                                 selected = "varimax"
                                     ),
                                     
                                     tags$hr(),
                                     
                                     # Loading Threshold
                                     sliderInput("sovi_threshold",
                                                 "Loading Threshold (\u03c4)",
                                                 min = 0.3, max = 0.9, value = 0.5, step = 0.05),
                                     
                                     tags$hr(),
                                     
                                     # Tombol Run
                                     actionButton("run_sovi", "Run SoVI",
                                                  class = "btn-primary btn-lg btn-block",
                                                  icon  = icon("play")),
                                     tags$br(), tags$br(),
                                     uiOutput("sovi_progress"),
                                     tags$br(),
                                     uiOutput("perf_advisor")
                                   )
                            ),
                            
                            # ── Kolom Kanan: Output ───────────────────────────────────────────────
                            column(9,
                                   shinydashboard::box(
                                     title       = tags$span(icon("chart-area"), " SoVI Results"),
                                     status      = "info",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     tabsetPanel(
                                       
                                       # Tab 1: Diagnostik PCA
                                       tabPanel(
                                         title = tags$span(icon("stethoscope"), " PCA Diagnostics"),
                                         tags$br(),
                                         fluidRow(
                                           column(6,
                                                  div(class = "step-header", "KMO per Variable"),
                                                  DT::DTOutput("pca_kmo_df")
                                           ),
                                           column(6,
                                                  div(class = "step-header", "Bartlett's Test"),
                                                  verbatimTextOutput("pca_bartlett")
                                           )
                                         ),
                                         tags$br(),
                                         div(class = "step-header", "Communality per Variable"),
                                         DT::DTOutput("pca_communality")
                                       ),
                                       
                                       # Tab 2: Variance & Loadings
                                       tabPanel(
                                         title = tags$span(icon("layer-group"), " Variance & Loadings"),
                                         tags$br(),
                                         div(class = "step-header", "Variance Explained"),
                                         DT::DTOutput("pca_variance"),
                                         tags$br(),
                                         div(class = "step-header", "Loading Matrix"),
                                         DT::DTOutput("pca_loadings")
                                       ),
                                       
                                       # Tab 3: Assignment
                                       tabPanel(
                                         title = tags$span(icon("sitemap"), " Assignment"),
                                         tags$br(),
                                         DT::DTOutput("sovi_assignment")
                                       ),
                                       
                                       # Tab 4: SoVI Scores
                                       tabPanel(
                                         title = tags$span(icon("list-ol"), " SoVI Scores"),
                                         tags$br(),
                                         DT::DTOutput("sovi_table")
                                       ),
                                       
                                       # Tab 5: Class Distribution
                                       tabPanel(
                                         title = tags$span(icon("chart-pie"), " Class Distribution"),
                                         tags$br(),
                                         fluidRow(
                                           column(7, plotOutput("sovi_class_plot", height = "300px")),
                                           column(5, tags$br(), DT::DTOutput("sovi_class_table"))
                                         )
                                       ),
                                       
                                       # Tab 6: SoVI Map
                                       tabPanel(
                                         title = tags$span(icon("map"), " SoVI Map"),
                                         tags$br(),
                                         leaflet::leafletOutput("sovi_map", height = "500px")
                                       ),
                                       
                                       # Tab 7: Top 10
                                       tabPanel(
                                         title = tags$span(icon("sort-amount-down"), " Top 10"),
                                         tags$br(),
                                         DT::DTOutput("sovi_top10")
                                       )
                                       
                                     ) # end tabsetPanel
                                   )   # end box
                            )     # end column kanan
                            
                          ) # end fluidRow
  )   # end tabItem
}