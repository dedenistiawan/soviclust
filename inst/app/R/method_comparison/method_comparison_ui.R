# =============================================================================
# R/method_comparison/method_comparison_ui.R
# UI Tab Method Comparison (Opsional)
#
# DIPANGGIL DARI: ui.R via method_comparison_tab_ui()
# =============================================================================

method_comparison_tab_ui <- function() {
  
  shinydashboard::tabItem("tab_comparison",
                          
                          fluidRow(
                            
                            # ── Kolom Kiri: Parameter ─────────────────────────────────────────────
                            column(3,
                                   shinydashboard::box(
                                     title       = tags$span(icon("sliders-h"), " Parameter"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     # Info opsional
                                     div(class = "info-card",
                                         style = "border-left-color:#f39c12; padding:10px;",
                                         tags$p(style = "font-size:12.5px; margin:0;",
                                                icon("star", style = "color:#f39c12;"),
                                                tags$strong(" This tab is optional. "),
                                                "You can skip to SoVI Computation.")),
                                     tags$br(),
                                     
                                     sliderInput("comp_threshold", "Loading Threshold (\u03c4)",
                                                 min = 0.3, max = 0.9, value = 0.5, step = 0.05),
                                     tags$br(),
                                     
                                     actionButton("run_comparison", "Run Comparison",
                                                  class = "btn-warning btn-lg btn-block",
                                                  icon  = icon("play")),
                                     tags$br(), tags$br(),
                                     uiOutput("comparison_progress")
                                   )
                            ),
                            
                            # ── Kolom Kanan: Output ───────────────────────────────────────────────
                            column(9,
                                   shinydashboard::box(
                                     title       = tags$span(icon("chart-line"), " 3-Way Comparison Results"),
                                     status      = "info",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     tabsetPanel(
                                       
                                       # Tab 1: All Evaluation Metrics (shown first)
                                       tabPanel(
                                         title = tags$span(icon("ruler-combined"), " Evaluation Metrics"),
                                         tags$br(),
                                         uiOutput("comp_metrics_ui")
                                       ),
                                       
                                       # Tab 2: Spearman
                                       tabPanel(
                                         title = tags$span(icon("table"), " Spearman"),
                                         tags$br(),
                                         DT::DTOutput("comp_spearman"),
                                         tags$br(),
                                         verbatimTextOutput("comp_interpretation")
                                       ),
                                       
                                       # Tab 3: Scatter
                                       tabPanel(
                                         title = tags$span(icon("dot-circle"), " Scatter"),
                                         plotOutput("comp_scatter", height = "400px")
                                       ),
                                       
                                       # Tab 4: Class Distribution
                                       tabPanel(
                                         title = tags$span(icon("chart-bar"), " Class Distribution"),
                                         plotOutput("comp_class_dist", height = "360px")
                                       ),
                                       
                                       # Tab 5: Component Profile
                                       tabPanel(
                                         title = tags$span(icon("th"), " Component Profile"),
                                         plotOutput("comp_profile", height = "360px")
                                       ),
                                       
                                       # Tab 6: Recommendation
                                       tabPanel(
                                         title = tags$span(icon("lightbulb"), " Recommendation"),
                                         tags$br(),
                                         uiOutput("comp_recommendation")
                                       )
                                       
                                     ) # end tabsetPanel
                                   )   # end box
                            )     # end column kanan
                            
                          ) # end fluidRow
  )   # end tabItem
}