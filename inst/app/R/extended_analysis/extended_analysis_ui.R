# =============================================================================
# R/extended_analysis/extended_analysis_ui.R
# UI Tab Extended Analysis
#
# DIPANGGIL DARI: ui.R via extended_analysis_tab_ui()
# =============================================================================

extended_analysis_tab_ui <- function() {
  
  shinydashboard::tabItem("tab_analysis",
                          
                          fluidRow(
                            
                            # ── Kolom Kiri: Pilih Analisis ────────────────────────────────────────
                            column(3,
                                   shinydashboard::box(
                                     title       = tags$span(icon("flask"), " Pilih Analisis"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     checkboxGroupInput("selected_analyses", NULL,
                                                        choices = c(
                                                          "1. Dominant Component"   = "dominant",
                                                          "2. Component Profile"    = "profile",
                                                          "3. Moran's I + LISA"     = "moran",
                                                          "4. Sensitivity Analysis" = "sensitivity",
                                                          "5. Cutter Comparison"    = "cutter_comp"
                                                        ),
                                                        selected = c("dominant", "profile", "moran")
                                     ),
                                     
                                     tags$hr(),
                                     
                                     actionButton("run_analysis", "Run Extended Analysis",
                                                  class = "btn-success btn-lg btn-block",
                                                  icon  = icon("play")),
                                     tags$br(), tags$br(),
                                     uiOutput("analysis_progress")
                                   )
                            ),
                            
                            # ── Kolom Kanan: Output ───────────────────────────────────────────────
                            column(9,
                                   shinydashboard::box(
                                     title       = tags$span(icon("poll"), " Hasil Extended Analysis"),
                                     status      = "info",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     tabsetPanel(
                                       
                                       # Tab 1: Dominant Component
                                       tabPanel(
                                         title = tags$span(icon("crown"), " 1. Dominant"),
                                         tags$br(),
                                         plotOutput("plot_dominant", height = "360px"),
                                         tags$br(),
                                         DT::DTOutput("table_dominant")
                                       ),
                                       
                                       # Tab 2: Component Profile
                                       tabPanel(
                                         title = tags$span(icon("th-large"), " 2. Profile"),
                                         tags$br(),
                                         plotOutput("plot_heatmap", height = "300px"),
                                         tags$br(),
                                         plotOutput("plot_radar",   height = "400px")
                                       ),
                                       
                                       # Tab 3: Moran's I + LISA
                                       tabPanel(
                                         title = tags$span(icon("globe-asia"), " 3. Moran+LISA"),
                                         tags$br(),
                                         verbatimTextOutput("moran_global"),
                                         tags$br(),
                                         DT::DTOutput("table_lisa"),
                                         tags$br(),
                                         leaflet::leafletOutput("map_lisa", height = "460px")
                                       ),
                                       
                                       # Tab 4: Sensitivity Analysis
                                       tabPanel(
                                         title = tags$span(icon("adjust"), " 4. Sensitivity"),
                                         tags$br(),
                                         verbatimTextOutput("sensitivity_result"),
                                         tags$br(),
                                         plotOutput("plot_sensitivity", height = "360px")
                                       ),
                                       
                                       # Tab 5: Cutter Comparison
                                       tabPanel(
                                         title = tags$span(icon("exchange-alt"), " 5. Cutter"),
                                         tags$br(),
                                         verbatimTextOutput("cutter_result"),
                                         tags$br(),
                                         plotOutput("plot_cutter", height = "360px"),
                                         tags$br(),
                                         DT::DTOutput("table_cutter_diff")
                                       )
                                       
                                     ) # end tabsetPanel
                                   )   # end box
                            )     # end column kanan
                            
                          ) # end fluidRow
  )   # end tabItem
}