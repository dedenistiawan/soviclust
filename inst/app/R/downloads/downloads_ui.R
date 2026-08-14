# =============================================================================
# R/downloads/downloads_ui.R
# UI Tab Downloads
#
# DIPANGGIL DARI: ui.R via downloads_tab_ui()
# =============================================================================

downloads_tab_ui <- function() {
  
  shinydashboard::tabItem("tab_download",
                          
                          fluidRow(
                            
                            # ── Kolom 1: SoVI Core Output ──────────────────────────────────────────
                            column(4,
                                   shinydashboard::box(
                                     title       = tags$span(icon("file-csv"), " SoVI Core Output"),
                                     status      = "success",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     downloadButton("dl_sovi_csv",
                                                    tags$span(icon("download"), " SoVI Result (.csv)"),
                                                    class = "btn-primary btn-block"),
                                     tags$br(),
                                     downloadButton("dl_assignment",
                                                    tags$span(icon("download"), " Assignment Variabel (.csv)"),
                                                    class = "btn-info btn-block")
                                   )
                            ),
                            
                            # ── Kolom 2: Extended Analysis ─────────────────────────────────────────
                            column(4,
                                   shinydashboard::box(
                                     title       = tags$span(icon("chart-bar"), " Extended Analysis"),
                                     status      = "success",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     downloadButton("dl_dominant",
                                                    tags$span(icon("download"), " Dominant Component (.csv)"),
                                                    class = "btn-info btn-block"), tags$br(),
                                     downloadButton("dl_profile",
                                                    tags$span(icon("download"), " Component Profile (.csv)"),
                                                    class = "btn-info btn-block"), tags$br(),
                                     downloadButton("dl_cluster",
                                                    tags$span(icon("download"), " Cluster Result (.csv)"),
                                                    class = "btn-info btn-block"), tags$br(),
                                     downloadButton("dl_lisa",
                                                    tags$span(icon("download"), " LISA Result (.csv)"),
                                                    class = "btn-info btn-block"), tags$br(),
                                     downloadButton("dl_sensitivity",
                                                    tags$span(icon("download"), " Sensitivity (.csv)"),
                                                    class = "btn-info btn-block"), tags$br(),
                                     downloadButton("dl_cutter",
                                                    tags$span(icon("download"), " Cutter Comparison (.csv)"),
                                                    class = "btn-info btn-block")
                                   )
                            ),
                            
                            # ── Kolom 3: Figures & Maps ────────────────────────────────────────────
                            column(4,
                                   shinydashboard::box(
                                     title       = tags$span(icon("image"), " Figures & Maps (PNG)"),
                                     status      = "success",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     downloadButton("dl_map_sovi",
                                                    tags$span(icon("map"), " Peta SoVI (.png)"),
                                                    class = "btn-success btn-block"), tags$br(),
                                     downloadButton("dl_map_cluster",
                                                    tags$span(icon("map"), " Peta Cluster (.png)"),
                                                    class = "btn-success btn-block"), tags$br(),
                                     downloadButton("dl_map_lisa",
                                                    tags$span(icon("map"), " Peta LISA (.png)"),
                                                    class = "btn-success btn-block"), tags$br(),
                                     downloadButton("dl_fig_dominant",
                                                    tags$span(icon("chart-bar"), " Fig Dominant (.png)"),
                                                    class = "btn-success btn-block"), tags$br(),
                                     downloadButton("dl_fig_scatter",
                                                    tags$span(icon("dot-circle"), " Fig Sensitivity (.png)"),
                                                    class = "btn-success btn-block")
                                   )
                            )
                            
                          ) # end fluidRow
  )   # end tabItem
}