# =============================================================================
# R/var_config/var_config_ui.R
# UI Tab Variable Configuration
#
# DIPANGGIL DARI: ui.R via var_config_tab_ui()
# =============================================================================

var_config_tab_ui <- function() {
  
  shinydashboard::tabItem("tab_varconfig",
                          
                          fluidRow(
                            
                            # ── Kolom Kiri: Pilih Variabel ────────────────────────────────────────
                            column(4,
                                   shinydashboard::box(
                                     title       = tags$span(icon("check-square"), " Select Variables"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     div(class = "step-header", "Step 1: Select numeric variables"),
                                     checkboxGroupInput("sovi_vars", NULL, choices = NULL),
                                     tags$br(),
                                     
                                     fluidRow(
                                       column(6,
                                              actionButton("select_all_vars", "Select All",
                                                           class = "btn-sm btn-info btn-block",
                                                           icon  = icon("check-double"))
                                       ),
                                       column(6,
                                              actionButton("deselect_all_vars", "Deselect All",
                                                           class = "btn-sm btn-default btn-block",
                                                           icon  = icon("times"))
                                       )
                                     )
                                   )
                            ),
                            
                            # ── Kolom Kanan: Direction per Variabel ───────────────────────────────
                            column(8,
                                   shinydashboard::box(
                                     title       = tags$span(icon("arrows-alt-h"), " Direction per Variable"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     div(class = "step-header", "Step 2: Set direction (+/-)"),
                                     tags$p(style = "color:#546e7a; font-size:13px; margin-bottom:10px;",
                                            icon("info-circle"),
                                            tags$strong(" (+)"), " = INCREASES vulnerability  |  ",
                                            tags$strong(" (-)"), " = DECREASES vulnerability"),
                                     
                                     # Tabel direction variabel (dirender secara dinamis)
                                     div(class = "direction-scroll", uiOutput("direction_ui")),
                                     tags$br(),
                                     
                                     fluidRow(
                                       # Status konfigurasi (live update)
                                       column(6, uiOutput("varconfig_status")),
                                       
                                       # Tombol konfirmasi
                                       column(6,
                                              div(style = "text-align:right;",
                                                  actionButton("confirm_vars",
                                                               tags$span(icon("check"), " Confirm Configuration \u2192"),
                                                               class = "btn-success btn-lg")
                                              )
                                       )
                                     )
                                   )
                            )
                            
                          ) # end fluidRow
  )   # end tabItem
}