# =============================================================================
# ui.R — SoVI Shiny Application (Refactored)
#
# File ini hanya berisi:
#   1. Sidebar menu
#   2. Tab-tab statis (Home, Info, Data Upload) yang tidak dipisah
#   3. Pemanggilan fungsi UI dari setiap modul
#
# Tab yang SUDAH dipisah ke modul masing-masing:
#   var_config_tab_ui()             — R/var_config/var_config_ui.R
#   method_comparison_tab_ui()      — R/method_comparison/method_comparison_ui.R
#   sovi_computation_tab_ui()       — R/sovi_computation/sovi_computation_ui.R
#   extended_analysis_tab_ui()      — R/extended_analysis/extended_analysis_ui.R
#   clustgeo_tab_ui()               — R/cluster_geo/clustgeo_ui.R
#   fgwc_tab_ui()                   — R/FGWC/fgwc_ui.R
#   lfgwc_tab_ui()                  — R/LFGWC/lfgwc_ui.R
#   sovi_analysis_tab_ui()          — R/sovi_analysis/sovi_analysis_ui.R
#   downloads_tab_ui()              — R/downloads/downloads_ui.R
# =============================================================================

# Tab Home, Info, Data — isi kontennya sama dengan ui.R lama
# Dipindah ke fungsi lokal agar ui.R lebih terstruktur

home_tab_ui         <- function() shinydashboard::tabItem("tab_home",
                                                          fluidRow(column(12, div(class = "home-hero",
                                                                                  fluidRow(
                                                                                    column(9,
                                                                                           tags$h2(icon("shield-alt"), " Social Vulnerability Index (SoVI)"),
                                                                                           tags$p(tags$strong("SoVI Interactive Mapper:"),
                                                                                                  "Practical platform for analyzing social vulnerability and disaster risk spatially.")
                                                                                    ),
                                                                                    column(3, div(style = "text-align:right; opacity:0.20; font-size:80px;",
                                                                                                  icon("map-marked-alt")))
                                                                                  )
                                                          ))),
                                                          fluidRow(column(12,
                                                                          tags$h4(style = "color:#1a73c1; font-weight:700; margin:0 0 14px 4px;",
                                                                                  icon("star"), " Main Features")
                                                          )),
                                                          fluidRow(lapply(list(
                                                            list("upload",       "Upload Data",       ".xlsx/.csv dataset and area shapefile"),
                                                            list("sliders-h",    "Variable Config",   "Select variables & set vulnerability direction (+/-)"),
                                                            list("calculator",   "Compute SoVI",      "PCA varimax, loading weights, Jenks classification"),
                                                            list("map",          "Interactive Map",   "Visualize SoVI results with Leaflet"),
                                                            list("object-group", "Cluster Analysis",  "ClustGeo, FGWC & LFGWC: flexible spatial clustering"),
                                                            list("download",     "Download Results",  "Export CSV and PNG maps")
                                                          ), function(f) {
                                                            column(2, div(class = "home-feature-card",
                                                                          div(class = "icon-wrap", icon(f[[1]])),
                                                                          tags$h4(f[[2]]), tags$p(f[[3]])
                                                            ))
                                                          })),
                                                          fluidRow(column(12, tags$br(),
                                                                          shinydashboard::box(
                                                                            title = tags$span(icon("rocket"), " Quick Start \u2014 Workflow"),
                                                                            status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                                                                            fluidRow(
                                                                              column(6,
                                                                                     div(class="workflow-step", div(class="step-number","1"),
                                                                                         div(class="step-content", tags$h5("Upload Data"),
                                                                                             tags$p("Upload .xlsx/.csv dataset and area shapefile"))),
                                                                                     div(class="workflow-step", div(class="step-number","2"),
                                                                                         div(class="step-content", tags$h5("Variable Config"),
                                                                                             tags$p("Select SoVI variables and set direction (+/-)"))),
                                                                                     div(class="workflow-step", div(class="step-number","3"),
                                                                                         div(class="step-content",
                                                                                             tags$h5("Method Comparison", tags$span(class="badge-optional","Optional")),
                                                                                             tags$p("Compare 3 methods for variable direction")))
                                                                              ),
                                                                              column(6,
                                                                                     div(class="workflow-step", div(class="step-number","4"),
                                                                                         div(class="step-content", tags$h5("SoVI Computation"),
                                                                                             tags$p("Compute SoVI score, view map & PCA diagnostics"))),
                                                                                     div(class="workflow-step", div(class="step-number","5"),
                                                                                         div(class="step-content",
                                                                                             tags$h5("Cluster Analysis", tags$span(class="badge-optional","New")),
                                                                                             tags$p("ClustGeo, FGWC & LFGWC with flexible parameters"))),
                                                                                     div(class="workflow-step", div(class="step-number","6"),
                                                                                         div(class="step-content",
                                                                                             tags$h5("SoVI Analysis", tags$span(class="badge-optional","New")),
                                                                                             tags$p("SoVI Map Visualization per Variable"))),
                                                                                     div(class="workflow-step", div(class="step-number","7"),
                                                                                         div(class="step-content", tags$h5("Downloads"),
                                                                                             tags$p("Download CSV and PNG for publication")))
                                                                              )
                                                                            ),
                                                                            fluidRow(column(12, div(style="text-align:center; margin-top:12px;",
                                                                                                    actionButton("goto_upload",
                                                                                                                 tags$span(icon("arrow-right"), " Start: Upload Data"),
                                                                                                                 class = "btn-primary btn-lg")
                                                                            )))
                                                                          )
                                                          ))
)

upload_tab_ui <- function() shinydashboard::tabItem("tab_upload",

  # ── Banner: Pilihan Data ──────────────────────────────────────────────────
  fluidRow(column(12,
    div(class = "sample-data-banner",
      fluidRow(
        column(8,
          tags$h4(icon("database"), " Use Built-in Sample Data",
                  style = "margin:0 0 6px 0; color:#1a73c1; font-weight:700;"),
          tags$p(
            style = "margin:0; color:#546e7a; font-size:13px;",
            icon("info-circle"), " Dataset: ",
            tags$strong("514 Indonesian Regencies/Cities"),
            " (15 SoVI variables, year 2015) + Administrative shapefile.",
            tags$br(),
            "Suitable for exploration and demo before using your own data."
          )
        ),
        column(4, div(style = "text-align:right; padding-top:4px;",
          actionButton("load_sample",
            tags$span(icon("play-circle"), " Load Sample Data"),
            class = "btn-primary btn-lg"
          ),
          tags$br(), tags$br(),
          actionButton("use_own_data",
            tags$span(icon("upload"), " Upload Your Own Data"),
            class = "btn-default"
          )
        ))
      )
    )
  )),

  # ── Panel Upload (tersembunyi saat sample data aktif) ─────────────────────
  shinyjs::hidden(
    div(id = "panel_upload",
      fluidRow(
        column(6, shinydashboard::box(
          title = tags$span(icon("file-upload"), " Upload Dataset"),
          status = "primary", solidHeader = TRUE, width = 12,
          div(class = "upload-zone",
              icon("file-excel"),
              tags$p(style="color:#1a73c1;font-weight:600;margin:0;",
                     "Click to upload or drag & drop"),
              tags$p(style="color:#78909c;font-size:12px;margin:4px 0 0;",
                     "Format: .xlsx or .csv | Max 200 MB")),
          fileInput("file_data", NULL, accept = c(".xlsx",".csv"),
                    placeholder = "No file selected yet"),
          div(class = "step-header", "Column Configuration"),
          fluidRow(
            column(6, selectInput("id_col",   "Region ID Column",   choices = NULL)),
            column(6, selectInput("name_col", "Region Name Column", choices = NULL))
          ),
          div(class = "step-header", "Dataset Status"),
          verbatimTextOutput("data_status")
        )),
        column(6, shinydashboard::box(
          title = tags$span(icon("map"), " Upload Shapefile"),
          status = "primary", solidHeader = TRUE, width = 12,
          div(class = "upload-zone",
              icon("map-marked-alt"),
              tags$p(style="color:#1a73c1;font-weight:600;margin:0;",
                     "Select all shapefile files at once"),
              tags$p(style="color:#78909c;font-size:12px;margin:4px 0 0;",
                     ".shp + .dbf + .shx + .prj | Max 200 MB")),
          fileInput("file_shp", NULL, multiple = TRUE,
                    accept = c(".shp",".dbf",".shx",".prj",".cpg"),
                    placeholder = "No file selected yet"),
          div(class = "step-header", "Join Configuration"),
          selectInput("join_shp", "ID Column in Shapefile", choices = NULL),
          div(class = "step-header", "Shapefile Status"),
          verbatimTextOutput("shp_status")
        ))
      )
    ) # end hidden panel_upload
  ),

  # ── Status & Preview (selalu tampil) ─────────────────────────────────────
  fluidRow(column(12,
    div(id = "panel_status",
      shinydashboard::box(
        title = tags$span(icon("info-circle"), " Data Status"),
        status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
        fluidRow(
          column(6,
            div(class = "step-header", "Dataset"),
            verbatimTextOutput("data_status")
          ),
          column(6,
            div(class = "step-header", "Shapefile"),
            verbatimTextOutput("shp_status")
          )
        )
      )
    )
  )),

  fluidRow(column(12, shinydashboard::box(
    title = tags$span(icon("table"), " Dataset Preview"),
    status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
    DT::DTOutput("preview_data")
  ))),

  fluidRow(column(12, div(style="text-align:right; margin-bottom:20px;",
    actionButton("confirm_upload",
                 tags$span(icon("check-circle"), " Confirm & Continue \u2192"),
                 class = "btn-success btn-lg")
  )))
)

# =============================================================================
# UI UTAMA
# =============================================================================

ui <- shinydashboard::dashboardPage(
  title = "SoVI Analysis App",
  skin  = "blue",

  # ── Header ──────────────────────────────────────────────────────────────────
  shinydashboard::dashboardHeader(
    title = tags$span(
      icon("map-marked-alt", style = "color:#90caf9;"),
      tags$span(" Vulnerability Mapping", style = "font-weight:700;")
    ),
    titleWidth = 260
  ),
  
  # ── Sidebar ─────────────────────────────────────────────────────────────────
  shinydashboard::dashboardSidebar(
    width = 260,
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      
      # ── Navigasi Utama ──────────────────────────────────────────────────────
      shinydashboard::menuItem("Home", tabName = "tab_home",
                               icon = icon("home")),

      shinydashboard::menuItem("Info", icon = icon("info-circle"),
                               startExpanded = FALSE,
                               shinydashboard::menuSubItem("SoVI Workflow",   tabName = "tab_workflow",
                                                           icon = icon("project-diagram")),
                               shinydashboard::menuSubItem("SoVI Method",     tabName = "tab_sovimethod",
                                                           icon = icon("flask")),
                               shinydashboard::menuSubItem("Data Format", tabName = "tab_files",
                                                           icon = icon("file-alt")),
                               shinydashboard::menuSubItem("Development Team", tabName = "tab_team",
                                                           icon = icon("users"))
      ),

      shinydashboard::menuItem("Data", icon = icon("database"),
                               startExpanded = FALSE,
                               shinydashboard::menuSubItem("Upload Data", tabName = "tab_upload",
                                                           icon = icon("upload")),
                               shinydashboard::menuSubItem("Dataset Information", tabName = "tab_datainfo",
                                                           icon = icon("table"))
      ),

      tags$li(class = "divider-item"),

      # ── Pipeline SoVI ───────────────────────────────────────────────────────
      shinydashboard::menuItem("Variable Config",
                               tabName = "tab_varconfig",
                               icon    = icon("sliders-h")),

      shinydashboard::menuItem("Method Comparison",
                               tabName = "tab_comparison",
                               icon    = icon("balance-scale")),

      shinydashboard::menuItem("SoVI Computation",
                               tabName = "tab_sovi",
                               icon    = icon("calculator")),

      shinydashboard::menuItem("Extended Analysis",
                               tabName = "tab_analysis",
                               icon    = icon("chart-bar")),

      # ── Cluster Analysis ────────────────────────────────────────────────────
      shinydashboard::menuItem("Cluster Analysis",
                               icon = icon("object-group"), startExpanded = FALSE,
                               shinydashboard::menuSubItem("ClustGeo",  tabName = "tab_clustgeo_adv",
                                                           icon = icon("globe-asia")),
                               shinydashboard::menuSubItem("FGWC",      tabName = "tab_fgwc",
                                                           icon = icon("hubspot")),
                               shinydashboard::menuSubItem("LFGWC",     tabName = "tab_lfgwc",
                                                           icon = icon("map-pin")),
                               shinydashboard::menuSubItem("ALFGWC",    tabName = "tab_alfgwc",
                                                           icon = icon("map-marked-alt")),
                               shinydashboard::menuSubItem("K-Means",   tabName = "tab_kmeans",
                                                           icon = icon("circle-notch")),
                               shinydashboard::menuSubItem("DBSCAN",    tabName = "tab_dbscan",
                                                           icon = icon("project-diagram"))
      ),

      shinydashboard::menuItem("SoVI Analysis",
                               tabName = "tab_sovi_analysis",
                               icon    = icon("chart-area")),

      shinydashboard::menuItem("Download Results",
                               tabName = "tab_download",
                               icon    = icon("download")),
      
      # ── Footer Sidebar ──────────────────────────────────────────────────────
      tags$li(class = "divider-item"),
      tags$li(tags$div(
        class = "sidebar-footer-info",
        icon("user-tie"),
        tags$a(href   = "https://dedenistiawan.netlify.app/",
               target = "_blank",
               style  = "color:rgba(255,255,255,0.65); text-decoration:none;",
               " Deden Istiawan"),
        tags$br(),
        icon("code"),           " Built with R Shiny", tags$br(),
        icon("map-marker-alt"), " Indonesia"
      ))
    )
  ),
  
  # ── Body ────────────────────────────────────────────────────────────────────
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$script(src   = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js",
                  async = NA, id = "MathJax-script")
    ),
    
    shinydashboard::tabItems(
      
      # ── Tab Statis: Home ─────────────────────────────────────────────────
      home_tab_ui(),
      
      # ── Tab Statis: Info (SoVI Workflow, Method, Files, Team) ───────────
      # Konten Info tetap sama persis dengan ui.R lama
      # Disimpan dari ui.R lama tanpa perubahan
      shinydashboard::tabItem("tab_workflow",
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(icon("project-diagram"), " SoVI Pipeline \u2014 7 Phases"),
                                status = "primary", solidHeader = TRUE, width = 12,
                                tags$p(style = "color:#37474f; font-size:13.5px;",
                                       "Pipeline follows ", tags$strong("Cutter et al. (2003)"),
                                       " with enhancements: proportional loading weights and ",
                                       "three options for variable direction determination."),
                                fluidRow(lapply(list(
                                  list("1","Load Data","upload","User uploads dataset & shapefile"),
                                  list("2","Z-score","equals","Standardization: mean=0, SD=1"),
                                  list("3","PCA","cogs","KMO, Bartlett, communality, Kaiser, Varimax"),
                                  list("4","Variable Selection","filter","Threshold |\u03c4|\u22650.5, 1 variable \u2192 1 component"),
                                  list("5","Aggregation","calculator","Proportional loading weights, normalized 0\u20131"),
                                  list("6","Jenks","tags","Natural Breaks 5 vulnerability classes"),
                                  list("7","Output","map","Leaflet map, LISA, ClustGeo, Sensitivity")
                                ), function(p) {
                                  column(width = 12, div(class = "workflow-step",
                                                         div(class = "step-number", p[[1]]),
                                                         div(class = "step-content",
                                                             tags$h5(icon(p[[3]]), " ", p[[2]]),
                                                             tags$p(p[[4]]))
                                  ))
                                }))
                              )))
      ),
      
      shinydashboard::tabItem("tab_sovimethod",
                              
                              # ── Hero header ────────────────────────────────────────────────────
                              fluidRow(column(12,
                                              div(style = "background:linear-gradient(135deg,#1a73c1,#42a5f5);
                       border-radius:10px; padding:28px 32px; margin-bottom:20px; color:#fff;",
                                                  tags$h2(style = "margin:0 0 6px; font-weight:700;",
                                                          icon("flask"), " SoVI Direction Method"),
                                                  tags$p(style = "margin:0; font-size:14.5px; opacity:.9;",
                                                         "Explanation of three methods for determining variable direction ",
                                                         "in the Social Vulnerability Index (SoVI) pipeline. Each method ",
                                                         "produces different weights in the component score aggregation.")
                                              )
                              )),
                              
                              # ── Ringkasan 3 metode ─────────────────────────────────────────────
                              fluidRow(
                                column(4, div(class="info-card", style="border-left-color:#1a73c1; min-height:130px;",
                                              tags$h4(icon("star", style="color:#1a73c1"), " Theory-Based (PM)"),
                                              tags$p(style="font-size:13px; color:#37474f;",
                                                     "Variable direction determined ", tags$strong("entirely based on theory"),
                                                     " social vulnerability. Set a priori before PCA."),
                                              tags$span(class="badge-optional",
                                                        style="background:#e3f2fd;color:#1a73c1;border-color:#90caf9;",
                                                        "Recommended")
                                )),
                                column(4, div(class="info-card", style="border-left-color:#1976d2; min-height:130px;",
                                              tags$h4(icon("chart-line", style="color:#1976d2"), " Loading Sign"),
                                              tags$p(style="font-size:13px; color:#37474f;",
                                                     "Variable direction determined ", tags$strong("from PCA loading signs"),
                                                     " empirically. Variables follow their loading sign in the component they belong to.")
                                )),
                                column(4, div(class="info-card", style="border-left-color:#42a5f5; min-height:130px;",
                                              tags$h4(icon("book", style="color:#42a5f5"), " Cutter's Method"),
                                              tags$p(style="font-size:13px; color:#37474f;",
                                                     "Component direction from ", tags$strong("dominant variable"),
                                                     " (largest absolute loading) per component — original Cutter et al. (2003) method.")
                                ))
                              ),
                              
                              # ── Method 1: Theory-Based ─────────────────────────────────────────
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(
                                  tags$span(style="background:#1a73c1;color:#fff;padding:3px 10px;
                             border-radius:4px;font-size:13px;margin-right:8px;", "01"),
                                  tags$span(style="font-weight:700; font-size:15px;", "Theory-Based (PM)")
                                ),
                                status = "primary", solidHeader = FALSE, width = 12, collapsible = TRUE,
                                fluidRow(
                                  column(6,
                                         div(class="step-header","Basic Concept"),
                                         tags$p(style="font-size:13.5px;color:#37474f;line-height:1.8;",
                                                "Based on the premise that the direction of each variable's contribution is known from theory ",
                                                "before analysis begins. Researchers assign each variable as ",
                                                tags$strong("+1"), " (increase) or ", tags$strong("-1"), " (decrease) vulnerability."),
                                         div(class="step-header","Formula"),
                                         div(style="background:#f8faff;border:1px solid #90caf9;border-radius:8px;padding:14px 18px;",
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Direction (theory):")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ d_i = \\begin{cases} +1 & \\text{increase vulnerability} \\\\ -1 & \\text{decrease vulnerability} \\end{cases} \\]")),
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Proportional loading weight:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ w_{ik} = \\frac{|\\lambda_{ik}|}{\\sum_{j \\in C_k} |\\lambda_{jk}|} \\]")),
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Component score & SoVI:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ RC_k = \\sum_{i \\in C_k} w_{ik} \\cdot d_i \\cdot z_i \\quad\\Rightarrow\\quad \\text{SoVI} = \\frac{\\sum RC_k - \\min}{\\max - \\min} \\]"))
                                         )
                                  ),
                                  column(6,
                                         div(class="info-card", style="border-left-color:#27ae60;padding:12px 16px;margin-top:30px;",
                                             tags$h4(style="font-size:13.5px;color:#27ae60;margin-bottom:6px;",
                                                     icon("check-circle"), " Advantages"),
                                             tags$ul(style="font-size:13px;margin:0;padding-left:18px;",
                                                     tags$li("Consistent with social vulnerability theory"),
                                                     tags$li("Independent of PCA mathematical artifacts"),
                                                     tags$li("Results are easier to interpret substantively"),
                                                     tags$li("Reproducible across different datasets")
                                             )
                                         )
                                  )
                                )
                              ))),
                              
                              # ── Method 2: Loading Sign ─────────────────────────────────────────
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(
                                  tags$span(style="background:#1976d2;color:#fff;padding:3px 10px;
                             border-radius:4px;font-size:13px;margin-right:8px;", "02"),
                                  tags$span(style="font-weight:700; font-size:15px;", "Loading Sign")
                                ),
                                status = "info", solidHeader = FALSE, width = 12, collapsible = TRUE,
                                fluidRow(
                                  column(6,
                                         div(class="step-header","Basic Concept"),
                                         tags$p(style="font-size:13.5px;color:#37474f;line-height:1.8;",
                                                "This method is ", tags$strong("entirely empirical"),
                                                ". Variable contribution direction is determined from PCA loading signs. ",
                                                "No a priori direction setting — data determines its structure."),
                                         div(class="step-header","Formula"),
                                         div(style="background:#f8faff;border:1px solid #90caf9;border-radius:8px;padding:14px 18px;",
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Direction from loading sign:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ d_i = \\begin{cases} +\\,\\text{sign}(\\lambda_{ik^*}) & i \\notin \\text{neg\\_vars} \\\\ -\\,\\text{sign}(\\lambda_{ik^*}) & i \\in \\text{neg\\_vars} \\end{cases} \\]")),
                                             tags$p(style="font-size:12px;color:#78909c;",
                                                    HTML("where \\( k^* = \\arg\\max_k |\\lambda_{ik}| \\) is the dominant component of variable \\( i \\)"))
                                         )
                                  ),
                                  column(6,
                                         div(class="info-card", style="border-left-color:#e74c3c;padding:12px 16px;margin-top:30px;",
                                             tags$h4(style="font-size:13.5px;color:#e74c3c;margin-bottom:6px;",
                                                     icon("exclamation-triangle"), " Caution"),
                                             tags$ul(style="font-size:13px;margin:0;padding-left:18px;",
                                                     tags$li("Negative loadings may be PCA mathematical artifacts, not theory reflections"),
                                                     tags$li("Results may vary across datasets due to correlation structure"),
                                                     tags$li("Suitable for exploration, but requires theoretical validation")
                                             )
                                         )
                                  )
                                )
                              ))),
                              
                              # ── Method 3: Cutter's Method ──────────────────────────────────────
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(
                                  tags$span(style="background:#42a5f5;color:#fff;padding:3px 10px;
                             border-radius:4px;font-size:13px;margin-right:8px;", "03"),
                                  tags$span(style="font-weight:700; font-size:15px;",
                                            "Cutter's Method",
                                            tags$span(style="font-size:12px;font-weight:400;color:#78909c;margin-left:8px;",
                                                      "Cutter et al., 2003"))
                                ),
                                status = "info", solidHeader = FALSE, width = 12, collapsible = TRUE,
                                fluidRow(
                                  column(6,
                                         div(class="step-header","Basic Concept"),
                                         tags$p(style="font-size:13.5px;color:#37474f;line-height:1.8;",
                                                "Original Cutter et al. (2003) method. Direction of each component is determined by ",
                                                tags$strong("the dominant variable"), " (largest absolute loading) in that component. ",
                                                "All variables in one component follow that component's direction."),
                                         div(class="step-header","Formula"),
                                         div(style="background:#f8faff;border:1px solid #90caf9;border-radius:8px;padding:14px 18px;",
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Dominant variable per component:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ v_k^* = \\arg\\max_{i \\in C_k}\\, |\\lambda_{ik}| \\]")),
                                             tags$p(style="color:#37474f;margin-bottom:6px;", tags$strong("Component direction from dominant variable:")),
                                             tags$p(style="text-align:center;",
                                                    HTML("\\[ D_k = \\text{sign}\\left(\\lambda_{v_k^*, k}\\right) \\quad\\Rightarrow\\quad RC_k = D_k \\cdot F_k \\]")),
                                             tags$p(style="font-size:12px;color:#78909c;",
                                                    HTML("where \\( F_k \\) is the factor score of component \\(k\\) from PCA"))
                                         )
                                  ),
                                  column(6,
                                         div(class="info-card", style="border-left-color:#42a5f5;padding:12px 16px;margin-top:30px;",
                                             tags$h4(style="font-size:13.5px;color:#42a5f5;margin-bottom:6px;",
                                                     icon("book-open"), " Original Reference"),
                                             tags$p(style="font-size:13px;margin:0;",
                                                    tags$strong("Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003)."),
                                                    " Social vulnerability to environmental hazards. ",
                                                    tags$em("Social Science Quarterly"), ", 84(2), 242\u2013261.",
                                                    tags$br(),
                                                    tags$a(href="https://doi.org/10.1111/1540-6237.8402002",
                                                           target="_blank", style="font-size:12px;",
                                                           icon("external-link-alt"), " doi.org/10.1111/1540-6237.8402002")
                                             )
                                         )
                                  )
                                )
                              ))),
                              
                              # ── Tabel Perbandingan ─────────────────────────────────────────────
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(icon("table"), " Comparison of Three Methods"),
                                status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                                tags$table(class = "format-table",
                                           tags$thead(tags$tr(
                                             tags$th("Aspect"),
                                             tags$th(icon("star"), " Theory-Based (PM)"),
                                             tags$th(icon("chart-line"), " Loading Sign"),
                                             tags$th(icon("book"), " Cutter's Method")
                                           )),
                                           tags$tbody(
                                             tags$tr(tags$td(tags$strong("Direction Source")),
                                                     tags$td("Theory (a priori)"),
                                                     tags$td("PCA loading signs"),
                                                     tags$td("Dominant variable per component")),
                                             tags$tr(tags$td(tags$strong("Direction Unit")),
                                                     tags$td("Per variable"), tags$td("Per variable"), tags$td("Per component")),
                                             tags$tr(tags$td(tags$strong("Aggregation weight")),
                                                     tags$td("Proportional |loading|"),
                                                     tags$td("Proportional |loading|"),
                                                     tags$td("Direct PCA factor scores")),
                                             tags$tr(tags$td(tags$strong("Data Dependency")),
                                                     tags$td(tags$span(style="color:#27ae60;","Low")),
                                                     tags$td(tags$span(style="color:#e74c3c;","High")),
                                                     tags$td(tags$span(style="color:#f39c12;","Moderate"))),
                                             tags$tr(tags$td(tags$strong("Reproducibility")),
                                                     tags$td(tags$span(style="color:#27ae60;","High")),
                                                     tags$td(tags$span(style="color:#f39c12;","Moderate")),
                                                     tags$td(tags$span(style="color:#27ae60;","High"))),
                                             tags$tr(tags$td(tags$strong("Recommendation")),
                                                     tags$td(tags$span(style="color:#27ae60;font-weight:700;",
                                                                       icon("check-circle"), " Primary")),
                                                     tags$td(tags$span(style="color:#f39c12;",
                                                                       icon("exclamation-circle"), " Exploratory")),
                                                     tags$td(tags$span(style="color:#1a73c1;",
                                                                       icon("book-open"), " Cutter Replication")))
                                           )
                                )
                              )))
      ),
      
      shinydashboard::tabItem("tab_files",
                              fluidRow(
                                column(6, shinydashboard::box(
                                  title = tags$span(icon("file-excel"), " Dataset Format"),
                                  status = "primary", solidHeader = TRUE, width = 12,
                                  tags$p(style="font-size:13px; color:#37474f;",
                                         "Upload a ", tags$code(".xlsx"), " or ", tags$code(".csv"),
                                         " file with minimum structure:"),
                                  tags$table(class = "format-table",
                                             tags$thead(tags$tr(tags$th("Column"), tags$th("Type"), tags$th("Description"))),
                                             tags$tbody(
                                               tags$tr(tags$td(tags$code("DISTRICTCODE")), tags$td("Chr/Num"),
                                                       tags$td("Unique ID \u2014 must match shapefile")),
                                               tags$tr(tags$td(tags$code("KABUPATEN")), tags$td("Character"),
                                                       tags$td("Region name")),
                                               tags$tr(tags$td(tags$code("VAR_1...n")), tags$td("Numeric"),
                                                       tags$td("SoVI Variables in % (0\u2013100)"))
                                             )
                                  )
                                )),
                                column(6, shinydashboard::box(
                                  title = tags$span(icon("map"), " Shapefile Format"),
                                  status = "primary", solidHeader = TRUE, width = 12,
                                  tags$p(style="font-size:13px; color:#37474f;",
                                         "Upload all files ", tags$strong("at once"), ":"),
                                  tags$table(class = "format-table",
                                             tags$thead(tags$tr(tags$th("Extension"), tags$th("Description"), tags$th("Status"))),
                                             tags$tbody(
                                               tags$tr(tags$td(tags$code(".shp")), tags$td("Area geometry"),
                                                       tags$td(tags$span(style="color:#27ae60;font-weight:bold;","\\u2713 Required"))),
                                               tags$tr(tags$td(tags$code(".dbf")), tags$td("Data attributes"),
                                                       tags$td(tags$span(style="color:#27ae60;font-weight:bold;","\\u2713 Required"))),
                                               tags$tr(tags$td(tags$code(".shx")), tags$td("Geometry index"),
                                                       tags$td(tags$span(style="color:#27ae60;font-weight:bold;","\\u2713 Required"))),
                                               tags$tr(tags$td(tags$code(".prj")), tags$td("CRS projection"), tags$td("Recommended")),
                                               tags$tr(tags$td(tags$code(".cpg")), tags$td("Character encoding"), tags$td("Optional"))
                                             )
                                  )
                                ))
                              )
      ),
      
      shinydashboard::tabItem("tab_team",
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(icon("users"), " Development Team"),
                                status = "primary", solidHeader = TRUE, width = 12,
                                fluidRow(
                                  column(3, div(class="team-card",
                                                div(class="team-avatar", icon("user-tie")),
                                                tags$h5("Deden Istiawan"), tags$p("Lead Researcher"),
                                                tags$p(icon("envelope"), " deden.istiawan@itesa.ac.id"))),
                                  column(3, div(class="team-card",
                                                div(class="team-avatar", icon("user-tie")),
                                                tags$h5("Herman Yuliansyah"), tags$p("TPM Head"),
                                                tags$p(icon("envelope"), " herman.yuliansyah@tif.uad.ac.id"))),
                                  column(3, div(class="team-card",
                                                div(class="team-avatar", icon("user-tie")),
                                                tags$h5("Rusydi Umar"), tags$p("TPM Member"),
                                                tags$p(icon("envelope"), " rusydi@mti.uad.ac.id"))),
                                  column(3, div(class="team-card",
                                                div(class="team-avatar", icon("university")),
                                                tags$h5("Itesa Muhammadiyah"), tags$p("University"),
                                                tags$p(icon("globe"), " www.itesa.ac.id")))
                                ),
                                tags$hr(),
                                div(class = "info-card",
                                    tags$h4(icon("book-open"), " Main References"),
                                    tags$p(style="font-size:13px;",
                                           tags$strong("Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003)."),
                                           tags$br(), "Social vulnerability to environmental hazards.",
                                           tags$em(" Social Science Quarterly"), ", 84(2), 242\u2013261.",
                                           tags$br(),
                                           tags$a(href="https://doi.org/10.1111/1540-6237.8402002",
                                                  target="_blank", icon("external-link-alt"),
                                                  " doi.org/10.1111/1540-6237.8402002")))
                              )))
      ),
      
      shinydashboard::tabItem("tab_datainfo",
                              fluidRow(column(12, shinydashboard::box(
                                title = tags$span(icon("table"), " Dataset Information & Requirements"),
                                status = "info", solidHeader = TRUE, width = 12,
                                fluidRow(
                                  column(6, div(class="info-card",
                                                tags$h4(icon("list-ol"), " Required Column Structure"),
                                                tags$table(class="format-table",
                                                           tags$thead(tags$tr(tags$th("No"), tags$th("Column"), tags$th("Content"))),
                                                           tags$tbody(
                                                             tags$tr(tags$td("1"), tags$td(tags$code("DISTRICTCODE")), tags$td("Unique area ID")),
                                                             tags$tr(tags$td("2"), tags$td(tags$code("KABUPATEN")),    tags$td("Region name")),
                                                             tags$tr(tags$td("3+"),tags$td(tags$code("VARIABEL_n")),   tags$td("Numeric SoVI variable (%)"))
                                                           )
                                                )
                                  )),
                                  column(6, div(class="info-card",
                                                tags$h4(icon("check-circle"), " Data Requirements"),
                                                tags$ul(style="font-size:13px;",
                                                        tags$li("Variables in ", tags$strong("percentage units (0\u2013100)")),
                                                        tags$li("No ", tags$strong("missing values")),
                                                        tags$li("At least ", tags$strong("2 variables"), " for PCA"),
                                                        tags$li("At least ", tags$strong("50 area units")),
                                                        tags$li("Area ID ", tags$strong("unique & consistent"), " with shapefile")
                                                )
                                  ))
                                )
                              )))
      ),
      
      # ── Tab Upload Data ──────────────────────────────────────────────────
      upload_tab_ui(),
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB MODULAR — Dipanggil dari modul masing-masing
      # ══════════════════════════════════════════════════════════════════════
      
      # Pipeline SoVI
      var_config_tab_ui(),
      method_comparison_tab_ui(),
      sovi_computation_tab_ui(),
      extended_analysis_tab_ui(),
      
      # Cluster Analysis
      clustgeo_tab_ui(),
      fgwc_tab_ui(),
      lfgwc_tab_ui(),
      alfgwc_tab_ui(),
      kmeans_tab_ui(),
      dbscan_tab_ui(),
      
      # Analisis Tambahan
      sovi_analysis_tab_ui(),
      
      # Downloads
      downloads_tab_ui()
      
    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage
