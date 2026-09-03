# =============================================================================
# R/cluster_geo/clustgeo_ui.R
# UI Tab ClustGeo Advanced — Submenu Cluster Analysis
#
# DIPANGGIL DARI: ui.R via clustgeo_tab_ui()
# BERISI       : definisi tabItem("tab_clustgeo_adv", ...)
# =============================================================================

clustgeo_tab_ui <- function() {
  
  shinydashboard::tabItem("tab_clustgeo_adv",
                          
                          fluidRow(
                            
                            # ════════════════════════════════════════════════════════════════════════
                            # PANEL KIRI — Konfigurasi Parameter
                            # ════════════════════════════════════════════════════════════════════════
                            column(3,
                                   shinydashboard::box(
                                     title       = tags$span(icon("sliders-h"), " ClustGeo Configuration"),
                                     status      = "primary",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     # ── 1. Data Source ────────────────────────────────────────────────
                                     div(class = "step-header", "1. Data Source"),
                                     radioButtons("cga_data_source", NULL,
                                                  choices = c(
                                                    "Original Data (no transformation)" = "raw",
                                                    "Normalized Data (0\u20131)"          = "raw_norm",
                                                    "Standardized Data (Z-score)"       = "standardized",
                                                    "SoVI Score"                        = "sovi",
                                                    "RC Scores (PCA components)"        = "rc"
                                                  ),
                                                  selected = "rc"
                                     ),
                                     
                                     # Variable selection (appears if raw/raw_norm/standardized)
                                     conditionalPanel(
                                       "input.cga_data_source == 'raw' ||
             input.cga_data_source == 'raw_norm' ||
             input.cga_data_source == 'standardized'",
                                       div(class = "step-header", "Select Variables"),
                                       uiOutput("cga_var_selector")
                                     ),
                                     
                                     # Data source info (sovi/rc)
                                     conditionalPanel(
                                       "input.cga_data_source == 'sovi' ||
             input.cga_data_source == 'rc'",
                                       uiOutput("cga_datasource_info")
                                     ),
                                     
                                     tags$hr(),
                                     
                                     # ── 2. Number of Clusters (k) ─────────────────────────────────────────
                                     div(class = "step-header", "2. Number of Clusters (k)"),
                                     radioButtons("cga_k_mode", NULL,
                                                  choices  = c("Manual" = "manual",
                                                               "Automatic (Silhouette)" = "auto"),
                                                  selected = "manual",
                                                  inline   = TRUE
                                     ),
                                     
                                     conditionalPanel(
                                       "input.cga_k_mode == 'manual'",
                                       sliderInput("cga_k", "Number of Clusters (k)",
                                                   min = 2, max = 10, value = 4, step = 1)
                                     ),
                                     
                                     conditionalPanel(
                                       "input.cga_k_mode == 'auto'",
                                       sliderInput("cga_k_max", "Maximum k for Search",
                                                   min = 3, max = 10, value = 8, step = 1),
                                       div(class = "progress-box",
                                           style = "background:#e3f2fd; border-left-color:#1a73c1;
                         font-size:12px;",
                                           icon("info-circle"),
                                           " Optimal k is selected based on the highest mean silhouette value.")
                                     ),
                                     
                                     tags$hr(),
                                     
                                     # ── 3. Alpha (\u03b1) \u2014 Spatial Weight ──────────────────────────────────────
                                     div(class = "step-header", "3. Alpha (\u03b1) \u2014 Spatial Weight"),
                                     radioButtons("cga_alpha_mode", NULL,
                                                  choices  = c("Manual" = "manual",
                                                               "Automatic (chooseAlpha)" = "auto"),
                                                  selected = "manual",
                                                  inline   = TRUE
                                     ),
                                     
                                     conditionalPanel(
                                       "input.cga_alpha_mode == 'manual'",
                                       sliderInput("cga_alpha", "Alpha Value (\u03b1)",
                                                   min = 0.0, max = 1.0, value = 0.2, step = 0.05),
                                       div(class = "progress-box",
                                           style = "background:#f8f9fa; border-left-color:#adb5bd;
                         font-size:12px;",
                                           "\u03b1 = 0 \u2192 attribute only | \u03b1 = 1 \u2192 spatial only")
                                     ),
                                     
                                     conditionalPanel(
                                       "input.cga_alpha_mode == 'auto'",
                                       div(class = "progress-box",
                                           style = "background:#e3f2fd; border-left-color:#1a73c1;
                         font-size:12px;",
                                           icon("info-circle"),
                                           " Optimal \u03b1 is selected at the best trade-off point
                  between attribute (Q1) and spatial (Q2) homogeneity.")
                                     ),
                                     
                                     tags$hr(),
                                     
                                     # ── Run Button ────────────────────────────────────────────────────
                                     actionButton("run_clustgeo_adv",
                                                  tags$span(icon("play"), " Run ClustGeo"),
                                                  class = "btn-primary btn-lg btn-block"),
                                     tags$br(), tags$br(),
                                     uiOutput("cga_progress"),
                                     
                                     tags$hr(),
                                     
                                     # ── Download ──────────────────────────────────────────────────────
                                     div(class = "step-header", "Download Results"),
                                     downloadButton("dl_cga_csv",
                                                    tags$span(icon("download"), " Cluster Results (.csv)"),
                                                    class = "btn-info btn-block"),
                                     tags$br(),
                                     downloadButton("dl_cga_map_png",
                                                    tags$span(icon("map"), " Cluster Map (.png)"),
                                                    class = "btn-success btn-block")
                                   )
                            ),
                            
                            # ════════════════════════════════════════════════════════════════════════
                            # RIGHT PANEL — Output Tabs
                            # ════════════════════════════════════════════════════════════════════════
                            column(9,
                                   shinydashboard::box(
                                     title       = tags$span(icon("globe-asia"), " ClustGeo Results"),
                                     status      = "info",
                                     solidHeader = TRUE,
                                     width       = 12,
                                     
                                     tabsetPanel(
                                       
                                       # ── Tab 1: Parameter Summary ──────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("info-circle"), " Parameter"),
                                         tags$br(),
                                         uiOutput("cga_summary_params")
                                       ),
                                       
                                       # ── Tab 2: Interactive Map ──────────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("map"), " Interactive Map"),
                                         tags$br(),
                                         leaflet::leafletOutput("cga_map", height = "520px")
                                       ),
                                       
                                       # ── Tab 3: Silhouette ───────────────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("chart-line"), " Silhouette"),
                                         tags$br(),
                                         fluidRow(
                                           column(6,
                                                  div(class = "step-header", "Silhouette Plot per Cluster"),
                                                  plotOutput("cga_plot_silhouette", height = "320px")
                                           ),
                                           column(6,
                                                  div(class = "step-header", "Avg. Silhouette Width Table"),
                                                  DT::DTOutput("cga_table_silhouette"),
                                                  tags$br(),
                                                  uiOutput("cga_silhouette_interp")
                                           )
                                         ),
                                         # Optimal k plot — only shown in auto mode
                                         conditionalPanel(
                                           "input.cga_k_mode == 'auto'",
                                           tags$hr(),
                                           div(class = "step-header", "Optimal k Search"),
                                           plotOutput("cga_plot_kopt", height = "260px")
                                         )
                                       ),
                                       
                                       # ── Tab 4: Alpha Trade-off ──────────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("balance-scale"), " Alpha Trade-off"),
                                         tags$br(),
                                         uiOutput("cga_alpha_info"),
                                         tags$br(),
                                         plotOutput("cga_plot_alpha", height = "320px")
                                       ),
                                       
                                       # ── Tab 5: Profil Cluster ───────────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("th"), " Cluster Profile"),
                                         tags$br(),
                                         div(class = "step-header", "Tabel Profil (Mean per Cluster)"),
                                         DT::DTOutput("cga_table_profile"),
                                         tags$hr(),
                                         div(class = "step-header", "Heatmap Profil"),
                                         tags$p(style = "font-size:12px; color:#78909c;",
                                                "Warna merah = nilai tinggi, biru = nilai rendah."),
                                         plotOutput("cga_plot_heatmap", height = "340px"),
                                         tags$hr(),
                                         div(class = "step-header", "Radar Chart per Cluster"),
                                         tags$p(style = "font-size:12px; color:#78909c;",
                                                "Values normalized 0\u20131. Area size = cluster profile intensity."),
                                         plotOutput("cga_plot_radar", height = "420px")
                                       ),
                                       
                                       # ── Tab 6: Data Cluster ─────────────────────────────────────────
                                       tabPanel(
                                         title = tags$span(icon("list-ol"), " Cluster Data"),
                                         tags$br(),
                                         DT::DTOutput("cga_table_result")
                                       )
                                       
                                     ) # end tabsetPanel
                                   )   # end box
                            )     # end column kanan
                          )       # end fluidRow
  )         # end tabItem
}