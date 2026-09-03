# =============================================================================
# R/downloads/downloads_ui.R
# UI Tab Downloads
#
# DIPANGGIL DARI: ui.R via downloads_tab_ui()
# =============================================================================

downloads_tab_ui <- function() {

  shinydashboard::tabItem("tab_download",

    # ── Panel Laporan Otomatis ─────────────────────────────────────────────────
    fluidRow(column(12,
      shinydashboard::box(
        title       = tags$span(icon("file-alt"), " Automatic Report"),
        status      = "primary",
        solidHeader = TRUE,
        width       = 12,
        collapsible = FALSE,

        fluidRow(
          column(4,
            radioButtons("report_format", NULL,
                        choices  = c("HTML (Recommended)" = "html",
                                     "PDF (Requires LaTeX)" = "pdf",
                                     "Word"               = "docx"),
                        inline   = TRUE,
                        selected = "html")
          ),
          column(4,
            textInput("report_title",
                      "Report Title:",
                      value = "Social Vulnerability Index (SoVI) Report",
                      placeholder = "Report title...")
          ),
          column(3,
            textInput("report_institution",
                      "Institution / Author:",
                      value = "soviclust",
                      placeholder = "Institution name...")
          ),
          column(1,
            div(style = "margin-top: 25px;",
              downloadButton("dl_report",
                             tags$span(icon("file-download"), " Generate"),
                             class = "btn-primary btn-block")
            )
          )
        ),

        div(class = "info-card",
            style = "margin-top:8px; padding:10px 14px; background:#e3f2fd; border-left: 5px solid #1a73c1;",
            icon("info-circle"), tags$strong(" Report Contents:"),
            tags$span(style = "font-size:13px; color:#546e7a;",
                      " Data summary \u2022 PCA diagnostics \u2022 SoVI distribution \u2022 ",
                      "Top/bottom 10 regions \u2022 Moran's I \u2022 ",
                      "Clustering results \u2022 Automatic interpretation"),
            tags$br(),
            tags$small(style = "color:#78909c;",
                       icon("exclamation-circle"),
                       " Make sure SoVI Computation & Cluster Analysis have been run first.")
        )
      )
    )),

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
                              tags$span(icon("download"), " Variable Assignment (.csv)"),
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
                              tags$span(icon("map"), " SoVI Map (.png)"),
                              class = "btn-success btn-block"), tags$br(),
               downloadButton("dl_map_cluster",
                              tags$span(icon("map"), " Cluster Map (.png)"),
                              class = "btn-success btn-block"), tags$br(),
               downloadButton("dl_map_lisa",
                              tags$span(icon("map"), " LISA Map (.png)"),
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