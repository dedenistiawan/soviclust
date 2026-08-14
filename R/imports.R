# =============================================================================
# R/imports.R
# Deklarasi import semua package yang digunakan oleh aplikasi Shiny di inst/app/
#
# Menggunakan @importFrom spesifik untuk menghindari konflik nama fungsi.
# =============================================================================

# Package tanpa konflik — import semua
#' @import shinydashboard
#' @import shinyWidgets
#' @import leaflet
#' @import ClustGeo
#' @import spdep
#' @import cluster
#' @import RColorBrewer
#' @import patchwork
#' @import rdist
#' @import stabledist
#' @import Rtsne
#' @import uwot
#' @import tmap
#' @import classInt
#' @import readxl
#' @import tidyr

# shiny — import semua (fondasi utama)
#' @import shiny

# shinyjs — hindari 'alert' (konflik dg shinyWidgets) dan 'runExample' (konflik dg shiny)
#' @importFrom shinyjs useShinyjs js runjs hide show toggle enable disable
#'   hidden disabled extendShinyjs addClass removeClass html
#'   click delay reset

# DT — hindari 'dataTableOutput' dan 'renderDataTable' (alias duplikat dari shiny)
#' @importFrom DT datatable DTOutput renderDT formatStyle formatRound

# ggplot2 — hindari '%+%' dan 'alpha' (konflik dg psych)
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_col
#'   geom_histogram scale_fill_manual scale_color_manual labs theme_minimal
#'   theme element_text element_blank facet_wrap coord_flip geom_boxplot
#'   geom_tile scale_fill_gradient2 geom_hline geom_vline geom_text
#'   scale_x_discrete scale_y_continuous theme_bw geom_density
#'   geom_errorbar position_dodge scale_fill_brewer vars

# dplyr — hindari 'select' (konflik dg MASS)
#' @importFrom dplyr filter mutate arrange group_by summarise left_join
#'   rename bind_rows bind_cols n distinct pull slice ungroup
#'   across everything contains starts_with

# psych — fungsi utama saja (hindari '%+%' dan 'alpha' yg konflik dg ggplot2)
#' @importFrom psych KMO cortest.bartlett principal fa polychoric

# MASS — hindari 'select' (konflik dg dplyr) dan 'area' (konflik dg patchwork)
#' @importFrom MASS lda stepAIC mvrnorm ginv

# fmsb — fungsi utama (hindari 'geary.test' yg konflik dg spdep)
#' @importFrom fmsb radarchart

# sf — fungsi utama
#' @importFrom sf st_read st_write st_crs st_transform st_make_valid
#'   st_sf st_as_sf st_geometry sf_use_s2 st_bbox st_drop_geometry
NULL
