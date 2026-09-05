# =============================================================================
# R/imports.R
# Namespace declarations for packages used by soviclust and its Shiny application.
#
# General strategy:
# - Import complete packages only when their functions are used extensively
#   without namespace prefixes in the application.
# - Use @importFrom for packages with common name conflicts.
# - Packages called exclusively with package::function syntax do not need
#   NAMESPACE imports, although they must remain listed in DESCRIPTION Imports.
# =============================================================================

# -----------------------------------------------------------------------------
# Core Shiny and UI packages
# -----------------------------------------------------------------------------

# shiny — main application framework
#' @import shiny

# shinydashboard — dashboard layout components
#' @import shinydashboard

# shinyWidgets — extended Shiny input widgets
#' @import shinyWidgets

# leaflet — interactive maps
#' @import leaflet

# -----------------------------------------------------------------------------
# Spatial analysis and clustering
# -----------------------------------------------------------------------------

# ClustGeo — spatially constrained hierarchical clustering
#' @import ClustGeo

# spdep — spatial weights, Moran's I, LISA, neighborhood operations
#' @import spdep

# cluster — silhouette and clustering utilities
#' @import cluster

# classInt — Jenks and other class intervals
#' @import classInt

# rdist — distance calculations
#' @import rdist

# tmap — thematic map export
#' @import tmap

# -----------------------------------------------------------------------------
# Visualization and supporting packages
# -----------------------------------------------------------------------------

# RColorBrewer — palettes
#' @import RColorBrewer

# patchwork — combining ggplot objects
#' @import patchwork

# stabledist — stable distributions used by optimization routines
#' @import stabledist

# Rtsne — t-SNE
#' @import Rtsne

# uwot — UMAP
#' @import uwot

# readxl — Excel input
#' @import readxl

# tidyr — data reshaping
#' @import tidyr

# -----------------------------------------------------------------------------
# Asynchronous execution
# -----------------------------------------------------------------------------

# future and promises are loaded by inst/app/global.R and are required for
# asynchronous/non-blocking Shiny workflows.
#' @import future
#' @import promises

# -----------------------------------------------------------------------------
# Packages imported selectively to avoid namespace conflicts
# -----------------------------------------------------------------------------

# shinyjs — avoid alert() conflict with shinyWidgets and runExample() conflict
# with shiny.
#' @importFrom shinyjs useShinyjs js runjs hide show toggle enable disable
#'   hidden disabled extendShinyjs addClass removeClass html
#'   click delay reset

# DT — avoid dataTableOutput() and renderDataTable() aliases that overlap shiny.
#' @importFrom DT datatable DTOutput renderDT formatStyle formatRound

# ggplot2 — avoid selected conflicting exports such as alpha().
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_col
#'   geom_histogram scale_fill_manual scale_color_manual labs theme_minimal
#'   theme element_text element_blank facet_wrap coord_flip geom_boxplot
#'   geom_tile scale_fill_gradient2 geom_hline geom_vline geom_text
#'   scale_x_discrete scale_y_continuous theme_bw geom_density
#'   geom_errorbar position_dodge scale_fill_brewer vars
#'   annotate scale_fill_distiller

# dplyr — avoid select(), which conflicts with MASS::select().
#' @importFrom dplyr filter mutate arrange group_by summarise left_join
#'   rename bind_rows bind_cols n distinct pull slice ungroup
#'   across everything contains starts_with

# psych — import only functions used by the SoVI/PCA workflow.
#' @importFrom psych KMO cortest.bartlett principal fa polychoric

# MASS — avoid select() and other unnecessary exports.
#' @importFrom MASS lda stepAIC mvrnorm ginv sammon

# fmsb — import radar-chart function only.
#' @importFrom fmsb radarchart

# sf — spatial data I/O and geometry operations.
#' @importFrom sf st_read st_write st_crs st_transform st_make_valid
#'   st_sf st_as_sf st_geometry sf_use_s2 st_bbox st_drop_geometry
#'   st_centroid st_coordinates st_join st_intersects st_touches

# digest — used for cache-key hashing.
#' @importFrom digest digest

# -----------------------------------------------------------------------------
# Intentionally not imported into NAMESPACE
# -----------------------------------------------------------------------------
#
# The following packages remain in DESCRIPTION Imports but are called with
# explicit namespace prefixes (package::function) in the application:
#
# - dbscan     -> dbscan::dbscan(), dbscan::kNNdist()
# - ppclust    -> ppclust::fcm()
# - shiny.i18n -> shiny.i18n::Translator$new()
#
# Keeping these calls namespaced reduces the risk of function-name conflicts.
#
# After editing this file, regenerate NAMESPACE with:
#
#   devtools::document()
#
# or:
#
#   roxygen2::roxygenise()
#
# Do not edit NAMESPACE manually.
# -----------------------------------------------------------------------------

NULL
