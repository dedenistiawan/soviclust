# =============================================================================
# global.R — SoVI Shiny Application (Refactored)
# Hanya berisi: library, options, dan source semua modul
#
# STRUKTUR FOLDER:
#   R/core/           — konstanta, helper, pipeline SoVI, cluster, analisis
#   R/shared/         — engine algoritma FGWC (dipakai FGWC & LFGWC)
#   R/cluster_geo/    — ClustGeo UI & Server
#   R/FGWC/           — FGWC wrapper, UI & Server
#   R/LFGWC/          — LFGWC wrapper, UI & Server
#   R/sovi_analysis/  — SoVI Analysis UI, Server & Utils
#   R/var_config/     — Variable Config UI & Server
#   R/method_comparison/ — Method Comparison UI & Server
#   R/sovi_computation/  — SoVI Computation UI & Server
#   R/extended_analysis/ — Extended Analysis UI & Server
#   R/downloads/      — Downloads UI & Server
# =============================================================================

options(shiny.maxRequestSize = 200 * 1024^2)

# =============================================================================
# LIBRARIES
# =============================================================================
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(psych)
library(classInt)
library(sf)
library(tmap)
library(leaflet)
library(ClustGeo)
library(spdep)
library(cluster)
library(fmsb)
library(RColorBrewer)
library(patchwork)
library(rdist)
library(stabledist)
library(MASS)      # Sammon Mapping
library(Rtsne)     # t-SNE
library(uwot)      # UMAP
library(shiny.i18n)
library(digest)    # Cache key hashing untuk PCA caching
library(future)
library(promises)

tmap::tmap_mode("view")

# =============================================================================
# INTERNASIONALISASI (i18n)
# Tersedia global untuk semua modul UI dan server
# =============================================================================
i18n <- shiny.i18n::Translator$new(
  translation_json_path = "i18n/translation.json"
)
i18n$set_translation_language("id")  # default: Bahasa Indonesia


# =============================================================================
# TAHAP 1: CORE — Fondasi (harus di-source pertama)
# Konstanta, helpers, dan semua fungsi pipeline SoVI
# =============================================================================
source("R/core/helpers.R")          # normalize_id, normalize_01, VULN_PAL, read_shapefile
source("R/core/sovi_core.R")        # standardize_data, run_pca, run_sovi_core, dll
source("R/core/cluster_core.R")     # build_feature_matrix, run_clustgeo_advanced, dll
source("R/core/analysis_core.R")    # run_moran_lisa, run_sensitivity, run_3way_comparison, dll
source("R/core/map_core.R")         # build_leaflet_sovi, build_leaflet_lisa, dll

# =============================================================================
# TAHAP 2: SHARED — Engine Algoritma (dipakai FGWC & LFGWC)
# File dipindah dari R/FGWC_OPT/Function/ ke R/shared/Function/
# =============================================================================
local({
  func_dir <- file.path("R", "shared", "Function")
  src      <- function(f) source(file.path(func_dir, f), local = FALSE)
  
  # Core engine
  src("fgwc.R")           # fgwcuv, vi, uij, renew_uij, gen_uij, gen_vi, dll
  src("index.R")          # index_fgwc, PC1, CE1, SC1, SI1, XB1, IFV1, Kwon1
  src("ei.R")             # eiDist, logchaotic, kentchaotic, update_alpha
  
  # Wrapper utama
  src("mainfunction.R")   # fgwc(), get_param_abc/fpa/gsa/hho/ifa/pso/tlbo/woa
  
  # Algoritma optimasi
  src("abcfgwc.R")        # Artificial Bee Colony
  src("fpafgwc.R")        # Flower Pollination Algorithm
  src("gsafgwc.R")        # Gravitational Search Algorithm
  src("gwofgwc.R")        # Grey Wolf Optimizer
  src("hhofgwc.R")        # Harris-Hawk Optimization
  src("ifafgwc.R")        # Intelligent Firefly Algorithm
  src("psofgwc.R")        # Particle Swarm Optimization
  src("tlbofgwc.R")       # Teaching-Learning Based Optimization
  src("woafgwc.R")        # Whale Optimization Algorithm
})

# =============================================================================
# TAHAP 3: MODUL CLUSTER ANALYSIS
# ClustGeo, FGWC, LFGWC — masing-masing 3 file (wrapper/ui/server)
# =============================================================================

# ── ClustGeo ──────────────────────────────────────────────────────────────────
source("R/cluster_geo/clustgeo_ui.R")     # clustgeo_tab_ui()
source("R/cluster_geo/clustgeo_server.R") # clustgeo_server()

# ── FGWC ──────────────────────────────────────────────────────────────────────
source("R/FGWC/fgwc_wrapper.R")    # run_fgwc_shiny, build_leaflet_fgwc,
                                   # build_fgwc_feature_matrix, dsb.
source("R/FGWC/fgwc_ui.R")         # fgwc_tab_ui()
source("R/FGWC/fgwc_server.R")     # fgwc_server()

# ── LFGWC ─────────────────────────────────────────────────────────────────────
source("R/LFGWC/lfgwc_wrapper.R")  # run_lfgwc_shiny, build_lfgwc_weights, dll.
source("R/LFGWC/lfgwc_ui.R")       # lfgwc_tab_ui()
source("R/LFGWC/lfgwc_server.R")   # lfgwc_server()

# ── ALFGWC ────────────────────────────────────────────────────────────────────
source("R/ALFGWC/alfgwc_wrapper.R")
source("R/ALFGWC/alfgwc_ui.R")
source("R/ALFGWC/alfgwc_server.R")

# ── K-Means ───────────────────────────────────────────────────────────────────
source("R/kmeans/kmeans_ui.R")     # kmeans_tab_ui()
source("R/kmeans/kmeans_server.R") # kmeans_server()

# ── DBSCAN ────────────────────────────────────────────────────────────────────
source("R/dbscan/dbscan_ui.R")     # dbscan_tab_ui()
source("R/dbscan/dbscan_server.R") # dbscan_server()

# =============================================================================
# TAHAP 4: MODUL SoVI ANALYSIS
# =============================================================================
source("R/sovi_analysis/sovi_analysis_utils.R")  # compute_gvf, classify_variable_jenks, dll
source("R/sovi_analysis/sovi_analysis_ui.R")     # sovi_analysis_tab_ui()
source("R/sovi_analysis/sovi_analysis_server.R") # sovi_analysis_server()

# =============================================================================
# TAHAP 5: MODUL PIPELINE SoVI
# Urutan source mengikuti alur workflow user
# =============================================================================

# ── Variable Configuration ────────────────────────────────────────────────────
source("R/var_config/var_config_ui.R")       # var_config_tab_ui()
source("R/var_config/var_config_server.R")   # var_config_server()

# ── Method Comparison (Opsional) ──────────────────────────────────────────────
source("R/method_comparison/method_comparison_ui.R")     # method_comparison_tab_ui()
source("R/method_comparison/method_comparison_server.R") # method_comparison_server()

# ── SoVI Computation ──────────────────────────────────────────────────────────
source("R/sovi_computation/sovi_computation_ui.R")     # sovi_computation_tab_ui()
source("R/sovi_computation/sovi_computation_server.R") # sovi_computation_server()

# ── Extended Analysis ─────────────────────────────────────────────────────────
source("R/extended_analysis/extended_analysis_ui.R")     # extended_analysis_tab_ui()
source("R/extended_analysis/extended_analysis_server.R") # extended_analysis_server()

# ── Downloads ─────────────────────────────────────────────────────────────────
source("R/downloads/downloads_ui.R")     # downloads_tab_ui()
source("R/downloads/downloads_server.R") # downloads_server()