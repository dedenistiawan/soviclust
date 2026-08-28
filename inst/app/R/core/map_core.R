# =============================================================================
# R/core/map_core.R
# Leaflet map builder functions untuk semua menu
#
# BERISI:
#   - build_leaflet_sovi()    — peta choropleth SoVI score
#   - build_leaflet_cluster() — peta choropleth cluster (Extended Analysis)
#   - build_leaflet_lisa()    — peta choropleth LISA
#
# DEPENDENSI:
#   - R/core/helpers.R  (normalize_id, VULN_PAL, VULN_CLASSES)
#   - Package: leaflet, dplyr, RColorBrewer
# =============================================================================


# =============================================================================
# LEAFLET BUILDER — SoVI Score
# Dipakai di: SoVI Computation tab "Peta SoVI"
#             Downloads (PNG export)
# =============================================================================

#' Bangun peta Leaflet choropleth SoVI score
#'
#' @param sovi_df  data.frame hasil SoVI (harus ada sovi_score, vuln_class)
#' @param shp      Shapefile sf object
#' @param join_shp Nama kolom ID di shapefile
#' @param join_df  Nama kolom ID di sovi_df
#' @param name_col Nama kolom nama wilayah
#'
#' @return Objek leaflet map
build_leaflet_sovi <- function(sovi_df, shp, join_shp, join_df,
                               name_col = "KABUPATEN") {
  
  # ── Normalisasi ID & join ──────────────────────────────────────────────────
  sovi_df[[join_df]] <- normalize_id(sovi_df[[join_df]])
  shp[[join_shp]]    <- normalize_id(shp[[join_shp]])
  
  peta <- dplyr::left_join(shp, sovi_df,
                           by = setNames(join_df, join_shp))
  
  # Pastikan vuln_class adalah ordered factor
  peta$vuln_class <- factor(
    peta$vuln_class,
    levels  = VULN_CLASSES,
    ordered = TRUE
  )
  
  # ── Palet warna berdasarkan kelas kerentanan ──────────────────────────────
  pal <- leaflet::colorFactor(
    palette  = unname(VULN_PAL),
    levels   = VULN_CLASSES,
    na.color = "#D3D3D3"
  )
  
  # ── Popup teks ────────────────────────────────────────────────────────────
  popup_text <- paste0(
    "<b>", peta[[name_col]], "</b><br>",
    "SoVI Score : ", round(peta$sovi_score, 4), "<br>",
    "Kelas      : ", as.character(peta$vuln_class)
  )
  
  # ── Bangun peta ───────────────────────────────────────────────────────────
  leaflet::leaflet(peta) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
    leaflet::addPolygons(
      fillColor        = ~pal(vuln_class),
      fillOpacity      = 0.75,
      color            = "#555555",
      weight           = 0.5,
      popup            = popup_text,
      label            = ~get(name_col),
      highlightOptions = leaflet::highlightOptions(
        weight      = 2,
        color       = "#333333",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors   = unname(VULN_PAL),
      labels   = VULN_CLASSES,
      title    = "Kelas SoVI",
      opacity  = 0.9
    )
}


# =============================================================================
# LEAFLET BUILDER — Cluster (Extended Analysis)
# Dipakai di: Extended Analysis tab cluster (versi sederhana)
# =============================================================================

#' Bangun peta Leaflet choropleth cluster dari Extended Analysis
#'
#' @param sovi_df  data.frame hasil SoVI dengan kolom cluster
#' @param shp      Shapefile sf object
#' @param join_shp Nama kolom ID di shapefile
#' @param join_df  Nama kolom ID di sovi_df
#' @param name_col Nama kolom nama wilayah
#' @param k        Jumlah cluster
#'
#' @return Objek leaflet map
build_leaflet_cluster <- function(sovi_df, shp, join_shp, join_df,
                                  name_col = "KABUPATEN", k = 4) {
  
  # ── Normalisasi ID & join ──────────────────────────────────────────────────
  sovi_df[[join_df]] <- normalize_id(sovi_df[[join_df]])
  shp[[join_shp]]    <- normalize_id(shp[[join_shp]])
  
  peta <- dplyr::left_join(shp, sovi_df,
                           by = setNames(join_df, join_shp))
  
  # ── Palet warna cluster ───────────────────────────────────────────────────
  pal_clust <- leaflet::colorFactor(
    palette  = RColorBrewer::brewer.pal(max(k, 3), "Set2")[1:k],
    domain   = as.character(1:k),
    na.color = "#D3D3D3"
  )
  
  # ── Popup teks ────────────────────────────────────────────────────────────
  popup_text <- paste0(
    "<b>", peta[[name_col]], "</b><br>",
    "Cluster    : ", peta$cluster, "<br>",
    "SoVI Score : ", round(peta$sovi_score, 4), "<br>",
    "Kelas      : ", as.character(peta$vuln_class)
  )
  
  # ── Bangun peta ───────────────────────────────────────────────────────────
  leaflet::leaflet(peta) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
    leaflet::addPolygons(
      fillColor        = ~pal_clust(as.character(cluster)),
      fillOpacity      = 0.75,
      color            = "#555555",
      weight           = 0.5,
      popup            = popup_text,
      label            = ~get(name_col),
      highlightOptions = leaflet::highlightOptions(
        weight      = 2,
        color       = "#333333",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal      = pal_clust,
      values   = as.character(1:k),
      title    = "Cluster",
      opacity  = 0.9
    )
}


# =============================================================================
# LEAFLET BUILDER — LISA
# Dipakai di: Extended Analysis tab "Moran+LISA"
# =============================================================================

#' Bangun peta Leaflet choropleth LISA cluster
#'
#' @param peta        sf object hasil run_moran_lisa() (sudah ada kolom lisa_quad)
#' @param name_col    Nama kolom nama wilayah
#' @param lisa_colors Named vector warna per kategori LISA
#'
#' @return Objek leaflet map
build_leaflet_lisa <- function(peta,
                               name_col    = "KABUPATEN",
                               lisa_colors) {
  
  # ── Palet warna berdasarkan kategori LISA ─────────────────────────────────
  pal_lisa <- leaflet::colorFactor(
    palette  = unname(lisa_colors),
    levels   = names(lisa_colors),
    na.color = "#D3D3D3"
  )
  
  # ── Popup teks ────────────────────────────────────────────────────────────
  popup_text <- paste0(
    "<b>", peta[[name_col]], "</b><br>",
    "LISA    : ", as.character(peta$lisa_quad), "<br>",
    "Local I : ", round(peta$lisa_I, 4), "<br>",
    "p-value : ", round(peta$lisa_p, 4)
  )
  
  # ── Bangun peta ───────────────────────────────────────────────────────────
  leaflet::leaflet(peta) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
    leaflet::addPolygons(
      fillColor        = ~pal_lisa(lisa_quad),
      fillOpacity      = 0.75,
      color            = "#555555",
      weight           = 0.5,
      popup            = popup_text,
      label            = ~get(name_col),
      highlightOptions = leaflet::highlightOptions(
        weight      = 2,
        color       = "#333333",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors   = unname(lisa_colors),
      labels   = names(lisa_colors),
      title    = "LISA Cluster",
      opacity  = 0.9
    )
}

message("[map_core.R] Leaflet builder functions dimuat.")