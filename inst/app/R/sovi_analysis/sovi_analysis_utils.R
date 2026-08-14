# =============================================================================
# R/sovi_analysis/sovi_analysis_utils.R
# Utility functions untuk SoVI Analysis Menu
# =============================================================================

# =============================================================================
# 1. COMPUTE GVF (Goodness of Variance Fit)
# Mengukur seberapa baik klasifikasi menangkap variasi data.
# GVF = 1 - (SDCM / SDAM)
#   SDAM = Sum of Squared Deviations from Array Mean
#   SDCM = Sum of Squared Deviations from Class Mean
# =============================================================================

compute_gvf <- function(x, k) {
  x <- x[!is.na(x)]
  if (length(x) < k) return(0)
  
  brks <- classInt::classIntervals(x, n = k, style = "jenks")$brks
  cls  <- cut(x, breaks = brks, include.lowest = TRUE, labels = FALSE)
  
  sdam <- sum((x - mean(x))^2)
  sdcm <- sum(tapply(x, cls, function(xi) sum((xi - mean(xi))^2)), na.rm = TRUE)
  
  gvf <- 1 - sdcm / sdam
  return(round(gvf, 4))
}

# =============================================================================
# 2. FIND OPTIMAL K VIA GVF
# Cari k terkecil yang memenuhi threshold GVF (default 0.85)
# Mulai dari k=2 hingga k_max=10
# =============================================================================

find_optimal_k_gvf <- function(x, gvf_threshold = 0.85, k_max = 10) {
  x <- x[!is.na(x)]
  
  gvf_df <- data.frame(
    k   = 2:k_max,
    gvf = vapply(2:k_max, function(k) compute_gvf(x, k), numeric(1))
  )
  
  # k optimal = k terkecil dengan GVF >= threshold
  pass <- gvf_df[gvf_df$gvf >= gvf_threshold, ]
  k_opt <- if (nrow(pass) > 0) pass$k[1] else k_max
  
  return(list(
    k_opt  = k_opt,
    gvf_df = gvf_df
  ))
}

# =============================================================================
# 3. GET VARIABLE OPTIONS
# Mengembalikan daftar variabel yang tersedia berdasarkan sumber data.
# Memanfaatkan build_feature_matrix dari global.R (sudah tersedia).
# =============================================================================

get_variable_options <- function(data_source, raw_data, sovi_result,
                                 sovi_vars = NULL) {
  if (data_source %in% c("raw", "raw_norm")) {
    # Semua kolom numerik dari dataset asli
    num_cols <- names(raw_data)[sapply(raw_data, is.numeric)]
    return(num_cols)
    
  } else if (data_source == "standardized") {
    if (is.null(sovi_result)) return(character(0))
    return(names(sovi_result$std_out$Z))
    
  } else if (data_source == "sovi") {
    return("sovi_score")
    
  } else if (data_source == "rc") {
    if (is.null(sovi_result)) return(character(0))
    rc_cols <- grep("^RC", names(sovi_result$sovi_df), value = TRUE)
    return(rc_cols)
  }
  
  return(character(0))
}

# =============================================================================
# 4. EXTRACT VARIABLE VECTOR
# Ambil vektor nilai satu variabel dari sumber data yang dipilih.
# =============================================================================

extract_variable_vector <- function(var_name, data_source, raw_data,
                                    sovi_result) {
  if (data_source == "raw") {
    return(as.numeric(raw_data[[var_name]]))
    
  } else if (data_source == "raw_norm") {
    x <- as.numeric(raw_data[[var_name]])
    return(normalize_01(x))   # fungsi dari global.R
    
  } else if (data_source == "standardized") {
    return(as.numeric(sovi_result$std_out$Z[[var_name]]))
    
  } else if (data_source == "sovi") {
    return(as.numeric(sovi_result$sovi_df$sovi_score))
    
  } else if (data_source == "rc") {
    return(as.numeric(sovi_result$sovi_df[[var_name]]))
  }
  
  return(NULL)
}

# =============================================================================
# 5. CLASSIFY VARIABLE (Jenks)
# Mengembalikan list berisi: kelas per unit, breaks, tabel distribusi.
# =============================================================================

classify_variable_jenks <- function(x, k, var_label = "Variabel") {
  x_clean <- x
  x_clean[is.na(x_clean)] <- median(x, na.rm = TRUE)
  
  brks   <- classInt::classIntervals(x_clean, n = k, style = "jenks")$brks
  brks_r <- round(brks, 4)
  
  # Helper format angka
  fmt_num <- function(v) {
    ifelse(abs(v) < 10,
           formatC(v, digits = 3, format = "f"),
           formatC(v, digits = 2, format = "f"))
  }
  
  # Label kelas berupa range angka: "0.123 - 1.456"
  cls_labels <- paste0(
    fmt_num(brks_r[-length(brks_r)]), " - ", fmt_num(brks_r[-1])
  )
  
  cls <- cut(x_clean,
             breaks         = brks,
             labels         = cls_labels,
             include.lowest = TRUE)
  
  # Tabel distribusi
  dist_tbl <- as.data.frame(table(Kelas = cls))
  dist_tbl$Persen <- round(dist_tbl$Freq / length(x_clean) * 100, 1)
  
  # Tabel breaks lengkap
  brks_tbl <- data.frame(
    No    = seq_len(k),
    Range = cls_labels,
    Min   = round(brks[-length(brks)], 4),
    Max   = round(brks[-1], 4),
    stringsAsFactors = FALSE
  )
  
  return(list(
    cls        = cls,
    cls_raw    = as.integer(cls),
    breaks     = brks_r,
    dist_tbl   = dist_tbl,
    brks_tbl   = brks_tbl,
    cls_labels = cls_labels,
    k          = k,
    var_label  = var_label
  ))
}

# =============================================================================
# 6. BUILD LEAFLET — SINGLE MAP (Choropleth + Centroid Overlay Opsional)
# Palet: hijau → merah (n kelas)
#
# CATATAN name_col vs shapefile:
#   name_col (dari input$name_col) adalah nama kolom di dataset CSV/Excel.
#   Shapefile punya kolom nama sendiri yang BERBEDA (misal NAMOBJ, KAB_KOTA).
#   Solusi: join sovi_df ke shp via join_shp/id_col untuk membawa kolom nama
#   dari dataset ke shapefile, sehingga hover menampilkan nama kabupaten.
# =============================================================================

build_leaflet_sovi_analysis <- function(shp,
                                        cls_result,
                                        join_shp,
                                        sovi_df       = NULL,
                                        join_df       = NULL,
                                        name_col      = NULL,
                                        show_centroid = FALSE,
                                        var_label     = "Variabel",
                                        sovi_result   = NULL,
                                        id_col        = NULL) {
  
  # ── Tambahkan kolom klasifikasi ke shapefile ─────────────────────────────
  shp_work              <- shp
  shp_work[["sa_cls"]] <- as.integer(cls_result$cls_raw)
  shp_work[["sa_lbl"]] <- as.character(cls_result$cls)
  
  # ── Dapatkan nama kabupaten dari sovi_df atau sovi_result ────────────────
  # Strategi: join kolom nama dari sovi_df (yang sudah punya name_col dari CSV)
  # ke shp_work menggunakan join_shp (ID shapefile) dan id_col (ID dataset).
  # Ini memastikan hover menampilkan nama kabupaten yang sama dengan dataset.
  
  nama_vec <- NULL  # vector nama kabupaten sesuai urutan baris shp
  
  # Coba ambil dari sovi_result$sovi_df (selalu tersedia jika SoVI sudah jalan)
  if (!is.null(sovi_result) && !is.null(id_col) && !is.null(name_col)) {
    tryCatch({
      df_nama <- sovi_result$sovi_df[, c(id_col, name_col), drop = FALSE]
      df_nama[[id_col]] <- normalize_id(df_nama[[id_col]])
      shp_ids           <- normalize_id(shp_work[[join_shp]])
      idx               <- match(shp_ids, df_nama[[id_col]])
      nama_vec          <- as.character(df_nama[[name_col]][idx])
    }, error = function(e) NULL)
  }
  
  # Fallback: coba dari sovi_df yang dipass (untuk centroid)
  if (is.null(nama_vec) && !is.null(sovi_df) && !is.null(join_df) && !is.null(name_col)) {
    tryCatch({
      df_nama <- sovi_df[, c(join_df, name_col), drop = FALSE]
      df_nama[[join_df]] <- normalize_id(df_nama[[join_df]])
      shp_ids            <- normalize_id(shp_work[[join_shp]])
      idx                <- match(shp_ids, df_nama[[join_df]])
      nama_vec           <- as.character(df_nama[[name_col]][idx])
    }, error = function(e) NULL)
  }
  
  # Fallback terakhir: cari kolom nama di shapefile sendiri (bukan kolom ID)
  if (is.null(nama_vec) || all(is.na(nama_vec))) {
    geom_col        <- attr(shp, "sf_column")
    shp_cols        <- setdiff(names(shp), c(geom_col, join_shp))
    name_candidates <- shp_cols[grep("name|nama|kab|city|wilayah|kabupaten",
                                     shp_cols, ignore.case = TRUE)]
    shp_name_col <- if (length(name_candidates) > 0) {
      name_candidates[1]
    } else if (length(shp_cols) > 0) {
      shp_cols[1]
    } else {
      NULL
    }
    if (!is.null(shp_name_col)) {
      nama_vec <- as.character(shp_work[[shp_name_col]])
    } else {
      nama_vec <- as.character(seq_len(nrow(shp_work)))
    }
  }
  
  # ── Palet: hijau → merah sesuai jumlah kelas ────────────────────────────
  k <- cls_result$k
  pal_colors <- grDevices::colorRampPalette(
    c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")
  )(k)
  
  pal_fn <- leaflet::colorFactor(
    palette  = pal_colors,
    domain   = 1:k,
    na.color = "#D3D3D3"
  )
  
  # ── Label range untuk legenda ────────────────────────────────────────────
  brks <- cls_result$breaks
  fmt_num <- function(v) {
    ifelse(abs(v) < 10,
           formatC(v, digits = 3, format = "f"),
           formatC(v, digits = 2, format = "f"))
  }
  range_labels <- paste0(
    fmt_num(brks[-length(brks)]), " \u2013 ", fmt_num(brks[-1])
  )
  
  # ── Popup saat klik — gunakan nama_vec ───────────────────────────────────
  popup_text <- paste0(
    "<b>", nama_vec, "</b><br>",
    "<b>", var_label, "</b><br>",
    "Kelas : ", shp_work[["sa_lbl"]], "<br>",
    "Range : ", range_labels[shp_work[["sa_cls"]]]
  )
  
  # ── Hover label — gunakan nama_vec (bukan formula ~get()) ────────────────
  hover_labels <- nama_vec
  
  # ── Base map ─────────────────────────────────────────────────────────────
  m <- leaflet::leaflet(shp_work) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      fillColor        = ~pal_fn(sa_cls),
      fillOpacity      = 0.75,
      color            = "#555555",
      weight           = 0.5,
      popup            = popup_text,
      label            = hover_labels,
      labelOptions     = leaflet::labelOptions(
        style     = list("font-weight" = "bold", padding = "4px 8px"),
        textsize  = "13px",
        direction = "auto"
      ),
      highlightOptions = leaflet::highlightOptions(
        weight       = 2,
        color        = "#333333",
        fillOpacity  = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors   = pal_colors,
      labels   = range_labels,
      title    = var_label,
      opacity  = 0.9
    )
  
  # ── Centroid Overlay (opsional) ──────────────────────────────────────────
  if (show_centroid && !is.null(sovi_df) && !is.null(join_df)) {
    tryCatch({
      sf::sf_use_s2(FALSE)
      shp_proj  <- sf::st_transform(shp, crs = 4326)
      centroids <- sf::st_centroid(sf::st_geometry(shp_proj))
      sf::sf_use_s2(TRUE)
      coords <- as.data.frame(sf::st_coordinates(centroids))
      names(coords) <- c("lng", "lat")
      
      # Ambil kelas SoVI
      sovi_df_work            <- sovi_df
      sovi_df_work[[join_df]] <- normalize_id(sovi_df_work[[join_df]])
      shp_ids                 <- normalize_id(shp[[join_shp]])
      
      vuln_vec <- sovi_df_work$vuln_class[
        match(shp_ids, sovi_df_work[[join_df]])
      ]
      vuln_vec <- as.character(vuln_vec)
      
      # Warna centroid berdasarkan kelas SoVI
      sovi_pal <- leaflet::colorFactor(
        palette  = unname(VULN_PAL),
        levels   = VULN_CLASSES,
        na.color = "#888888"
      )
      
      coords$vuln <- vuln_vec
      
      m <- m |>
        leaflet::addCircleMarkers(
          data        = coords,
          lng         = ~lng,
          lat         = ~lat,
          radius      = 5,
          color       = "#333333",
          weight      = 0.8,
          fillColor   = ~sovi_pal(vuln),
          fillOpacity = 0.85,
          popup       = paste0("SoVI Class: ", coords$vuln),
          group       = "Centroid SoVI"
        ) |>
        leaflet::addLegend(
          position = "bottomleft",
          colors   = unname(VULN_PAL),
          labels   = VULN_CLASSES,
          title    = "SoVI Class",
          opacity  = 0.9
        )
    }, error = function(e) {
      message("Centroid overlay error: ", e$message)
    })
  }
  
  return(m)
}

# =============================================================================
# 7. BUILD SUMMARY TABLE
# Gabungkan tabel breaks dan distribusi untuk Tab Ringkasan.
# =============================================================================

build_summary_table <- function(cls_result) {
  brks <- cls_result$brks_tbl        # kolom: No, Range, Min, Max
  dist <- cls_result$dist_tbl        # kolom: Kelas (= Range), Freq, Persen
  
  # Gabungkan berdasarkan Range
  merged <- merge(brks, dist,
                  by.x = "Range", by.y = "Kelas",
                  all.x = TRUE)
  
  # Urutkan berdasarkan No kelas
  merged <- merged[order(merged$No), ]
  
  return(merged[, c("No", "Range", "Min", "Max", "Freq", "Persen")])
}

# =============================================================================
# 8. SOURCE DATA LABEL
# Helper untuk label display sumber data.
# =============================================================================

get_source_label <- function(data_source) {
  switch(data_source,
         "raw"          = "Data Asli (tanpa transformasi)",
         "raw_norm"     = "Data Asli Ternormalisasi (0-1)",
         "standardized" = "Data Ter-standardisasi (Z-score)",
         "sovi"         = "SoVI Score",
         "rc"           = "Skor RC (Komponen PCA)",
         data_source
  )
}