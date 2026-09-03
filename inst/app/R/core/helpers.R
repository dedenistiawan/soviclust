# =============================================================================
# R/core/helpers.R
# Konstanta global, helper functions, dan shapefile reader
#
# BERISI:
#   - Konstanta  : VULN_CLASSES, VULN_PAL
#   - Helpers    : normalize_id(), normalize_01()
#   - Shapefile  : read_shapefile()
#
# DIPAKAI OLEH:
#   Semua modul (global.R men-source file ini pertama kali)
# =============================================================================


# =============================================================================
# KONSTANTA GLOBAL
# =============================================================================

# Label kelas kerentanan (urutan dari rendah ke tinggi)
VULN_CLASSES <- c("Very Low", "Low", "Moderate", "High", "Very High")

# Palet warna kelas kerentanan (hijau → merah)
VULN_PAL <- c(
  "Very Low" = "#1a9641",
  "Low"        = "#a6d96a",
  "Moderate"        = "#ffffbf",
  "High"        = "#fdae61",
  "Very High" = "#d7191c"
)


# =============================================================================
# HELPER: NORMALISASI ID
# Dipakai untuk join shapefile dengan dataset agar tidak case-sensitive
# =============================================================================

#' Normalisasi ID wilayah: lowercase + trim whitespace
#' @param x Vektor karakter ID
#' @return Vektor karakter yang sudah dinormalisasi
normalize_id <- function(x) {
  trimws(tolower(as.character(x)))
}


# =============================================================================
# HELPER: NORMALISASI MIN-MAX [0, 1]
# Dipakai di berbagai modul untuk scaling sebelum clustering
# =============================================================================

#' Normalisasi vektor numerik ke rentang [0, 1]
#' @param x Vektor numerik
#' @return Vektor numerik ternormalisasi. Jika semua nilai sama, return 0.5
normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / diff(rng)
}


# =============================================================================
# SHAPEFILE READER
# Membaca shapefile dari multiple file upload (shiny fileInput multiple=TRUE)
# =============================================================================

#' Baca shapefile dari hasil upload Shiny (multiple files)
#'
#' @param files_df data.frame dari input$file_shp (kolom: name, datapath)
#' @return Objek sf (Simple Features) yang sudah divalidasi geometrinya
#'
#' @details
#' Alur kerja:
#'   1. Copy semua file upload ke tempdir (agar .shp bisa menemukan .dbf dll)
#'   2. Cari file .shp di antara file yang diupload
#'   3. Baca dengan sf::st_read()
#'   4. Validasi geometri dengan sf::st_make_valid()
read_shapefile <- function(files_df) {
  
  # ── Langkah 1: Copy semua file ke folder temporer ─────────────────────────
  tmpdir <- tempfile()
  dir.create(tmpdir)
  exts <- tools::file_ext(files_df$name)
  
  for (i in seq_len(nrow(files_df))) {
    dest <- file.path(tmpdir, files_df$name[i])
    file.copy(files_df$datapath[i], dest, overwrite = TRUE)
  }
  
  # ── Langkah 2: Identifikasi file .shp ─────────────────────────────────────
  shp_name <- files_df$name[tolower(exts) == "shp"]
  if (length(shp_name) == 0)
    stop(".shp file not found in upload.")
  
  # ── Langkah 3: Baca shapefile ─────────────────────────────────────────────
  shp_path <- file.path(tmpdir, shp_name[1])
  shp      <- sf::st_read(shp_path, quiet = TRUE)
  
  # ── Langkah 4: Validasi geometri ──────────────────────────────────────────
  sf::sf_use_s2(FALSE)
  shp <- sf::st_make_valid(shp)
  sf::sf_use_s2(TRUE)
  
  return(shp)
}


# =============================================================================
# VALIDASI DATA UPLOAD
# Fungsi-fungsi validasi yang mengembalikan list(ok, msg)
# =============================================================================

#' Validasi file data atribut (Excel/CSV)
#' @param df data.frame yang sudah dibaca
#' @param filename Nama file asli (untuk cek ekstensi)
#' @return list(ok = logical, msg = character)
validate_data_file <- function(df, filename) {
  ext <- tolower(tools::file_ext(filename))

  # Cek ekstensi
  if (!ext %in% c("xlsx", "csv")) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Unsupported file format: '.", ext, "'.\n",
      "Use an Excel (.xlsx) or CSV (.csv) file."
    )))
  }

  # Cek minimal baris
  if (nrow(df) < 2) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Dataset too small: only ", nrow(df), " rows.\n",
      "At least 2 regions are required for analysis."
    )))
  }

  # Cek kolom numerik
  num_cols <- sum(sapply(df, is.numeric))
  if (num_cols < 3) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Dataset has only ", num_cols, " numeric columns.\n",
      "At least 3 numeric variables required for PCA/SoVI."
    )))
  }

  # Cek baris kosong seluruhnya
  all_na_rows <- sum(rowSums(is.na(df)) == ncol(df))
  if (all_na_rows > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u26a0\ufe0f Found ", all_na_rows, " completely empty rows (NA).\n",
      "Remove empty rows from the dataset before uploading."
    )))
  }

  list(ok = TRUE, msg = paste0(
    "\u2713 Dataset valid: ", nrow(df), " rows, ",
    num_cols, " numeric variables."
  ))
}

#' Validasi kecocokan ID antara data dan shapefile
#' @param data_ids Vektor ID dari dataset
#' @param shp_ids  Vektor ID dari shapefile
#' @param data_col Nama kolom ID di data (untuk pesan error)
#' @param shp_col  Nama kolom ID di shapefile (untuk pesan error)
#' @return list(ok = logical, msg = character)
validate_id_match <- function(data_ids, shp_ids, data_col, shp_col) {
  data_ids <- normalize_id(data_ids)
  shp_ids  <- normalize_id(shp_ids)

  n_data <- length(data_ids)
  n_shp  <- length(shp_ids)

  # Cek duplikat di data
  dup_data <- sum(duplicated(data_ids))
  if (dup_data > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c ID column '", data_col, "' in dataset has ",
      dup_data, " duplicate values.\n",
      "Each row must have a unique ID."
    )))
  }

  # Cek duplikat di shapefile
  dup_shp <- sum(duplicated(shp_ids))
  if (dup_shp > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c ID column '", shp_col, "' in shapefile has ",
      dup_shp, " duplicate values.\n",
      "Each spatial feature must have a unique ID."
    )))
  }

  # Cek jumlah baris
  if (n_data != n_shp) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Row count mismatch:\n",
      "  - Dataset  : ", n_data, " rows\n",
      "  - Shapefile: ", n_shp, " features\n",
      "Ensure both files have the same number of regions."
    )))
  }

  # Cek overlap ID
  matched    <- sum(data_ids %in% shp_ids)
  unmatched  <- setdiff(data_ids, shp_ids)

  if (matched == 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c No matching IDs between dataset and shapefile.\n",
      "  - Data column    : '", data_col, "' (example: ", paste(head(data_ids, 3), collapse = ", "), ")\n",
      "  - Shapefile column: '", shp_col, "' (example: ", paste(head(shp_ids, 3), collapse = ", "), ")\n",
      "Ensure both columns contain the same ID values."
    )))
  }

  if (length(unmatched) > 0) {
    n_unmatched <- length(unmatched)
    contoh <- paste(head(unmatched, 3), collapse = ", ")
    return(list(ok = FALSE, msg = paste0(
      "\u26a0\ufe0f ", n_unmatched, " IDs in dataset not found in shapefile.\n",
      "Unmatched ID examples: ", contoh, "\n",
      "Check whether ID format is consistent (e.g., numeric vs text, spaces, etc)."
    )))
  }

  list(ok = TRUE, msg = paste0(
    "\u2713 ID cocok: ", matched, " regions detected."
  ))
}

#' Validasi variabel SoVI yang dipilih
#' @param selected_vars Vektor nama variabel yang dipilih
#' @param df data.frame dataset
#' @return list(ok = logical, msg = character)
validate_sovi_vars <- function(selected_vars, df) {
  if (length(selected_vars) < 3) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Select at least 3 SoVI variables.\n",
      "Currently only ", length(selected_vars), " variables selected."
    )))
  }

  non_numeric <- selected_vars[!sapply(df[, selected_vars, drop = FALSE], is.numeric)]
  if (length(non_numeric) > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c The following variables are not numeric: ",
      paste(non_numeric, collapse = ", "), ".\n",
      "Select only numeric variables as SoVI indicators."
    )))
  }

  # Cek variabel dengan semua NA
  all_na_vars <- selected_vars[sapply(df[, selected_vars, drop = FALSE],
                                       function(x) all(is.na(x)))]
  if (length(all_na_vars) > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c The following variables have all missing values (NA): ",
      paste(all_na_vars, collapse = ", "), ".\n",
      "Remove or fill missing values before analysis."
    )))
  }

  list(ok = TRUE, msg = paste0(
    "\u2713 ", length(selected_vars), " SoVI variables valid."
  ))
}

message("[helpers.R] Konstanta dan helper functions dimuat.")
