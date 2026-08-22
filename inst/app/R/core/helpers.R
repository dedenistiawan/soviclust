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
VULN_CLASSES <- c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi")

# Palet warna kelas kerentanan (hijau → merah)
VULN_PAL <- c(
  "Sangat Rendah" = "#1a9641",
  "Rendah"        = "#a6d96a",
  "Sedang"        = "#ffffbf",
  "Tinggi"        = "#fdae61",
  "Sangat Tinggi" = "#d7191c"
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
    stop("File .shp tidak ditemukan dalam upload.")
  
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
      "\u274c Format file tidak didukung: '.", ext, "'.\n",
      "Gunakan file Excel (.xlsx) atau CSV (.csv)."
    )))
  }

  # Cek minimal baris
  if (nrow(df) < 2) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Dataset terlalu kecil: hanya ", nrow(df), " baris.\n",
      "Diperlukan minimal 2 wilayah untuk analisis."
    )))
  }

  # Cek kolom numerik
  num_cols <- sum(sapply(df, is.numeric))
  if (num_cols < 3) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Dataset hanya memiliki ", num_cols, " kolom numerik.\n",
      "Diperlukan minimal 3 variabel numerik untuk PCA/SoVI."
    )))
  }

  # Cek baris kosong seluruhnya
  all_na_rows <- sum(rowSums(is.na(df)) == ncol(df))
  if (all_na_rows > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u26a0\ufe0f Ditemukan ", all_na_rows, " baris yang seluruhnya kosong (NA).\n",
      "Hapus baris kosong dari dataset sebelum upload."
    )))
  }

  list(ok = TRUE, msg = paste0(
    "\u2713 Dataset valid: ", nrow(df), " baris, ",
    num_cols, " variabel numerik."
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
      "\u274c Kolom ID '", data_col, "' di dataset memiliki ",
      dup_data, " nilai duplikat.\n",
      "Setiap baris harus memiliki ID unik."
    )))
  }

  # Cek duplikat di shapefile
  dup_shp <- sum(duplicated(shp_ids))
  if (dup_shp > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Kolom ID '", shp_col, "' di shapefile memiliki ",
      dup_shp, " nilai duplikat.\n",
      "Setiap fitur spasial harus memiliki ID unik."
    )))
  }

  # Cek jumlah baris
  if (n_data != n_shp) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Jumlah baris tidak cocok:\n",
      "  - Dataset: ", n_data, " baris\n",
      "  - Shapefile: ", n_shp, " fitur\n",
      "Pastikan kedua file memiliki jumlah wilayah yang sama."
    )))
  }

  # Cek overlap ID
  matched    <- sum(data_ids %in% shp_ids)
  unmatched  <- setdiff(data_ids, shp_ids)

  if (matched == 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Tidak ada ID yang cocok antara dataset dan shapefile.\n",
      "  - Kolom data   : '", data_col, "' (contoh: ", paste(head(data_ids, 3), collapse = ", "), ")\n",
      "  - Kolom shapefile: '", shp_col, "' (contoh: ", paste(head(shp_ids, 3), collapse = ", "), ")\n",
      "Pastikan kedua kolom berisi nilai ID yang sama."
    )))
  }

  if (length(unmatched) > 0) {
    n_unmatched <- length(unmatched)
    contoh <- paste(head(unmatched, 3), collapse = ", ")
    return(list(ok = FALSE, msg = paste0(
      "\u26a0\ufe0f ", n_unmatched, " ID di dataset tidak ditemukan di shapefile.\n",
      "Contoh ID yang tidak cocok: ", contoh, "\n",
      "Periksa apakah format ID konsisten (misalnya: angka vs teks, spasi, dll)."
    )))
  }

  list(ok = TRUE, msg = paste0(
    "\u2713 ID cocok: ", matched, " wilayah terdeteksi."
  ))
}

#' Validasi variabel SoVI yang dipilih
#' @param selected_vars Vektor nama variabel yang dipilih
#' @param df data.frame dataset
#' @return list(ok = logical, msg = character)
validate_sovi_vars <- function(selected_vars, df) {
  if (length(selected_vars) < 3) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Pilih minimal 3 variabel SoVI.\n",
      "Saat ini hanya ", length(selected_vars), " variabel dipilih."
    )))
  }

  non_numeric <- selected_vars[!sapply(df[, selected_vars, drop = FALSE], is.numeric)]
  if (length(non_numeric) > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Variabel berikut bukan numerik: ",
      paste(non_numeric, collapse = ", "), ".\n",
      "Hanya pilih variabel numerik sebagai indikator SoVI."
    )))
  }

  # Cek variabel dengan semua NA
  all_na_vars <- selected_vars[sapply(df[, selected_vars, drop = FALSE],
                                       function(x) all(is.na(x)))]
  if (length(all_na_vars) > 0) {
    return(list(ok = FALSE, msg = paste0(
      "\u274c Variabel berikut seluruh nilainya kosong (NA): ",
      paste(all_na_vars, collapse = ", "), ".\n",
      "Hapus atau isi nilai yang hilang sebelum analisis."
    )))
  }

  list(ok = TRUE, msg = paste0(
    "\u2713 ", length(selected_vars), " variabel SoVI valid."
  ))
}

message("[helpers.R] Konstanta dan helper functions dimuat.")
