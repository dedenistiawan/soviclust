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

message("[helpers.R] Konstanta dan helper functions dimuat.")
