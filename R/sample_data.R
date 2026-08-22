# R/sample_data.R
# =============================================================================
# Fungsi pembantu untuk mengakses dataset sampel soviclust langsung dari R
# tanpa harus membuka aplikasi Shiny
# =============================================================================


#' Baca Data SoVI 514 Kabupaten/Kota Indonesia (2015)
#'
#' @description
#' Membaca dataset sampel SoVI 514 Kabupaten/Kota Indonesia tahun 2015
#' yang disertakan dalam package. Dataset ini siap digunakan untuk
#' eksplorasi atau sebagai template format data untuk analisis Anda sendiri.
#'
#' @return `data.frame` dengan 514 baris dan 17 kolom:
#'   \describe{
#'     \item{DISTRICTCODE}{Kode unik wilayah (ID).}
#'     \item{KABUPATEN}{Nama Kabupaten/Kota.}
#'     \item{AGE014, FEMPOP, AGE65P, ...}{Indikator SoVI (15 variabel).}
#'   }
#'
#' @examples
#' df <- sovi_sample_data()
#' head(df[, 1:5])
#' dim(df)
#'
#' # Lihat nama semua variabel
#' names(df)
#'
#' # Statistik deskriptif variabel numerik
#' summary(df[, -(1:2)])
#'
#' @export
sovi_sample_data <- function() {
  path <- system.file("extdata", "sovi_data_kab_514_15.xlsx",
                      package = "soviclust")
  if (path == "") {
    stop(
      "File 'sovi_data_kab_514_15.xlsx' tidak ditemukan.\n",
      "Pastikan package 'soviclust' terinstall dengan benar.",
      call. = FALSE
    )
  }
  as.data.frame(readxl::read_excel(path))
}


#' Baca Shapefile 514 Kabupaten/Kota Indonesia
#'
#' @description
#' Membaca shapefile batas wilayah 514 Kabupaten/Kota Indonesia
#' yang disertakan dalam package sebagai objek `sf`.
#'
#' @return Objek `sf` dengan 514 fitur (Polygon/MultiPolygon).
#'   Kolom ID wilayah: `idkab`.
#'   Sistem koordinat: WGS84 (EPSG:4326).
#'
#' @examples
#' shp <- sovi_sample_shapefile()
#' class(shp)
#' names(shp)
#' nrow(shp)
#'
#' # Tampilkan peta sederhana
#' plot(sf::st_geometry(shp))
#'
#' # Gabungkan dengan data SoVI
#' df  <- sovi_sample_data()
#' shp_sovi <- merge(shp, df, by.x = "idkab", by.y = "DISTRICTCODE")
#'
#' @export
sovi_sample_shapefile <- function() {
  shp_path <- system.file("app", "map", "514_kabupaten.shp",
                          package = "soviclust")
  if (shp_path == "") {
    stop(
      "Shapefile '514_kabupaten.shp' tidak ditemukan.\n",
      "Pastikan package 'soviclust' terinstall dengan benar.",
      call. = FALSE
    )
  }
  sf::sf_use_s2(FALSE)
  sf::st_read(shp_path, quiet = TRUE)
}


#' Baca Koordinat Centroid 514 Kabupaten/Kota
#'
#' @description
#' Membaca data koordinat geografis (longitude dan latitude) titik centroid
#' 514 Kabupaten/Kota Indonesia. Berguna untuk menghitung matriks jarak
#' Haversine secara manual atau untuk visualisasi.
#'
#' @return `data.frame` dengan 514 baris dan 3 kolom:
#'   \describe{
#'     \item{DISTRICTCODE}{Kode unik wilayah.}
#'     \item{longitude}{Koordinat bujur (derajat desimal, WGS84).}
#'     \item{latitude}{Koordinat lintang (derajat desimal, WGS84).}
#'   }
#'
#' @examples
#' coord <- sovi_sample_coords()
#' head(coord)
#'
#' # Rentang koordinat
#' range(coord$longitude)  # ~[95, 141]
#' range(coord$latitude)   # ~[-11, 6]
#'
#' # Plot lokasi centroid
#' plot(coord$longitude, coord$latitude,
#'      pch = 20, cex = 0.5, col = "steelblue",
#'      xlab = "Longitude", ylab = "Latitude",
#'      main = "Centroid 514 Kabupaten/Kota Indonesia")
#'
#' @export
sovi_sample_coords <- function() {
  path <- system.file("extdata", "Koordinat.xlsx", package = "soviclust")
  if (path == "") {
    stop(
      "File 'Koordinat.xlsx' tidak ditemukan.\n",
      "Pastikan package 'soviclust' terinstall dengan benar.",
      call. = FALSE
    )
  }
  df <- as.data.frame(readxl::read_excel(path))
  df
}


#' Baca Data Populasi 514 Kabupaten/Kota
#'
#' @description
#' Membaca data jumlah penduduk 514 Kabupaten/Kota Indonesia
#' yang disertakan dalam package. Data ini digunakan sebagai bobot
#' populasi pada analisis FGWC, LFGWC, dan ALFGWC.
#'
#' @return `data.frame` dengan 514 baris dan minimal 2 kolom (ID + populasi).
#'
#' @examples
#' pop_df <- sovi_sample_pop()
#' head(pop_df)
#'
#' # Ambil vektor populasi
#' pop <- pop_df[[which(sapply(pop_df, is.numeric))[1]]]
#' summary(pop)
#' hist(pop / 1e6, main = "Distribusi Populasi (juta jiwa)",
#'      xlab = "Populasi (juta)", col = "steelblue")
#'
#' @export
sovi_sample_pop <- function() {
  path <- system.file("extdata", "sovi_data_pop_514.xlsx",
                      package = "soviclust")
  if (path == "") {
    stop(
      "File 'sovi_data_pop_514.xlsx' tidak ditemukan.\n",
      "Pastikan package 'soviclust' terinstall dengan benar.",
      call. = FALSE
    )
  }
  as.data.frame(readxl::read_excel(path))
}


#' Baca Matriks Jarak 514 Kabupaten/Kota
#'
#' @description
#' Membaca matriks jarak simetris 514 × 514 antar Kabupaten/Kota Indonesia
#' yang disertakan dalam package.
#'
#' @return Matriks numerik 514 × 514. Nilai diagonal = 0.
#'
#' @note
#' File ini berukuran ~3.8 MB dan mungkin memerlukan beberapa saat untuk dibaca.
#'
#' @examples
#' mat <- sovi_sample_distmat()
#' dim(mat)        # [1] 514 514
#' diag(mat)[1:5]  # [1] 0 0 0 0 0
#'
#' # Jarak dari wilayah pertama ke semua wilayah lain
#' hist(mat[1, -1], main = "Distribusi Jarak dari Wilayah 1",
#'      xlab = "Jarak", col = "coral")
#'
#' @export
sovi_sample_distmat <- function() {
  path <- system.file("extdata", "Distance_matrix_514.xlsx",
                      package = "soviclust")
  if (path == "") {
    stop(
      "File 'Distance_matrix_514.xlsx' tidak ditemukan.\n",
      "Pastikan package 'soviclust' terinstall dengan benar.",
      call. = FALSE
    )
  }
  df <- as.data.frame(readxl::read_excel(path))
  if (!is.numeric(df[[1]])) df <- df[, -1]
  data.matrix(df)
}
