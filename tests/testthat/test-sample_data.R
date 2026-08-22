# tests/testthat/test-sample_data.R
# =============================================================================
# Test: Dataset Sampel
# Memastikan semua file data sampel dapat dibaca dengan benar dan
# memiliki struktur yang diharapkan
# =============================================================================

# Helper: dapatkan path extdata
extdata_path <- function(file) {
  system.file("extdata", file, package = "soviclust")
}

# =============================================================================
# sovi_data_kab_514_15.xlsx
# =============================================================================

test_that("sovi_data_kab_514_15.xlsx tersedia di extdata", {
  path <- extdata_path("sovi_data_kab_514_15.xlsx")
  expect_true(
    nchar(path) > 0 && file.exists(path),
    label = "sovi_data_kab_514_15.xlsx ditemukan"
  )
})

test_that("sovi_data_kab_514_15.xlsx dapat dibaca dan memiliki struktur benar", {
  path <- extdata_path("sovi_data_kab_514_15.xlsx")
  skip_if(!file.exists(path), "File tidak ditemukan")

  df <- readxl::read_excel(path)

  # Jumlah baris: 514 kabupaten/kota
  expect_equal(nrow(df), 514, label = "Dataset memiliki 514 baris")

  # Kolom wajib
  expect_true("DISTRICTCODE" %in% names(df), label = "Kolom DISTRICTCODE ada")
  expect_true("KABUPATEN"    %in% names(df), label = "Kolom KABUPATEN ada")

  # Tidak ada baris yang semuanya NA
  expect_false(any(rowSums(is.na(df)) == ncol(df)), label = "Tidak ada baris kosong")

  # Kolom numerik tersedia (minimal 10)
  num_cols <- sum(sapply(df, is.numeric))
  expect_gte(num_cols, 10, label = "Minimal 10 kolom numerik tersedia")
})

test_that("DISTRICTCODE pada sovi_data_kab_514_15.xlsx bersifat unik", {
  path <- extdata_path("sovi_data_kab_514_15.xlsx")
  skip_if(!file.exists(path), "File tidak ditemukan")

  df <- readxl::read_excel(path)
  expect_equal(
    length(unique(df$DISTRICTCODE)), nrow(df),
    label = "DISTRICTCODE unik untuk setiap baris"
  )
})

# =============================================================================
# Koordinat.xlsx
# =============================================================================

test_that("Koordinat.xlsx tersedia di extdata", {
  path <- extdata_path("Koordinat.xlsx")
  expect_true(
    nchar(path) > 0 && file.exists(path),
    label = "Koordinat.xlsx ditemukan"
  )
})

test_that("Koordinat.xlsx memiliki kolom longitude dan latitude", {
  path <- extdata_path("Koordinat.xlsx")
  skip_if(!file.exists(path), "File tidak ditemukan")

  df   <- readxl::read_excel(path)
  cols <- tolower(names(df))

  expect_true(
    any(cols %in% c("longitude", "lon", "long", "x")),
    label = "Kolom longitude ditemukan"
  )
  expect_true(
    any(cols %in% c("latitude", "lat", "y")),
    label = "Kolom latitude ditemukan"
  )
  expect_equal(nrow(df), 514, label = "Koordinat memiliki 514 baris")
})

test_that("Nilai longitude dan latitude dalam rentang valid Indonesia", {
  path <- extdata_path("Koordinat.xlsx")
  skip_if(!file.exists(path), "File tidak ditemukan")

  df      <- readxl::read_excel(path)
  cols    <- tolower(names(df))
  lon_col <- names(df)[which(cols %in% c("longitude", "lon", "long", "x"))[1]]
  lat_col <- names(df)[which(cols %in% c("latitude", "lat", "y"))[1]]

  lon <- as.numeric(df[[lon_col]])
  lat <- as.numeric(df[[lat_col]])

  # Indonesia: lon 95-141, lat -11 sampai 6
  expect_true(all(lon >= 90  & lon <= 145, na.rm = TRUE), label = "Longitude dalam rentang Indonesia")
  expect_true(all(lat >= -15 & lat <= 10,  na.rm = TRUE), label = "Latitude dalam rentang Indonesia")
})

# =============================================================================
# Distance_matrix_514.xlsx
# =============================================================================

test_that("Distance_matrix_514.xlsx tersedia di extdata", {
  path <- extdata_path("Distance_matrix_514.xlsx")
  expect_true(
    nchar(path) > 0 && file.exists(path),
    label = "Distance_matrix_514.xlsx ditemukan"
  )
})

test_that("Distance_matrix_514.xlsx adalah matriks persegi 514x514", {
  path <- extdata_path("Distance_matrix_514.xlsx")
  skip_if(!file.exists(path), "File tidak ditemukan")

  df <- readxl::read_excel(path)

  # Bisa berisi kolom label di kolom pertama
  if (!is.numeric(df[[1]])) df <- df[, -1]

  expect_equal(nrow(df), ncol(df), label = "Matriks jarak berbentuk persegi")
  expect_equal(nrow(df), 514,      label = "Matriks jarak berukuran 514x514")
})

test_that("Nilai diagonal Distance_matrix_514.xlsx adalah 0", {
  path <- extdata_path("Distance_matrix_514.xlsx")
  skip_if(!file.exists(path), "File tidak ditemukan")

  df <- readxl::read_excel(path)
  if (!is.numeric(df[[1]])) df <- df[, -1]
  mat <- data.matrix(df)

  expect_true(all(diag(mat) == 0), label = "Diagonal matriks jarak bernilai 0")
})

# =============================================================================
# sovi_data_pop_514.xlsx
# =============================================================================

test_that("sovi_data_pop_514.xlsx tersedia di extdata", {
  path <- extdata_path("sovi_data_pop_514.xlsx")
  expect_true(
    nchar(path) > 0 && file.exists(path),
    label = "sovi_data_pop_514.xlsx ditemukan"
  )
})

test_that("sovi_data_pop_514.xlsx memiliki 514 baris dan kolom populasi numerik", {
  path <- extdata_path("sovi_data_pop_514.xlsx")
  skip_if(!file.exists(path), "File tidak ditemukan")

  df      <- readxl::read_excel(path)
  num_col <- which(sapply(df, is.numeric))[1]

  expect_equal(nrow(df), 514, label = "Data populasi memiliki 514 baris")
  expect_false(is.na(num_col), label = "Minimal satu kolom numerik (populasi) tersedia")

  pop <- as.numeric(df[[num_col]])
  expect_true(all(pop > 0, na.rm = TRUE), label = "Semua nilai populasi positif")
})

# =============================================================================
# Shapefile
# =============================================================================

test_that("Shapefile 514_kabupaten.shp tersedia di inst/app/map/", {
  shp_path <- system.file("app", "map", "514_kabupaten.shp", package = "soviclust")
  expect_true(
    nchar(shp_path) > 0 && file.exists(shp_path),
    label = "514_kabupaten.shp ditemukan"
  )
})

test_that("Shapefile dapat dibaca oleh sf dan memiliki 514 fitur", {
  shp_path <- system.file("app", "map", "514_kabupaten.shp", package = "soviclust")
  skip_if(!file.exists(shp_path), "Shapefile tidak ditemukan")

  sf::sf_use_s2(FALSE)
  shp <- sf::st_read(shp_path, quiet = TRUE)

  expect_equal(nrow(shp), 514, label = "Shapefile memiliki 514 fitur")
  expect_true("idkab" %in% names(shp), label = "Kolom idkab ada di shapefile")
  expect_s3_class(shp, "sf")
})
