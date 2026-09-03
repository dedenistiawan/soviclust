# tests/testthat/test-sample_data.R
# =============================================================================
# Test: Sample Datasets
# Ensures all sample data files can be read correctly and
# have the expected structure
# =============================================================================

# Helper: get extdata path
extdata_path <- function(file) {
  system.file("extdata", file, package = "soviclust")
}

# =============================================================================
# sovi_data_kab_514_15.xlsx
# =============================================================================

test_that("sovi_data_kab_514_15.xlsx is available in extdata", {
  path <- extdata_path("sovi_data_kab_514_15.xlsx")
  expect_true(
    nchar(path) > 0 && file.exists(path),
    label = "sovi_data_kab_514_15.xlsx found"
  )
})

test_that("sovi_data_kab_514_15.xlsx can be read and has correct structure", {
  path <- extdata_path("sovi_data_kab_514_15.xlsx")
  skip_if(!file.exists(path), "File not found")

  df <- readxl::read_excel(path)

  # Row count: 514 districts
  expect_equal(nrow(df), 514, label = "Dataset has 514 rows")

  # Required columns
  expect_true("DISTRICTCODE" %in% names(df), label = "DISTRICTCODE column exists")
  expect_true("KABUPATEN"    %in% names(df), label = "KABUPATEN column exists")

  # No completely empty rows
  expect_false(any(rowSums(is.na(df)) == ncol(df)), label = "No completely empty rows")

  # At least 10 numeric columns
  num_cols <- sum(sapply(df, is.numeric))
  expect_gte(num_cols, 10, label = "At least 10 numeric columns available")
})

test_that("DISTRICTCODE in sovi_data_kab_514_15.xlsx is unique", {
  path <- extdata_path("sovi_data_kab_514_15.xlsx")
  skip_if(!file.exists(path), "File not found")

  df <- readxl::read_excel(path)
  expect_equal(
    length(unique(df$DISTRICTCODE)), nrow(df),
    label = "DISTRICTCODE is unique for each row"
  )
})

# =============================================================================
# Koordinat.xlsx
# =============================================================================

test_that("Koordinat.xlsx is available in extdata", {
  path <- extdata_path("Koordinat.xlsx")
  expect_true(
    nchar(path) > 0 && file.exists(path),
    label = "Koordinat.xlsx found"
  )
})

test_that("Koordinat.xlsx has longitude and latitude columns", {
  path <- extdata_path("Koordinat.xlsx")
  skip_if(!file.exists(path), "File not found")

  df   <- readxl::read_excel(path)
  cols <- tolower(names(df))

  expect_true(
    any(cols %in% c("longitude", "lon", "long", "x")),
    label = "Longitude column found"
  )
  expect_true(
    any(cols %in% c("latitude", "lat", "y")),
    label = "Latitude column found"
  )
  expect_equal(nrow(df), 514, label = "Coordinates have 514 rows")
})

test_that("Longitude and latitude values are within valid Indonesian range", {
  path <- extdata_path("Koordinat.xlsx")
  skip_if(!file.exists(path), "File not found")

  df      <- readxl::read_excel(path)
  cols    <- tolower(names(df))
  lon_col <- names(df)[which(cols %in% c("longitude", "lon", "long", "x"))[1]]
  lat_col <- names(df)[which(cols %in% c("latitude", "lat", "y"))[1]]

  lon <- as.numeric(df[[lon_col]])
  lat <- as.numeric(df[[lat_col]])

  # Indonesia: lon 95-141, lat -11 to 6
  expect_true(all(lon >= 90  & lon <= 145, na.rm = TRUE), label = "Longitude within Indonesian range")
  expect_true(all(lat >= -15 & lat <= 10,  na.rm = TRUE), label = "Latitude within Indonesian range")
})

# =============================================================================
# Distance_matrix_514.xlsx
# =============================================================================

test_that("Distance_matrix_514.xlsx is available in extdata", {
  path <- extdata_path("Distance_matrix_514.xlsx")
  expect_true(
    nchar(path) > 0 && file.exists(path),
    label = "Distance_matrix_514.xlsx found"
  )
})

test_that("Distance_matrix_514.xlsx is a square 514x514 matrix", {
  path <- extdata_path("Distance_matrix_514.xlsx")
  skip_if(!file.exists(path), "File not found")

  df <- readxl::read_excel(path)

  # May contain a label column in first column
  if (!is.numeric(df[[1]])) df <- df[, -1]

  expect_equal(nrow(df), ncol(df), label = "Distance matrix is square")
  expect_equal(nrow(df), 514,      label = "Distance matrix is 514x514")
})

test_that("Diagonal values of Distance_matrix_514.xlsx are zero", {
  path <- extdata_path("Distance_matrix_514.xlsx")
  skip_if(!file.exists(path), "File not found")

  df <- readxl::read_excel(path)
  if (!is.numeric(df[[1]])) df <- df[, -1]
  mat <- data.matrix(df)

  expect_true(all(diag(mat) == 0), label = "Distance matrix diagonal values are 0")
})

# =============================================================================
# sovi_data_pop_514.xlsx
# =============================================================================

test_that("sovi_data_pop_514.xlsx is available in extdata", {
  path <- extdata_path("sovi_data_pop_514.xlsx")
  expect_true(
    nchar(path) > 0 && file.exists(path),
    label = "sovi_data_pop_514.xlsx found"
  )
})

test_that("sovi_data_pop_514.xlsx has 514 rows and a numeric population column", {
  path <- extdata_path("sovi_data_pop_514.xlsx")
  skip_if(!file.exists(path), "File not found")

  df      <- readxl::read_excel(path)
  num_col <- which(sapply(df, is.numeric))[1]

  expect_equal(nrow(df), 514, label = "Population data has 514 rows")
  expect_false(is.na(num_col), label = "At least one numeric (population) column available")

  pop <- as.numeric(df[[num_col]])
  expect_true(all(pop > 0, na.rm = TRUE), label = "All population values are positive")
})

# =============================================================================
# Shapefile
# =============================================================================

test_that("Shapefile 514_kabupaten.shp is available in inst/app/map/", {
  shp_path <- system.file("app", "map", "514_kabupaten.shp", package = "soviclust")
  expect_true(
    nchar(shp_path) > 0 && file.exists(shp_path),
    label = "514_kabupaten.shp found"
  )
})

test_that("Shapefile can be read by sf and has 514 features", {
  shp_path <- system.file("app", "map", "514_kabupaten.shp", package = "soviclust")
  skip_if(!file.exists(shp_path), "Shapefile not found")

  sf::sf_use_s2(FALSE)
  shp <- sf::st_read(shp_path, quiet = TRUE)

  expect_equal(nrow(shp), 514, label = "Shapefile has 514 features")
  expect_true("idkab" %in% names(shp), label = "idkab column exists in shapefile")
  expect_s3_class(shp, "sf")
})
