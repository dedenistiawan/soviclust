# tests/testthat/test-sample_data.R
# =============================================================================
# Tests for exported sample-data helper functions.
# =============================================================================

test_that("sample SoVI data can be loaded", {
  df <- sovi_sample_data()

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 514)
  expect_true("DISTRICTCODE" %in% names(df))
  expect_true("KABUPATEN" %in% names(df))
  expect_equal(length(unique(df$DISTRICTCODE)), nrow(df))
  expect_gte(sum(vapply(df, is.numeric, logical(1))), 10)
})

test_that("sample coordinates can be loaded", {
  coord <- sovi_sample_coords()

  expect_s3_class(coord, "data.frame")
  expect_equal(nrow(coord), 514)

  cols <- tolower(names(coord))
  expect_true(any(cols %in% c("longitude", "lon", "long", "x")))
  expect_true(any(cols %in% c("latitude", "lat", "y")))
})

test_that("sample population data can be loaded", {
  pop <- sovi_sample_pop()

  expect_s3_class(pop, "data.frame")
  expect_equal(nrow(pop), 514)
  expect_true(any(vapply(pop, is.numeric, logical(1))))
})

test_that("sample distance matrix can be loaded", {
  mat <- sovi_sample_distmat()

  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(514L, 514L))
  expect_true(is.numeric(mat))
  expect_true(all(diag(mat) == 0))
})

test_that("sample administrative boundaries can be loaded", {
  shp <- sovi_sample_shapefile()

  expect_s3_class(shp, "sf")
  expect_equal(nrow(shp), 514)
  expect_true("idkab" %in% names(shp))
  expect_false(is.na(sf::st_crs(shp)))
})
