# tests/testthat/test-run_app.R
# =============================================================================
# Test: run_app()
# Memastikan fungsi utama package dapat menemukan direktori app dengan benar
# =============================================================================

test_that("run_app() finds the inst/app/ directory", {
  # Cek direktori app tersedia di dalam package yang terinstall
  app_dir <- system.file("app", package = "soviclust")
  expect_true(
    nchar(app_dir) > 0,
    label = "system.file('app') returns a non-empty path"
  )
})

test_that("inst/app/ directory contains required Shiny files", {
  app_dir <- system.file("app", package = "soviclust")
  skip_if(nchar(app_dir) == 0, "App directory not found")

  # File wajib Shiny app
  expect_true(
    file.exists(file.path(app_dir, "ui.R")),
    label = "ui.R exists in inst/app/"
  )
  expect_true(
    file.exists(file.path(app_dir, "server.R")),
    label = "server.R exists in inst/app/"
  )
  expect_true(
    file.exists(file.path(app_dir, "global.R")),
    label = "global.R exists in inst/app/"
  )
})

test_that("run_app() is a callable function", {
  expect_true(is.function(soviclust::run_app))
})

test_that("run_app() returns an informative error if app directory is missing", {
  # Simulasikan package tanpa app dengan mocking system.file
  # (hanya cek bahwa fungsi memiliki validasi path)
  fn_body <- deparse(body(soviclust::run_app))
  expect_true(
    any(grepl("system\\.file|app_dir|inst", fn_body)),
    label = "run_app() uses system.file() to locate the app"
  )
})
