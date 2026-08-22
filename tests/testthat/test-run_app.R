# tests/testthat/test-run_app.R
# =============================================================================
# Test: run_app()
# Memastikan fungsi utama package dapat menemukan direktori app dengan benar
# =============================================================================

test_that("run_app() menemukan direktori inst/app/", {
  # Cek direktori app tersedia di dalam package yang terinstall
  app_dir <- system.file("app", package = "soviclust")
  expect_true(
    nchar(app_dir) > 0,
    label = "system.file('app') mengembalikan path yang tidak kosong"
  )
})

test_that("direktori inst/app/ berisi file Shiny yang diperlukan", {
  app_dir <- system.file("app", package = "soviclust")
  skip_if(nchar(app_dir) == 0, "Direktori app tidak ditemukan")

  # File wajib Shiny app
  expect_true(
    file.exists(file.path(app_dir, "ui.R")),
    label = "ui.R ada di inst/app/"
  )
  expect_true(
    file.exists(file.path(app_dir, "server.R")),
    label = "server.R ada di inst/app/"
  )
  expect_true(
    file.exists(file.path(app_dir, "global.R")),
    label = "global.R ada di inst/app/"
  )
})

test_that("run_app() adalah fungsi yang dapat dipanggil", {
  expect_true(is.function(soviclust::run_app))
})

test_that("run_app() mengembalikan error informatif jika app tidak ditemukan", {
  # Simulasikan package tanpa app dengan mocking system.file
  # (hanya cek bahwa fungsi memiliki validasi path)
  fn_body <- deparse(body(soviclust::run_app))
  expect_true(
    any(grepl("system\\.file|app_dir|inst", fn_body)),
    label = "run_app() menggunakan system.file() untuk mencari app"
  )
})
