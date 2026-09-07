# tests/testthat/test-optimizer-v3-lfgwc-ui-server.R
# =============================================================================
# Patch v3.2d — LFGWC UI/server optimizer wiring
# =============================================================================

.find_lfgwc_v32d_file <- function(...) {
  parts <- c(...)
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  probe <- root

  repeat {
    candidate <- do.call(
      file.path,
      as.list(c(probe, "inst", "app", parts))
    )

    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }

    parent <- dirname(probe)
    if (identical(parent, probe)) break
    probe <- parent
  }

  installed <- do.call(
    system.file,
    c(as.list(c("app", parts)), list(package = "soviclust"))
  )

  if (nzchar(installed) && file.exists(installed)) {
    return(normalizePath(installed, winslash = "/", mustWork = TRUE))
  }

  stop(
    paste("Could not locate installed/source LFGWC file:", paste(parts, collapse = "/")),
    call. = FALSE
  )
}

.read_lfgwc_v32d <- function(...) {
  paste(
    readLines(
      .find_lfgwc_v32d_file(...),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
}

test_that("LFGWC UI exposes all canonical optimizers and exact NFE budget", {
  ui <- .read_lfgwc_v32d("R", "LFGWC", "lfgwc_ui.R")

  for (algorithm in c("abc", "fpa", "gsa", "gwo", "hho", "ifa", "pso", "tlbo", "woa")) {
    expect_match(
      ui,
      paste0('= "', algorithm, '"'),
      fixed = TRUE,
      info = algorithm
    )
  }

  expect_match(ui, 'numericInput("lfgwc_max_nfe"', fixed = TRUE)
  expect_match(ui, "value = 2000, min = 100, step = 100", fixed = TRUE)
  expect_match(ui, "same LFGWC evaluator and NFE budget", fixed = TRUE)
  expect_false(grepl("initial centroid optimization", ui, fixed = TRUE))
  expect_false(grepl("DLFGWC-PSO", ui, fixed = TRUE))
})

test_that("primary LFGWC server path builds algorithm-specific optimizer params", {
  server <- .read_lfgwc_v32d("R", "LFGWC", "lfgwc_server.R")

  expect_match(server, "switch(\n          algo,", fixed = TRUE)
  expect_match(server, "max_nfe = as.integer(input$lfgwc_max_nfe)", fixed = TRUE)
  expect_match(server, "vmax = input$lfgwc_gsa_vmax", fixed = TRUE)
  expect_match(server, "vmax = input$lfgwc_pso_vmax", fixed = TRUE)
  expect_match(server, "gamma    = input$lfgwc_fpa_gamma", fixed = TRUE)
  expect_match(server, "gamma    = input$lfgwc_ifa_gamma", fixed = TRUE)
  expect_match(server, "algo = input$lfgwc_hho_algo", fixed = TRUE)
  expect_match(server, "pso        = FALSE", fixed = TRUE)
})

test_that("stability path uses the same algorithm-specific contract and NFE budget", {
  server <- .read_lfgwc_v32d("R", "LFGWC", "lfgwc_server.R")

  expect_match(server, "stability_algo <- res$algorithm", fixed = TRUE)
  expect_match(server, "switch(\n          stability_algo,", fixed = TRUE)
  expect_match(
    server,
    "max_nfe = as.integer(input$lfgwc_max_nfe %||% 2000L)",
    fixed = TRUE
  )
  expect_match(server, "vmax = input$lfgwc_gsa_vmax %||% 0.7", fixed = TRUE)
  expect_match(server, "vmax = input$lfgwc_pso_vmax %||% 0.8", fixed = TRUE)
  expect_match(server, "algo = input$lfgwc_hho_algo %||% \"bairathi\"", fixed = TRUE)
})
