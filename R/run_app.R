#' Launch the SoVI Interactive Mapper Application
#'
#' @description
#' Opens the **SoVI Interactive Mapper** Shiny application in your default browser.
#' This application provides an interactive platform for computing, visualizing,
#' and analyzing the Social Vulnerability Index (SoVI) at the administrative unit level.
#'
#' By default, the application opens in an external browser
#' (not the RStudio Viewer pane) at `http://127.0.0.1:<port>`.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'   For example: \code{port = 3838}, \code{launch.browser = FALSE}.
#'
#' @return Invisible. Called for its side effect of launching the Shiny app.
#'
#' @examples
#' \dontrun{
#'   # Simplest usage — open in default browser
#'   soviclust::run_app()
#'
#'   # Specify a custom port
#'   soviclust::run_app(port = 3838)
#'
#'   # Run without automatically opening a browser
#'   soviclust::run_app(launch.browser = FALSE)
#' }
#'
#' @export
run_app <- function(...) {

  # ── Cek dependensi yang diperlukan ──────────────────────────────────────────
  required_pkgs <- c(
    "shinydashboard", "shinyjs", "shinyWidgets",
    "DT", "ggplot2", "dplyr", "tidyr", "readxl",
    "psych", "classInt", "sf", "tmap", "leaflet",
    "ClustGeo", "spdep", "cluster", "fmsb",
    "RColorBrewer", "patchwork", "rdist",
    "stabledist", "MASS", "Rtsne", "uwot"
  )

  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                        quietly = TRUE, FUN.VALUE = logical(1))]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required but not installed:\n",
      paste(" -", missing_pkgs, collapse = "\n"), "\n\n",
      "Please install them with:\n",
      "  install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))",
      call. = FALSE
    )
  }

  # ── Jalankan aplikasi ───────────────────────────────────────────────────────
  app_dir <- system.file("app", package = "soviclust")
  if (app_dir == "") {
    stop(
      "Cannot find the application directory.\n",
      "Please ensure the 'soviclust' package is installed correctly.\n",
      "Try: remotes::install_github('dedenistiawan/soviclust')",
      call. = FALSE
    )
  }

  # Secara default buka di browser eksternal (bukan RStudio Viewer pane).
  # `launch.browser = TRUE` (logical) memaksa shiny memanggil
  # utils::browseURL(), sehingga mem-bypass opsi shiny.launch.browser
  # yang biasanya diarahkan RStudio ke Viewer pane. Pengguna tetap bisa
  # override, mis. soviclust::run_app(launch.browser = FALSE).
  dots <- list(...)
  if (is.null(dots[["launch.browser"]])) {
    dots[["launch.browser"]] <- TRUE
  }

  do.call(shiny::runApp, c(list(appDir = app_dir), dots))
}
