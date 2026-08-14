#' Jalankan Aplikasi SoVI Interactive Mapper
#'
#' @description
#' Membuka aplikasi Shiny **SoVI Interactive Mapper** di browser default Anda.
#' Aplikasi ini menyediakan platform interaktif untuk menghitung, memvisualisasikan,
#' dan menganalisis Social Vulnerability Index (SoVI) di tingkat wilayah administratif.
#'
#' @param ... Argumen tambahan yang diteruskan ke \code{\link[shiny]{runApp}}.
#'   Misalnya: \code{port = 3838}, \code{launch.browser = FALSE}.
#'
#' @return Tidak mengembalikan nilai (dipanggil karena side-effect menjalankan app).
#'
#' @examples
#' \dontrun{
#'   # Cara paling sederhana — buka di browser default
#'   soviclust::run_app()
#'
#'   # Tentukan port tertentu
#'   soviclust::run_app(port = 3838)
#'
#'   # Jalankan tanpa membuka browser otomatis
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
      "Package berikut diperlukan tetapi belum terinstall:\n",
      paste(" -", missing_pkgs, collapse = "\n"), "\n\n",
      "Silakan install dengan:\n",
      "  install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))",
      call. = FALSE
    )
  }

  # ── Jalankan aplikasi ───────────────────────────────────────────────────────
  app_dir <- system.file("app", package = "soviclust")
  if (app_dir == "") {
    stop(
      "Tidak dapat menemukan direktori aplikasi.\n",
      "Pastikan package 'soviclust' sudah terinstall dengan benar.\n",
      "Coba: remotes::install_github('dedenistiawan/soviclust')",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, ...)
}
