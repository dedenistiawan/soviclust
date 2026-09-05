# R/sample_data.R
# =============================================================================
# Helper functions to access soviclust sample datasets directly from R
# without opening the Shiny application.
# =============================================================================

#' Read SoVI Data for 514 Indonesian Districts
#'
#' Reads the bundled SoVI sample dataset for 514 Indonesian regencies/cities.
#'
#' @return A `data.frame` with 514 rows containing district identifiers,
#'   district names, and Social Vulnerability Index (SoVI) indicators.
#'
#' @examples
#' df <- sovi_sample_data()
#' head(df[, 1:5])
#' dim(df)
#'
#' @export
sovi_sample_data <- function() {
  path <- system.file(
    "extdata", "sovi_data_kab_514_15.xlsx",
    package = "soviclust"
  )

  if (!nzchar(path) || !file.exists(path)) {
    stop(
      "File 'sovi_data_kab_514_15.xlsx' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }

  as.data.frame(readxl::read_excel(path))
}


#' Read Administrative Boundaries for 514 Indonesian Districts
#'
#' Reads the bundled administrative-boundary object for 514 Indonesian
#' regencies/cities. The spatial object is stored internally as an RDS file
#' to reduce package size.
#'
#' @return An `sf` object with 514 spatial features.
#'
#' @examples
#' shp <- sovi_sample_shapefile()
#' class(shp)
#' nrow(shp)
#' names(shp)
#'
#' @export
sovi_sample_shapefile <- function() {
  path <- system.file(
    "app", "map", "514_kabupaten.rds",
    package = "soviclust"
  )

  if (!nzchar(path) || !file.exists(path)) {
    stop(
      "Spatial boundary file '514_kabupaten.rds' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }

  shp <- readRDS(path)

  if (!inherits(shp, "sf")) {
    stop(
      "The bundled spatial boundary object is not a valid 'sf' object.",
      call. = FALSE
    )
  }

  shp
}


#' Read Centroid Coordinates for 514 Indonesian Districts
#'
#' Reads bundled centroid longitude and latitude data.
#'
#' @return A `data.frame` with 514 rows.
#'
#' @examples
#' coord <- sovi_sample_coords()
#' head(coord)
#' dim(coord)
#'
#' @export
sovi_sample_coords <- function() {
  path <- system.file("extdata", "Koordinat.xlsx", package = "soviclust")

  if (!nzchar(path) || !file.exists(path)) {
    stop(
      "File 'Koordinat.xlsx' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }

  as.data.frame(readxl::read_excel(path))
}


#' Read Population Data for 514 Indonesian Districts
#'
#' Reads the bundled population data used by spatial-interaction weighting.
#'
#' @return A `data.frame` with 514 rows.
#'
#' @examples
#' pop <- sovi_sample_pop()
#' head(pop)
#' nrow(pop)
#'
#' @export
sovi_sample_pop <- function() {
  path <- system.file(
    "extdata", "sovi_data_pop_514.xlsx",
    package = "soviclust"
  )

  if (!nzchar(path) || !file.exists(path)) {
    stop(
      "File 'sovi_data_pop_514.xlsx' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }

  as.data.frame(readxl::read_excel(path))
}


#' Read Distance Matrix for 514 Indonesian Districts
#'
#' Reads the bundled inter-district distance matrix.
#'
#' @return A numeric 514 x 514 matrix.
#'
#' @examples
#' mat <- sovi_sample_distmat()
#' dim(mat)
#' diag(mat)[1:5]
#'
#' @export
sovi_sample_distmat <- function() {
  path <- system.file(
    "extdata", "Distance_matrix_514.xlsx",
    package = "soviclust"
  )

  if (!nzchar(path) || !file.exists(path)) {
    stop(
      "File 'Distance_matrix_514.xlsx' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }

  df <- as.data.frame(readxl::read_excel(path))

  if (ncol(df) > 0L && !is.numeric(df[[1L]])) {
    df <- df[, -1L, drop = FALSE]
  }

  data.matrix(df)
}
