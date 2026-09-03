# R/sample_data.R
# =============================================================================
# Helper functions to access soviclust sample datasets directly from R
# without opening the Shiny application
# =============================================================================


#' Read SoVI Data for 514 Indonesian Districts (2015)
#'
#' @description
#' Reads the bundled SoVI sample dataset for 514 Indonesian districts (year 2015)
#' included in the package. This dataset is ready to use for exploration
#' or as a data format template for your own analysis.
#'
#' @return A `data.frame` with 514 rows and 17 columns:
#'   \describe{
#'     \item{DISTRICTCODE}{Unique district ID code.}
#'     \item{KABUPATEN}{District name.}
#'     \item{AGE014, FEMPOP, AGE65P, ...}{SoVI indicator variables (15 variables).}
#'   }
#'
#' @examples
#' df <- sovi_sample_data()
#' head(df[, 1:5])
#' dim(df)
#'
#' # View all variable names
#' names(df)
#'
#' # Descriptive statistics for numeric variables
#' summary(df[, -(1:2)])
#'
#' @export
sovi_sample_data <- function() {
  path <- system.file("extdata", "sovi_data_kab_514_15.xlsx",
                      package = "soviclust")
  if (path == "") {
    stop(
      "File 'sovi_data_kab_514_15.xlsx' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }
  as.data.frame(readxl::read_excel(path))
}


#' Read Shapefile for 514 Indonesian Districts
#'
#' @description
#' Reads the bundled administrative boundary shapefile for 514 Indonesian
#' districts included in the package as an `sf` object.
#'
#' @return An `sf` object with 514 features (Polygon/MultiPolygon).
#'   District ID column: `idkab`.
#'   Coordinate reference system: WGS84 (EPSG:4326).
#'
#' @examples
#' shp <- sovi_sample_shapefile()
#' class(shp)
#' names(shp)
#' nrow(shp)
#'
#' # Display a simple map
#' plot(sf::st_geometry(shp))
#'
#' # Join with SoVI data
#' df  <- sovi_sample_data()
#' shp_sovi <- merge(shp, df, by.x = "idkab", by.y = "DISTRICTCODE")
#'
#' @export
sovi_sample_shapefile <- function() {
  shp_path <- system.file("app", "map", "514_kabupaten.shp",
                          package = "soviclust")
  if (shp_path == "") {
    stop(
      "Shapefile '514_kabupaten.shp' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }
  sf::sf_use_s2(FALSE)
  sf::st_read(shp_path, quiet = TRUE)
}


#' Read Centroid Coordinates for 514 Indonesian Districts
#'
#' @description
#' Reads the geographic centroid coordinates (longitude and latitude) for
#' 514 Indonesian districts included in the package. Useful for computing
#' Haversine distance matrices manually or for visualization.
#'
#' @return A `data.frame` with 514 rows and 3 columns:
#'   \describe{
#'     \item{DISTRICTCODE}{Unique district ID code.}
#'     \item{longitude}{Longitude in decimal degrees (WGS84).}
#'     \item{latitude}{Latitude in decimal degrees (WGS84).}
#'   }
#'
#' @examples
#' coord <- sovi_sample_coords()
#' head(coord)
#'
#' # Coordinate ranges
#' range(coord$longitude)  # ~[95, 141]
#' range(coord$latitude)   # ~[-11, 6]
#'
#' # Plot centroid locations
#' plot(coord$longitude, coord$latitude,
#'      pch = 20, cex = 0.5, col = "steelblue",
#'      xlab = "Longitude", ylab = "Latitude",
#'      main = "Centroids of 514 Indonesian Districts")
#'
#' @export
sovi_sample_coords <- function() {
  path <- system.file("extdata", "Koordinat.xlsx", package = "soviclust")
  if (path == "") {
    stop(
      "File 'Koordinat.xlsx' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }
  df <- as.data.frame(readxl::read_excel(path))
  df
}


#' Read Population Data for 514 Indonesian Districts
#'
#' @description
#' Reads the population data for 514 Indonesian districts included in the
#' package. This data is used as a population weight in FGWC, LFGWC,
#' and ALFGWC clustering analyses.
#'
#' @return A `data.frame` with 514 rows and at least 2 columns (ID + population).
#'
#' @examples
#' pop_df <- sovi_sample_pop()
#' head(pop_df)
#'
#' # Extract population vector
#' pop <- pop_df[[which(sapply(pop_df, is.numeric))[1]]]
#' summary(pop)
#' hist(pop / 1e6, main = "Population Distribution (millions)",
#'      xlab = "Population (millions)", col = "steelblue")
#'
#' @export
sovi_sample_pop <- function() {
  path <- system.file("extdata", "sovi_data_pop_514.xlsx",
                      package = "soviclust")
  if (path == "") {
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
#' @description
#' Reads the symmetric 514 x 514 inter-district distance matrix for
#' Indonesian districts included in the package.
#'
#' @return A numeric matrix of dimensions 514 x 514. Diagonal values = 0.
#'
#' @note
#' This file is approximately 3.8 MB and may take a moment to load.
#'
#' @examples
#' mat <- sovi_sample_distmat()
#' dim(mat)        # [1] 514 514
#' diag(mat)[1:5]  # [1] 0 0 0 0 0
#'
#' # Distance from the first district to all others
#' hist(mat[1, -1], main = "Distance Distribution from District 1",
#'      xlab = "Distance", col = "coral")
#'
#' @export
sovi_sample_distmat <- function() {
  path <- system.file("extdata", "Distance_matrix_514.xlsx",
                      package = "soviclust")
  if (path == "") {
    stop(
      "File 'Distance_matrix_514.xlsx' not found.\n",
      "Please ensure the 'soviclust' package is installed correctly.",
      call. = FALSE
    )
  }
  df <- as.data.frame(readxl::read_excel(path))
  if (!is.numeric(df[[1]])) df <- df[, -1]
  data.matrix(df)
}
