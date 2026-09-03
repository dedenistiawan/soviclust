# R/data.R
# =============================================================================
# Documentation for sample datasets included in the soviclust package
# Datasets are stored in inst/extdata/ and accessed via system.file()
# =============================================================================

#' SoVI Data for 514 Indonesian Districts (2015)
#'
#' @description
#' Social Vulnerability Index (SoVI) dataset for 514 Indonesian districts
#' (Kabupaten/Kota) in 2015, containing 15 socioeconomic and vulnerability indicators.
#'
#' @format A data frame with 514 rows and 17 columns:
#' \describe{
#'   \item{DISTRICTCODE}{Unique district ID code.}
#'   \item{KABUPATEN}{District name.}
#'   \item{AGE014}{Percentage of population aged 0--14 years (\%).}
#'   \item{FEMPOP}{Percentage of female population (\%).}
#'   \item{AGE65P}{Percentage of population aged 65 years and above (\%).}
#'   \item{FEMHH}{Percentage of households with female head of household (\%).}
#'   \item{HHSIZE}{Average household size (persons).}
#'   \item{NOELEC}{Percentage of households without electricity access (\%).}
#'   \item{LOWEDUC}{Percentage of population with low education level (\%).}
#'   \item{POPGRW}{Population growth rate (\%).}
#'   \item{POOR}{Percentage of population living in poverty (\%).}
#'   \item{ILLIT}{Illiteracy rate (\%).}
#'   \item{NOTRAIN}{Percentage of population without formal job training (\%).}
#'   \item{DISAREA}{Percentage of district area classified as disaster-prone (\%).}
#'   \item{RENTHH}{Percentage of households renting their dwelling (\%).}
#'   \item{NOSAN}{Percentage of households without adequate sanitation (\%).}
#'   \item{NOWATER}{Percentage of households without clean water access (\%).}
#' }
#'
#' @source
#' Data compiled from Statistics Indonesia (BPS), 2015.
#' Methodology follows Cutter et al. (2003).
#'
#' @examples
#' # Access the sample dataset
#' path <- system.file("extdata", "sovi_data_kab_514_15.xlsx",
#'                     package = "soviclust")
#' df <- readxl::read_excel(path)
#' head(df[, 1:5])
#'
#' @references
#' Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003).
#' Social Vulnerability to Environmental Hazards.
#' \emph{Social Science Quarterly}, 84(2), 242--261.
#' \doi{10.1111/1540-6237.8402002}
#'
#' @name sovi_data_kab_514_15
#' @aliases sovi_data_kab_514_15.xlsx
NULL


#' Centroid Coordinates for 514 Indonesian Districts
#'
#' @description
#' Geographic centroid coordinates (longitude and latitude) for 514 Indonesian
#' districts. Used to compute Haversine distance matrices for FGWC, LFGWC,
#' and ALFGWC clustering analyses.
#'
#' @format A data frame with 514 rows and at least 3 columns:
#' \describe{
#'   \item{DISTRICTCODE}{Unique district ID code.}
#'   \item{longitude}{Longitude in decimal degrees (WGS84).
#'     Range: 95--141 (Indonesian territory).}
#'   \item{latitude}{Latitude in decimal degrees (WGS84).
#'     Range: -11 to 6 (Indonesian territory).}
#' }
#'
#' @details
#' Inter-district distances are computed automatically using the
#' \strong{Haversine Distance} formula (unit: kilometers) when selecting
#' the "Upload Longitude & Latitude" option in the Cluster Analysis tab.
#'
#' @examples
#' # Access sample coordinates
#' path <- system.file("extdata", "Koordinat.xlsx", package = "soviclust")
#' coord <- readxl::read_excel(path)
#' head(coord)
#'
#' @name Koordinat
#' @aliases Koordinat.xlsx
NULL


#' Distance Matrix for 514 Indonesian Districts
#'
#' @description
#' A symmetric 514 x 514 inter-district distance matrix for Indonesian districts.
#' Diagonal values are 0 (distance from a district to itself).
#' Used as the distance matrix input for FGWC, LFGWC, and ALFGWC analyses.
#'
#' @format A numeric matrix (514 x 514) or data frame with 514 rows and
#' 514+ columns (the first column may contain district labels):
#' \describe{
#'   \item{[row i, column j]}{Distance between district i and district j.
#'     Units follow the source data (may be km or other units).}
#' }
#'
#' @details
#' The matrix is symmetric: \eqn{d(i,j) = d(j,i)}.
#' Diagonal values: \eqn{d(i,i) = 0}.
#'
#' As an alternative, use \code{Koordinat.xlsx} and let the application
#' compute Haversine distances automatically.
#'
#' @examples
#' # Access the sample distance matrix
#' path <- system.file("extdata", "Distance_matrix_514.xlsx",
#'                     package = "soviclust")
#' df  <- readxl::read_excel(path)
#' # Remove label column if present
#' if (!is.numeric(df[[1]])) df <- df[, -1]
#' mat <- data.matrix(df)
#' dim(mat)  # [1] 514 514
#'
#' @name Distance_matrix_514
#' @aliases Distance_matrix_514.xlsx
NULL


#' Population Data for 514 Indonesian Districts
#'
#' @description
#' Population count data for 514 Indonesian districts. Used as a population
#' weight vector in FGWC, LFGWC, and ALFGWC clustering analyses.
#'
#' @format A data frame with 514 rows and at least 2 columns:
#' \describe{
#'   \item{DISTRICTCODE}{Unique district ID code.}
#'   \item{POPULATION}{Total population (persons). All values are positive.}
#' }
#'
#' @examples
#' # Access sample population data
#' path <- system.file("extdata", "sovi_data_pop_514.xlsx",
#'                     package = "soviclust")
#' pop_df <- readxl::read_excel(path)
#' # Extract population vector (first numeric column)
#' pop <- as.numeric(pop_df[[which(sapply(pop_df, is.numeric))[1]]])
#' summary(pop)
#'
#' @name sovi_data_pop_514
#' @aliases sovi_data_pop_514.xlsx
NULL
