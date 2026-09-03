# R/soviclust-package.R
# =============================================================================
# Package-level documentation for soviclust
# This file generates the ?soviclust help page in R
# =============================================================================

#' soviclust: Social Vulnerability Index Analysis and Spatial Clustering
#'
#' @description
#' The **soviclust** package provides an interactive Shiny application for
#' computing, visualizing, and analyzing the **Social Vulnerability Index (SoVI)**
#' at the administrative unit level. It implements the methodology of
#' Cutter et al. (2003) with several enhancements.
#'
#' @section Getting Started:
#' Launch the application with:
#' ```r
#' library(soviclust)
#' soviclust::run_app()
#' ```
#'
#' @section Key Features:
#' \itemize{
#'   \item \strong{Import Data} - Upload Excel/CSV data and shapefiles,
#'     or use the bundled sample dataset of 514 Indonesian districts.
#'   \item \strong{Variable Config} - Configure variables and direction (+/-)
#'     for each SoVI indicator.
#'   \item \strong{Method Comparison} - Compare 3 direction methods:
#'     Theory-Based, Loading Sign, and Cutter Method.
#'   \item \strong{SoVI Computation} - Automated pipeline: Z-score ->
#'     PCA (KMO, Bartlett, Varimax) -> weighted aggregation -> Jenks classification.
#'   \item \strong{Extended Analysis} - Moran I, LISA, dominant component,
#'     component profile (radar chart), and sensitivity analysis.
#'   \item \strong{Cluster Analysis} - Four methods: ClustGeo, FGWC, LFGWC,
#'     and ALFGWC with 9 metaheuristic optimizers.
#'   \item \strong{SoVI Analysis} - Per-variable choropleth maps with GVF index.
#'   \item \strong{Downloads} - Export results as CSV and high-resolution PNG maps.
#' }
#'
#' @section Bundled Sample Data:
#' The package includes ready-to-use sample datasets in \code{inst/extdata/}:
#' \itemize{
#'   \item \code{sovi_data_kab_514_15.xlsx} - SoVI data for 514 Indonesian
#'     districts (15 variables, year 2015). ID column: \code{DISTRICTCODE},
#'     Name column: \code{KABUPATEN}.
#'   \item \code{Koordinat.xlsx} - Centroid coordinates (longitude, latitude)
#'     for 514 districts.
#'   \item \code{Distance_matrix_514.xlsx} - Inter-district distance matrix
#'     (514 x 514).
#'   \item \code{sovi_data_pop_514.xlsx} - Population data for 514 districts.
#' }
#'
#' @section SoVI Methodology:
#' Based on Cutter et al. (2003), SoVI is computed using the formula:
#' \deqn{RC_k = \sum_i \left(\frac{|\lambda_{ik}|}{\sum_j |\lambda_{jk}|} \times d_i \times z_i\right)}
#' \deqn{SoVI^* = \frac{SoVI - \min}{\max - \min} \in [0, 1]}
#' where \eqn{\lambda_{ik}} is the factor loading of variable \eqn{i} on
#' component \eqn{k}, \eqn{d_i} is the direction (+1 or -1), and
#' \eqn{z_i} is the standardized (Z-score) value.
#'
#' @section References:
#' \itemize{
#'   \item Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003).
#'     Social Vulnerability to Environmental Hazards.
#'     \emph{Social Science Quarterly}, 84(2), 242--261.
#'     \doi{10.1111/1540-6237.8402002}
#'   \item Wijayanto, H., & Purwaningsih, T. (2020). Fuzzy Geographically
#'     Weighted Clustering Using Artificial Bee Colony.
#'     \emph{Journal of Physics: Conference Series}.
#'   \item Grekousis, G. (2020). \emph{Spatial Analysis Methods and Practice}.
#'     Cambridge University Press. \doi{10.1017/9781108614528}
#'   \item Kurniawan, R., Nasution, B.I., Agustina, N., & Yuniarto, B. (2022).
#'     Revisiting social vulnerability analysis in Indonesia data.
#'     \emph{Data in Brief}, 40, 107743. \doi{10.1016/j.dib.2021.107743}
#' }
#'
#' @author Deden Istiawan \email{dedenistiawan@@gmail.com}
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{run_app}} - Main function to launch the application
#'   \item GitHub: \url{https://github.com/dedenistiawan/soviclust}
#'   \item Bug reports: \url{https://github.com/dedenistiawan/soviclust/issues}
#' }
#'
#' @keywords internal
"_PACKAGE"
