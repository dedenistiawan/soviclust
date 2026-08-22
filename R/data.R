# R/data.R
# =============================================================================
# Dokumentasi dataset sampel yang disertakan dalam package soviclust
# Dataset berada di inst/extdata/ dan diakses via system.file()
# =============================================================================

#' Data SoVI 514 Kabupaten/Kota Indonesia (2015)
#'
#' @description
#' Dataset Social Vulnerability Index (SoVI) untuk 514 Kabupaten/Kota
#' di Indonesia tahun 2015, berisi 15 indikator sosial-ekonomi dan kerentanan.
#'
#' @format Data frame dengan 514 baris dan 17 kolom:
#' \describe{
#'   \item{DISTRICTCODE}{Kode unik wilayah Kabupaten/Kota (ID wilayah).}
#'   \item{KABUPATEN}{Nama Kabupaten/Kota.}
#'   \item{AGE014}{Persentase penduduk usia 0--14 tahun (\%).}
#'   \item{FEMPOP}{Persentase penduduk perempuan (\%).}
#'   \item{AGE65P}{Persentase penduduk usia 65 tahun ke atas (\%).}
#'   \item{FEMHH}{Persentase rumah tangga dengan kepala keluarga perempuan (\%).}
#'   \item{HHSIZE}{Rata-rata jumlah anggota rumah tangga (jiwa).}
#'   \item{NOELEC}{Persentase rumah tangga tanpa akses listrik (\%).}
#'   \item{LOWEDUC}{Persentase penduduk dengan pendidikan rendah (\%).}
#'   \item{POPGRW}{Tingkat pertumbuhan penduduk (\%).}
#'   \item{POOR}{Persentase penduduk miskin (\%).}
#'   \item{ILLIT}{Tingkat buta huruf (\%).}
#'   \item{NOTRAIN}{Persentase penduduk tanpa pelatihan kerja formal (\%).}
#'   \item{DISAREA}{Persentase luas wilayah rawan bencana (\%).}
#'   \item{RENTHH}{Persentase rumah tangga yang menyewa tempat tinggal (\%).}
#'   \item{NOSAN}{Persentase rumah tangga tanpa fasilitas sanitasi layak (\%).}
#'   \item{NOWATER}{Persentase rumah tangga tanpa akses air bersih (\%).}
#' }
#'
#' @source
#' Data diolah dari Badan Pusat Statistik (BPS) Indonesia, tahun 2015.
#' Metodologi mengacu pada Cutter et al. (2003).
#'
#' @examples
#' # Akses dataset sampel
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


#' Koordinat Centroid 514 Kabupaten/Kota Indonesia
#'
#' @description
#' Dataset koordinat geografis (longitude dan latitude) titik centroid
#' 514 Kabupaten/Kota di Indonesia. Digunakan untuk menghitung matriks
#' jarak Haversine pada analisis FGWC, LFGWC, dan ALFGWC.
#'
#' @format Data frame dengan 514 baris dan minimal 3 kolom:
#' \describe{
#'   \item{DISTRICTCODE}{Kode unik wilayah Kabupaten/Kota.}
#'   \item{longitude}{Koordinat bujur (derajat desimal, WGS84).
#'     Rentang: 95--141 (wilayah Indonesia).}
#'   \item{latitude}{Koordinat lintang (derajat desimal, WGS84).
#'     Rentang: -11 hingga 6 (wilayah Indonesia).}
#' }
#'
#' @details
#' Jarak antar wilayah dihitung otomatis menggunakan formula
#' \strong{Haversine Distance} (satuan: kilometer) ketika memilih opsi
#' "Upload Longitude & Latitude" pada tab Cluster Analysis.
#'
#' @examples
#' # Akses koordinat sampel
#' path <- system.file("extdata", "Koordinat.xlsx", package = "soviclust")
#' coord <- readxl::read_excel(path)
#' head(coord)
#'
#' @name Koordinat
#' @aliases Koordinat.xlsx
NULL


#' Matriks Jarak 514 Kabupaten/Kota Indonesia
#'
#' @description
#' Matriks jarak simetris berukuran 514 × 514 antar Kabupaten/Kota Indonesia.
#' Nilai diagonal adalah 0 (jarak suatu wilayah ke dirinya sendiri).
#' Digunakan sebagai input matriks jarak pada analisis FGWC, LFGWC, dan ALFGWC.
#'
#' @format Matriks numerik (514 × 514) atau data frame dengan 514 baris dan
#' 514+ kolom (kolom pertama mungkin berisi label wilayah):
#' \describe{
#'   \item{[baris i, kolom j]}{Jarak antara Kabupaten/Kota ke-i dan ke-j.
#'     Satuan mengikuti sumber data (dapat berupa km atau unit lain).}
#' }
#'
#' @details
#' Matriks ini bersifat simetris: \eqn{d(i,j) = d(j,i)}.
#' Nilai diagonal: \eqn{d(i,i) = 0}.
#'
#' Sebagai alternatif, gunakan \code{Koordinat.xlsx} dan biarkan aplikasi
#' menghitung jarak Haversine secara otomatis.
#'
#' @examples
#' # Akses matriks jarak sampel
#' path <- system.file("extdata", "Distance_matrix_514.xlsx",
#'                     package = "soviclust")
#' df  <- readxl::read_excel(path)
#' # Buang kolom label jika ada
#' if (!is.numeric(df[[1]])) df <- df[, -1]
#' mat <- data.matrix(df)
#' dim(mat)  # [1] 514 514
#'
#' @name Distance_matrix_514
#' @aliases Distance_matrix_514.xlsx
NULL


#' Data Populasi 514 Kabupaten/Kota Indonesia
#'
#' @description
#' Dataset jumlah penduduk untuk 514 Kabupaten/Kota di Indonesia.
#' Digunakan sebagai vektor bobot populasi pada analisis FGWC, LFGWC,
#' dan ALFGWC.
#'
#' @format Data frame dengan 514 baris dan minimal 2 kolom:
#' \describe{
#'   \item{DISTRICTCODE}{Kode unik wilayah Kabupaten/Kota.}
#'   \item{POPULATION}{Jumlah penduduk (jiwa). Semua nilai positif.}
#' }
#'
#' @examples
#' # Akses data populasi sampel
#' path <- system.file("extdata", "sovi_data_pop_514.xlsx",
#'                     package = "soviclust")
#' pop_df <- readxl::read_excel(path)
#' # Ekstrak vektor populasi (kolom numerik pertama)
#' pop <- as.numeric(pop_df[[which(sapply(pop_df, is.numeric))[1]]])
#' summary(pop)
#'
#' @name sovi_data_pop_514
#' @aliases sovi_data_pop_514.xlsx
NULL
