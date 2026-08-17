# Rencana Pengembangan Package `soviclust`

> Dokumen ini berisi saran dan rencana pengembangan package `soviclust`
> yang diprioritaskan berdasarkan dampak dan kemudahan implementasi.

---

## Ringkasan Status Saat Ini

| Komponen | Status |
|---|---|
| Shiny app berjalan via `run_app()` | ✅ Selesai |
| Instalasi dari GitHub | ✅ Selesai |
| Data sampel bawaan | ✅ Selesai |
| Pesan startup `.onAttach()` | ✅ Selesai |
| R CMD check 0 errors/warnings/notes | ✅ Selesai |
| CRAN-ready | ⏳ Belum |
| Unit testing | ❌ Belum |
| Dokumentasi fungsi (roxygen2) | ⚠️ Sebagian |
| Vignette / artikel | ❌ Belum |
| Website pkgdown | ❌ Belum |
| CI/CD GitHub Actions | ❌ Belum |

---

## Prioritas 1 — Segera (Dampak Tinggi, Mudah)

### 1.1 Tambah Unit Test (`testthat`)

Unit test memastikan package tidak rusak ketika ada perubahan kode.

**Yang perlu ditest:**
- `run_app()` dapat menemukan direktori `inst/app/`
- Dataset sampel dapat dibaca dengan benar
- Fungsi helper (parsing matriks jarak, populasi) bekerja sesuai harapan

```r
# Buat struktur test
usethis::use_testthat()
usethis::use_test("run_app")
usethis::use_test("sample_data")
```

**File yang perlu dibuat:**
```
tests/
└── testthat/
    ├── test-run_app.R
    └── test-sample_data.R
```

---

### 1.2 Lengkapi Dokumentasi Roxygen2

Saat ini hanya `run_app()` yang terdokumentasi. Tambahkan dokumentasi untuk:

- Deskripsi package (`R/soviclust-package.R`)
- Semua dataset di `inst/extdata/` (buat `R/data.R`)
- Fungsi internal yang penting

```r
# Buat file dokumentasi package
usethis::use_package_doc()

# Contoh isi R/soviclust-package.R:
#' @title soviclust: Social Vulnerability Index Analysis
#' @description Interactive Shiny application for SoVI computation
#'   and spatial clustering analysis.
#' @keywords internal
"_PACKAGE"
```

---

### 1.3 Tambah `NEWS.md` (Changelog)

Penting untuk tracking perubahan antar versi, terutama menjelang CRAN.

```markdown
# soviclust 0.1.0

## Initial Release

* Aplikasi Shiny interaktif untuk analisis SoVI
* Mendukung ClustGeo, FGWC, LFGWC, dan ALFGWC
* Data sampel 514 Kabupaten/Kota Indonesia (2015)
* Instalasi via `remotes::install_github()`
```

---

### 1.4 CI/CD dengan GitHub Actions

Otomatis jalankan `R CMD check` setiap kali ada `git push`.

```r
# Setup GitHub Actions
usethis::use_github_action("check-standard")
usethis::use_github_action_badge("check-standard")
```

Hasilnya: badge [![R-CMD-check](https://github.com/...)] di README.

---

## Prioritas 2 — Jangka Menengah (Kualitas & Keterbacaan)

### 2.1 Vignette: Tutorial Lengkap

Vignette adalah dokumen HTML yang menjelaskan cara pakai package
secara step-by-step. Sangat berguna untuk pengguna baru.

**Vignette yang disarankan:**

| Judul | Isi |
|-------|-----|
| `getting-started.Rmd` | Instalasi, data sampel, menjalankan app |
| `data-preparation.Rmd` | Format data input, tips persiapan data |
| `sovi-methodology.Rmd` | Teori SoVI, formula, interpretasi hasil |
| `clustering-methods.Rmd` | Perbandingan ClustGeo, FGWC, LFGWC, ALFGWC |

```r
usethis::use_vignette("getting-started", "Getting Started with soviclust")
```

---

### 2.2 Website Dokumentasi (`pkgdown`)

Website otomatis dari dokumentasi roxygen2 dan vignette.
Dapat di-host gratis di GitHub Pages.

```r
# Setup pkgdown
usethis::use_pkgdown()
usethis::use_github_action("pkgdown")

# Build lokal untuk preview
pkgdown::build_site()
```

Website akan tersedia di: `https://dedenistiawan.github.io/soviclust/`

---

### 2.3 Tambah Fungsi Pembantu yang Dapat Dipanggil Langsung

Saat ini semua fungsionalitas hanya bisa diakses lewat Shiny.
Pertimbangkan menambahkan fungsi R yang bisa dipanggil langsung:

```r
# Contoh fungsi yang bisa ditambahkan:

# Baca dataset sampel
sovi_sample_data()        # returns data.frame
sovi_sample_shapefile()   # returns sf object
sovi_sample_coords()      # returns koordinat lon/lat

# Contoh penggunaan:
library(soviclust)
df  <- sovi_sample_data()
shp <- sovi_sample_shapefile()
head(df)
```

Ini memudahkan pengguna yang ingin mengakses data tanpa membuka Shiny.

---

### 2.4 Perbaikan Error Handling di Shiny

Tambahkan pesan error yang lebih informatif ketika:

- File upload format salah
- Jumlah baris data tidak sesuai shapefile
- Kolom ID tidak cocok antara data dan shapefile
- Dataset terlalu besar untuk divisualisasikan

---

## Prioritas 3 — Jangka Panjang (Fitur Baru)

### 3.1 Laporan Otomatis (PDF/HTML)

Tambahkan fitur **"Generate Report"** di tab Downloads yang menghasilkan
laporan lengkap berformat PDF atau HTML menggunakan R Markdown/Quarto.

Isi laporan:
- Ringkasan data (statistik deskriptif)
- Hasil PCA dan diagnostik
- Peta SoVI
- Hasil clustering
- Interpretasi singkat otomatis

```r
# Dependensi yang diperlukan:
# - rmarkdown
# - knitr
# - ggplot2 (sudah ada)
```

---

### 3.2 Dukungan Multi-Bahasa (i18n)

Saat ini antarmuka hanya tersedia dalam Bahasa Indonesia.
Tambahkan opsi **Bahasa Inggris** untuk jangkauan pengguna yang lebih luas.

Implementasi: Buat file `inst/app/i18n/id.json` dan `en.json`,
gunakan package `shiny.i18n` untuk switching bahasa.

---

### 3.3 Optimasi Performa untuk Dataset Besar

Untuk dataset > 1000 wilayah:

- Gunakan `data.table` untuk operasi data yang lebih cepat
- Implementasi `future`/`promises` untuk komputasi asinkron (non-blocking UI)
- Progress bar yang lebih informatif untuk proses lama (PCA, clustering)
- Caching hasil PCA menggunakan `shiny::reactiveVal` + `memoise`

---

### 3.4 Tambah Metode Clustering Baru

| Metode | Package | Keterangan |
|--------|---------|-----------|
| **K-Means Spasial** | `skmeans` | Alternatif sederhana |
| **DBSCAN Spasial** | `dbscan` | Clustering berbasis densitas |
| **Regionalization** | `rgeoda` | SKATER, REDCAP, Max-P |
| **SOM** | `kohonen` | Self-Organizing Map |

---

### 3.5 Integrasi Data Eksternal

Tambahkan kemampuan import data dari sumber eksternal:

- **BPS API** — Data langsung dari Badan Pusat Statistik Indonesia
- **OpenStreetMap** — Shapefile wilayah dari Nominatim
- **Google Sheets** — Import spreadsheet publik

---

### 3.6 Mode Perbandingan Multi-Dataset

Fitur untuk membandingkan SoVI antar **tahun** atau antar **dataset**:

- Upload 2+ dataset pada periode berbeda
- Visualisasi perubahan SoVI (delta map)
- Analisis trend kerentanan per wilayah

---

## Prioritas 4 — Persiapan CRAN

Jika ingin mempublikasikan ke CRAN, langkah yang diperlukan:

### Checklist CRAN

- [ ] `R CMD check --as-cran` tanpa ERROR, WARNING, NOTE
- [ ] Semua fungsi terdokumentasi dengan `@param`, `@return`, `@examples`
- [ ] `@examples` dapat dijalankan tanpa error (atau dibungkus `\dontrun{}`)
- [ ] File `DESCRIPTION` lengkap (Title, Description, Authors, URL)
- [ ] `NEWS.md` berisi changelog
- [ ] Lisensi jelas (MIT, GPL-3, dll.)
- [ ] Tidak ada file besar (> 5 MB) di `inst/extdata/` tanpa justifikasi

### Kendala Utama untuk CRAN

> ⚠️ **File besar**: `Distance_matrix_514.xlsx` berukuran ~3.8 MB.
> CRAN membatasi ukuran package total ≤ 5 MB.
>
> **Solusi**: Pindahkan dataset besar ke package terpisah (data package)
> atau gunakan `drat` repository untuk distribusi.

```r
# Cek ukuran total package
devtools::check_built_package_size()
```

---

## Roadmap Versi

| Versi | Target | Fokus |
|-------|--------|-------|
| **0.1.x** | Sekarang | Bugfix, stabilitas, README |
| **0.2.0** | 1-2 bulan | Unit test, dokumentasi lengkap, CI/CD |
| **0.3.0** | 3-4 bulan | Vignette, pkgdown, fungsi pembantu |
| **0.4.0** | 5-6 bulan | Laporan otomatis, multi-bahasa |
| **1.0.0** | > 6 bulan | CRAN submission atau stable release |

---

## Referensi Pengembangan Package R

- [R Packages (2e) — Hadley Wickham](https://r-pkgs.org/)
- [rOpenSci Packaging Guide](https://devguide.ropensci.org/)
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [pkgdown documentation](https://pkgdown.r-lib.org/)
- [GitHub Actions for R](https://github.com/r-lib/actions)

---

*Dokumen ini dibuat pada: 2026-08-15*
*Package version: soviclust 0.1.0*
