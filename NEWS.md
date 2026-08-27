# NEWS.md — Changelog soviclust

Format mengacu pada [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
dan [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

# soviclust 0.5.0

### Fixed
- Trailing comma pada `dashboardHeader()` di `inst/app/ui.R` yang menyebabkan
  `run_app()` gagal dengan error "argument is missing, with no default".
- Perbaikan shapefile 19MB -> 0.94MB (RDS) untuk mengatasi timeout
  `install_github()`.
- Perbaikan duplicate i18n keys yang menyebabkan error `row.names` saat
  `run_app()`.
- Perbaikan urutan argumen `update_lang()` untuk kompatibilitas shiny.i18n
  v0.3.0.

### Changed
- Aplikasi kini English-only; language switcher dan opsi bahasa lain
  dihapus, default i18n diset ke `en`.
- Sumber data K-Means dan DBSCAN disatukan mengikuti pola 5 opsi FGWC
  (raw/raw_norm/std/sovi/rc).

### Removed
- 4 file extdata yang tidak lagi digunakan (~2.1MB): `sovi_data_kab_514.RData`,
  `Sovi_pop_514.RData`, `sovi_dist_514.RData`, `sovi_data_kab_514_19.xlsx`.

---

# soviclust 0.1.0


### Added

#### Aplikasi Shiny
- Aplikasi interaktif lengkap dapat dijalankan via `soviclust::run_app()`.
- Tab **Import / Load Data**: upload data Excel/CSV dan shapefile, atau gunakan
  data sampel 514 Kabupaten/Kota Indonesia bawaan.
- Tab **Variable Config**: konfigurasi variabel dan direction (+/-) per indikator.
- Tab **Method Comparison**: perbandingan 3 direction method (Theory-Based,
  Loading Sign, Cutter's Method) secara bersamaan.
- Tab **SoVI Computation**: pipeline otomatis Z-score → PCA → agregasi berbobot
  → klasifikasi Jenks Natural Breaks 5 kelas.
- Tab **Extended Analysis**: Moran's I, LISA, dominant component, component
  profile (radar chart), dan sensitivity analysis.
- Tab **Cluster Analysis** dengan 4 metode:
  - **ClustGeo** — clustering hierarki spasial dengan alpha optimal otomatis.
  - **FGWC** — Fuzzy Geographically Weighted Clustering dengan 9 algoritma
    metaheuristik (PSO, ABC, GWO, WOA, HHO, FPA, GSA, TLBO, IFA).
  - **LFGWC** — Local FGWC (Grekousis 2020).
  - **ALFGWC** — Adaptive LFGWC.
- Data sampel otomatis untuk FGWC/LFGWC/ALFGWC: tombol "Muat Data Sampel"
  memuat `Distance_matrix_514.xlsx`, `Koordinat.xlsx`, dan
  `sovi_data_pop_514.xlsx`.
- Tab **SoVI Analysis**: peta choropleth per variabel dengan GVF.
- Tab **Downloads**: ekspor hasil CSV dan peta PNG resolusi tinggi.

#### Package Infrastructure
- Fungsi `run_app()` dengan validasi dependensi dan path otomatis.
- Pesan startup informatif via `.onAttach()`.
- 4 dataset sampel bawaan di `inst/extdata/`:
  - `sovi_data_kab_514_15.xlsx` — 514 Kab/Kota, 15 variabel, tahun 2015.
  - `Koordinat.xlsx` — koordinat centroid 514 wilayah.
  - `Distance_matrix_514.xlsx` — matriks jarak 514 × 514.
  - `sovi_data_pop_514.xlsx` — data populasi 514 wilayah.
- Shapefile bawaan: `514_kabupaten.shp` (514 fitur, kolom ID: `idkab`).
- Dokumentasi roxygen2 lengkap: `?soviclust`, `?run_app`, dan halaman dataset.
- 16 unit test dengan testthat (test `run_app` dan `sample_data`).

---

## soviclust 0.0.1 (2024-07-01) — Pre-release

- Versi awal aplikasi Shiny standalone (sebelum dikonversi menjadi R package).
- Implementasi pipeline SoVI dasar dan visualisasi Leaflet.

---

*Untuk melaporkan bug atau request fitur, buka issue di:*
*<https://github.com/dedenistiawan/soviclust/issues>*
