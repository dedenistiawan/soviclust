# soviclust <img src="man/figures/logo.png" align="right" height="139" alt="" />

> **R Package for Social Vulnerability Index (SoVI) Analysis and Spatial Clustering**

[![R version](https://img.shields.io/badge/R-%3E%3D4.1.0-blue)](https://cran.r-project.org/)
[![GitHub](https://img.shields.io/github/license/dedenistiawan/soviclust)](LICENSE)
[![GitHub release](https://img.shields.io/github/v/release/dedenistiawan/soviclust)](https://github.com/dedenistiawan/soviclust/releases)

---

## Tentang Package

**soviclust** adalah R package berupa aplikasi Shiny interaktif untuk menghitung, memvisualisasikan, dan menganalisis **Social Vulnerability Index (SoVI)** di tingkat wilayah administratif. Package ini mengimplementasikan metodologi Cutter et al. (2003) dengan sejumlah peningkatan, termasuk tiga algoritma *spatial clustering*:

- 🌍 **ClustGeo** — Clustering hierarki spasial
- 🤖 **FGWC** — Fuzzy Geographically Weighted Clustering
- 📍 **LFGWC** — Local FGWC (Grekousis 2020)
- ⚡ **ALFGWC** — Adaptive LFGWC

---

## Instalasi

### Dari GitHub

```r
# Install remotes jika belum ada
install.packages("remotes")

# Install soviclust dari GitHub
remotes::install_github("dedenistiawan/soviclust")
```

> Semua dependensi (25 package) akan **otomatis terinstall** bersama soviclust.

### Dari Lokal (untuk pengembang)

```r
# Pastikan devtools tersedia
install.packages("devtools")

# Install dari folder lokal
devtools::install("path/to/soviclust")
```

---

## Cara Menggunakan

### 1. Muat package

Ketika `library(soviclust)` dipanggil, akan muncul pesan:

```
┌─────────────────────────────────────────┐
│   SoVI Interactive Mapper (soviclust)   │
│   Social Vulnerability Index Analysis   │
└─────────────────────────────────────────┘

To start the app, please run:
  soviclust::run_app()

Version: 0.1.0
```

### 2. Jalankan aplikasi

```r
library(soviclust)
soviclust::run_app()
```

Aplikasi akan terbuka di browser default Anda secara otomatis.

---

## Alur Penggunaan Aplikasi

### Langkah 1 — Import / Load Data

Pada tab **Import / Load Data**, Anda memiliki dua pilihan:

#### A. Gunakan Data Sampel (direkomendasikan untuk pertama kali)

Klik tombol **"▶ Muat Data Sampel"**. Aplikasi akan otomatis memuat:

| Data | File | Keterangan |
|------|------|-----------|
| Dataset SoVI | `sovi_data_kab_514_15.xlsx` | 514 Kabupaten/Kota, 15 variabel, tahun 2015 |
| Shapefile | `514_kabupaten.shp` | Batas wilayah Kabupaten/Kota Indonesia |

Kolom ID wilayah: **`DISTRICTCODE`** | Kolom Nama: **`KABUPATEN`**

#### B. Upload Data Sendiri

Klik **"Upload Data Sendiri"**, lalu:

1. Upload file Excel/CSV berisi data sosial-ekonomi wilayah
2. Upload shapefile (`.shp` + file pendukung `.dbf`, `.shx`, `.prj`)
3. Tentukan kolom ID wilayah dan nama wilayah
4. Pilih variabel SoVI yang akan dianalisis

**Format data yang diterima:**

| Format | Ekstensi | Keterangan |
|--------|----------|-----------|
| Excel | `.xlsx` | Sheet pertama yang akan dibaca |
| CSV | `.csv` | Separator koma atau titik koma |
| Shapefile | `.shp` | Upload bersama `.dbf`, `.shx`, `.prj` |

---

### Langkah 2 — Variable Config

Konfigurasikan variabel SoVI:

- **Pilih variabel** yang akan dimasukkan ke analisis
- **Tentukan direction** (+/-) untuk setiap variabel:
  - `+` = meningkatkan kerentanan (positif terhadap SoVI)
  - `-` = mengurangi kerentanan (negatif terhadap SoVI)

---

### Langkah 3 — Method Comparison

Bandingkan **3 direction method** secara bersamaan:

| Method | Deskripsi |
|--------|-----------|
| **Theory-Based (PM)** | Direction dari teori kerentanan sosial (a priori) |
| **Loading Sign** | Direction dari tanda loading PCA secara empiris |
| **Cutter's Method** | Direction dari variabel dominan per komponen (Cutter 2003) |

---

### Langkah 4 — SoVI Computation

Pipeline otomatis:

```
Data Input
  └─► Z-score Standardisasi
        └─► PCA + Diagnostik (KMO, Bartlett, Varimax)
              └─► Seleksi Variabel (|λ| ≥ threshold)
                    └─► Agregasi Skor Berbobot
                          └─► Klasifikasi Jenks (5 kelas)
                                └─► Peta Choropleth + Tabel Hasil
```

**Formula SoVI:**

```
RC_k   = Σ (w_ik × d_i × z_i),   w_ik = |λ_ik| / Σ|λ_jk|
SoVI   = Σ RC_k
SoVI*  = (SoVI - min) / (max - min)  ∈ [0, 1]
```

---

### Langkah 5 — Extended Analysis

Analisis lanjutan meliputi:

- **Dominant Component** — Komponen dominan per wilayah
- **Component Profile** — Radar chart profil komponen per klaster
- **Moran's I** — Uji autokorelasi spasial global
- **LISA** — Local Indicator of Spatial Autocorrelation
- **Sensitivity Analysis** — Pengaruh pemilihan threshold

---

### Langkah 6 — Cluster Analysis

Tiga metode clustering tersedia:

#### ClustGeo

Clustering hierarki spasial yang menggabungkan jarak atribut dan jarak geografis.

```
Parameter:
  - Jumlah cluster (k)
  - Alpha optimal (dihitung otomatis)
```

#### FGWC / LFGWC / ALFGWC

Fuzzy clustering dengan bobot geografis. Setiap metode memerlukan:

**Data Matriks Jarak** — dua opsi:

| Opsi | Format | Keterangan |
|------|--------|-----------|
| Upload Matriks n×n | Excel/CSV | Matriks jarak antar wilayah (n×n) |
| Upload Koordinat | Excel/CSV | Kolom: `DISTRICTCODE`, `longitude`, `latitude` |

> Jika menggunakan koordinat, jarak dihitung otomatis dengan **Haversine Distance** (km).

**Data Populasi** — Excel/CSV dengan 1 kolom berisi jumlah penduduk (n baris).

**Gunakan Data Sampel FGWC/LFGWC/ALFGWC:**

Klik tombol **"▶ Muat Data Sampel"** di panel Data Pendukung. Data yang dimuat:

| Data | File |
|------|------|
| Matriks Jarak (n×n) | `Distance_matrix_514.xlsx` |
| Koordinat Lon/Lat | `Koordinat.xlsx` |
| Data Populasi | `sovi_data_pop_514.xlsx` |

**Algoritma Optimasi (FGWC/LFGWC/ALFGWC):**

| Kode | Algoritma |
|------|-----------|
| PSO | Particle Swarm Optimization |
| ABC | Artificial Bee Colony |
| GWO | Grey Wolf Optimizer |
| WOA | Whale Optimization Algorithm |
| HHO | Harris-Hawk Optimization |
| FPA | Flower Pollination Algorithm |
| GSA | Gravitational Search Algorithm |
| TLBO | Teaching-Learning Based Optimization |
| IFA | Intelligent Firefly Algorithm |

---

### Langkah 7 — SoVI Analysis

Peta choropleth per variabel dengan:
- Klasifikasi Jenks Natural Breaks
- Indeks GVF (Goodness of Variance Fit)
- Visualisasi interaktif Leaflet

---

### Langkah 8 — Downloads

Ekspor hasil:

- 📄 **CSV** — Tabel hasil SoVI dan klaster
- 🗺️ **PNG** — Peta resolusi tinggi

---

## Dataset Bawaan

Package ini menyertakan dataset sampel siap pakai:

| File | Deskripsi | Baris | Kolom |
|------|-----------|-------|-------|
| `sovi_data_kab_514_15.xlsx` | Data SoVI 514 Kab/Kota Indonesia 2015 | 514 | 17 |
| `Koordinat.xlsx` | Koordinat centroid 514 Kab/Kota | 514 | 3 |
| `Distance_matrix_514.xlsx` | Matriks jarak antar 514 Kab/Kota | 514 | 514 |
| `sovi_data_pop_514.xlsx` | Data populasi 514 Kab/Kota | 514 | 2 |

### Variabel dalam `sovi_data_kab_514_15.xlsx`

| Variabel | Deskripsi |
|----------|-----------|
| `DISTRICTCODE` | Kode wilayah Kabupaten/Kota |
| `KABUPATEN` | Nama Kabupaten/Kota |
| `AGE014` | Persentase penduduk usia 0-14 tahun |
| `FEMPOP` | Persentase penduduk perempuan |
| `AGE65P` | Persentase penduduk usia 65+ tahun |
| `FEMHH` | Persentase rumah tangga kepala keluarga perempuan |
| `HHSIZE` | Rata-rata ukuran rumah tangga |
| `NOELEC` | Persentase rumah tangga tanpa listrik |
| `LOWEDUC` | Persentase penduduk berpendidikan rendah |
| `POPGRW` | Tingkat pertumbuhan penduduk |
| `POOR` | Persentase penduduk miskin |
| `ILLIT` | Tingkat buta huruf |
| `NOTRAIN` | Persentase tanpa pelatihan kerja |
| `DISAREA` | Kepadatan wilayah bencana |
| `RENTHH` | Persentase rumah tangga sewa |
| `NOSAN` | Persentase tanpa sanitasi |
| `NOWATER` | Persentase tanpa akses air bersih |

---

## Metode SoVI

Berdasarkan Cutter et al. (2003), SoVI mengukur kerentanan sosial menggunakan analisis komponen utama (PCA) dengan formula agregasi berbobot:

```
SoVI*  = (SoVI - min) / (max - min),  SoVI ∈ [0, 1]
```

Nilai mendekati **1** = kerentanan sangat tinggi  
Nilai mendekati **0** = kerentanan sangat rendah

### Klasifikasi Kerentanan

| Kelas | Label | Warna |
|-------|-------|-------|
| 1 | Sangat Rendah | Hijau tua |
| 2 | Rendah | Hijau muda |
| 3 | Sedang | Kuning |
| 4 | Tinggi | Oranye |
| 5 | Sangat Tinggi | Merah |

---

## Persyaratan Sistem

- **R** ≥ 4.1.0
- **RStudio** (direkomendasikan)
- **RAM** minimal 4 GB (disarankan 8 GB untuk dataset besar)
- **OS**: Windows 10/11, macOS, atau Linux

---

## Referensi

- Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003). Social Vulnerability to Environmental Hazards. *Social Science Quarterly*, 84(2), 242–261.
- Wijayanto, H., & Purwaningsih, T. (2020). Fuzzy Geographically Weighted Clustering. *Journal of Physics: Conference Series*.
- Grekousis, G. (2020). Spatial Analysis Methods and Practice. Cambridge University Press.

---

## Pengembang

Dikembangkan oleh **Tim Peneliti ITESA Muhammadiyah**

- 📧 Email: [dedenistiawan@gmail.com](mailto:dedenistiawan@gmail.com)
- 🐙 GitHub: [dedenistiawan/soviclust](https://github.com/dedenistiawan/soviclust)
- 🐛 Bug report: [GitHub Issues](https://github.com/dedenistiawan/soviclust/issues)

---

## Lisensi

MIT License © 2024 Deden Istiawan