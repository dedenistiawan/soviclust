# SoVI Interactive Mapper

> **Platform analisis Social Vulnerability Index (SoVI) berbasis R Shiny**  
> Dikembangkan oleh Tim Peneliti ITESA Muhammadiyah

---

## Daftar Isi

- [Tentang Aplikasi](#tentang-aplikasi)
- [Metode SoVI](#metode-sovi)
- [Fitur Utama](#fitur-utama)
- [Persyaratan Sistem](#persyaratan-sistem)
- [Instalasi](#instalasi)
- [Struktur Direktori](#struktur-direktori)
- [Alur Penggunaan](#alur-penggunaan)
- [Dokumentasi Modul](#dokumentasi-modul)
  - [Core Functions](#core-functions)
  - [Variable Config](#variable-config)
  - [Method Comparison](#method-comparison)
  - [SoVI Computation](#sovi-computation)
  - [Extended Analysis](#extended-analysis)
  - [Cluster Analysis — ClustGeo](#cluster-analysis--clustgeo)
  - [Cluster Analysis — FGWC](#cluster-analysis--fgwc)
  - [Cluster Analysis — LFGWC](#cluster-analysis--lfgwc)
  - [SoVI Analysis](#sovi-analysis)
  - [Downloads](#downloads)
- [Format Data Input](#format-data-input)
- [Referensi](#referensi)
- [Tim Pengembang](#tim-pengembang)

---

## Tentang Aplikasi

**SoVI Interactive Mapper** adalah aplikasi web interaktif berbasis R Shiny untuk menghitung, memvisualisasikan, dan menganalisis **Social Vulnerability Index (SoVI)** di tingkat wilayah administratif. Aplikasi ini mengimplementasikan metodologi Cutter et al. (2003) dengan sejumlah peningkatan, termasuk:

- Bobot proporsional berdasarkan factor loading PCA
- Tiga opsi metode penentuan arah variabel (direction method)
- Integrasi analisis spasial (Moran's I, LISA)
- Tiga algoritma clustering spasial: **ClustGeo**, **FGWC**, dan **LFGWC**
- Dukungan 9 algoritma metaheuristik untuk optimasi centroid FGWC/LFGWC

---

## Metode SoVI

Pipeline SoVI terdiri dari 6 fase berurutan:

| Fase | Nama | Deskripsi |
|------|------|-----------|
| 2 | Z-score Standardisasi | Normalisasi data: mean=0, SD=1 |
| 3 | PCA + Diagnostik | KMO, Bartlett, communality, Kaiser criterion, rotasi Varimax |
| 4 | Seleksi Variabel | Assign variabel ke komponen dominan, threshold \|λ\| ≥ τ |
| 5 | Agregasi Skor | Bobot proporsional loading, normalisasi [0, 1] |
| 6 | Klasifikasi Jenks | Natural Breaks 5 kelas kerentanan |

### Tiga Direction Method

| Method | Deskripsi | Rekomendasi |
|--------|-----------|-------------|
| **Theory-Based (PM)** | Direction ditentukan dari teori kerentanan sosial (a priori) | ✅ Utama |
| **Loading Sign** | Direction dari tanda loading PCA secara empiris | Eksplorasi |
| **Cutter's Method** | Direction dari variabel dominan per komponen (Cutter 2003) | Replikasi |

### Formula Agregasi

```
RC_k = Σ (w_ik × d_i × z_i)
         dimana w_ik = |λ_ik| / Σ|λ_jk|

SoVI_raw  = Σ RC_k
SoVI_score = (SoVI_raw - min) / (max - min)  ∈ [0, 1]
```

---

## Fitur Utama

| Fitur | Deskripsi |
|-------|-----------|
| 📊 **Upload Data** | Dataset Excel/CSV dan shapefile wilayah |
| ⚙️ **Variable Config** | Pilih variabel dan tentukan direction (+/-) per variabel |
| 🔬 **Method Comparison** | Bandingkan 3 direction method secara bersamaan |
| 🧮 **SoVI Computation** | Pipeline PCA → agregasi → klasifikasi Jenks |
| 📈 **Extended Analysis** | Dominant component, component profile, Moran's I, LISA, sensitivity |
| 🌍 **ClustGeo** | Clustering hierarki spasial dengan alpha optimal otomatis |
| 🤖 **FGWC** | Fuzzy clustering dengan 10 pilihan algoritma optimasi |
| 📍 **LFGWC** | Local FGWC berbasis neighborhood radius (Grekousis 2020) |
| 🗺️ **SoVI Analysis** | Peta choropleth per variabel dengan klasifikasi Jenks & GVF |
| 💾 **Downloads** | Ekspor hasil CSV dan peta PNG resolusi tinggi |

---

## Persyaratan Sistem

- **R** versi ≥ 4.2.0
- **RStudio** (direkomendasikan) atau R GUI
- **RAM** minimal 4 GB (disarankan 8 GB untuk dataset besar)
- **OS**: Windows 10/11, macOS, atau Linux

### Package R yang Diperlukan

```r
# Interface
install.packages(c("shiny", "shinydashboard", "shinyjs", "shinyWidgets"))

# Tabel & Visualisasi
install.packages(c("DT", "ggplot2", "patchwork", "fmsb"))

# Data Wrangling
install.packages(c("dplyr", "tidyr", "readxl"))

# Statistik & PCA
install.packages(c("psych", "classInt", "cluster"))

# Spasial
install.packages(c("sf", "tmap", "leaflet", "spdep", "ClustGeo"))

# Clustering
install.packages(c("RColorBrewer", "rdist", "stabledist"))
```

---

## Instalasi

### 1. Clone atau Download Proyek

```bash
# Jika menggunakan Git
git clone https://github.com/username/shiny_sovi_ver_2.git

# Atau download ZIP dan extract ke folder lokal
```

### 2. Install Semua Package

Buka RStudio, jalankan script berikut:

```r
packages <- c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets",
  "DT", "ggplot2", "patchwork", "fmsb",
  "dplyr", "tidyr", "readxl",
  "psych", "classInt", "cluster",
  "sf", "tmap", "leaflet", "spdep", "ClustGeo",
  "RColorBrewer", "rdist", "stabledist"
)

install.packages(packages[!packages %in% installed.packages()[,"Package"]])
```

### 3. Pindahkan File Algoritma FGWC

> ⚠️ **PENTING**: File algoritma FGWC harus dipindah secara manual.

```
DARI: R/FGWC_OPT/Function/
KE  : R/shared/Function/

File yang dipindah:
  fgwc.R, mainfunction.R, index.R, ei.R,
  abcfgwc.R, fpafgwc.R, gsafgwc.R, gwofgwc.R,
  hhofgwc.R, ifafgwc.R, psofgwc.R, tlbofgwc.R, woafgwc.R

File yang DIHAPUS (tidak diperlukan):
  naspaclust.R  (file kosong)
  data.R        (dokumentasi package, tidak dipakai di Shiny)
```

### 4. Jalankan Aplikasi

```r
# Dari RStudio, buka folder proyek lalu:
shiny::runApp()

# Atau jalankan langsung:
shiny::runApp("path/to/shiny_sovi_ver_2")
```

---

## Struktur Direktori

```
shiny_sovi_ver_2/
│
├── global.R                    # Entry point: library + source semua modul
├── server.R                    # Server utama: reactive values + data upload
├── ui.R                        # UI utama: sidebar + memanggil tab modular
│
├── www/
│   └── custom.css              # Stylesheet kustom (tema biru)
│
├── R/
│   │
│   ├── core/                   # ── FONDASI: Fungsi inti (tidak bergantung Shiny)
│   │   ├── helpers.R           #    Konstanta (VULN_PAL), normalize_id/01, read_shapefile
│   │   ├── sovi_core.R         #    Pipeline SoVI: standardize → PCA → select → compute → classify
│   │   ├── cluster_core.R      #    ClustGeo: build_feature_matrix, find_optimal_k/alpha
│   │   ├── analysis_core.R     #    Extended: Moran's I, sensitivity, Cutter comparison
│   │   └── map_core.R          #    Leaflet builders: SoVI, cluster, LISA
│   │
│   ├── shared/                 # ── ENGINE: Algoritma FGWC (dipakai FGWC & LFGWC)
│   │   ├── INSTRUKSI_PINDAH_FILE.R
│   │   └── Function/
│   │       ├── fgwc.R          #    Core FGWC engine (fgwcuv, vi, uij, renew_uij)
│   │       ├── mainfunction.R  #    Wrapper fgwc() + get_param_*
│   │       ├── index.R         #    Indeks validasi: PC, CE, SC, SI, XB, IFV, Kwon
│   │       ├── ei.R            #    Distribusi chaotic + update_alpha
│   │       ├── abcfgwc.R       #    ABC — Artificial Bee Colony
│   │       ├── fpafgwc.R       #    FPA — Flower Pollination Algorithm
│   │       ├── gsafgwc.R       #    GSA — Gravitational Search Algorithm
│   │       ├── gwofgwc.R       #    GWO — Grey Wolf Optimizer
│   │       ├── hhofgwc.R       #    HHO — Harris-Hawk Optimization
│   │       ├── ifafgwc.R       #    IFA — Intelligent Firefly Algorithm
│   │       ├── psofgwc.R       #    PSO — Particle Swarm Optimization
│   │       ├── tlbofgwc.R      #    TLBO — Teaching-Learning Based Optimization
│   │       └── woafgwc.R       #    WOA — Whale Optimization Algorithm
│   │
│   ├── cluster_geo/            # ── MODUL: ClustGeo Advanced
│   │   ├── clustgeo_ui.R       #    UI tab ClustGeo (clustgeo_tab_ui)
│   │   └── clustgeo_server.R   #    Server logic ClustGeo (clustgeo_server)
│   │
│   ├── FGWC/                   # ── MODUL: Fuzzy Geographically Weighted Clustering
│   │   ├── fgwc_wrapper.R      #    run_fgwc_shiny, build_leaflet_fgwc, haversine_matrix
│   │   ├── fgwc_ui.R           #    UI tab FGWC (fgwc_tab_ui)
│   │   └── fgwc_server.R       #    Server logic FGWC (fgwc_server)
│   │
│   ├── LFGWC/                  # ── MODUL: Local FGWC (Grekousis 2020)
│   │   ├── lfgwc_wrapper.R     #    run_lfgwc_shiny, build_lfgwc_weights, build_leaflet_lfgwc
│   │   ├── lfgwc_ui.R          #    UI tab LFGWC (lfgwc_tab_ui)
│   │   └── lfgwc_server.R      #    Server logic LFGWC (lfgwc_server)
│   │
│   ├── sovi_analysis/          # ── MODUL: SoVI Analysis (peta per variabel)
│   │   ├── sovi_analysis_utils.R  # compute_gvf, classify_variable_jenks, build_leaflet_sovi_analysis
│   │   ├── sovi_analysis_ui.R     # UI tab SoVI Analysis (sovi_analysis_tab_ui)
│   │   └── sovi_analysis_server.R # Server logic (sovi_analysis_server)
│   │
│   ├── var_config/             # ── MODUL: Variable Configuration
│   │   ├── var_config_ui.R     #    UI tab Variable Config (var_config_tab_ui)
│   │   └── var_config_server.R #    Server logic (var_config_server)
│   │
│   ├── method_comparison/      # ── MODUL: Method Comparison (opsional)
│   │   ├── method_comparison_ui.R     # UI tab (method_comparison_tab_ui)
│   │   └── method_comparison_server.R # Server logic (method_comparison_server)
│   │
│   ├── sovi_computation/       # ── MODUL: SoVI Computation
│   │   ├── sovi_computation_ui.R     # UI tab (sovi_computation_tab_ui)
│   │   └── sovi_computation_server.R # Server logic (sovi_computation_server)
│   │
│   ├── extended_analysis/      # ── MODUL: Extended Analysis
│   │   ├── extended_analysis_ui.R     # UI tab (extended_analysis_tab_ui)
│   │   └── extended_analysis_server.R # Server logic (extended_analysis_server)
│   │
│   └── downloads/              # ── MODUL: Downloads
│       ├── downloads_ui.R      #    UI tab Downloads (downloads_tab_ui)
│       └── downloads_server.R  #    Server logic (downloads_server)
│
├── data/                       # Dataset eksperimen (.xlsx / .csv)
└── map/                        # Shapefile wilayah (.shp, .dbf, .shx, .prj)
```

---

## Alur Penggunaan

Aplikasi menggunakan **sequential tab locking** — tab berikutnya baru terbuka setelah tab sebelumnya diselesaikan.

```
┌─────────────────────────────────────────────────────────────┐
│  STEP 1: Upload Data                                        │
│  • Upload dataset (.xlsx/.csv)                              │
│  • Upload shapefile (.shp + .dbf + .shx)                    │
│  • Klik "Konfirmasi & Lanjut"                               │
└────────────────────────┬────────────────────────────────────┘
                         │ unlock
┌────────────────────────▼────────────────────────────────────┐
│  STEP 2: Variable Config                                    │
│  • Pilih variabel SoVI dari dataset                         │
│  • Tentukan direction (+/-) per variabel                    │
│  • Klik "Konfirmasi Konfigurasi"                            │
└────────────────────────┬────────────────────────────────────┘
                         │ unlock
┌────────────────────────▼────────────────────────────────────┐
│  STEP 3: Method Comparison  [OPSIONAL]                      │
│  • Bandingkan 3 direction method                            │
│  • Lihat korelasi Spearman antar method                     │
└────────────────────────┬────────────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────────────┐
│  STEP 4: SoVI Computation  ← WAJIB                         │
│  • Pilih direction method & rotasi PCA                      │
│  • Set loading threshold (τ)                                │
│  • Klik "Run SoVI"                                          │
│  • Lihat diagnostik PCA, loading matrix, peta SoVI          │
└────────────────────────┬────────────────────────────────────┘
                         │ unlock semua tab downstream
          ┌──────────────┼──────────────┬──────────────┐
          ▼              ▼              ▼              ▼
  Extended Analysis  ClustGeo       FGWC           LFGWC
  Cluster Analysis   Cluster        Cluster        Cluster
  SoVI Analysis      Spasial        Fuzzy          Local Fuzzy
  Downloads
```

---

## Dokumentasi Modul

### Core Functions

Lokasi: `R/core/`

#### `helpers.R`

| Fungsi / Konstanta | Deskripsi |
|-------------------|-----------|
| `VULN_CLASSES` | Vector 5 label kelas kerentanan |
| `VULN_PAL` | Named vector warna (#hex) per kelas |
| `normalize_id(x)` | Lowercase + trim whitespace untuk join ID |
| `normalize_01(x)` | Normalisasi min-max ke [0, 1] |
| `read_shapefile(files_df)` | Baca shapefile dari multiple upload Shiny |

#### `sovi_core.R`

| Fungsi | Input Utama | Output |
|--------|-------------|--------|
| `standardize_data()` | data, sovi_vars, id_col, name_col | List: Z, unit_ids, kabupaten |
| `run_pca()` | Z, rotation, min_eigenvalue | List: pca_result, loadings, kmo, bartlett |
| `select_variables_per_component()` | pca_out, negative_vars, loading_threshold, direction_method | List: assignment, unassigned_vars |
| `compute_sovi()` | std_out, selection_out, pca_out | data.frame: DISTRICTCODE, sovi_score, RC* |
| `classify_sovi()` | sovi_df, n_classes | List: sovi_df+vuln_class, breaks |
| `run_sovi_core()` | data, sovi_vars, neg_vars, ... | List lengkap hasil pipeline |

#### `cluster_core.R`

| Fungsi | Deskripsi |
|--------|-----------|
| `build_feature_matrix()` | Bangun matriks fitur dari 5 sumber: raw, raw_norm, standardized, sovi, rc |
| `find_optimal_k()` | Cari k optimal via mean silhouette |
| `find_optimal_alpha()` | Cari alpha optimal via trade-off Q1/Q2 |
| `run_clustgeo()` | ClustGeo sederhana (Extended Analysis) |
| `run_clustgeo_advanced()` | ClustGeo Advanced (menu Cluster Analysis) |
| `build_leaflet_clustgeo_adv()` | Leaflet choropleth hasil ClustGeo |

#### `analysis_core.R`

| Fungsi | Deskripsi |
|--------|-----------|
| `run_dominant_component()` | Identifikasi RC dominan per distrik |
| `run_component_profile()` | Rata-rata RC per kelas kerentanan |
| `run_moran_lisa()` | Global Moran's I + Local Moran (LISA) |
| `run_sensitivity()` | Uji robustness terhadap 3 nilai threshold |
| `run_cutter_comparison()` | Bandingkan dengan metode Cutter (2003) |
| `run_3way_comparison()` | Jalankan dan bandingkan 3 direction method |

#### `map_core.R`

| Fungsi | Deskripsi |
|--------|-----------|
| `build_leaflet_sovi()` | Peta choropleth SoVI score + kelas |
| `build_leaflet_cluster()` | Peta choropleth cluster (Extended Analysis) |
| `build_leaflet_lisa()` | Peta choropleth LISA cluster |

---

### Variable Config

Lokasi: `R/var_config/`

**Fungsi:** Memungkinkan user memilih variabel SoVI dari dataset dan menentukan direction (+/-) setiap variabel.

| Input | Deskripsi |
|-------|-----------|
| `sovi_vars` | Checkbox group — pilih variabel numerik |
| `dir_{varname}` | Radio button (+/-) per variabel |

| Output | Deskripsi |
|--------|-----------|
| `rv$sovi_vars` | Vector nama variabel yang dipilih |
| `rv$neg_vars` | Vector variabel dengan direction negatif |
| `rv$vars_ok` | TRUE setelah konfirmasi |

---

### Method Comparison

Lokasi: `R/method_comparison/`

**Fungsi:** Menjalankan `run_3way_comparison()` dan menampilkan perbandingan 3 direction method melalui Spearman correlation, scatter plot, distribusi kelas, dan heatmap component profile.

> Tab ini **opsional** — user bisa langsung ke SoVI Computation.

---

### SoVI Computation

Lokasi: `R/sovi_computation/`

**Fungsi:** Menjalankan pipeline SoVI lengkap (`run_sovi_core()`).

| Parameter | Pilihan | Default |
|-----------|---------|---------|
| Direction Method | theory, loading, cutter | theory |
| Rotasi PCA | varimax, oblimin, promax, quartimax, none | varimax |
| Loading Threshold (τ) | 0.3 — 0.9 | 0.5 |

**Output Tab:**

| Tab | Konten |
|-----|--------|
| Diagnostik PCA | KMO, Bartlett's Test, Communality |
| Variansi & Loading | Tabel variansi + loading matrix |
| Assignment | Variabel → komponen dengan loading & direction |
| Skor SoVI | Tabel lengkap sovi_raw, sovi_score, vuln_class, RC* |
| Distribusi Kelas | Bar chart + tabel frekuensi 5 kelas |
| Peta SoVI | Leaflet choropleth interaktif |
| Top 10 | 10 distrik paling rentan |

---

### Extended Analysis

Lokasi: `R/extended_analysis/`

**Fungsi:** Lima analisis lanjutan setelah SoVI dihitung.

| Analisis | Fungsi | Output |
|----------|--------|--------|
| 1. Dominant Component | `run_dominant_component()` | Bar chart + tabel RC dominan per distrik |
| 2. Component Profile | `run_component_profile()` | Heatmap + radar chart RC per kelas |
| 3. Moran's I + LISA | `run_moran_lisa()` | Global Moran's I + peta LISA |
| 4. Sensitivity Analysis | `run_sensitivity()` | Perbandingan skor pada τ = 0.5, 0.6, 0.7 |
| 5. Cutter Comparison | `run_cutter_comparison()` | Spearman ρ vs metode Cutter (2003) |

---

### Cluster Analysis — ClustGeo

Lokasi: `R/cluster_geo/`

**Fungsi:** Clustering hierarki spasial menggunakan `ClustGeo::hclustgeo()` yang menggabungkan dissimilarity atribut (D0) dan dissimilarity spasial (D1).

| Parameter | Pilihan | Keterangan |
|-----------|---------|------------|
| Sumber Data | raw, raw_norm, standardized, sovi, rc | Matriks fitur untuk D0 |
| k | Manual (2—10) / Otomatis | Otomatis via max mean silhouette |
| α (alpha) | Manual (0—1) / Otomatis | Otomatis via min \|Q1 - Q2\| |

**Tab Output:** Parameter · Peta Interaktif · Silhouette · Alpha Trade-off · Profil Cluster · Data Cluster

---

### Cluster Analysis — FGWC

Lokasi: `R/FGWC/`

**Fungsi:** Fuzzy Geographically Weighted Clustering — menggabungkan Fuzzy C-Means dengan pengaruh tetangga (populasi + jarak).

#### Data Pendukung yang Diperlukan

| File | Format | Keterangan |
|------|--------|------------|
| Matriks Jarak | Excel/CSV (n×n) | Jarak antar wilayah, **atau** |
| Koordinat Lon/Lat | Excel/CSV | Kolom: DISTRICTCODE, longitude, latitude → dihitung Haversine otomatis |
| Data Populasi | Excel/CSV | 1 kolom numerik, n baris |

#### Algoritma yang Tersedia

| Kode | Algoritma | Referensi |
|------|-----------|-----------|
| `classic` | Classic FGWC | Mason & Jacobson (2007) |
| `abc` | Artificial Bee Colony | Karaboga (2007) |
| `fpa` | Flower Pollination Algorithm | Yang (2012) |
| `gsa` | Gravitational Search Algorithm | Rashedi et al. (2009) |
| `gwo` | Grey Wolf Optimizer | Mirjalili et al. (2014) |
| `hho` | Harris-Hawk Optimization | Heidari (2019), Bairathi (2018) |
| `ifa` | Intelligent Firefly Algorithm | Yang (2009) |
| `pso` | Particle Swarm Optimization | Kennedy & Eberhart (1995) |
| `tlbo` | Teaching-Learning Based Optimization | Rao (2012) |
| `woa` | Whale Optimization Algorithm | Mirjalili & Lewis (2016) |

#### Indeks Validasi

| Indeks | Interpretasi |
|--------|-------------|
| PC (max) | Partition Coefficient — nilai besar lebih baik |
| CE (min) | Classification Entropy — nilai kecil lebih baik |
| SC (min) | SC Index — nilai kecil lebih baik |
| SI (min) | Separation Index — nilai kecil lebih baik |
| XB (min) | Xie-Beni Index — nilai kecil lebih baik |
| IFV (max) | IFV Index — nilai besar lebih baik |
| Kwon (min) | Kwon Index — nilai kecil lebih baik |

---

### Cluster Analysis — LFGWC

Lokasi: `R/LFGWC/`

**Fungsi:** Local Fuzzy Geographically Weighted Clustering (Grekousis, 2020). Berbeda dengan FGWC global, LFGWC hanya mempertimbangkan unit dalam radius `dthr` sebagai tetangga.

#### Perbedaan LFGWC vs FGWC

| Aspek | FGWC (Global) | LFGWC (Local) |
|-------|--------------|--------------|
| Cakupan tetangga | Semua n unit | Unit dalam radius `dthr` |
| Pembobotan | SIM-PF: (Pi·Pj)^b / d^a | Distance Decay: 1/d^exp |
| Normalisasi | Via parameter A | Row-standardized otomatis |
| Referensi | Mason & Jacobson (2007) | Grekousis (2020) |

#### Varian LFGWC

| Mode | `dthr` | `si` | Deskripsi |
|------|--------|------|-----------|
| Global FGWC | -99 | TRUE | Semua unit, SIM-PF |
| LFGWC | > 0 | TRUE | Lokal, SIM-PF |
| DLFGWC | > 0 | FALSE | Lokal, Distance Decay |
| DLFGWC-PSO | > 0 | FALSE + PSO | Varian terbaik sesuai paper |

**Tab Output tambahan:** Peta Max Membership — menampilkan derajat keanggotaan tertinggi per unit (nilai mendekati 1 = unit jelas masuk 1 cluster).

---

### SoVI Analysis

Lokasi: `R/sovi_analysis/`

**Fungsi:** Visualisasi peta choropleth untuk setiap variabel secara individual, dengan fitur:
- Klasifikasi Jenks per variabel
- GVF (Goodness of Variance Fit) untuk menentukan k kelas optimal
- Side-by-side comparison dua variabel
- Overlay centroid SoVI

#### GVF (Goodness of Variance Fit)

```
GVF = 1 - (SDCM / SDAM)

SDAM = Sum of Squared Deviations from Array Mean
SDCM = Sum of Squared Deviations from Class Mean

GVF = 1.0  → klasifikasi sempurna
GVF ≥ 0.85 → klasifikasi diterima (threshold default)
```

---

### Downloads

Lokasi: `R/downloads/`

**Fungsi:** Ekspor semua hasil analisis.

| File | Format | Isi |
|------|--------|-----|
| SoVI Result | CSV | sovi_score, vuln_class, RC* per distrik |
| Variable Assignment | CSV | Variabel → komponen, loading, direction |
| Dominant Component | CSV | RC dominan per distrik |
| Component Profile | CSV | Mean RC per kelas kerentanan |
| Cluster Result | CSV | Nomor cluster per distrik |
| LISA Result | CSV | lisa_I, lisa_p, lisa_quad per distrik |
| Sensitivity | CSV | Skor SoVI pada τ = 0.5, 0.6, 0.7 |
| Cutter Comparison | CSV | Perbandingan skor & ranking |
| Peta SoVI | PNG (300 DPI) | Choropleth 5 kelas kerentanan |
| Peta Cluster | PNG (300 DPI) | Choropleth cluster |
| Peta LISA | PNG (300 DPI) | Choropleth LISA |
| Fig Dominant | PNG (300 DPI) | Bar chart komponen dominan |
| Fig Sensitivity | PNG (300 DPI) | Scatter plot sensitivity |

---

## Format Data Input

### Dataset (.xlsx / .csv)

| Kolom | Tipe | Keterangan |
|-------|------|------------|
| `DISTRICTCODE` | Karakter / Numerik | ID unik wilayah — **harus cocok dengan shapefile** |
| `KABUPATEN` | Karakter | Nama wilayah |
| `VAR_1` ... `VAR_n` | Numerik | Variabel SoVI dalam satuan **persentase (0–100)** |

**Ketentuan:**
- Tidak ada missing value (NA)
- Minimal 2 variabel untuk PCA
- Minimal 50 unit wilayah
- Semua variabel dalam satuan persentase

**Contoh variabel umum:**

```
POVERTY     — % penduduk miskin
LOWEDU      — % pendidikan rendah (tidak tamat SD)
ELDERLY     — % penduduk lansia (≥ 60 tahun)
NOELECTRIC  — % rumah tangga tanpa listrik
ILLITERATE  — % buta huruf
DPRONE      — % wilayah rawan bencana
FEMALE_HH   — % rumah tangga kepala keluarga perempuan
DISABILITY  — % penyandang disabilitas
```

### Shapefile

Upload semua file berikut **sekaligus** dalam satu dialog upload:

| Ekstensi | Keterangan | Status |
|----------|------------|--------|
| `.shp` | Geometri poligon wilayah | ✅ Wajib |
| `.dbf` | Atribut data | ✅ Wajib |
| `.shx` | Index geometri | ✅ Wajib |
| `.prj` | Proyeksi koordinat (CRS) | Disarankan |
| `.cpg` | Encoding karakter | Opsional |

**Catatan:**
- CRS yang disarankan: **WGS 84 (EPSG:4326)** atau UTM Zone 49S (EPSG:32749)
- Kolom ID di shapefile harus cocok dengan kolom `DISTRICTCODE` di dataset

### Data Pendukung FGWC / LFGWC

#### Matriks Jarak (n×n)

```
         Kab_A  Kab_B  Kab_C
Kab_A      0    12.5   34.2
Kab_B    12.5     0    22.1
Kab_C    34.2   22.1     0
```

#### Koordinat Lon/Lat (alternatif matriks jarak)

```
DISTRICTCODE  longitude   latitude
3301          110.3654    -7.5231
3302          110.4521    -7.6123
...
```

#### Data Populasi

```
populasi
125000
98500
234000
...
```

---

## Referensi

### Metodologi SoVI

```
Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003).
Social vulnerability to environmental hazards.
Social Science Quarterly, 84(2), 242–261.
https://doi.org/10.1111/1540-6237.8402002
```

### LFGWC

```
Grekousis, G. (2020).
Local fuzzy geographically weighted clustering: a new method
for geodemographic segmentation.
International Journal of Geographical Information Science.
https://doi.org/10.1080/13658816.2020.1808221
```

### Algoritma FGWC

```
Mason, G.A., & Jacobson, R.D. (2007).
Fuzzy geographically weighted clustering.
Proceedings of GISRUK 2007.

Karaboga, D. (2007). An Idea Based On Honey Bee Swarm for Numerical
Optimization. Technical Report TR06, Erciyes University.

Yang, X.S. (2012). Flower Pollination Algorithm for Global Optimization.
UCNC 2012.

Rashedi, E., Nezamabadi-pour, H., & Saryazdi, S. (2009).
GSA: A Gravitational Search Algorithm. Information Sciences, 179(13), 2232–2248.

Mirjalili, S., Mirjalili, S.M., & Lewis, A. (2014).
Grey Wolf Optimizer. Advances in Engineering Software, 69, 46–61.

Mirjalili, S., & Lewis, A. (2016).
The Whale Optimization Algorithm.
Advances in Engineering Software, 95, 51–67.
```

---

## Tim Pengembang

| Nama | Peran | Email |
|------|-------|-------|
| **Deden Istiawan** | Ketua Peneliti | deden.istiawan@itesa.ac.id |
| **Herman Yuliansyah** | Ketua TPM | herman.yuliansyah@tif.uad.ac.id |
| **Rusydi Umar** | Anggota TPM | rusydi@mti.uad.ac.id |

**Institusi:** ITESA Muhammadiyah — [www.itesa.ac.id](https://www.itesa.ac.id)

---

## Lisensi

Aplikasi ini dikembangkan untuk keperluan penelitian akademik.  
Untuk penggunaan lebih lanjut, hubungi tim pengembang.

---

*Built with ❤️ using R Shiny — Indonesia*