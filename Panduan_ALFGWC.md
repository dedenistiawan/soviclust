# Panduan Penggunaan Adaptive Local Fuzzy Geographically Weighted Clustering (ALFGWC)

> **Package:** `soviclust`  
> **Module:** Cluster Analysis → ALFGWC  
> **Status:** Research method under active development  
> **Documentation version:** September 2026  
> **Source alignment:** `inst/app/R/ALFGWC/alfgwc_wrapper.R`, `alfgwc_ui.R`, dan `alfgwc_server.R`

---

## 1. Tentang ALFGWC

**Adaptive Local Fuzzy Geographically Weighted Clustering (ALFGWC)** adalah metode spatial fuzzy clustering yang dikembangkan dalam `soviclust` sebagai perluasan dari **Local Fuzzy Geographically Weighted Clustering (LFGWC)**.

LFGWC memperbaiki FGWC dengan membatasi pengaruh spasial pada lingkungan lokal (_local neighborhood_) daripada menggunakan seluruh unit spasial secara global. Dalam implementasi LFGWC pada `soviclust`, lingkungan lokal dibentuk terutama menggunakan **Distance Threshold (`dthr`)**.

ALFGWC memperluas kerangka tersebut dalam **dua aspek utama**:

1. **Adaptive spatial influence**  
   Parameter pengaruh spasial tidak lagi berupa satu nilai global yang sama untuk seluruh wilayah. ALFGWC menggunakan parameter adaptif $\alpha_i$ untuk setiap unit spasial $i$, yang ditentukan berdasarkan pola **Local Moran's I**.

2. **Flexible neighborhood definition**  
   ALFGWC memperluas pembentukan tetangga dari pendekatan Distance Threshold menjadi empat pilihan:
   - Distance Threshold (`dthr`);
   - Queen contiguity;
   - Rook contiguity; dan
   - Bishop contiguity.

Dengan dua mekanisme tersebut, ALFGWC memungkinkan **definisi tetangga** dan **besarnya pengaruh tetangga** disesuaikan secara lebih fleksibel terhadap struktur spasial data.

---

## 2. Posisi ALFGWC dalam Keluarga Metode

Secara konseptual, perkembangan metode dalam `soviclust` dapat diringkas sebagai berikut:

```text
FCM
│
│  hanya menggunakan kemiripan atribut
▼
FGWC
│
│  menambahkan pengaruh geografis secara global
▼
LFGWC
│
│  membatasi pengaruh geografis ke neighborhood lokal
│  neighborhood utama: Distance Threshold (dthr)
▼
ALFGWC
   │
   ├── adaptive α_i berbasis Local Moran's I
   └── neighborhood fleksibel:
       dthr / Queen / Rook / Bishop
```

Perbedaan utama dapat diringkas dalam tabel berikut.

| Metode     | Fuzzy | Spatial | Lingkup spasial    | Neighborhood                     | Parameter pengaruh spasial            |
| ---------- | :---: | :-----: | ------------------ | -------------------------------- | ------------------------------------- |
| FCM        |   ✓   |    –    | –                  | –                                | –                                     |
| FGWC       |   ✓   |    ✓    | Global             | Seluruh unit                     | Global                                |
| LFGWC      |   ✓   |    ✓    | Local              | Distance Threshold (`dthr`)      | Global                                |
| **ALFGWC** |   ✓   |    ✓    | **Local adaptive** | **dthr / Queen / Rook / Bishop** | **Adaptive per wilayah ($\alpha_i$)** |

---

## 3. Prinsip Dasar Fuzzy Clustering

Berbeda dengan hard clustering, setiap wilayah pada fuzzy clustering dapat memiliki derajat keanggotaan pada lebih dari satu cluster.

Jika terdapat $c$ cluster, maka untuk unit spasial $i$:

```math
\sum_{k=1}^{c} U_{ik}=1,
```

dengan:

- $U_{ik}$ = membership unit $i$ pada cluster $k$;
- $0 \le U_{ik} \le 1$.

Contoh membership satu wilayah:

| Cluster   | Membership |
| --------- | ---------: |
| Cluster 1 |       0.72 |
| Cluster 2 |       0.18 |
| Cluster 3 |       0.07 |
| Cluster 4 |       0.03 |

Hard cluster dapat diperoleh dari membership terbesar, tetapi matriks membership penuh tetap penting untuk mengidentifikasi wilayah transisi atau wilayah dengan ketidakpastian cluster yang tinggi.

---

# BAGIAN A — PERSIAPAN DATA

## 4. Data yang Diperlukan

Untuk menjalankan ALFGWC, aplikasi membutuhkan tiga kelompok input:

1. **data atribut** untuk clustering;
2. **informasi spasial** untuk membangun neighborhood dan bobot spasial; dan
3. **data populasi** jika menggunakan `Spatial Interaction`.

Selain itu, shapefile/polygon geometry yang sudah dimuat pada aplikasi digunakan untuk contiguity dan pemetaan.

### 4.1 Konsistensi ID

Pastikan:

- setiap baris data mewakili satu unit spasial;
- ID unit pada data atribut unik;
- urutan atau ID pada distance matrix/population dapat dipadankan dengan data;
- ID dataset cocok dengan ID shapefile.

Mismatch ID dapat menyebabkan clustering gagal atau hubungan tetangga salah.

---

## 5. Distance Matrix

ALFGWC menyediakan dua cara memperoleh matriks jarak.

### 5.1 Upload matriks jarak $n \times n$

Pilih:

`Upload n×n Distance Matrix`

Matriks harus:

- berbentuk persegi;
- memiliki jumlah baris dan kolom sama dengan jumlah unit;
- diagonal idealnya 0;
- menggunakan satuan jarak yang konsisten.

Jika matriks diunggah langsung, `soviclust` mempertahankan **satuan asli** matriks.

### 5.2 Upload longitude dan latitude

Pilih:

`Upload Longitude & Latitude`

Aplikasi mendeteksi kolom seperti:

- `longitude`, `lon`, `long`, atau `x`;
- `latitude`, `lat`, atau `y`.

Jarak dihitung dengan **Haversine distance** dan dinyatakan dalam **kilometer**.

> Jika memakai `dthr`, nilai threshold harus mengikuti satuan distance matrix. Untuk koordinat longitude-latitude yang dihitung oleh aplikasi, `dthr` menggunakan kilometer.

---

## 6. Population Data

Population data diperlukan terutama ketika memilih:

`Spatial Interaction (LFGWC)`

Panjang vektor populasi harus sama dengan jumlah unit spasial.

Contoh:

| DISTRICTCODE | POPULATION |
| ------------ | ---------: |
| 1101         |     123456 |
| 1102         |     284391 |
| ...          |        ... |

Population vector tidak digunakan dalam formula `Distance Decay`, tetapi tetap menjadi bagian dari input ALFGWC pada workflow aplikasi saat ini.

---

## 7. Feature Data Source

ALFGWC dapat menggunakan lima sumber fitur.

### 7.1 Original Data

`Original Data (no transformation)`

Menggunakan nilai asli tanpa transformasi.

**Gunakan dengan hati-hati** jika variabel mempunyai skala yang sangat berbeda karena jarak Euclidean dapat didominasi variabel dengan rentang besar.

### 7.2 Normalized Data

`Normalized Data (0–1)`

Setiap variabel ditransformasikan menggunakan min-max normalization:

```math
x'_{ij}
=
\frac{x_{ij}-\min(x_j)}
{\max(x_j)-\min(x_j)}.
```

Pilihan ini direkomendasikan ketika indikator memiliki unit atau rentang berbeda.

### 7.3 Standardized Data

`Standardized Data (Z-score)`

Menggunakan hasil standardisasi dari tahap SoVI.

Dalam implementasi saat ini, Z-score tersebut kemudian dinormalisasi kembali ke rentang `[0,1]` sebelum digunakan dalam ALFGWC.

### 7.4 SoVI Score

`SoVI Score`

Menggunakan satu skor SoVI sebagai fitur clustering.

Pilihan ini menghasilkan clustering berdasarkan **tingkat kerentanan komposit**, bukan profil multivariat indikator.

### 7.5 RC Scores

`RC Scores (PCA components)`

Menggunakan skor Rotated Components dari tahap PCA/Varimax.

RC scores dinormalisasi ke `[0,1]` sebelum clustering.

Pilihan ini berguna jika tujuan clustering adalah mengelompokkan wilayah berdasarkan **dimensi laten kerentanan** dan bukan seluruh indikator asli.

---

## 8. Rekomendasi Pemilihan Fitur

| Tujuan analisis                                     | Sumber fitur yang disarankan |
| --------------------------------------------------- | ---------------------------- |
| Profil kerentanan detail                            | Normalized raw indicators    |
| Membandingkan seluruh indikator dalam skala seragam | Normalized Data              |
| Menggunakan hasil preprocessing SoVI                | Standardized Data            |
| Clustering berdasarkan tingkat SoVI saja            | SoVI Score                   |
| Clustering berdasarkan dimensi PCA                  | RC Scores                    |

Untuk analisis metodologis dan benchmarking ALFGWC, gunakan sumber data yang sama pada semua algoritma yang dibandingkan.

---

# BAGIAN B — PEMBENTUKAN NEIGHBORHOOD

## 9. Neighborhood dalam LFGWC dan ALFGWC

Salah satu kontribusi ALFGWC dalam `soviclust` adalah memperluas cara mendefinisikan tetangga.

### LFGWC

Pada implementasi LFGWC:

```text
Neighborhood → Distance Threshold (dthr)
```

### ALFGWC

Pada ALFGWC:

```text
Neighborhood
├── Distance Threshold (dthr)
├── Queen Contiguity
├── Rook Contiguity
└── Bishop Contiguity
```

---

## 10. Distance Threshold (`dthr`)

Dua unit $i$ dan $j$ dianggap bertetangga apabila:

```math
d_{ij} \le d_{thr}.
```

Semakin kecil `dthr`, semakin lokal struktur neighborhood.

Semakin besar `dthr`, semakin banyak unit yang dianggap bertetangga.

### Global mode

Nilai:

```text
dthr = -99
```

digunakan dalam aplikasi untuk membuat semua unit lain sebagai tetangga.

Mode ini berguna untuk eksperimen pembanding, tetapi tidak lagi merepresentasikan neighborhood lokal.

### Safe minimum dthr

Aplikasi menghitung informasi:

`Safe min. dthr`

yaitu threshold minimum berdasarkan distance matrix agar secara teoritis setiap unit memiliki sedikitnya satu tetangga.

Gunakan informasi ini sebagai **diagnostic aid**, bukan sebagai parameter optimum otomatis.

---

## 11. Queen Contiguity

Queen contiguity mendefinisikan dua polygon sebagai tetangga jika keduanya:

- berbagi sisi (_edge_); atau
- bertemu pada titik sudut (_vertex_).

Queen biasanya menghasilkan neighborhood lebih luas dibanding Rook.

Cocok ketika interaksi antarwilayah dapat terjadi melalui kontak batas sekecil apa pun.

---

## 12. Rook Contiguity

Rook contiguity mensyaratkan polygon berbagi **sisi/batas**, bukan hanya titik.

Rook biasanya menghasilkan jumlah tetangga lebih sedikit dibanding Queen.

Cocok jika hubungan spasial diinterpretasikan memerlukan batas administratif bersama yang lebih kuat.

---

## 13. Bishop Contiguity

Dalam source `soviclust`, Bishop dibentuk sebagai:

```text
Queen neighbors − Rook neighbors
```

Dengan demikian, Bishop merepresentasikan unit yang bertemu pada **vertex** tetapi tidak berbagi sisi.

Bishop biasanya menghasilkan neighborhood yang lebih jarang dan perlu digunakan dengan pertimbangan substantif.

---

## 14. Penanganan Wilayah Tanpa Tetangga

Untuk Queen, Rook, Bishop, atau `dthr`, suatu wilayah dapat tidak memiliki tetangga.

Dalam implementasi ALFGWC saat ini, unit tersebut otomatis diberi fallback menggunakan:

```text
K-Nearest Neighbors (KNN), k = 1
```

berdasarkan centroid polygon.

Tujuan fallback adalah mencegah baris spatial weights tanpa tetangga.

> Dalam publikasi, laporkan jika terdapat wilayah yang memperoleh KNN fallback karena hal ini memengaruhi definisi neighborhood.

---

# BAGIAN C — SPATIAL WEIGHTING

## 15. Spatial Weighting Scheme

Setelah neighborhood $N_i$ ditentukan, ALFGWC menghitung spatial weights hanya pada anggota neighborhood tersebut.

Aplikasi menyediakan dua mode:

1. `DISTANCE_DECAY`
2. `SPATIAL_INTERACTION`

Bobot kemudian di-_row-standardized_ sehingga:

```math
\sum_{j \in N_i} W_{ij}=1.
```

---

## 16. Distance Decay

Raw weight dihitung dengan:

```math
f_{ij}
=
\frac{1}{d_{ij}^{\gamma}},
```

kemudian dinormalisasi:

```math
W_{ij}
=
\frac{f_{ij}}
{\sum_{j \in N_i} f_{ij}}.
```

dengan:

- $d_{ij}$ = jarak antara unit $i$ dan $j$;
- $\gamma$ = distance-decay exponent.

### Interpretasi $\gamma$

- $\gamma$ kecil → penurunan bobot terhadap jarak lebih lambat;
- $\gamma$ besar → tetangga dekat jauh lebih dominan.

Default aplikasi:

```text
γ = 2
```

Rentang UI saat ini:

```text
1 sampai 4
```

Nilai optimum tidak universal dan sebaiknya diuji melalui sensitivity analysis.

---

## 17. Spatial Interaction

**Penting:** implementasi ALFGWC saat ini menggunakan local population-distance interaction berikut:

```math
\phi_{ij}
=
\frac{P_j}{d_{ij}},
```

kemudian:

```math
W_{ij}
=
\frac{\phi_{ij}}
{\sum_{j \in N_i}\phi_{ij}}.
```

dengan:

- $P_j$ = populasi unit tetangga $j$;
- $d_{ij}$ = jarak unit $i$ ke $j$.

Artinya, tetangga dengan:

- populasi lebih besar; dan/atau
- jarak lebih dekat

mendapat pengaruh lebih besar.

> Formula ini adalah formula yang **benar-benar digunakan oleh source ALFGWC saat ini**. Ia berbeda dari implementasi LFGWC tertentu yang menggunakan bentuk umum SIM-PF seperti $(P_iP_j)^b/d_{ij}^a$.

---

# BAGIAN D — PARAMETER FUZZY CLUSTERING

## 18. Number of Clusters (`c`)

Parameter `c` menentukan jumlah cluster.

Rentang UI:

```text
2–10
```

Jangan memilih `c` hanya berdasarkan satu validity index.

Bandingkan beberapa nilai `c` menggunakan:

- PC;
- CE;
- SC;
- XB;
- IFV;
- Kwon;
- silhouette sebagai diagnostic tambahan;
- interpretabilitas profil cluster; dan
- kestabilan hasil antar-run.

---

## 19. Fuzzifier (`m`)

Fuzzifier mengontrol derajat keaburan membership.

Rentang UI:

```text
1.1–3.0
```

Default:

```text
m = 2.0
```

Interpretasi umum:

- $m \to 1$ → membership semakin tegas/hard;
- $m$ lebih besar → membership semakin diffuse/fuzzy.

Untuk eksperimen, beberapa nilai seperti:

```text
m = 1.5, 2.0, 2.5
```

dapat dibandingkan, tetapi pemilihan akhir harus didasarkan pada validity dan interpretasi data.

---

## 20. Membership FCM

Untuk centroid $V_k$, jarak Euclidean unit $i$ ke cluster $k$ adalah:

```math
D_{ik}
=
\lVert x_i - V_k \rVert_2.
```

Membership awal dihitung mengikuti FCM:

```math
U_{ik}
=
\frac{D_{ik}^{-\frac{2}{m-1}}}
{\sum_{h=1}^{c} D_{ih}^{-\frac{2}{m-1}}}.
```

Membership ini kemudian digunakan sebagai dasar adaptive spatial modification.

---

# BAGIAN E — MEKANISME ADAPTIF ALFGWC

## 21. Local Moran's I

ALFGWC menggunakan Local Moran's I untuk membedakan tingkat pengaruh neighborhood antarwilayah.

Local Moran's I merupakan Local Indicator of Spatial Association (LISA) yang mengukur asosiasi spasial lokal setiap unit terhadap tetangganya.

Aplikasi menggunakan:

```r
spdep::localmoran()
```

dengan row-standardized spatial list weights.

---

## 22. Target Variable for Local Moran's I

Pengguna dapat memilih variabel yang digunakan untuk menghitung Local Moran's I.

Untuk sumber fitur multivariat, aplikasi menyediakan:

- satu variabel terpilih; atau
- `Mean of selected features`.

Jika memilih mean:

```math
q_i
=
\frac{1}{p}
\sum_{r=1}^{p} x_{ir},
```

dengan $p$ jumlah fitur yang dipilih.

Pemilihan target variable penting karena hasil Local Moran's I menentukan $\alpha_i$.

---

## 23. Jangan Menyamakan Tanda Local Moran's I dengan Hotspot/Coldspot

Dokumentasi lama menyebut:

```text
I_i > 0 = Hotspot
I_i < 0 = Coldspot
```

Interpretasi tersebut **terlalu sederhana**.

Secara statistik:

- $I_i > 0$ dan signifikan menunjukkan **positive local spatial association**. Ini dapat mencakup pola High–High **atau** Low–Low.
- $I_i < 0$ dan signifikan menunjukkan **negative local spatial association / spatial outlier**, yang dapat mencakup High–Low atau Low–High.
- $p \ge 0.05$ diperlakukan sebagai pola tidak signifikan pada mekanisme adaptif saat ini.

Source ALFGWC saat ini menggunakan **tanda $I_i$ dan p-value**, bukan kuadran HH/LL/HL/LH secara eksplisit.

---

## 24. Adaptive Alpha Rules

Source ALFGWC membentuk $\alpha_i$ dengan aturan:

```text
if p < 0.05 and I_i > 0  → Alpha High
if p < 0.05 and I_i < 0  → Alpha Low
otherwise                 → Alpha Mid
```

Default:

| Kondisi                               | Parameter  | Default |
| ------------------------------------- | ---------- | ------: |
| Positive local association signifikan | Alpha High |     0.8 |
| Negative local association signifikan | Alpha Low  |     0.2 |
| Tidak signifikan / lainnya            | Alpha Mid  |     0.5 |

---

## 25. Arti $\alpha_i$ pada ALFGWC

**Ini merupakan bagian yang sangat penting.**

Pada source ALFGWC saat ini, $\alpha_i$ didefinisikan sebagai:

> **adaptive neighborhood influence coefficient**

bukan sebagai bobot membership lama.

Neighborhood-membership component dihitung:

```math
G_{ik}
=
\sum_{j \in N_i} W_{ij}U_{jk}.
```

Adaptive spatial membership dihitung:

```math
U_{ik}^{*}
=
(1-\alpha_i)U_{ik}
+
\alpha_iG_{ik}.
```

### Interpretasi

Jika:

```text
α_i = 0.8
```

maka:

```math
U_{ik}^{*}
=
0.2U_{ik}
+
0.8G_{ik}.
```

Artinya pengaruh neighborhood sebesar 80%.

Jika:

```text
α_i = 0.2
```

maka:

```math
U_{ik}^{*}
=
0.8U_{ik}
+
0.2G_{ik}.
```

Artinya karakteristik membership unit sendiri lebih dominan.

Sehingga:

```text
α_i → 0  : attribute/original-membership driven
α_i → 1  : neighborhood driven
```

---

## 26. Perbedaan Konvensi Alpha dengan LFGWC

Pada implementasi LFGWC `soviclust`, formula dasarnya menggunakan:

```math
U_{ik}^{*}
=
\alpha U_{ik}
+
(1-\alpha)G_{ik},
```

sehingga $\alpha$ pada LFGWC merupakan **bobot membership lama**.

Sebaliknya, implementasi ALFGWC saat ini menggunakan:

```math
U_{ik}^{*}
=
(1-\alpha_i)U_{ik}
+
\alpha_iG_{ik}.
```

sehingga $\alpha_i$ merupakan **bobot neighborhood**.

Dengan demikian, arti numerik parameter `alpha` pada LFGWC dan ALFGWC **tidak identik**.

> Untuk paper ALFGWC, disarankan mendefinisikan $\alpha_i$ secara eksplisit sebagai _adaptive neighborhood influence coefficient_ agar tidak terjadi ambiguitas.

---

## 27. Mengapa Alpha High untuk Positive Spatial Association?

Untuk unit dengan positive local association yang signifikan:

```text
Alpha High = 0.8
```

memberikan bobot lebih besar kepada neighborhood karena nilai lokal menunjukkan keterkaitan spasial yang kuat dengan lingkungan sekitarnya.

Untuk negative spatial association:

```text
Alpha Low = 0.2
```

mempertahankan bobot yang lebih besar pada membership unit sendiri sehingga spatial outlier tidak terlalu dipaksa mengikuti pola tetangga.

Alpha Mid memberikan kompromi:

```text
Alpha Mid = 0.5
```

untuk wilayah tanpa bukti asosiasi lokal yang signifikan.

Nilai 0.8/0.5/0.2 merupakan **default implementasi**, bukan nilai universal yang telah terbukti optimum untuk seluruh dataset.

---

# BAGIAN F — ALUR ITERASI ALFGWC

## 28. Tahapan Algoritma

Secara ringkas, ALFGWC saat ini menjalankan alur:

```text
1. Bangun feature matrix
        ↓
2. Bangun neighborhood
        ↓
3. Hitung spatial weights W
        ↓
4. Hitung Local Moran's I
        ↓
5. Bentuk adaptive alpha_i
        ↓
6. Inisialisasi centroid
        ↓
7. Hitung FCM membership U
        ↓
8. Geographic modification U*
        ↓
9. Update centroid menggunakan U*
        ↓
10. Hitung ulang membership dari centroid baru
        ↓
11. Hitung objective function J
        ↓
12. Cek convergence
        ↓
13. Ulangi jika belum konvergen
        ↓
14. Hard label = cluster dengan membership maksimum
        ↓
15. Validasi, profiling, mapping, stability analysis
```

---

## 29. Update Centroid

Setelah geographic modification, centroid cluster dihitung dengan:

```math
V_k
=
\frac{
\sum_{i=1}^{n}
(U_{ik}^{*})^m x_i
}{
\sum_{i=1}^{n}
(U_{ik}^{*})^m
}.
```

Dengan demikian, spatially adjusted membership memengaruhi posisi centroid pada iterasi berikutnya.

---

## 30. Objective Function

Objective function yang digunakan adalah bentuk fuzzy within-cluster dispersion:

```math
J
=
\sum_{i=1}^{n}
\sum_{k=1}^{c}
U_{ik}^{m}D_{ik}^{2}.
```

**Semakin kecil nilai $J$, semakin baik** dalam konteks minimisasi fungsi objektif.

Dokumentasi lama yang menyebut objective function harus “maksimal” adalah tidak tepat.

---

## 31. Convergence

Iterasi berhenti jika salah satu kondisi tercapai:

```math
|J^{(t)}-J^{(t-1)}| < \epsilon
```

atau:

```text
iteration >= Max. Iterations
```

Default:

```text
Max. Iterations = 100
ε = 0.001
```

---

## 32. Catatan tentang Membership pada Implementasi Saat Ini

Dalam source saat ini:

1. $U$ dimodifikasi menjadi $U^*$ menggunakan spatial neighborhood;
2. $U^*$ digunakan untuk memperbarui centroid;
3. membership kemudian dihitung kembali terhadap centroid yang baru;
4. membership hasil recomputation digunakan untuk objective, hard label, dan output akhir.

Dengan demikian, pengaruh ALFGWC masuk secara iteratif melalui **spatially informed centroid updating**.

Bagian ini penting ketika menjelaskan implementasi ALFGWC dalam paper atau melakukan replikasi numerik.

---

# BAGIAN G — OPTIMASI CENTROID

## 33. Classic ALFGWC

Jika memilih:

`Classic ALFGWC`

centroid awal dihasilkan secara acak di dalam rentang masing-masing fitur.

Pada source ALFGWC saat ini, classic initialization menggunakan **uniform random initialization**.

Random Seed membuat inisialisasi dapat direproduksi.

---

## 34. Metaheuristic Initialization

Selain Classic, tersedia:

- ABC — Artificial Bee Colony;
- FPA — Flower Pollination Algorithm;
- GSA — Gravitational Search Algorithm;
- GWO — Grey Wolf Optimizer;
- HHO — Harris Hawks Optimization;
- IFA — Intelligent Firefly Algorithm;
- PSO — Particle Swarm Optimization;
- TLBO — Teaching-Learning-Based Optimization;
- WOA — Whale Optimization Algorithm.

Metaheuristic digunakan untuk mencari **initial centroid configuration**, kemudian centroid tersebut masuk ke proses refinement ALFGWC.

Dengan kata lain, optimizer pada modul ini:

> **mengoptimasi inisialisasi centroid**, bukan mengoptimasi $\alpha_i$ atau struktur neighborhood.

---

## 35. Universal Optimizer Parameters

Untuk optimizer non-classic tersedia:

| Parameter                    | Default | Arti                                 |
| ---------------------------- | ------: | ------------------------------------ |
| Number of Particles / Agents |      10 | Jumlah kandidat solusi               |
| Convergence (`same`)         |      10 | Batas stagnasi/konvergensi optimizer |
| Initialization Distribution  | Uniform | Distribusi kandidat awal             |

Lebih banyak agent meningkatkan ruang eksplorasi tetapi juga meningkatkan waktu komputasi.

---

## 36. Parameter Spesifik Optimizer

### ABC

| Parameter     | Default |
| ------------- | ------: |
| Onlooker Bees |       5 |
| Scout Limit   |       5 |

### FPA

| Parameter          |    Default |
| ------------------ | ---------: |
| Switch Probability |        0.7 |
| Gamma              |        1.2 |
| Lambda             |        1.5 |
| EI Distribution    | logchaotic |

### GSA

| Parameter              | Default |
| ---------------------- | ------: |
| Gravitational Constant |       1 |
| Vmax                   |     0.7 |
| New GSA version        |   FALSE |

### GWO

GWO menggunakan parameter universal jumlah agent, stagnation/convergence, dan initialization distribution pada interface saat ini.

### HHO

| Parameter   |  Default |
| ----------- | -------: |
| HHO Variant | Bairathi |
| a1          |        3 |
| a2          |        1 |
| a3          |      0.4 |

### IFA

| Parameter          |    Default |
| ------------------ | ---------: |
| Selected Fireflies |          3 |
| Gamma              |          1 |
| Beta               |          1 |
| EI Distribution    | logchaotic |

### PSO

| Parameter    | Default |
| ------------ | ------: |
| Vmax         |     0.8 |
| c1           |     0.7 |
| c2           |     0.6 |
| Inertia Type | chaotic |
| wmax         |     0.8 |
| wmin         |     0.3 |

### TLBO

| Parameter            | Default |
| -------------------- | ------: |
| Number of Selections |      10 |
| Elitism              |   FALSE |
| Number of Elites     |       2 |

### WOA

| Parameter             | Default |
| --------------------- | ------: |
| Spiral Constant (`b`) |       1 |

> GWO dan WOA merupakan optimization extensions yang ditambahkan dalam `soviclust` dan masih berada dalam proses methodological benchmarking. Hindari klaim bahwa keduanya lebih baik dari optimizer lain tanpa eksperimen multi-run dan evaluasi statistik yang memadai.

---

# BAGIAN H — MENJALANKAN ALFGWC

## 37. Workflow Praktis

Urutan penggunaan yang disarankan:

### Tahap 1 — Pastikan data utama dan shapefile sudah dimuat

Periksa:

- ID data;
- ID shapefile;
- missing values;
- variabel konstan;
- jumlah unit.

### Tahap 2 — Siapkan spatial supporting data

Upload:

- distance matrix atau longitude-latitude;
- population data.

### Tahap 3 — Pilih Feature Data Source

Untuk analisis multivariat umum, mulai dari:

```text
Normalized Data (0–1)
```

atau RC scores jika menggunakan hasil PCA.

### Tahap 4 — Tentukan jumlah cluster

Mulai dari beberapa kandidat, misalnya:

```text
c = 2–7
```

dan bandingkan validity index.

### Tahap 5 — Tentukan neighborhood

Pilih salah satu:

```text
Queen
Rook
Bishop
dthr
```

Untuk studi administratif berbasis polygon, Queen sering menjadi titik awal yang masuk akal, tetapi bukan default universal terbaik.

### Tahap 6 — Tentukan weighting scheme

Pilih:

```text
Spatial Interaction
```

jika populasi menjadi bagian dari konsep interaksi spasial.

Pilih:

```text
Distance Decay
```

jika ingin pengaruh tetangga terutama ditentukan oleh kedekatan geografis.

### Tahap 7 — Tentukan adaptive alpha

Baseline awal:

```text
Alpha High = 0.8
Alpha Mid  = 0.5
Alpha Low  = 0.2
```

### Tahap 8 — Pilih optimizer

Mulai dengan:

```text
Classic
```

sebagai baseline.

Kemudian bandingkan metaheuristic menggunakan parameter dan jumlah run yang konsisten.

### Tahap 9 — Set Random Seed

Gunakan seed tetap untuk satu eksperimen reproducible.

### Tahap 10 — Klik `Run ALFGWC`

Evaluasi hasil secara menyeluruh, bukan hanya berdasarkan satu indeks.

---

# BAGIAN I — INTERPRETASI OUTPUT

## 38. Summary

Tab Summary menampilkan konfigurasi utama seperti:

- algorithm;
- data source;
- jumlah features;
- jumlah iterasi;
- weighting mode;
- gamma.

Gunakan bagian ini untuk memastikan konfigurasi yang dijalankan sesuai eksperimen.

---

## 39. Objective Function Convergence

Kurva convergence menunjukkan perubahan $J$ selama iterasi.

Interpretasi umum:

- penurunan cepat → algoritma bergerak menuju solusi lebih compact;
- kurva mendatar → convergence;
- fluktuasi besar → periksa parameter, initialization, atau optimizer.

Karena objective diminimalkan:

> **nilai lebih rendah lebih baik**, tetapi perbandingan harus menggunakan data, $c$, $m$, neighborhood, dan weighting scheme yang sama.

---

## 40. Partition Coefficient (PC)

```math
PC
=
\frac{1}{n}
\sum_{i=1}^{n}
\sum_{k=1}^{c}
U_{ik}^{2}.
```

Interpretasi:

> **lebih tinggi lebih baik**

PC tinggi menunjukkan membership lebih terkonsentrasi.

Namun nilai PC dipengaruhi jumlah cluster sehingga jangan digunakan sendirian.

---

## 41. Classification Entropy (CE)

```math
CE
=
-\frac{1}{n}
\sum_{i=1}^{n}
\sum_{k=1}^{c}
U_{ik}\log U_{ik}.
```

Interpretasi:

> **lebih rendah lebih baik**

CE rendah menunjukkan ketidakpastian membership yang lebih kecil.

---

## 42. Separation Coefficient / Partition Index (SC)

SC menilai kombinasi compactness dan separation.

Interpretasi pada implementasi `soviclust`:

> **lebih rendah lebih baik**

Gunakan bersama PC, CE, XB, dan IFV.

---

## 43. Xie–Beni Index (XB)

XB membandingkan within-cluster fuzzy dispersion dengan separation minimum antarcentroid.

Secara umum:

```math
XB
=
\frac{
\sum_{i=1}^{n}
\sum_{k=1}^{c}
U_{ik}^{m}
\lVert x_i-V_k\rVert^2
}{
n
\min_{k\ne h}
\lVert V_k-V_h\rVert^2
}.
```

Interpretasi:

> **lebih rendah lebih baik**

---

## 44. IFV

IFV merupakan fuzzy validity index yang mempertimbangkan konsentrasi membership, separation cluster, dan dispersion.

Interpretasi pada aplikasi:

> **lebih tinggi lebih baik**

---

## 45. Kwon Index

Kwon Index merupakan indeks fuzzy compactness/separation lainnya.

Interpretasi:

> **lebih rendah lebih baik**

---

## 46. Silhouette Index

Silhouette pada modul ALFGWC dihitung setelah fuzzy membership diubah menjadi hard cluster menggunakan:

```text
cluster_i = argmax_k(U_ik)
```

Kemudian silhouette dihitung menggunakan Euclidean distance pada feature space.

Interpretasi:

- mendekati 1 → hard partition relatif kompak dan terpisah;
- sekitar 0 → overlap/boundary;
- negatif → unit lebih dekat dengan cluster lain.

**Jangan diabaikan**, tetapi interpretasikan sebagai **supplementary hard-partition diagnostic**, bukan ukuran utama kualitas fuzzy-spatial membership.

---

## 47. Cluster Map

Cluster Map menampilkan hard cluster berdasarkan membership terbesar.

Gunakan peta untuk mengevaluasi:

- pola spasial;
- fragmentasi cluster;
- wilayah transisi;
- kemungkinan cluster yang terlalu tersebar;
- hubungan dengan konteks geografis.

Warna cluster tidak menunjukkan urutan tinggi-rendah kecuali cluster telah diberi label substantif.

---

## 48. Max Membership Map

Max Membership:

```math
M_i
=
\max_k U_{ik}.
```

Nilai mendekati 1:

> unit memiliki dominant membership yang kuat.

Nilai lebih rendah:

> unit berada pada boundary atau memiliki karakteristik campuran.

Max membership sangat berguna untuk mengidentifikasi **spatial transition zones**.

---

## 49. Cluster Profile

Cluster Profile menghitung rata-rata fitur per cluster.

Gunakan untuk memberi interpretasi substantif.

Contoh:

```text
Cluster 1 → poverty tinggi, low education tinggi, sanitation buruk
Cluster 2 → infrastructure baik, poverty rendah
...
```

Hindari memberi label seperti “baik” atau “buruk” hanya berdasarkan nomor cluster.

---

## 50. Heatmap

Heatmap memudahkan perbandingan rata-rata setiap fitur antarcluster.

Periksa:

- fitur pembeda utama;
- indikator dominan;
- cluster dengan pola serupa;
- variabel yang tidak memberikan diferensiasi berarti.

---

## 51. Radar Chart

Radar chart berguna untuk komunikasi profil multivariat.

Gunakan terutama jika feature sudah berada dalam skala seragam.

Radar chart lebih cocok sebagai alat interpretasi daripada sebagai alat validasi.

---

## 52. Dimensional Reduction

ALFGWC menyediakan:

- Sammon Mapping;
- t-SNE;
- UMAP.

Visualisasi tersebut merupakan **projection diagnostics**, bukan bukti formal bahwa clustering optimal.

Gunakan untuk melihat apakah hard cluster tampak terpisah dalam representasi 2D.

---

# BAGIAN J — STABILITY ANALYSIS

## 53. Mengapa Stability Analysis Penting?

Metaheuristic dan random initialization bersifat stochastic.

Satu run dengan hasil sangat baik belum membuktikan algoritma stabil.

Karena itu, `soviclust` menyediakan:

`Stability Analysis`

untuk menjalankan ALFGWC berulang kali dengan seed berbeda.

---

## 54. Parameter Stability

Default:

```text
Number of Runs = 30
Seed Start     = 1
```

Rentang UI:

```text
5–100 runs
```

Jika:

```text
Number of Runs = 30
Seed Start = 1
```

maka eksperimen menggunakan seed:

```text
1, 2, 3, ..., 30
```

---

## 55. Output Stability

Untuk setiap validity index, aplikasi menyediakan:

- Mean;
- Standard Deviation;
- Best;
- Worst;
- Median.

Selain itu tersedia:

- boxplot distribusi antar-run;
- tabel detail setiap run;
- jumlah run berhasil/gagal;
- runtime;
- objective function;
- CSV download.

---

## 56. Membaca Stability Result

Metode yang baik seharusnya tidak hanya mempunyai:

```text
best score yang bagus
```

tetapi juga:

```text
mean yang bagus + SD yang kecil.
```

Contoh:

| Metode | Mean PC | SD PC |
| ------ | ------: | ----: |
| A      |    0.72 | 0.001 |
| B      |    0.74 | 0.080 |

Metode B mempunyai mean lebih tinggi, tetapi jauh lebih tidak stabil.

Dalam software paper, laporkan setidaknya:

```text
Mean ± SD
Best
Median
Execution Time
```

untuk setiap optimizer.

---

# BAGIAN K — REKOMENDASI EKSPERIMEN

## 57. Baseline Parameter

Konfigurasi awal yang dapat digunakan untuk eksplorasi:

| Parameter     | Baseline              |
| ------------- | --------------------- |
| Feature       | Normalized `[0,1]`    |
| `c`           | 4                     |
| `m`           | 2                     |
| Neighborhood  | Queen                 |
| Weighting     | Spatial Interaction   |
| Gamma         | 2 jika Distance Decay |
| Alpha High    | 0.8                   |
| Alpha Mid     | 0.5                   |
| Alpha Low     | 0.2                   |
| Max Iteration | 100                   |
| Tolerance     | 0.001                 |
| Optimizer     | Classic               |
| Seed          | 0                     |

Parameter tersebut **bukan parameter optimum universal**.

---

## 58. Eksperimen Jumlah Cluster

Disarankan mencoba:

```text
c = 2, 3, 4, 5, 6, 7
```

dengan parameter lain tetap.

Bandingkan:

- PC ↑
- CE ↓
- SC ↓
- XB ↓
- IFV ↑
- Kwon ↓
- Silhouette ↑ sebagai supplementary metric
- interpretabilitas cluster
- stability

---

## 59. Eksperimen Fuzzifier

Contoh:

```text
m = 1.5
m = 2.0
m = 2.5
```

Jangan membandingkan metode menggunakan `m` berbeda jika tujuan Anda adalah membandingkan kemampuan algoritma.

---

## 60. Eksperimen Neighborhood

Karena neighborhood merupakan salah satu extension utama ALFGWC, lakukan komparasi:

```text
dthr
Queen
Rook
Bishop
```

dengan fitur dan parameter lain sama.

Evaluasi tidak hanya CVI, tetapi juga:

- jumlah neighbors;
- jumlah KNN fallback;
- spatial coherence;
- cluster profile;
- stability.

---

## 61. Eksperimen Adaptive Alpha

Untuk membuktikan manfaat adaptasi, bandingkan misalnya:

### Fixed

```text
α_i = 0.5 untuk seluruh wilayah
```

versus:

### Adaptive

```text
positive association → 0.8
non-significant      → 0.5
negative association → 0.2
```

Ini dapat membantu menjawab apakah mekanisme adaptive memberikan improvement dibanding fixed spatial influence.

---

## 62. Eksperimen Optimizer

Bandingkan:

```text
Classic
ABC
FPA
GSA
GWO
HHO
IFA
PSO
TLBO
WOA
```

Gunakan:

- data sama;
- `c` sama;
- `m` sama;
- neighborhood sama;
- alpha rules sama;
- jumlah run sama;
- agent budget yang sebanding jika memungkinkan.

Disarankan minimal:

```text
30 independent runs
```

untuk algoritma stochastic.

---

# BAGIAN L — REPRODUCIBILITY

## 63. Parameter yang Harus Dilaporkan dalam Paper

Untuk replikasi ALFGWC, laporkan:

### Data

- dataset;
- jumlah wilayah;
- feature variables;
- scaling/normalization.

### Spatial structure

- distance source;
- distance unit;
- neighborhood type;
- `dthr` jika digunakan;
- jumlah KNN fallback;
- weighting scheme;
- $\gamma$ jika distance decay.

### Adaptive mechanism

- Local Moran target variable;
- significance threshold;
- Alpha High;
- Alpha Mid;
- Alpha Low.

### Fuzzy clustering

- number of clusters;
- fuzzifier;
- max iteration;
- convergence tolerance.

### Optimization

- optimizer;
- agent count;
- optimizer-specific parameters;
- seed;
- number of independent runs.

### Evaluation

- objective;
- PC;
- CE;
- SC;
- XB;
- IFV;
- Kwon;
- silhouette;
- runtime;
- stability statistics.

---

## 64. Contoh Ringkasan Konfigurasi Paper

```text
Feature data       : 15 normalized vulnerability indicators
Number of units    : 514 regencies/cities
Clusters           : 4
Fuzzifier          : 2.0
Neighborhood       : Queen contiguity
Weighting          : Distance Decay
Gamma              : 2
Adaptive alpha     : 0.8 / 0.5 / 0.2
LISA significance  : p < 0.05
Optimizer          : GWO
Agents             : 20
Independent runs   : 30
Max iteration      : 100
Tolerance          : 0.001
```

---

# BAGIAN M — TROUBLESHOOTING

## 65. “IDs in dataset do not match IDs in shapefile”

Periksa:

- format ID;
- leading zero;
- tipe numeric vs character;
- unit yang hilang;
- duplicate IDs.

---

## 66. “Distance matrix and population data are required”

Pastikan kedua supporting input sudah terbaca.

Jumlah unit distance matrix dan population vector harus konsisten dengan feature data.

---

## 67. Cluster Kosong

Kemungkinan penyebab:

- jumlah cluster terlalu besar;
- centroid initialization buruk;
- fitur kurang informatif;
- parameter fuzzy terlalu ekstrem;
- neighborhood/weight terlalu dominan.

Coba:

- kurangi `c`;
- ganti seed;
- gunakan optimizer;
- cek scaling;
- evaluasi neighborhood.

---

## 68. Max Membership Rendah

Hal ini tidak selalu error.

Dapat menunjukkan:

- overlap cluster tinggi;
- wilayah transisi;
- fuzzifier terlalu besar;
- profil cluster kurang terpisah.

Periksa PC, CE, XB, IFV, profile, dan stability.

---

## 69. Hasil Berubah Setiap Run

Hal tersebut normal untuk stochastic initialization/optimizer.

Gunakan:

- fixed seed untuk reproducibility;
- Stability Analysis untuk robustness;
- 30 atau lebih independent runs untuk benchmarking.

---

## 70. Runtime Lama

Runtime meningkat karena:

- banyak unit;
- banyak fitur;
- banyak cluster;
- neighborhood padat;
- jumlah agents tinggi;
- optimizer kompleks;
- banyak independent runs.

Mulai dengan konfigurasi kecil untuk debugging, lalu tingkatkan computational budget pada eksperimen final.

---

# BAGIAN N — CATATAN IMPLEMENTASI PENTING

## 71. Arti Alpha di UI

Pada source UI saat ini masih terdapat teks:

```text
Old membership weights, dynamically adjusted
```

Teks tersebut **tidak konsisten** dengan formula ALFGWC aktual.

Pada source ALFGWC aktual:

```math
U_{ik}^{*}
=
(1-\alpha_i)U_{ik}
+
\alpha_iG_{ik},
```

sehingga $\alpha_i$ adalah **neighbor influence weight**.

Untuk konsistensi software, label UI sebaiknya diubah menjadi misalnya:

```text
Adaptive Neighborhood Influence (α_i)
```

atau:

```text
Neighborhood Influence Weight
```

---

## 72. Positive Local Moran Tidak Selalu “Hotspot”

UI saat ini masih menggunakan istilah:

```text
Hotspot region I>0
Coldspot region I<0
```

Secara statistik label ini sebaiknya diperbaiki menjadi:

```text
Positive Local Association (I>0, p<0.05)
Negative Local Association (I<0, p<0.05)
```

karena penentuan High–High, Low–Low, High–Low, dan Low–High memerlukan informasi kuadran nilai dan spatial lag, bukan hanya tanda Local Moran's I.

---

## 73. Research Status

ALFGWC merupakan metode yang dikembangkan di dalam proyek `soviclust` dan masih berada dalam proses:

- methodological validation;
- sensitivity analysis;
- benchmark against LFGWC/FGWC;
- optimizer comparison;
- stability evaluation;
- software-paper documentation.

Karena itu, hindari klaim seperti:

```text
“ALFGWC is superior to all previous methods”
```

sebelum didukung eksperimen dan pengujian yang memadai.

Lebih aman menggunakan:

```text
“ALFGWC extends LFGWC through adaptive neighborhood influence
and flexible neighborhood definitions.”
```

---

# BAGIAN O — SAMPLE DATA

## 74. Dataset Indonesia

Sample workflow `soviclust` menggunakan data 514 kabupaten/kota Indonesia yang bersumber dari dataset:

Kurniawan, R., Nasution, B. I., Agustina, N., & Yuniarto, B. (2022).  
**Revisiting social vulnerability analysis in Indonesia data.**  
_Data in Brief, 40_, 107743.  
https://doi.org/10.1016/j.dib.2021.107743

Jika sample data tersebut digunakan dalam publikasi, cite:

1. dataset Kurniawan et al. (2022); dan
2. `soviclust` sebagai software yang digunakan untuk analisis.

---

# BAGIAN P — REFERENSI

## 75. Referensi Metodologis Utama

- Anselin, L. (1995). Local Indicators of Spatial Association—LISA. _Geographical Analysis, 27_(2), 93–115. https://doi.org/10.1111/j.1538-4632.1995.tb00338.x
- Bezdek, J. C. (1981). _Pattern Recognition with Fuzzy Objective Function Algorithms_. Springer.
- Grekousis, G. (2021). Local fuzzy geographically weighted clustering: a new method for geodemographic segmentation. _International Journal of Geographical Information Science, 35_(1), 152–174. https://doi.org/10.1080/13658816.2020.1808221
- Kurniawan, R., Nasution, B. I., Agustina, N., & Yuniarto, B. (2022). Revisiting social vulnerability analysis in Indonesia data. _Data in Brief, 40_, 107743. https://doi.org/10.1016/j.dib.2021.107743
- Mason, G. A., & Jacobson, R. D. (2007). _Fuzzy Geographically Weighted Clustering_. Methodological foundation for geographically weighted fuzzy clustering.

---

# BAGIAN Q — RINGKASAN

## 76. Inti ALFGWC dalam `soviclust`

ALFGWC dapat diringkas sebagai:

```text
FCM membership
      +
local spatial neighborhood
      +
row-standardized spatial weights
      +
adaptive neighborhood influence α_i
      +
optional metaheuristic centroid initialization
      ↓
adaptive spatial fuzzy geodemographic clustering
```

Kontribusi utamanya terhadap LFGWC adalah:

```text
1. Fixed spatial influence
   → adaptive α_i per wilayah berbasis Local Moran's I

2. Distance-threshold neighborhood only
   → dthr / Queen / Rook / Bishop

3. Single-run workflow
   → integrated multi-run stability assessment in soviclust

4. Conventional initialization
   → optional multi-metaheuristic centroid initialization
```

Untuk penggunaan ilmiah, interpretasikan ALFGWC melalui **kombinasi membership, validity indices, convergence, cluster profiles, spatial maps, dan multi-run stability**, bukan hanya dari hard cluster label atau satu indeks tunggal.

---

## Citation

Jika menggunakan `soviclust`, cite software:

```bibtex
@software{istiawan2026soviclust,
  author  = {Istiawan, Deden},
  title   = {{soviclust}: An Interactive R Package for Social Vulnerability
             Assessment, Spatial Diagnostics, and Fuzzy Geodemographic Clustering},
  year    = {2026},
  url     = {https://github.com/dedenistiawan/soviclust}
}
```

Untuk sample dataset Indonesia, cite Kurniawan et al. (2022) secara terpisah.

---

**Maintainer:** Deden Istiawan  
**Repository:** `dedenistiawan/soviclust`  
**License:** GNU General Public License v3 (GPL-3)
