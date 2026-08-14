# Panduan Penggunaan Menu ALFGWC

Dokumen ini menjelaskan tahapan lengkap dalam menggunakan menu **Adaptive Local Fuzzy Geographically Weighted Clustering (ALFGWC)** pada aplikasi Shiny ini, mulai dari persiapan input data hingga interpretasi output, beserta penjelasan setiap parameter yang tersedia.

---

## Tahap 1: Konfigurasi Data Pendukung
Kolom pertama ini mengatur basis data fitur (atribut) dan pembentukan struktur keruangan (spasial) antar wilayah.

- **Sumber Data Atribut**: Memilih jenis data yang akan di-*cluster*.
  - *Raw Data Asli*: Data asli tanpa diubah.
  - *Raw Data (Ternormalisasi min-max 0-1)*: Data di-skala antara 0 dan 1 (Sangat disarankan untuk algoritma *Fuzzy*).
  - *Standardized (Z-Score)*: Variabel SoVI yang sudah distandardisasi.
  - *Skor SoVI*: Menggunakan skor akhir kerentanan tunggal.
  - *Skor RC*: Menggunakan komponen utama hasil ekstraksi PCA.
- **Pilih Variabel Atribut**: Checklist kolom mana saja yang ingin diikutsertakan dalam klasterisasi.
- **Tipe Geometri Tetangga**: Menentukan bagaimana dua wilayah dikatakan bertetangga.
  - *Queen Contiguity*: Bersinggungan garis batas (edge) atau hanya 1 titik sudut (vertex).
  - *Rook Contiguity*: Hanya yang bersinggungan garis batas penuh (edge).
  - *Bishop Contiguity*: Hanya bersinggungan pada titik sudut (jarang dipakai).
  - *Distance Threshold (Jarak)*: Dua wilayah bertetangga jika jarak geografisnya $\le d_{thr}$.
- **Distance Threshold (Radius)**: Jika opsi jarak dipilih, masukkan nilai radiusnya. Angka -99 menandakan semua wilayah saling terhubung (Global).

---

## Tahap 2: Parameter Dasar ALFGWC
Bagian ini mengatur algoritma fundamental dari sistem *Fuzzy Clustering*.

- **Jumlah Cluster (k)**: Jumlah kelompok (*clusters*) yang ingin dibentuk.
- **Fuzzifier (m)**: Derajat keaburan/fuzziness (umumnya 2.0). Semakin besar, keanggotaan makin merata; jika mendekati 1, berperilaku seperti kluster tegas (K-Means).
- **Spatial Weighting Scheme**: Menentukan bagaimana bobot keruangan ($W$) dikalkulasi. Matriks bobot ini akan di-*row-standardized* sedemikian rupa sehingga $\sum_{j} W_{ij} = 1$.
  - *DISTANCE_DECAY*: Bobot melemah seiring bertambahnya jarak antar tetangga. Rumus dasarnya: 
    $$W_{ij} = \frac{1}{d_{ij}^\gamma}$$
  - *SPATIAL_INTERACTION*: Mempertimbangkan jumlah populasi ($P$) selain dari faktor jarak (menggunakan rumus SIM-PF):
    $$W_{ij} = \frac{(P_i \times P_j)^b}{d_{ij}^a}$$
- **Eksponen Jarak ($\gamma$, $a$, $b$)**: Kekuatan penalti pelemahan jarak. Semakin besar angka ini, hanya tetangga yang benar-benar dekat saja yang memberi dampak signifikan.
- **Maks. Iterasi**: Batasan siklus perulangan maksimal untuk mencegah *looping* tanpa akhir (contoh: 100).
- **Toleransi Konvergensi ($\epsilon$)**: Batas kesalahan iterasi. Bila selisih nilai fungsi objektif antar iterasi lebih kecil dari angka ini (misal 0.001), maka iterasi berhenti secara otomatis.
- **Random Seed**: Angka kunci untuk generator bilangan acak, agar hasil selalu identik/bisa direproduksi jika parameter yang dimasukkan sama.

---

## Tahap 3: Mekanisme Adaptif (Pembeda ALFGWC)
Faktor utama yang membedakan ALFGWC dengan algoritma pendahulunya (LFGWC) adalah kemampuan menyesuaikan nilai proporsi ketetanggaan ($\alpha$) secara cerdas (adaptif) untuk masing-masing wilayah $i$ berdasarkan deteksi pola *Hotspot / Coldspot* (Local Moran's I).

Pada LFGWC biasa, pembaruan keanggotaan spasial menggunakan $\alpha$ global (sama untuk semua wilayah):
$$U_{ij} = \alpha \cdot u_{ij} + (1 - \alpha) \sum_{k \in \text{Neighbor}} W_{ik} \cdot u_{kj}$$

Pada **ALFGWC**, nilai $\alpha$ berubah menjadi $\alpha_i$ (adaptif per wilayah):
$$U_{ij} = \alpha_i \cdot u_{ij} + (1 - \alpha_i) \sum_{k \in \text{Neighbor}} W_{ik} \cdot u_{kj}$$

Penentuan $\alpha_i$ diatur melalui:
- **Target Variabel Local Moran's I**: Variabel yang digunakan untuk menghitung Indeks Autokorelasi Spasial ($I_i$).
- **Alpha High (Hotspot)**: Digunakan jika wilayah bernilai tinggi dikelilingi wilayah bernilai tinggi ($I_i > 0$ dan $p < 0.05$). **Saran: 0.8**. Artinya, pertahankan 80% karakteristik asli daerah tersebut ($u_{ij}$), karena daerah sekelilingnya juga sudah mirip dengannya.
- **Alpha Low (Coldspot)**: Digunakan jika wilayah berlawanan dengan sekelilingnya atau bernilai rendah dikelilingi yang rendah ($I_i < 0$ dan $p < 0.05$). **Saran: 0.2**. Artinya, serap lebih banyak (80%) informasi dari tetangga-tetangganya.
- **Alpha Mid**: Untuk wilayah acak/tidak signifikan secara autokorelasi spasial ($p \ge 0.05$). **Saran: 0.5**. Karakteristik dibagi rata (50-50) antara wilayah sendiri dan tetangganya.

---

## Tahap 4: Algoritma Optimasi Inisialisasi
Mencari posisi ideal matriks Centroid ($V$) sebelum perulangan ALFGWC dimulai, menggunakan optimasi cerdas (*metaheuristics / swarm intelligence*).

- **Pilih Algoritma**: 
  - *Classic ALFGWC*: Memilih posisi centroid secara acak total (tanpa optimasi pra-pemrosesan). Cepat, tapi berpotensi terjebak di *local minima*.
  - Algoritma Swarm seperti PSO, ABC, FPA, dsb.: Agen optimasi akan berkeliling di ruang data untuk mencari titik awal centroid terbaik. Sangat dianjurkan agar fungsi obyektif akhir ($J$) maksimal.
- **Parameter Algoritma**: Parameter khusus algoritma yang Anda pilih. Contoh universal:
  - *Jumlah Partikel/Agen*: Semakin banyak, semakin teliti pencariannya, tapi semakin berat (lambat) kerjanya (Saran: 10 - 20).
  - *Vmax, c1, c2 (Contoh untuk PSO)*: Parameter kecepatan partikel, bobot kognitif (menuju pengalaman partikel terbaik), dan bobot sosial (menuju pengalaman global terbaik).

---

## Tahap 5: Eksekusi dan Penafsiran Hasil
Klik **Jalankan ALFGWC** dan biarkan *progress bar* di bawah tombol berjalan. Setelah selesai, lihat *box* Hasil di bagian bawah:

1. **Peta Interaktif (Tab Peta Wilayah)**: Menampilkan peta *choropleth* (warna-warni). Anda dapat meng-klik poligon/area untuk melihat pop-up yang berisi status Kluster, Skor Maximum Membership, serta atribut SoVI.
2. **Peta Max Membership**: Menampilkan gradasi warna seberapa dominan keanggotaan tertinggi pada setiap wilayah. (Semakin pucat, semakin "bingung" algoritma menentukan batas wilayah tersebut).
3. **Profil Cluster (Tab Profil Cluster)**: 
   - Anda bisa membaca Heatmap atau Radar Chart. Chart ini akan sangat berguna untuk memberi 'Nama Kluster'. (Misal: Kluster 1 tinggi pada fitur edukasi dan ekonomi, sehingga dilabeli "Kluster Wilayah Maju").
4. **Indeks Validasi (Tab Indeks Validasi)**:
   Perhatikan indikator berikut untuk mengetahui keandalan pemodelan Anda:
   - **Xie-Beni (XB)**: Rasio kekompakan dan perpisahan. *Semakin kecil, semakin baik (mendekati nol).*
   - **Partition Coefficient (PC)**: Seberapa tegas klusternya. *Semakin tinggi (mendekati 1), semakin baik.*
   - **Classification Entropy (CE)**: Ketidakpastian pengelompokan. *Semakin kecil, semakin baik.*
   - *(Note: Abaikan Mean Silhouette jika nilainya rendah, karena matriks Silhouette adalah algoritma "Hard Clustering" non-spasial, yang bertolak belakang dengan prinsip geografis ALFGWC).*

Jika Anda merasa hasil kurang optimal, ubah nilai Alpha, Fuzzifier, atau ganti algoritma optimasi (misalnya ke PSO), lalu klik Jalankan ALFGWC kembali. Gunakan tombol **Download** untuk mengunduh tabel Matriks Keanggotaan dan Hasil ke Excel (CSV) saat dirasa sudah sempurna.
