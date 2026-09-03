# NEWS.md - soviclust Changelog

Follows the [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
format and [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

# soviclust 0.6.0

### Changed
- Complete translation of all user-facing UI strings from Indonesian to English across
  all modules (ALFGWC, FGWC, LFGWC, kmeans, method_comparison, extended_analysis,
  sovi_core, cluster_geo, dbscan, downloads, var_config, sovi_analysis)
- Translated: notifications, error messages, plot titles/subtitles/axes, validation
  tables, silhouette interpretation labels, radar profiles, dim-reduction plots
  (Sammon/t-SNE/UMAP), recommendation boxes, convergence plots, status cards

### Fixed
- Syntax error in `sovi_core.R` caused by escaped quotes in `case_when` labels

### Documentation
- Rewrote `README.md` in English following standard R package conventions
- Added Overview, Features, Quick Start, Application Workflow, SoVI Formula,
  Bundled Sample Data, Clustering Algorithms, System Requirements, Citation (BibTeX),
  References, Contributing, and License sections

---

# soviclust 0.5.0

### Fixed
- Trailing comma in `dashboardHeader()` in `inst/app/ui.R` causing `run_app()`
  to fail with "argument is missing, with no default" error
- Shapefile size reduced from 19MB to 0.94MB (RDS format) to prevent timeout
  during `install_github()`
- Duplicate i18n keys causing `row.names` error on `run_app()`
- Corrected `update_lang()` argument order for shiny.i18n v0.3.0 compatibility

### Changed
- Application is now English-only; language switcher and other language options
  removed; default i18n language set to `en`
- K-Means and DBSCAN data sources unified to follow the same 5-option pattern
  as FGWC (raw / raw_norm / standardized / sovi / rc)

### Removed
- 4 unused extdata files (~2.1MB total): `sovi_data_kab_514.RData`,
  `Sovi_pop_514.RData`, `sovi_dist_514.RData`, `sovi_data_kab_514_19.xlsx`

---

# soviclust 0.1.0

### Added

#### Shiny Application
- Full interactive application launched via `soviclust::run_app()`
- Tab **Import / Load Data**: upload Excel/CSV data and shapefiles, or use
  the bundled 514 Indonesian district sample dataset
- Tab **Variable Config**: configure variables and direction (+/-) per indicator
- Tab **Method Comparison**: simultaneous comparison of 3 direction methods
  (Theory-Based, Loading Sign, Cutter's Method)
- Tab **SoVI Computation**: automated pipeline Z-score -> PCA -> weighted
  aggregation -> Jenks Natural Breaks classification (5 classes)
- Tab **Extended Analysis**: Moran's I, LISA, dominant component, component
  profile (radar chart), and sensitivity analysis
- Tab **Cluster Analysis** with 4 methods:
  - **ClustGeo** — spatial hierarchical clustering with automatic optimal alpha
  - **FGWC** — Fuzzy Geographically Weighted Clustering with 9 metaheuristic
    optimizers (PSO, ABC, GWO, WOA, HHO, FPA, GSA, TLBO, IFA)
  - **LFGWC** — Local FGWC (Grekousis 2020)
  - **ALFGWC** — Adaptive LFGWC
- Automatic sample data loading for FGWC/LFGWC/ALFGWC via "Load Sample Data"
  button: `Distance_matrix_514.xlsx`, `Koordinat.xlsx`, `sovi_data_pop_514.xlsx`
- Tab **SoVI Analysis**: per-variable choropleth maps with GVF index
- Tab **Downloads**: export results as CSV and high-resolution PNG maps

#### Package Infrastructure
- `run_app()` function with dependency validation and automatic path resolution
- Informative startup message via `.onAttach()`
- 4 bundled sample datasets in `inst/extdata/`:
  - `sovi_data_kab_514_15.xlsx` — 514 districts, 15 variables, year 2015
  - `Koordinat.xlsx` — centroid coordinates for 514 districts
  - `Distance_matrix_514.xlsx` — 514x514 distance matrix
  - `sovi_data_pop_514.xlsx` — population data for 514 districts
- Bundled shapefile: `514_kabupaten.shp` (514 features, ID column: `idkab`)
- Full roxygen2 documentation: `?soviclust`, `?run_app`, and dataset pages
- 16 unit tests with testthat (testing `run_app` and `sample_data`)

---

# soviclust 0.0.1 (2024-07-01) - Pre-release

- Initial standalone Shiny application (before conversion to R package)
- Basic SoVI pipeline implementation and Leaflet visualization

---

*To report bugs or request features, open an issue at:*
*<https://github.com/dedenistiawan/soviclust/issues>*
