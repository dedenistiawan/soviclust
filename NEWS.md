# NEWS.md - soviclust Changelog

This changelog follows the [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
format and [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

# soviclust [Unreleased]

### Changed

- Relicensed the project from the MIT License to the **GNU General Public License v3 (GPL-3)**.
- Expanded and reorganized `README.md` to document:
  - the integrated SoVI and clustering workflow;
  - general and spatial clustering methods;
  - FGWC software provenance;
  - GWO and WOA optimization extensions;
  - LFGWC and ALFGWC methodology;
  - reproducibility and stability analysis;
  - input-data requirements;
  - related software and current limitations.
- Updated mathematical notation in `README.md` to use GitHub-compatible fenced `math` blocks for display equations.
- Updated the software citation year to **2026**.

### Documentation

- Added explicit attribution for FGWC-related source code adapted from the GPL-3-licensed `naspaclust` package.
- Added dataset provenance and citation for:
  - Kurniawan, R., Nasution, B. I., Agustina, N., & Yuniarto, B. (2022).
    _Revisiting social vulnerability analysis in Indonesia data_. Data in Brief, 40, 107743.
- Expanded `Panduan_ALFGWC.md` into a methodological and user guide covering:
  - the relationship between FCM, FGWC, LFGWC, and ALFGWC;
  - adaptive neighborhood influence coefficient (`alpha_i`);
  - Distance Threshold (`dthr`), Queen, Rook, and Bishop neighborhoods;
  - KNN fallback for spatial units without neighbors;
  - Distance Decay and Spatial Interaction weighting;
  - Local Moran's I-based adaptive rules;
  - fuzzy validity indices and silhouette diagnostics;
  - optimizer configuration;
  - multi-run stability analysis;
  - reproducibility and reporting recommendations.
- Clarified that, in the current ALFGWC implementation, larger `alpha_i` means stronger neighborhood influence.

### Notes

- `v0.3.0` and `v0.4.0`, which exist as Git tags, have now been restored to this changelog.
- No `v0.2.0` Git tag currently exists; therefore, no artificial `0.2.0` release entry is included.

---

# soviclust 0.7.0 - 2026-09-04

### Added

- **Stability Analysis Module** for FGWC, LFGWC, and ALFGWC:
  - repeated independent runs for stochastic clustering algorithms;
  - configurable number of runs and starting random seed;
  - callback-based progress reporting in Shiny;
  - summary statistics including `Mean`, `SD`, `Best`, `Worst`, and `Median`;
  - prominent `Mean ± SD` reporting for validation metrics;
  - validation-metric distribution boxplots using Plotly;
  - detailed run-by-run result tables;
  - per-run execution time and iteration summaries.
- Package startup banner in `zzz.R`.
- Core helper functions for loading package data and spatial resources.

### Changed

- Stability-analysis workflow standardized across FGWC, LFGWC, and ALFGWC.
- Improved handling of repeated stochastic runs and result summaries.

### Fixed

- Replaced invalid `status = "default"` with `status = "info"` in
  `shinydashboard::box()` components used by stability tabs.
- Corrected run-count handling so that user-selected stability parameters are not
  overwritten unexpectedly.

---

# soviclust 0.6.0 - 2026-09-03

### Added

- `run_app()` now opens the Shiny application in the system's external web browser
  by default instead of relying on the RStudio Viewer.

### Changed

- Completed translation of user-facing application text from Indonesian to English
  across major modules, including:
  - ALFGWC;
  - FGWC;
  - LFGWC;
  - K-Means;
  - DBSCAN;
  - ClustGeo;
  - Method Comparison;
  - Extended Analysis;
  - SoVI Core;
  - Variable Configuration;
  - SoVI Analysis;
  - Downloads.
- Translated:
  - labels and notifications;
  - error messages;
  - plot titles, subtitles, and axes;
  - validation tables;
  - silhouette interpretation;
  - radar-chart profiles;
  - Sammon, t-SNE, and UMAP outputs;
  - convergence plots;
  - recommendation and status boxes.
- Replaced the `CartoDB.Positron` basemap with `Esri.WorldGrayCanvas` to avoid
  CARTO watermark/API-key-related display issues.

### Fixed

- Corrected a syntax error in `sovi_core.R` caused by escaped quotes in
  `case_when()` labels.
- Fixed remaining translation-related variable-name mismatches, including
  `Fitur` → `Feature` in heatmap data processing.

### Documentation

- Rewrote `README.md` in English following standard R-package conventions.
- Added sections for:
  - Overview;
  - Features;
  - Quick Start;
  - Application Workflow;
  - SoVI methodology;
  - bundled sample data;
  - clustering algorithms;
  - system requirements;
  - citation;
  - references;
  - contributing;
  - license.

---

# soviclust 0.5.0 - 2026-08-27

### Changed

- Application changed to an **English-only** interface.
- Removed the language switcher and other runtime language-selection controls.
- Set English as the default language in the i18n infrastructure.
- Unified K-Means and DBSCAN data-source selection with the same five-option
  pattern used by FGWC:
  - raw data;
  - normalized raw data;
  - standardized data;
  - SoVI score;
  - rotated-component scores.

### Fixed

- Fixed a trailing comma in `dashboardHeader()` in `inst/app/ui.R` that caused
  `run_app()` to fail with:
  `"argument is missing, with no default"`.
- Reduced the bundled Indonesian administrative map from a large shapefile
  representation to a compact RDS representation to reduce installation/download
  size and avoid `install_github()` timeout problems.
- Removed duplicated i18n keys that caused a `row.names` error during application
  startup.
- Corrected `update_lang()` argument order for compatibility with
  `shiny.i18n` 0.3.0.

### Removed

- Removed four unused `inst/extdata` files to reduce package size:
  - `sovi_data_kab_514.RData`;
  - `Sovi_pop_514.RData`;
  - `sovi_dist_514.RData`;
  - `sovi_data_kab_514_19.xlsx`.

---

# soviclust 0.4.0 - 2026-08-22

### Added

- **K-Means clustering module** integrated into the Shiny Cluster Analysis workflow.
- **DBSCAN clustering module** integrated into the Shiny Cluster Analysis workflow.
- Added `dbscan` as a package dependency.
- DBSCAN functionality includes:
  - configurable `eps`;
  - configurable `minPts`;
  - k-nearest-neighbor distance plot to assist `eps` selection;
  - cluster and noise summaries;
  - detailed cluster-assignment output.
- Added dedicated UI and server components for:
  - `inst/app/R/kmeans/`;
  - `inst/app/R/dbscan/`.

### Changed

- Updated the main Shiny navigation and server routing to include K-Means and DBSCAN.
- Extended the clustering workflow beyond the spatial fuzzy-clustering family with
  conventional partitional and density-based clustering methods.

---

# soviclust 0.3.0 - 2026-08-22

### Added

#### Automated Reporting

- Added automated SoVI report generation through R Markdown.
- Reports can be exported as:
  - HTML;
  - PDF.
- Added `inst/app/report/sovi_report.Rmd`.
- Report workflow can include:
  - dataset summary;
  - SoVI/PCA information;
  - spatial-analysis summaries;
  - cluster information when available.
- Added report-title and institution metadata controls.

#### Internationalization Infrastructure

- Added `shiny.i18n` infrastructure.
- Added `inst/app/i18n/translation.json` as the translation resource.
- Prepared user-interface components for multilingual support.

#### Data Validation and Workflow Robustness

- Added reusable upload/data-validation helpers for:
  - supported file types;
  - minimum data size;
  - numeric-variable availability;
  - empty rows;
  - duplicate spatial IDs;
  - dataset/shapefile ID compatibility;
  - SoVI-variable validation.
- Added more granular progress callbacks to the SoVI computation pipeline.

#### Package Documentation and Website

- Added four package vignettes:
  - `getting-started.Rmd`;
  - `data-preparation.Rmd`;
  - `sovi-methodology.Rmd`;
  - `clustering-methods.Rmd`.
- Added `pkgdown` configuration.
- Added GitHub Actions workflow to build/deploy the package website.
- Added package logo and documentation assets.

#### Sample-Data Helpers

- Added R helper functions for accessing bundled example resources directly
  without opening the Shiny application, including:
  - SoVI sample data;
  - Indonesian administrative geometry;
  - centroid coordinates;
  - population data;
  - distance information.

#### Testing and Development Infrastructure

- Expanded the package test suite; the development milestone recorded
  **31 passing tests**.
- Added documentation/build-related package dependencies such as `knitr`,
  `rmarkdown`, and `kableExtra`.
- Added `digest` and supporting infrastructure for performance-oriented development.

### Changed

- Improved SoVI progress reporting to expose more detailed analysis stages.
- Updated package website metadata and development/build configuration.

---

# soviclust 0.1.0 - Historical package milestone (untagged)

> This version is retained because it was recorded in the original `NEWS.md`.
> A corresponding `v0.1.0` Git tag is not currently present in the public tag list.

### Added

#### Shiny Application

- Full interactive application launched with:

```r
soviclust::run_app()
```

- **Import / Load Data**
  - Excel/CSV attribute data;
  - spatial files;
  - bundled Indonesian sample data.
- **Variable Configuration**
  - indicator selection;
  - positive/negative vulnerability direction.
- **Method Comparison**
  - Theory-Based direction;
  - Loading Sign;
  - Cutter-style direction handling.
- **SoVI Computation**
  - Z-score standardization;
  - PCA;
  - weighted aggregation;
  - Jenks Natural Breaks classification.
- **Extended Analysis**
  - Global Moran's I;
  - LISA;
  - dominant-component analysis;
  - component profiles;
  - sensitivity analysis.
- **Cluster Analysis**
  - ClustGeo;
  - FGWC;
  - LFGWC;
  - ALFGWC.
- FGWC optimizer support including:
  - PSO;
  - ABC;
  - GWO;
  - WOA;
  - HHO;
  - FPA;
  - GSA;
  - TLBO;
  - IFA.
- Sample-data loading workflow for FGWC/LFGWC/ALFGWC.
- **SoVI Analysis**
  - variable-level choropleth maps;
  - GVF-based summaries.
- **Downloads**
  - CSV outputs;
  - high-resolution map export.

#### Package Infrastructure

- Added exported `run_app()` launcher.
- Added package dependency checking and application-path resolution.
- Added startup messaging through `.onAttach()`.
- Added bundled Indonesian SoVI supporting data.
- Added roxygen2 documentation for package functions and datasets.
- Added initial `testthat` coverage for package startup and sample data.

---

# soviclust 0.0.1 - 2024-07-01 - Pre-release

### Added

- Initial standalone Shiny application before conversion into an R package.
- Basic Social Vulnerability Index workflow.
- Initial Leaflet-based spatial visualization.

---

## Version history notes

The public GitHub tag sequence currently includes:

```text
v0.3.0  2026-08-22
v0.4.0  2026-08-22
v0.5.0  2026-08-27
v0.6.0  2026-09-03
v0.7.0  2026-09-04
```

`0.1.0` and `0.0.1` are retained as historical development milestones from the
original changelog. No public `v0.2.0` tag is currently present.

---

_To report bugs or request features, open an issue at:_  
<https://github.com/dedenistiawan/soviclust/issues>
