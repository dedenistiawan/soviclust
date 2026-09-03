# soviclust <img src="man/figures/logo.png" align="right" height="139" alt="soviclust logo" />

> **An Interactive R Shiny Package for Social Vulnerability Index (SoVI) Analysis and Spatial Clustering**

[![R version](https://img.shields.io/badge/R-%3E%3D4.1.0-blue)](https://cran.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![GitHub release](https://img.shields.io/github/v/release/dedenistiawan/soviclust)](https://github.com/dedenistiawan/soviclust/releases)
[![R-CMD-check](https://github.com/dedenistiawan/soviclust/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dedenistiawan/soviclust/actions/workflows/R-CMD-check.yaml)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

---

## Overview

**soviclust** is an R package that provides an interactive Shiny application for computing, visualizing, and analyzing the **Social Vulnerability Index (SoVI)** at the administrative unit level. It implements the methodology of Cutter et al. (2003) with several enhancements, including proportional weighting based on PCA factor loadings, three variable direction methods, and four spatial clustering algorithms.

---

## Features

- 📊 **SoVI Computation** — Automated pipeline: Z-score standardization → PCA (Varimax) → weighted aggregation → Jenks classification
- 🗺️ **Interactive Maps** — Choropleth maps with Leaflet for SoVI scores and cluster results
- ⚙️ **Variable Configuration** — Flexible variable direction assignment (+/−) with three methods
- 🔬 **Method Comparison** — Side-by-side comparison of Theory-Based, Loading Sign, and Cutter's method
- 📈 **Extended Analysis** — Dominant component, Moran's I, LISA, and sensitivity analysis
- 🤖 **Spatial Clustering** — Four algorithms:
  - **ClustGeo** — Spatial hierarchical clustering
  - **FGWC** — Fuzzy Geographically Weighted Clustering
  - **LFGWC** — Local FGWC (Grekousis, 2020)
  - **ALFGWC** — Adaptive Local FGWC
- 💾 **Export** — Download results as CSV and high-resolution PNG maps

---

## Installation

### From GitHub

```r
# Install remotes if not already installed
install.packages("remotes")

# Install the latest version of soviclust
remotes::install_github("dedenistiawan/soviclust")
```

To install a specific version:

```r
remotes::install_github("dedenistiawan/soviclust@v0.6.0")
```

To skip reinstalling existing dependencies:

```r
remotes::install_github("dedenistiawan/soviclust", upgrade = "never")
```

### From Local Source (for developers)

```r
# Clone the repository first, then:
install.packages("devtools")
devtools::install("path/to/soviclust")
```

---

## Quick Start

```r
library(soviclust)

# Launch the interactive Shiny application
soviclust::run_app()
```

The application will open automatically in your default web browser.

```
┌─────────────────────────────────────────┐
│   SoVI Interactive Mapper (soviclust)   │
│   Social Vulnerability Index Analysis   │
└─────────────────────────────────────────┘

To start the app, please run:
  soviclust::run_app()

Version: 0.6.0
```

---

## Application Workflow

The application is organized into eight sequential steps:

| Step | Tab | Description |
|------|-----|-------------|
| 1 | **Import / Load Data** | Upload your own data (Excel/CSV + Shapefile) or use the bundled sample dataset |
| 2 | **Variable Config** | Assign direction (+/−) for each SoVI variable |
| 3 | **Method Comparison** | Compare three direction methods using Spearman ρ, Kendall τ, MARD, RMSD, and Cohen's κ |
| 4 | **SoVI Computation** | Run the full SoVI pipeline and view the choropleth map |
| 5 | **Extended Analysis** | Dominant component, radar profiles, Moran's I, LISA, sensitivity analysis |
| 6 | **Cluster Analysis** | Run spatial clustering (ClustGeo / FGWC / LFGWC / ALFGWC) |
| 7 | **SoVI Analysis** | Variable-level choropleth maps with Jenks classification |
| 8 | **Downloads** | Export results as CSV and PNG |

---

## Clustering Optimization Algorithms

FGWC, LFGWC, and ALFGWC support nine metaheuristic optimizers for centroid initialization:

| Code | Algorithm |
|------|-----------|
| `classic` | Classic FCM (no optimizer) |
| `pso` | Particle Swarm Optimization |
| `abc` | Artificial Bee Colony |
| `gwo` | Grey Wolf Optimizer |
| `woa` | Whale Optimization Algorithm |
| `hho` | Harris-Hawk Optimization |
| `fpa` | Flower Pollination Algorithm |
| `gsa` | Gravitational Search Algorithm |
| `tlbo` | Teaching-Learning Based Optimization |
| `ifa` | Intelligent Firefly Algorithm |

---

## System Requirements

| Requirement | Minimum |
|-------------|---------|
| R | >= 4.1.0 |
| RAM | 4 GB (8 GB recommended for large datasets) |
| IDE | RStudio (recommended) |
| OS | Windows 10/11, macOS, Linux |

---

## Citation

If you use **soviclust** in your research, please cite:

```bibtex
@software{istiawan2024soviclust,
  author  = {Istiawan, Deden},
  title   = {{soviclust}: An Interactive R Shiny Package for Social Vulnerability
             Index Analysis and Spatial Clustering},
  year    = {2024},
  version = {0.6.0},
  url     = {https://github.com/dedenistiawan/soviclust}
}
```

---

## References

- Cutter, S.L., Boruff, B.J., & Shirley, W.L. (2003). Social Vulnerability to Environmental Hazards. *Social Science Quarterly*, 84(2), 242–261. https://doi.org/10.1111/1540-6237.8402002
- Wijayanto, H., & Purwaningsih, T. (2020). Fuzzy Geographically Weighted Clustering using Particle Swarm Optimization. *Journal of Physics: Conference Series*.
- Grekousis, G. (2020). *Spatial Analysis Methods and Practice*. Cambridge University Press. https://doi.org/10.1017/9781108614528

---

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository
2. Create a new branch: `git checkout -b feature/your-feature`
3. Commit your changes: `git commit -m 'feat: add your feature'`
4. Push to the branch: `git push origin feature/your-feature`
5. Open a Pull Request

For bug reports and feature requests, please use [GitHub Issues](https://github.com/dedenistiawan/soviclust/issues).

---

## Author

Developed by **Deden Istiawan** — ITESA Muhammadiyah Research Team

- 📧 Email: [dedenistiawan@gmail.com](mailto:dedenistiawan@gmail.com)
- 🐙 GitHub: [@dedenistiawan](https://github.com/dedenistiawan)
- 🐛 Bug reports: [GitHub Issues](https://github.com/dedenistiawan/soviclust/issues)

---

## License

MIT License © 2024 Deden Istiawan. See [LICENSE](LICENSE) for details.
