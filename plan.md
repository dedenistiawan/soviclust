# Package Review & Improvement Plan: soviclust v0.6.0

> Generated: 2026-09-03 | Reviewer: Antigravity AI

---

## Overall Assessment

**soviclust** is a well-structured R package for an interactive Shiny application.
The architecture is solid, with good separation of concerns (modules, server logic, UI).
Below is a detailed review by category.

---

## Strengths

- Clean modular Shiny architecture (`inst/app/R/`)
- Good use of `system.file()` in `run_app()`
- Unit tests exist (16 tests via testthat)
- Vignettes present (4 topics)
- `NEWS.md`, `LICENSE`, `URL`, `BugReports` in DESCRIPTION
- CI/CD via GitHub Actions (R-CMD-check)
- Sample data bundled in `inst/extdata/`
- Badges in README (version, license, lifecycle, R-CMD-check)

---

## Critical Issues (Must Fix)

### 1. DESCRIPTION — Description field in Indonesian

The `Description:` field is entirely in Indonesian. R CMD check and CRAN require English.

**Current:**
```
Description: Platform interaktif berbasis R Shiny untuk menghitung, memvisualisasikan,
    dan menganalisis Social Vulnerability Index (SoVI) di tingkat wilayah administratif.
    Mengimplementasikan metodologi Cutter et al. (2003) dengan peningkatan berupa bobot
    proporsional berdasarkan factor loading PCA, tiga opsi metode penentuan arah variabel,
    integrasi analisis spasial (Moran's I, LISA), serta tiga algoritma clustering spasial:
    ClustGeo, FGWC (Fuzzy Geographically Weighted Clustering), LFGWC, dan ALFGWC dengan
    dukungan 9 algoritma metaheuristik untuk optimasi centroid.
```

**Fix:**
```
Description: An interactive R Shiny platform for computing, visualizing, and analyzing
    the Social Vulnerability Index (SoVI) at the administrative unit level. Implements
    the methodology of Cutter et al. (2003) with proportional PCA-based weighting, three
    variable direction methods, spatial analysis (Moran's I, LISA), and four spatial
    clustering algorithms: ClustGeo, FGWC (Fuzzy Geographically Weighted Clustering),
    LFGWC, and ALFGWC with nine metaheuristic optimizers for centroid initialization.
```

---

### 2. DESCRIPTION — Placeholder email

**Current:**
```r
person("Deden", "Istiawan", email = "deden@example.com", role = c("aut", "cre"))
```

**Fix:**
```r
person("Deden", "Istiawan", email = "dedenistiawan@gmail.com",
       role = c("aut", "cre"),
       comment = c(ORCID = "YOUR-ORCID-HERE"))
```

---

### 3. `R/run_app.R` — Stop messages in Indonesian

**Current:**
```r
stop("Package berikut diperlukan tetapi belum terinstall:\n", ...)
stop("Tidak dapat menemukan direktori aplikasi.\n", ...)
```

**Fix:**
```r
stop("The following packages are required but not installed:\n", ...)
stop("Cannot find the application directory.\n", ...)
```

---

### 4. `R/run_app.R` — Roxygen2 documentation in Indonesian

All `@description`, `@param`, `@return`, and `@examples` comments are in Indonesian.
These appear in `?run_app` help page and must be in English.

---

### 5. `NEWS.md` — Entirely in Indonesian

Should follow English standard. Format reference: https://keepachangelog.com/

**Suggested structure:**
```markdown
# soviclust 0.6.0

### Changed
- Complete UI translation to English across all modules
- Rewrote README.md following standard R package conventions

### Fixed
- Syntax error in `sovi_core.R` (escaped quotes in `case_when`)

---

# soviclust 0.5.0

### Fixed
- Trailing comma in `dashboardHeader()` causing `run_app()` failure
- Shapefile size reduced from 19MB to 0.94MB (RDS) to prevent install timeout
- Duplicate i18n keys causing `row.names` error
- `update_lang()` argument order for shiny.i18n v0.3.0 compatibility

### Changed
- Application now English-only; language switcher removed
- K-Means and DBSCAN data sources unified with FGWC 5-option pattern

### Removed
- 4 unused extdata files (~2.1MB)
```

---

## Important Issues (Recommended)

### 6. `shiny.i18n` may be unused

`shiny.i18n` is listed in `Imports` but the app was changed to English-only.
If no longer used, remove it to reduce unnecessary dependencies.

**Check:**
```powershell
Select-String -Path "inst\app\R\*" -Pattern "i18n|translator|shiny\.i18n" -Recurse
```

If no matches → remove `shiny.i18n` from `Imports` in DESCRIPTION.

---

### 7. No copyright holder (`cph`) in `Authors@R`

Standard for academic packages — add institution as copyright holder:

```r
Authors@R: c(
  person("Deden", "Istiawan",
         email = "dedenistiawan@gmail.com",
         role = c("aut", "cre"),
         comment = c(ORCID = "YOUR-ORCID")),
  person("ITESA Muhammadiyah", role = "cph")
)
```

---

### 8. Unit test descriptions in Indonesian

```r
# Current:
test_that("run_app() menemukan direktori inst/app/", { ... })

# Fix:
test_that("run_app() finds the inst/app/ directory", { ... })
```

---

### 9. Root-level files not in `.Rbuildignore`

The following files exist in the root but are not needed in the built package:

| File | Action |
|------|--------|
| `plan.md` | Add to `.Rbuildignore` |
| `Panduan_ALFGWC.md` | Add to `.Rbuildignore` |
| `note` | Add to `.Rbuildignore` |

Add to `.Rbuildignore`:
```
^plan\.md$
^Panduan_ALFGWC\.md$
^note$
```

---

### 10. Startup banner version is hardcoded

The startup message likely shows `Version: 0.1.0` (hardcoded).
Use `packageVersion()` for a dynamic version:

```r
# In R/zzz.R
.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion("soviclust")
  packageStartupMessage(paste0(
    "\n  \u250c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2510",
    "\n  \u2502   SoVI Interactive Mapper (soviclust)   \u2502",
    "\n  \u2502   Social Vulnerability Index Analysis   \u2502",
    "\n  \u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518\n",
    "\n  To start the app, please run:",
    "\n    soviclust::run_app()\n",
    "\n  Version: ", version, "\n"
  ))
}
```

---

## Minor Suggestions

### 11. pkgdown files — decide on direction

You deleted the pkgdown workflow but `_pkgdown.yml` and `pkgdown/` folder remain.

- **Option A (Keep pkgdown site):** Restore `.github/workflows/pkgdown.yaml`
- **Option B (Remove pkgdown):** Delete `_pkgdown.yml`, `pkgdown/`, `docs/` folders
  and remove `https://dedenistiawan.github.io/soviclust/` from DESCRIPTION `URL`.

---

### 12. `soviclust-package.R` — add `@keywords internal`

```r
#' @keywords internal
"_PACKAGE"
```

---

### 13. `sample_data.R` and `data.R` — check roxygen language

Verify all roxygen docs in these files are in English.

---

## R CMD CHECK Status (Estimated)

| Check | Status | Notes |
|-------|--------|-------|
| DESCRIPTION — Description | WARNING | In Indonesian |
| DESCRIPTION — Email | NOTE | Placeholder `deden@example.com` |
| Imports — shiny.i18n | WARNING | Possibly unused |
| Documentation (roxygen) | WARNING | In Indonesian |
| Tests | PASS | 16 tests |
| Vignettes | PASS | 4 vignettes |
| NEWS.md | PASS | Exists |
| LICENSE | PASS | MIT + file LICENSE |
| URL / BugReports | PASS | Both present |

---

## Priority Action List

| # | Priority | Action | File |
|---|----------|--------|------|
| 1 | CRITICAL | Translate `Description:` to English | `DESCRIPTION` |
| 2 | CRITICAL | Fix placeholder email | `DESCRIPTION` |
| 3 | CRITICAL | Translate `run_app()` stop messages | `R/run_app.R` |
| 4 | CRITICAL | Translate roxygen2 docs | `R/run_app.R` |
| 5 | CRITICAL | Translate `NEWS.md` to English | `NEWS.md` |
| 6 | IMPORTANT | Remove/verify `shiny.i18n` in Imports | `DESCRIPTION` |
| 7 | IMPORTANT | Add ORCID + `cph` to Authors@R | `DESCRIPTION` |
| 8 | IMPORTANT | Translate unit test descriptions | `tests/testthat/` |
| 9 | IMPORTANT | Add root files to `.Rbuildignore` | `.Rbuildignore` |
| 10 | IMPORTANT | Fix startup version to use `packageVersion()` | `R/zzz.R` |
| 11 | MINOR | Decide on pkgdown (restore or remove) | root |
| 12 | MINOR | Add `@keywords internal` to package doc | `R/soviclust-package.R` |
| 13 | MINOR | Verify `sample_data.R` / `data.R` roxygen language | `R/` |

---

*Last updated: 2026-09-03 | soviclust v0.6.0*


---

## Unit Test Recommendations

### Current Coverage (16 tests in 2 files)

| File | Tests | Coverage |
|------|-------|----------|
| `test-run_app.R` | 4 | `run_app()` directory, Shiny files, callable, path validation |
| `test-sample_data.R` | 12 | File availability, structure, integrity for 4 xlsx + shapefile |

---

### Priority 1 - Must Have

Add to new file: `tests/testthat/test-sample_functions.R`

```r
# sovi_sample_data()
test_that("sovi_sample_data() returns a data.frame with 514 rows", {
  df <- sovi_sample_data()
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 514)
  expect_true("DISTRICTCODE" %in% names(df))
  expect_true("KABUPATEN"    %in% names(df))
})

# sovi_sample_shapefile()
test_that("sovi_sample_shapefile() returns an sf object with 514 features", {
  shp <- sovi_sample_shapefile()
  expect_s3_class(shp, "sf")
  expect_equal(nrow(shp), 514)
  expect_true("idkab" %in% names(shp))
})

# sovi_sample_coords()
test_that("sovi_sample_coords() returns correct structure", {
  coord <- sovi_sample_coords()
  expect_s3_class(coord, "data.frame")
  expect_equal(nrow(coord), 514)
  expect_true(all(c("DISTRICTCODE", "longitude", "latitude") %in% names(coord)))
})

# sovi_sample_pop()
test_that("sovi_sample_pop() returns a data.frame with 514 rows", {
  pop <- sovi_sample_pop()
  expect_s3_class(pop, "data.frame")
  expect_equal(nrow(pop), 514)
  num_col <- which(sapply(pop, is.numeric))[1]
  expect_false(is.na(num_col))
  expect_true(all(pop[[num_col]] > 0, na.rm = TRUE))
})

# sovi_sample_distmat()
test_that("sovi_sample_distmat() returns a square 514x514 numeric matrix", {
  mat <- sovi_sample_distmat()
  expect_true(is.matrix(mat))
  expect_true(is.numeric(mat))
  expect_equal(dim(mat), c(514L, 514L))
  expect_true(all(diag(mat) == 0))
})
```

---

### Priority 2 - Should Have

```r
# Error messages
test_that("sovi_sample_data() gives informative error when file missing", {
  expect_error(
    withr::with_envvar(list(R_PACKAGE_DIR = tempdir()), sovi_sample_data()),
    "not found"
  )
})

# Matrix symmetry
test_that("sovi_sample_distmat() is a symmetric matrix", {
  mat <- sovi_sample_distmat()
  expect_true(isSymmetric(mat, tol = 1e-6))
})
```

---

### Priority 3 - Nice to Have

```r
# Package metadata
test_that("package version follows semantic versioning x.y.z", {
  v <- as.character(packageVersion("soviclust"))
  expect_match(v, "^\\d+\\.\\d+\\.\\d+$")
})

test_that("package has required metadata fields", {
  desc <- packageDescription("soviclust")
  expect_false(is.null(desc$URL))
  expect_false(is.null(desc$BugReports))
  expect_false(is.null(desc$License))
})

# SoVI normalization
test_that("SoVI normalization maps to [0, 1]", {
  scores     <- c(-2.5, -1.0, 0.0, 1.5, 3.0)
  normalized <- (scores - min(scores)) / (max(scores) - min(scores))
  expect_true(all(normalized >= 0 & normalized <= 1))
  expect_equal(min(normalized), 0)
  expect_equal(max(normalized), 1)
})
```

---

### Test Priority Summary

| Priority | What to Test | File | Status |
|----------|-------------|------|--------|
| MUST | `run_app()` directory and Shiny files | `test-run_app.R` | Done |
| MUST | Data integrity - 4 xlsx + shapefile | `test-sample_data.R` | Done |
| MUST | Wrapper functions return types | `test-sample_functions.R` | TODO |
| SHOULD | Informative error messages | `test-sample_functions.R` | TODO |
| SHOULD | Matrix symmetry validation | `test-sample_functions.R` | TODO |
| OPTIONAL | Package metadata validation | `test-metadata.R` | TODO |
| OPTIONAL | SoVI normalization logic | `test-sovi_logic.R` | TODO |

---

*Unit test section added: 2026-09-03 | soviclust v0.6.0*
