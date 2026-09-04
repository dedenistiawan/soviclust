# =============================================================================
# R/zzz.R
# Startup message saat library(soviclust) dipanggil
# =============================================================================

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "  \u250c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2510\n",
    "  \u2502   SoVI Interactive Mapper (soviclust)  \u2502\n",
    "  \u2502   Social Vulnerability Index Analysis  \u2502\n",
    "  \u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518\n",
    "\n",
    "  To start the app, please run:\n",
    "    soviclust::run_app()\n",
    "\n",
    "  Version: ", utils::packageVersion("soviclust"), "\n"
  )
}
