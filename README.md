# petadexR

PETadexR provides data exploration and analysis tools for plastic‑degrading enzyme sequences.
Leveraging petabase‑scale computational biology and high‑throughput functional genomics,
PETadexR helps bioinformaticians and biologists identify, visualize, and prioritize novel
PETase homologs for experimental validation.

## Features

- **Data Import**: Load curated PAZy database with millions of homologs.
- **Visualization**: Standardized ggplot2‑based plots for sequence diversity, activity scans, and
  phylogenetics.
- **Prediction**: Integrate mutation and metadata to predict PETase activity.
- **Interactive**: Generate interactive HTML reports for exploratory analysis.

## Installation

```r
# from CRAN (when released)
install.packages("petadexR")

# or from GitHub
remotes::install_github("ababaian/petadexR")
