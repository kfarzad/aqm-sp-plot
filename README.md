# Air Quality Model Visualization & NetCDF Tools

## Overview

This repository contains a set of R scripts and functions developed during my PhD for working with air quality model output, particularly from **CMAQ** and **CAMx** models. The tools are designed to streamline common workflows for modelers, such as opening NetCDF files and producing high-quality spatial plots.

The primary feature of this codebase is the **spatial_plot** function in script_pkg_sp_plots.R, which can create clear and visually appealing maps tailored for air quality analysis.

## Features

- **NetCDF Opener** — Designed with CMAQ and CAMx output in mind; handles common variable structures used in air quality modeling.
- **Fancy Spatial Plots** — Produces publication-quality maps with customizable features for air quality model evaluation and visualization.
- Utility functions for quick exploration of modeling data.

## Intended Audience

- Air quality modelers
- Environmental scientists
- Researchers working with CMAQ or CAMx outputs

## Maintenance

This code was written for my research and is provided **as is**.
It may not receive significant updates in the future (and possibly never), but I hope it’s useful for others in the field.

## Requirements

- **R** (≥3.6 recommended)
- Required R packages:
  - `sf`
  - `maps`
  - `stringr`
  - `sp`
  - `raster`
  - `terra`
  - `rosm` *(for OpenStreetMap basemaps)*
  - `ncdf4`

## Usage

1. Clone or download this repository.
2. Install required packages in R:

```r
install.packages(c("sf", "maps", "stringr", "sp", "raster", "terra", "rosm", "ncdf4"))
```
