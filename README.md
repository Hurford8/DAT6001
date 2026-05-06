# Higher Education Data Request Builder

## Overview

The **Higher Education Data Request Builder** is an R Shiny application developed as part of the DAT6001 Work-Based Project. The application is designed to support recurring higher education data request workflows by providing a guided interface for dataset selection, filtering, variable selection, pivot-table generation and export.

The app was developed to reduce manual SQL/R editing, improve consistency of outputs, and make standard data request workflows more accessible to users who may not be confident editing scripts directly.

The application supports both:

- **Online mode**: connects to a SQL Server environment.
- **Offline mode**: uses synthetic test data so the workflow can be tested without access to live organisational data.

## Repository Contents

This repository contains the following main files:

| File | Description |
|---|---|
| `HE_Data_Request_Builder.R` | Main R Shiny application script. |
| `run_app.R` | Helper script used to launch the application. |
| `Package installing.R` | Package installation script for installing required R packages. |
| `variable_formats.R` | Optional formatting file used to apply variable labels, recodes and transformations. |

> Note: File names should match the final submitted version. If the main application script has a different name, update both this README and `run_app.R`.

## Minimum Requirements

The application requires:

- R version **4.5.2 or later**
- RStudio is recommended
- Internet access for package installation
- SQL Server access only if using online mode
- ODBC SQL Server driver if using online mode

The application was designed to run on standard workplace devices and does not require GPU acceleration or high-performance computing resources.

## Required R Packages

The main packages used by the application include:

- `shiny`
- `bslib`
- `DBI`
- `odbc`
- `pool`
- `dplyr`
- `purrr`
- `stringr`
- `tidyr`
- `rpivotTable`
- `jsonlite`
- `openxlsx`
- `DT`
- `htmltools`

These can be installed using the included `Package installing.R` script.

## Installation

1. Download or clone this repository.

2. Open the project folder in RStudio.

3. Run the package installation script:

```r
source("Package installing.R")
