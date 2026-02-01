# LifeTableBuilder

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18446884.svg)](https://doi.org/10.5281/zenodo.18446884)

An R/Shiny application for constructing life tables in ecological and entomological studies, providing reproducible workflows for data validation, demographic calculations, visualization, and export of results.

## Features
- Interactive life-table construction via a Shiny web interface.
- Data validation and basic quality checks.
- Demographic calculations and summary outputs for reporting.
- Visualization and export of results for reproducible workflows.

## Installation
### Option 1: Run from GitHub (recommended)
1. Install R (>= 4.1) and RStudio (optional).
2. Install required packages in R:
3. source("app_requirements.R")
shiny::runApp("src/app.R")

## Example dataset

An example dataset is provided in `data/example_lifetable.csv` to demonstrate
the expected input structure for life-table construction.


## R

shiny::runApp("src/app.R")

## Run the application

```r
source("app_requirements.R")
shiny::runApp("src")


## Usage
You can test the app with data/example_lifetable.csv
