# Dose-Response IC50 Lab

This Shiny app loads dose-response data, fits several common sigmoid equations, plots the curves, and reports IC50 values in an open workflow.

## Features

- Import `csv`, `tsv`, `txt`, `xls`, and `xlsx`
- Map your own concentration/dose, response, and grouping columns
- Fit `4PL`, `5PL`, `3PL (Hill fixed = 1)`, `3PL (Bottom = 0)`, and `3PL (Top = 100)`
- Choose decreasing or increasing curves
- Fit either group means or all observations
- Optional normalization and SD-based weighting
- Export the plot as PNG and the fit results as CSV

## Expected data

Your file should include:

- one numeric concentration or dose column
- one numeric response column
- one optional grouping column such as `compound`

Concentration values must be greater than zero for standard log-scale curve fitting.

The app does not assume a fixed concentration unit. You can use nM, uM, mg/mL, or any other unit as long as the column is numeric, then set the exact unit in the x-axis title.

An example file is included at [example_dose_response.csv](C:\Users\HP\OneDrive\Escritorio\Ivan\backup-nuevo\Desktop\PEPTIDOS\Programas\app_ic50\example_dose_response.csv).

## Install packages

Run this once in R:

```r
install.packages(c("shiny", "bslib", "ggplot2", "DT", "readxl"))
```

## Run the app

From this folder in R or RStudio:

```r
shiny::runApp()
```
