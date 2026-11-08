# Tumor Monitor

An R-based project for managing preclinical tumor-study data and generating reproducible treatment-group visualizations.

This project supports animal records, treatment assignment, longitudinal tumor measurements, euthanasia/killed-animal tracking, and analysis-ready plots for tumor-growth and survival outcomes.

---

## Features

- Animal and cage record management
- Treatment-group and treatment-assignment management
- Longitudinal tumor-volume measurement tracking
- Tumor-volume cap handling
- Exclusion of selected animal IDs from analysis and figures
- Individual tumor-growth curves by treatment
- Group-level tumor-growth summaries with mean/median and error bars
- Baseline-normalized tumor-growth plots
- Endpoint tumor-volume plots
- Last-observation-carried-forward (LOCF) sensitivity plots
- Injection-day annotation support
- Kaplan–Meier time-to-euthanasia/killing curves
- Right-censored survival observations for animals alive at study end
- Endpoint treatment-group labels on survival plots

---

## Requirements

### R packages

```r
install.packages(c(
  "DBI",
  "RSQLite",
  "ggplot2"
))