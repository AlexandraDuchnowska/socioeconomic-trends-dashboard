# 📊 Socioeconomic & Environmental Trends Dashboard (2000–2023)

An interactive Shiny app for exploring socioeconomic and environmental trends across 9 countries over 24 years.

## Problem

How did key development indicators (health, education, economy, environment) evolve between 2000 and 2023 across countries at different development levels — and what relationships link these indicators?

## Data

- Source: **World Bank Open Data**
- Balanced panel: 9 countries (Switzerland, Australia, Sweden, France, Poland, China, Brazil, Kenya, Chad) × 24 years = 216 observations
- Indicators: life expectancy, GDP per capita, health expenditure, fertility rate, infant mortality, school enrollment (secondary & tertiary), CO₂ emissions, access to basic sanitation

## The app

An interactive Shiny (R) dashboard with 10 plotly-driven visualizations: time-series plots, animated bubble charts (GDP vs. life expectancy), scatter plots with country and year-range selection, and a correlation heatmap.

## Key findings

- A strong, consistent rise in life expectancy across all countries — most notable in Chad and Kenya
- Health expenditure correlates strongly with life expectancy, though the effect diminishes at very high spending levels (Switzerland)
- **No strong correlation between GDP and CO₂ emissions** — emissions depend more on energy mix and environmental policy than on wealth alone (Poland and China show high emissions despite moderate GDP)
- Sanitation is one of the strongest predictors of life expectancy in the entire dataset

## Repository structure

```
├── app/
│   └── Final_app_Socioeconomic_and_Environmental_Trends.R
├── data/
│   └── Data_Project_visualisation_and_querying.xlsx
├── report/
│   └── Final_Report.pdf
└── README.md
```

## Running the project

```r
install.packages(c("shiny", "plotly", "dplyr", "readxl", "scales", "ggplot2", "tidyr", "reshape2"))
shiny::runApp("app/")
```
