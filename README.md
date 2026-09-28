# Baltimore property suitability explorer

An R Shiny app that fetches current Baltimore City neighborhood boundaries and Real Property records each time you run an analysis. Choose neighborhoods and adjustable screening conditions. Review a property map, neighborhood summary, score breakdown, and downloadable Excel workbook.

## Run locally

1. Install R and RStudio.
2. Open this folder as a project and run:

```r
install.packages(c("shiny", "httr", "jsonlite", "sf", "dplyr", "tidyr", "leaflet", "DT", "openxlsx", "htmltools"))
shiny::runApp(".")
```

`sf` may need system libraries on Linux. The app needs internet access to the City's ArcGIS REST endpoints. A selection above 30,000 records stops with a message; split it into smaller runs.

## How scoring works

One point is added for each enabled condition: construction year at or before the chosen cutoff, recorded sale at least the chosen number of years ago, and an affirmative permanent home indicator. Missing values contribute zero. The default is a general screening example, not a formal eligibility determination. The recorded sale date is a proxy for tenure; permanent home is a source indicator and does not prove occupancy. Verify parcels and program requirements before decisions.

The Excel workbook contains neighborhood totals and rates, a score pivot, individual property fields and the settings and live source URLs used for the run. The map uses parcel interior points for responsive display and outlines the selected neighborhood boundaries.

## Data and refresh

The app reads the City [Real Property feature layer](https://geodata.baltimorecity.gov/egis/rest/services/CityView/Realproperty_OB/FeatureServer/0) and [neighborhood feature layer](https://geodata.baltimorecity.gov/egis/rest/services/CityView/Neighborhoods/FeatureServer/0). Data is requested when the user opens the app for neighborhood names and when they press **Run analysis** for features. Reopen or rerun to see updates published by the City. The City controls its update schedule; this app does not change or schedule source updates.

## GitHub

In a terminal opened in this folder:

```bash
git init
git add app.R R/data.R README.md .gitignore
git commit -m "Build live Baltimore property suitability explorer"
git branch -M main
git remote add origin https://github.com/YOUR-USERNAME/baltimore-suitability.git
git push -u origin main
```

Create an empty GitHub repository with that name first and replace `YOUR-USERNAME`. A public GitHub repository hosts the source code; deploy to shinyapps.io or a Shiny server to give users a working app URL.
