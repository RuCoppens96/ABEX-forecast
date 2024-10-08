# ABEX-forecast

**! Work in progress !**

Project to learn Git, Github Actions, Github Pages and Docker based on the git repository [Deploy and Monitor ML Pipelines with Open Source and Free Applications](https://github.com/RamiKrispin/useR2024-pipeline-workshop).

## Scope
**Goal**: Forecast the next ABEX-indices based on previous ABEX and general inflation. The ABEX-index reflects the evolution of housing construction costs in the Belgian market. This index is used by insurance companies to automatically index prices.

**Forecast Horizon**: One year in the future 

**Refresh**: Every month once general inflation is known.

## Data
The consumption price index is available via [STATBEL API](https://bestat.statbel.fgov.be/bestat/api/views/876acb9d-4eae-408e-93d9-88eae4ad1eaf/result/JSON) while the ABEX needs to be [scraped from the website](https://www.abex.be/nl/indexen-abex/).