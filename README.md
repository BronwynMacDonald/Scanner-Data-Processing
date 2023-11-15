# Data Processing for the State of the Salmon Scanner Tool

**This is an internal working version of the data processing code**


## Overview

### Current Scope

* Fraser Sockeye: TEXT
* Southern BC Chinook: TEXT
* Interior Fraser Coho: TEXT
* Fraser Chum: TEXT
* Fraser Coho: TEXT


### Overview of data processing steps

1) data prep -> clean files

2) merge files into single data set

3) calculate WSP metrics using [WSPMetrics package](https://github.com/Pacific-salmon-assess/WSP-Metrics-Pkg) and [MetricsCOSEWIC package](https://github.com/SOLV-Code/MetricsCOSEWIC)




### Repository Structure

short overview with links



## Data Sources

### Skeena and Nass Sockeye

Source files  used here are the outputs from data processing in a separate github repository,
as described in the [Skeena and Nass Data Source README](https://github.com/SOLV-Code/Scanner-Data-Processing/tree/main/DATA_IN/SOURCES/Skeena_Nass%20Sockeye).

The data needed to feed into that processing repo comes from 2 sources: 

* run reconstructions developed periodically for DFO by an external contractor. The two most recent updates were done up to 2019 and up to 2022, using funding from an escapememt goal review required under the PST. Follow-up with NC StAD is needed to determine how annual updates would be coordinated and funded.
* stock-level age composition data collected and compiled by NC StAD.  Updating requires both a compilation of new records and a review of assumptions (i.e., which annual or average samples to use for which CU).

Subsequent steps are fully automated (spawner-recruit model fits, benchmark calculations, output files), but should be reviewed periodically to ensure that assumptions and scoping decisions are still valid.


### Southern BC Chinook

Source files  used here are the outputs from data processing in a separate github repository,
as described in the [SBC Ck Data Source README](https://github.com/SOLV-Code/Scanner-Data-Processing/blob/main/DATA_IN/SOURCES/SBC%20Chinook/README.md).

The data needed to feed into that processing repo comes from 2 sources: 

* raw spawner records by site from the nuSEDS database. Current database holdings are available upon request from DFO's regional nuSEDS team on short notice any time of year. However, the data entry of new records by area staff typically occurs *WHEN?* once estimates have been finalized. Some manual changes to the nuSEDS query extract are currently required to prepare the input file, but could be automated.
* records of spawner enumaration and broodstock collection by SEP hatchery staff from the EPAD database. Current database holdings are available upon request from DFO's regional SEP team on short notice any time of year. However, the data entry of new records by hatchery staff typically occurs *WHEN?* once estimates have been finalized. Some manual changes to the EPAD query extract are currently required to prepare the input file, but could be automated.

Subsequent steps are fully automated (record matching  between data bases, quality filtering, infilling), but generate a large amount of diagnostic information that needs to be reviewed for each major update, typically leading to several iterations of tweaking data and/or settings. 

Site-specific settings should be reviewed periodically to ensure that assumptions and scoping decisions are still valid. These include: (1) which sites to include in each CU time series, (2) classifications of data quality, and (3) classifications of enhancement level.  

Important notes:

* there is no established process for re-estimating biological benchmarks for those CUs where they are currently available
* For some CUs, the method for determining enhancement level was recently revised, but this approach has not been expanded to cover all SBC Ck CUs.

















