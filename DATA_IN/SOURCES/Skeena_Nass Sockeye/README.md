
# Skeena and Nass Sockeye (Category 2 - CUs with CU-level data and rapid status): CU-Level Spawner and Recruit Series 

## OVERVIEW

### CUs and SMUs 

This data set covers 42 CUs in 2 SMUs. 

SMU	| CU 
--- | --- 
Nass Sockeye	| 8 CUs 
Skeena Sockeye |  34 CUs



### Available data

Annual raw data compilation is routine in-house as part of PST requirements. This happens every year for post-season review.

Stock-level run reconstructions are done externally upon request. The initial development of these run reconstructions covered all North and Central coast CUs up to 2017 and was mostly funded by Moore Foundation via PSF. Code base was implemented by an external contractor. Funding was requested for periodic updates (with a budget of ~20k each) for all North and Central Coast CUs but was not approved. Updates for Skeena and Nass Sockeye were completed in 2020 (up to 2019) and 2023 (up to 2022) as part of a large escapement goal review, with funding from DFO's budget for PST implementation.  

Data processing and estimation steps that go from stock-level run reconstructions to CU-level time series, biological benchmarks, and rapid statuses are fully automated. Once the stock-level run reconstructions become available, the benchmark updates and rapid statuses can be generated within a day. Transfer of the full workflow to NC StAD staff is in progress.

An unresolved challenge for Skeena and Nass Sockeye CU-level data is the CU delineations. The current EG review and current version of rapid statuses uses the Stock-to-CU match that is currently in use by area staff. This has been revised from the official list currently in nuSEDS and therefore there are no official CU IDs and GIS shapefiles for some of the CUs with rapid status. To align the official regional CU list with the applied local CU list would require several steps, as per process described in Wade et al. (2019). For Skeena and Nass sockeye, the information for step 1 can be extracted from an existing data report (Pestal et al. In prep).


### nuSEDS Link
The current data processing steps are not directly linked to a nuSEDS data extract. The run reconstructions start with escapement estimates for index systems, which are expanded based on expert judgment to total spawner abundance. Expansion factors generally use a long-term average (e.g., index escapement times 3.5), but for some stocks use year-specific scalars. Choices of index systems and expansion factors have been reviewed periodically and are documented in contractor reports (not official DFO tech reports). A summary is included in a recent DFO tech report and the resulting data was peer-reviewed as part of the on-going escapement goal review. The stock-level run reconstructions are generally accepted for the purposes of Canadian domestic processes, but reconstructed stock-level data are not officially accepted by the bilateral Can/U.S. processes under the PST.  
Spawner records for index systems that are used in the run reconstructions are mostly entered into nuSEDS, but there are some backlogs and inconsistencies to be resolved. The main challenge is availability of NC StAD staff time for nuSEDS entry and verification. Another challenge is that not all estimates are direct results of a spawner survey, but rather are derived some other way (e.g., aggregate estimate and genetic stock comp) and therefore don’t fit within the intent and scope of nuSEDS.
Metric Usability – Skeena and Nass Sockeye
Two groups of CUs in terms of metric usability:
*	26 CUs classified absolute abundance data with all 4 metrics considered applicable (i.e., stock-to-CU match is such that the stock-level run reconstructions reflect the whole CU or most of the CU)
*	14 CUs classified as relative index with none of the metrics considered applicable. These are due to a combination of current stock assessment approach and challenges with the stock-to-CU match (e.g., current survey estimates total abundance for a group of 3 lakes, data assigned to the largest lake, and the other 2 lakes are considered data deficient.)





### Required Files

This folder contains 2 files:

* *Generated_SR_Data_ForRapidStatus.csv*: This file contains the cleaned and processed spawner and recruit data 
* *Generated_StockInfo_ForRapidStatus.csv*: This file has the benchmarks for each CU


### Where do these files come from?

These file need to be copied over from the [Skeena and Nass spawner-recruit model fitting repository]( https://github.com/SOLV-Code/Skeena-Nass_Sk_SRFits). 

That repository requires several input files from different sources:

* *Stock-level Run reconstructions*: These are currently developed by LGL for DFO. They are not done routinely every year. A stand-alone contract is issued periodically to add a few years of data.
* *Stock-level age composition estimates*: These are compiled from NC StAD, and rely on scale sampling programs with variable annual coverage. Assumptions regarding which sample sets to use for which stock need to be periodically reviewed.


R scripts in the [SRFits repo](https://github.com/SOLV-Code/Skeena-Nass_Sk_SRFits) filter out brood years with unlikely numbers (RpS>45)and infill missing values where possible. This data processing step, which was adopted after extensive sensitivity testing, was peer-reviewed as part of the [escapement goal review process](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2023/2023_008-eng.html). The details are documented in a technical report (**In Prep**).


### What are the required steps for updating?

The annual estimates are pulled from this folder into a merged data file, so no additional steps 
are required for that component. 

**However, the benchmark estimates need to be copied manually into the file DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv**. Note
that the CU lookup file has some data deficient CUs that are not listed in the generated source file, so need to be careful pasting the values over (rows don't line up!).


### Important Notes

- Run reconstructions are done at the stock level
- Most Skeena and Nass stocks line up with CUs
- Spawner estimates are generally based on index stream escapements that are scaled up.


