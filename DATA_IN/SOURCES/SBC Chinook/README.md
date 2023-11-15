## SOURCE FILES FOR SBC Chinook

### Required Files

This folder requires 6 files:

* *Esc_Enh-Data_Stage1_MergeSources_CleanedforDB.csv*
* *Esc_Enh-Data_Stage2_QualFiltered_CleanedforDB.csv*
* *Esc_Enh-Data_Stage3_Infilled_CleanedforDB.csv*
* *SBC_Chinook_VerifiedSiteLookup.csv*
* *FlatFile_For_COSEWIC_ByCU.csv*
* *FlatFile_For_COSEWIC_BySite.csv*

### Where do these files come from?


These file need to be copied over from the [SBC Chinook Data processing repo](https://github.com/SOLV-Code/SBC-Ck-Dashboards-2.0). Five of the 6 files are output files from
a sequence of data processing steps, the 6th file is an input file with specifications (*SBC_Chinook_VerifiedSiteLookup.csv*). The data processing first has 3 steps done at the site level (i.e., individual nuSEDS popID), which merge records across sources, then filter records based on quality, and finally infill missing records where possible. For each data processing stage a summary file of site-level data is generated. Then site-level records are combined into CU-level estimates, tracking alternative versions (e.g., using all sites, using high-quality records only, etc.)


### What are the required steps for *basic* annual updates?

The [SBC Chinook Data processing repo](https://github.com/SOLV-Code/SBC-Ck-Dashboards-2.0) requires two main input files from different sources:
* raw spawner records by site from the nuSEDS database. Current database holdings are available upon request from DFO's regional nuSEDS team on short notice any time of year. However, the data entry of new records by area staff typically occurs *WHEN?* once estimates have been finalized. Some manual changes to the nuSEDS query extract are currently required to prepare the input file, but could be automated.
* records of spawner enumaration and broodstock collection by SEP hatchery staff from the EPAD database. Current database holdings are available upon request from DFO's regional SEP team on short notice any time of year. However, the data entry of new records by hatchery staff typically occurs *WHEN?* once estimates have been finalized. Some manual changes to the EPAD query extract are currently required to prepare the input file, but could be automated.

Subsequent steps are fully automated (record matching between databases, quality filtering, infilling), but generate a large amount of diagnostic information that needs to be reviewed for each major update, typically leading to several iterations of tweaking data and/or settings. 

This data processing step was developed and peer-reviewed as part of the [integrated status assessment for SBC Chinook](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2016/2016_042-eng.html). Data and data processing steps have been reviewed periodically since then, as documented in a [technical report]\(https://science-catalogue.canada.ca/record=4089004~S6).


### Important Notes

* There are many diagnostics generated during data processing. The first time a new annual update is processed, these should be checked carefully. For example: 
   * flag any missing popID (i.e., sites that previously had records but are not in the current databse extract)
   * flag any duplicate records (i.e., have 2 different records for the same popID and Year from the 2 databases, or from the same database after correcting popID for inconsistencies over time.

* The first time a new annual update is processed, the data processing scripts should be run line by line to ensure any errors are caught. For example, in the EPAD source file one specific variable seems to switch between prefixes *X26* and *X29* between years. Formal error handling could be built into the processing code, but that has not been done. There are too many potential variations of small inconsistencies in the source data to easily automate the error detection.


Site-specific settings should be reviewed periodically to ensure that assumptions and scoping decisions are still valid. These include: (1) which sites to include in each CU time series, (2) classifications of data quality, and (3) classifications of enhancement level.

Important notes:

* there is no established process for re-estimating biological benchmarks for those CUs where they are currently available
* For some CUs, the method for determining enhancement level was recently revised, but this approach has not been expanded to cover all SBC Ck CUs.