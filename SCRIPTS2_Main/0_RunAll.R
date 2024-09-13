# March 12 2024
# Run all integrated data prep steps

# Filtering occurs in the scripts
# datastage: "All", "Approved", "Explore", "Prep", c(list stages to include)
# runAllout: set to FALSE to just run dashboards 

if(FALSE){
# if want to rerun all the data prep,   
  source("SCRIPTS1_Prep/Prep3_CleanedFlatFile_SBC_Ck.R")
  source("SCRIPTS1_Prep/Prep4_CleanedFlatFile_Fraser_Sk.R")
  source("SCRIPTS1_Prep/Prep5_CleanedFlatFile_Fraser_Coho.R")
  source("SCRIPTS1_Prep/Prep6_Create Fraser Pink_2.R")
  source("SCRIPTS1_Prep/Prep7_Create Fraser Chum2.R")
  source("SCRIPTS1_Prep/Prep8_Create ISC Chum FlatFile.R")
  source("SCRIPTS1_Prep/Prep9_PrepCleanedFlatFile_SkeenaNassSockeye.R")
  source("SCRIPTS1_Prep/Prep10_PrepCleanedFlatFile_OkanaganSockeye.R")  
}



# NEED A CLEAR_OUTPUT() FUNCTION
# Testing the steps. THis seems to work, but giving some weird "permission denied" warnings. 
# Still need to add all the locations we want to clear
if(FALSE){
  
  out.files <- c(list.files("OUTPUT/DASHBOARDS", include.dirs = T, full.names = T, recursive = T),
                 list.files("OUTPUT/DATA_OUT", include.dirs = T, full.names = T, recursive = T)
                 )
  # remove the files
  file.remove(out.files)  
  
  
  
}


# IF YOU WANT TO INSTALL/UPDATE THE METRICS PACKAGES, 
# RUN THE CODE INSIDE THIS EXCLUSION LINE BY LINE
library(tidyverse)

if(FALSE){

  pkg.list <- installed.packages() %>% as.data.frame() %>% select(Package) %>% unlist()
  
  if("WSPMetrics" %in% pkg.list){
    remove.packages("WSPMetrics")
    detach("package:WSPMetrics", unload = TRUE)
  }
  
  
  library(devtools) # Load the devtools package.
   install_github("Pacific-salmon-assess/WSP-Metrics-Pkg", #ref = "DEV",  
                   dependencies = TRUE, build_vignettes = FALSE) 
    library(WSPMetrics)

  
}




Run <- function(datastage="Approved"){
      print(paste("---running", toupper(datastage),"data---"))
      if(datastage=="All") datastage <- c("Approved", "Explore", "Prep")
      print("---merging flat files---")
      source("SCRIPTS2_Main/1_MergeFlatFiles.R")
      print("---calculating WSP metrics---")
      source("SCRIPTS2_Main/2_CalculateMetrics_By_CU.R", local=TRUE)
      print("---reformatting data---")
      source("SCRIPTS2_Main/3_PrepData_for_RapidStatusAlgorithm.R", local=TRUE)
      print("---running WSP algorithm---")
      source("SCRIPTS2_Main/4_Runalgorithm_CreateScannerinputs.R", local=TRUE)
      print("---building metric and status dashboards---")
      source("SCRIPTS2_Main/5_GenerateSummaries.R", local=TRUE)
      print("---building timeline summary plots---")
      source("SCRIPTS2_Main/6_Summary_Timeline_Plots.R", local=TRUE)
      print("---building SOPO plots---")
      source("SCRIPTS2_Main/8_SMU_Summary_Timeline_PlotsforSOPO.R", local=TRUE)
      print("---creating Scanner input files---")
      source("SCRIPTS2_Main/10_GenerateScannerInputs.R", local=TRUE)
}

 Run(c("Approved"))

# Run("Explore") # SkeenaNass SK in this group for now
 
# Run("Prep")
 

