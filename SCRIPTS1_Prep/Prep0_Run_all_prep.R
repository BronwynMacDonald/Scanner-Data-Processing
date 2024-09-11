if(!dir.exists("DATA_PROCESSING")){dir.create("DATA_PROCESSING")}

source("SCRIPTS1_Prep/Prep3_CleanedFlatFile_SBC_Ck.R")
source("SCRIPTS1_Prep/Prep4_CleanedFlatFile_Fraser_Sk.R")
source("SCRIPTS1_Prep/Prep5_CleanedFlatFile_Fraser_Coho.R")
source("SCRIPTS1_Prep/Prep6_Create Fraser Pink_2.R")
source("SCRIPTS1_Prep/Prep7_Create Fraser Chum2.R")
source("SCRIPTS1_Prep/Prep8_Create ISC Chum FlatFile.R")
source("SCRIPTS1_Prep/Prep9_PrepCleanedFlatFile_SkeenaNassSockeye.R")
source("SCRIPTS1_Prep/Prep10_PrepCleanedFlatFile_OkanaganSockeye.R")