library(tidyverse)



if(FALSE){ # START REGULAR VERSION SKIP


ok.sk.src <- read_csv("DATA_IN/SOURCES/Okanagan Sockeye/GENERATED_SR_Data_Osoyoos.csv") %>%
                dplyr::rename(Spn = "Spawners") %>% 
                mutate(CU_Name = "Osoyoos-L")
head(ok.sk.src )


cu.info.main <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv")
names(cu.info.main)

ok.sk.src <- left_join(ok.sk.src, cu.info.main %>% select(CU_Name,CU_ID,DU_ID) )
ok.sk.src


ok.sk.clean <- ok.sk.src %>%
                                              mutate(Species = "Sockeye",
                             SpnForAbd_Total	 = Spn,
                             SpnForTrend_Total	 = Spn,
                             SpnForAbd_Wild		 = Spn,
                             SpnForTrend_Wild	 = Spn,
                             STK_ID = CU_ID,	STK_NAME = CU_Name ) %>%
                        select(Species,	Year,	SpnForAbd_Total,	SpnForTrend_Total,	SpnForAbd_Wild,
                          SpnForTrend_Wild,	CU_ID,	CU_Name,	DU_ID,	STK_ID,	STK_NAME)

head(ok.sk.clean)


write.csv(ok.sk.clean, "DATA_PROCESSING/Cleaned_FlatFile_ByCU_OkanaganSockeye.csv",row.names=FALSE)

} # END REGULAR VERSIONS




if(TRUE){ # START Alternative SCENARIOS VERSION
  
  
  ok.sk.src <- read_csv("DATA_IN/SOURCES/Okanagan Sockeye/OkanaganSk_AltSeries.csv") %>% 
    pivot_longer(-Year,names_to = "CU_Acro",values_to = "Spn") %>%
    select(CU_Acro,everything()) %>% arrange(CU_Acro, Year)
  
  

  head(ok.sk.src )
  
  
  cu.info.main <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv")
  names(cu.info.main)
  
  ok.sk.src <- left_join(ok.sk.src, cu.info.main %>% select(CU_Name,CU_Acro,CU_ID,DU_ID),by="CU_Acro" )
  ok.sk.src
  
  
  ok.sk.clean <- ok.sk.src %>%
    mutate(Species = "Sockeye",
           SpnForAbd_Total	 = Spn,
           SpnForTrend_Total	 = Spn,
           SpnForAbd_Wild		 = Spn,
           SpnForTrend_Wild	 = Spn,
           STK_ID = CU_ID,	STK_NAME = CU_Name ) %>%
    select(Species,	Year,	SpnForAbd_Total,	SpnForTrend_Total,	SpnForAbd_Wild,
           SpnForTrend_Wild,	CU_ID,	CU_Name,	DU_ID,	STK_ID,	STK_NAME)
  
  head(ok.sk.clean)
  
  
  write.csv(ok.sk.clean, "DATA_PROCESSING/Cleaned_FlatFile_ByCU_OkanaganSockeye.csv",row.names=FALSE)
  
} # END OK SK CSAS SCENARIOS VERSION


