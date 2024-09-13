
# NOTE: THIS SCRIPT DOES NOT YET RUN ON THE GITHUB VERSION OF THE REPO! It reads the output files! It also reads in the metrics
# calculated in Script #2. These SHOULD BE THE SAME  as those output by the algorithm output file and can be read from this file
# once we add in a script to run the algorithm in here!


# Data prep for Scanner
# Aug 4 2021
# BMac changed this to build the file GP needs to run synoptic and also the file that is read into the Scanner tool
#  USE THE dataframes built in script 3 they have the filtering by expert assessment included now!

# JULY 2023 UPDATE
# Data set needed for algorithm input is now in script 3 (recreated this script). This now includes only steps needed
# to make the Scanner input files
# Added in bits from GPs Script #5 to run the algorithm and create the main output file with algo results (Retro_Synoptic_Details.csv)

# JUNE 2024 Update
# pulling out Scanner data prep steps. This script will the algorithm

library(tidyverse)


if(exists("retro.lt3")){rm(retro.lt3)}
source("CODE/synopticFunction_Source.R")   

# READ IN DATA

# Lookup files
cu.lookup <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv") %>%
                     dplyr::mutate(CU_ID = gsub("_","-",CU_ID))
qual.score.tab <- read_csv("DATA_LOOKUP_FILES/Node_ConfidenceRating.csv")

# Escapement data
cu.data <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_CU_SUB.csv",stringsAsFactors = FALSE) 
cu.data.group <- cu.data %>%
                    mutate(CU_ID = gsub("_","-",CU_ID)) %>% 
                    dplyr::filter(!is.na(CU_Name)) %>%
                    left_join(cu.lookup %>% select(CU_Name,Group), by="CU_Name" )

# Retrospective metrics
retro.values <- read.csv(paste0("DATA_PROCESSING/FILTERED_DATA/Retrospective_Metrics_Values_",paste(datastage, collapse=""),".csv"),stringsAsFactors = FALSE) %>%
                left_join(cu.lookup %>% select("CU_ID",Group), by="CU_ID" )
retro.status <- read.csv(paste0("DATA_PROCESSING/FILTERED_DATA/Retrospective_Metrics_Status_",paste(datastage, collapse=""),".csv"),stringsAsFactors = FALSE)  %>%
                 left_join(cu.lookup %>% select("CU_ID",Group), by="CU_ID" )
metrics.long <- read.csv(paste0("DATA_PROCESSING/FILTERED_DATA/Metrics_Longform_SUB_",paste(datastage, collapse=""),".csv"))

# WSP integrated statuses
publ.status.raw <- read.csv("DATA_LOOKUP_FILES/Published_Integrated_Status_Summary.csv",stringsAsFactors = FALSE)
publ.int.status <- publ.status.raw %>%  dplyr::filter(Metric == "IntStatus") %>%
                                        select(CU_ID,Year, Status) %>% 
                                        dplyr::rename(IntStatusRaw = Status) %>%
                                        mutate(IntStatus = recode(IntStatusRaw, RedAmber = "Red", AmberGreen = "Amber")) %>%
                                        mutate(IntStatusRaw_Short = recode(IntStatusRaw, Red = "R",RedAmber = "RA", Amber = "A",AmberGreen = "AG",Green = "G"),
                                               IntStatus_Short = recode(IntStatus, Red = "R", Amber = "A",Green = "G"))

# Algorithm outputs IF reading these in
#algo.results <- read.csv("DATA_PROCESSING/Retro_Synoptic_Details.csv")



# ========================= RUN THE RETROSPECTIVE WITH THE RAPID STATUS FUNCTION ========================================= #

retro.lt3 <- rapid_status(data.df = retro.values, algorithm = "StateOfTheSalmon3" ,group.var = "Species") 
write.csv(retro.lt3$data, paste0("DATA_PROCESSING/FILTERED_DATA/Retro_Results_LT3_",paste(datastage, collapse=""),".csv"),row.names = FALSE)


retro.summary.tbl <- retro.lt3$data %>%
                              dplyr::rename(RapidStatus = SynStatus,RapidScore = SynScore) %>%
                              select(-IntStatus,-IntScore,-ErrorScore,-ErrorType) %>%
                              left_join(retro.status %>% select(-NumStdMetrics,-Group) %>% 
                                                         rename(RelAbdCat = RelAbd, AbsAbdCat = AbsAbd, LongTrendCat = LongTrend,              
                                                                 PercChangeCat = PercChange, ProbDeclBelowLBMCat = ProbDeclBelowLBM,
                                                                 PercentileCat = Percentile, RelLBMCat = RelLBM, RelUBMCat = RelUBM,
                                                                 AbsLBMCat = AbsLBM, AbsUBMCat = AbsUBM   )      , 
                                                          by = c("CU_ID","Species","Stock","DataType","Year")) %>%
                              left_join(metrics.long %>% filter(Metric == "RelLBM") %>% 
                                                         select(CU_ID, Stock, Year, RelAbd_LBM=LBM, RelAbd_UBM=UBM),
                                                         by = c("CU_ID","Stock","Year")) %>%
                              left_join( as.data.frame(publ.int.status),by=c("CU_ID","Year")) %>%
                              arrange(Species,Stock,Year) %>%
                              left_join(cu.data.group  %>%   
                                                 select(CU_ID,Year,SpnForAbd_Wild, SpnForTrend_Wild, SpnForAbd_Total, SpnForTrend_Total),
                                                 by= c("CU_ID","Year")   ) %>%
                              select(CU_ID,Species,Stock,	DataType,Year,SpnForAbd_Wild, SpnForTrend_Wild, everything()) %>%
                              left_join(qual.score.tab %>% 
                                                      dplyr::filter(Algorithm == "SotS3") %>% 
                                                      select(BinLabel, ConfidenceRating5, ConfidenceRating3, ConfidenceRating2),
                                                      by = "BinLabel") %>%
                              left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID") %>%
                                                      select(-CU_ID) %>%
                                                      rename(CU_ID=CU_ID_Alt2_CULookup)%>%
                                                      relocate(CU_ID, .before=Species)
                                



retro.summary.tbl$IntStatus5 <- retro.summary.tbl$IntStatusRaw
retro.summary.tbl$IntStatus3 <- dplyr::recode(retro.summary.tbl$IntStatusRaw,"RedAmber" = "Red","AmberGreen" = "Amber")
retro.summary.tbl$IntStatus2 <- dplyr::recode(retro.summary.tbl$IntStatus3, "Amber" = "NotRed","Green" = "NotRed")

if(!dir.exists("OUTPUT/DASHBOARDS")){dir.create("OUTPUT/DASHBOARDS")}
write.csv(retro.summary.tbl, paste0("OUTPUT/DASHBOARDS/Retro_Synoptic_Details_",paste(datastage, collapse=""),".csv"), row.names = FALSE)   



#############################
# EXPLORATORY CHANGES FOR SKEENA RESULTS

# as per CCH feedback
# Mcdonell > use as proxy status for Aldrich and Dennis
# Bukley, Maxan -> show as extirpated
# Morice -> proxy for Atna
# Stephens -> Proxy for Swan, Club
# Slamgeesh -> Proxy for  Damshilgwet
# Bear -> proxy for Azuklotz
# Sustut -> proxy for Johanson


# For the proxy statuses, making the following changes
# RapidStatus: copy from proxy source	
# BinLabel: Set to "Proxy"	
# BinPath: 	Set to "Proxy"
# RapidScore: copy from proxy source
# ConfidenceRating5: Set to "Low"	(not used)
# ConfidenceRating3: Set to "Low"	(used in plot)	
# ConfidenceRating2: Set to "Low"	(not used)
# New Variable "ProxySource": Set to full name of proxy

# For extirpated CUs, making the following changes
# DataType: Set to "Ext"
# RapidStatus: Set to "Ext"
# BinLabel: Set to "Ext"	
# BinPath: 	Set to "Ext"
# RapidScore: Set to NA
# ConfidenceRating5: Set to NA
# ConfidenceRating3: Set to NA
# ConfidenceRating2: Set to NA

# For now, just hardwiring these changes as we test out these ideas
# if we carry this forward, will need to set up a proper lookup file with specs
# and streamline the code

# extirpated CUs (not currently in df -> add the rows)
names(retro.summary.tbl)

#Sockeye	Sk	SkeenaNass_15b	Maxan


# Only run this if the selected datatype contains the Skeena/Nass CUs
if(any(unique(retro.summary.tbl$CU_ID) %in% c("SEL-20-08", "SEL-21-07", "SEL-21-09", "SEL-22-10", "SEL-22-03","SEL-22-12"))){
  
    bulkley.df <- data.frame(CU_ID= "SEL-21-03", Species= "Sockeye",
                             Stock = "Upper Bulkley Lakes", Year = 1995:2022,
                             RapidStatus = "Ext", BinLabel = "Ext",BinPath = "Ext",
                             RapidScore = NA, ConfidenceRating5 = NA, ConfidenceRating3 = NA,
                             ConfidenceRating2 = NA  )
    maxan.df <- data.frame(CU_ID= "SEL-21-06", Species= "Sockeye",
                             Stock = "Maxan", Year = 1995:2022,
                             RapidStatus = "Ext", BinLabel = "Ext",BinPath = "Ext",
                             RapidScore = NA, ConfidenceRating5 = NA, ConfidenceRating3 = NA,
                             ConfidenceRating2 = NA  )
    retro.summary.tbl.mod <- bind_rows(retro.summary.tbl,bulkley.df,maxan.df)
    
    
    
    # Mcdonell > use as proxy status for Aldrich and Dennis
    mcdonnel.src <- retro.summary.tbl %>% dplyr::filter(Stock == "Mcdonell") %>%
                        select(Year,Stock,RapidStatus,RapidScore) %>% 
                        dplyr::rename(ProxySource= Stock)
    aldrich.df <- data.frame(CU_ID= "SEL-20-02", Species= "Sockeye",
                          Stock = "Aldrich", Year = 1995:2022,
                          BinLabel = "Proxy",BinPath = "Proxy",
                          ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                          ConfidenceRating2 = "Low"  ) %>% 
                          left_join(mcdonnel.src,by="Year")
    
    dennis.df <- data.frame(CU_ID= "SEL-20-03", Species= "Sockeye",
                             Stock = "Dennis", Year = 1995:2022,
                             BinLabel = "Proxy",BinPath = "Proxy",
                             ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                             ConfidenceRating2 = "Low"  ) %>% 
                             left_join(mcdonnel.src,by="Year")
    
    # swap out records for Aldrich, Dennis
    retro.summary.tbl.mod <- retro.summary.tbl.mod %>% dplyr::filter(!(Stock %in% c("Aldrich","Dennis")))
    retro.summary.tbl.mod <- bind_rows(retro.summary.tbl.mod,aldrich.df,dennis.df)
    
    
    # Morice -> proxy for Atna
    morice.src <- retro.summary.tbl %>% dplyr::filter(Stock == "Morice") %>%
      select(Year,Stock,RapidStatus,RapidScore) %>% 
      dplyr::rename(ProxySource= Stock)
    atna.df <- data.frame(CU_ID= "SEL-21-01", Species= "Sockeye",
                             Stock = "Atna", Year = 1995:2022,
                             BinLabel = "Proxy",BinPath = "Proxy",
                             ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                             ConfidenceRating2 = "Low"  ) %>% 
      left_join(morice.src,by="Year")
    
    
    # swap out records for Atna
    retro.summary.tbl.mod <- retro.summary.tbl.mod %>% dplyr::filter(!(Stock %in% c("Atna")))
    retro.summary.tbl.mod <- bind_rows(retro.summary.tbl.mod,atna.df)
    
    
    # Stephens -> Proxy for Swan, Club
    stephens.src <- retro.summary.tbl %>% dplyr::filter(Stock == "Stephens") %>%
      select(Year,Stock,RapidStatus,RapidScore) %>% 
      dplyr::rename(ProxySource= Stock)
    
    swan.df <- data.frame(CU_ID= "SEL-21-10", Species= "Sockeye",
                             Stock = "Swan", Year = 1995:2022,
                             BinLabel = "Proxy",BinPath = "Proxy",
                             ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                             ConfidenceRating2 = "Low"  ) %>% 
      left_join(stephens.src,by="Year")
    
    club.df <- data.frame(CU_ID= "SEL-21-10-1", Species= "Sockeye",
                            Stock = "Club", Year = 1995:2022,
                            BinLabel = "Proxy",BinPath = "Proxy",
                            ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                            ConfidenceRating2 = "Low"  ) %>% 
      left_join(stephens.src,by="Year")
    
    # swap out records for Swan, Club
    retro.summary.tbl.mod <- retro.summary.tbl.mod %>% dplyr::filter(!(Stock %in% c("Swan","Club")))
    retro.summary.tbl.mod <- bind_rows(retro.summary.tbl.mod,swan.df,club.df)
    
    
    # Slamgeesh -> Proxy for  Damshilgwit
    slamg.src <- retro.summary.tbl %>% dplyr::filter(Stock == "Slamgeesh") %>%
      select(Year,Stock,RapidStatus,RapidScore) %>% 
      dplyr::rename(ProxySource= Stock)
    damshil.df <- data.frame(CU_ID= "SEL-22-04", Species= "Sockeye",
                          Stock = "Damshilgwit", Year = 1995:2022,
                          BinLabel = "Proxy",BinPath = "Proxy",
                          ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                          ConfidenceRating2 = "Low"  ) %>% 
      left_join(slamg.src,by="Year")
    
    
    # swap out records for  Damshilgwit
    retro.summary.tbl.mod <- retro.summary.tbl.mod %>% dplyr::filter(!(Stock %in% c("Damshilgwit")))
    retro.summary.tbl.mod <- bind_rows(retro.summary.tbl.mod,damshil.df)
    
    
    
    # Bear -> proxy for Azuklotz
    bear.src <- retro.summary.tbl %>% dplyr::filter(Stock == "Bear") %>%
      select(Year,Stock,RapidStatus,RapidScore) %>% 
      dplyr::rename(ProxySource= Stock)
    azukl.df <- data.frame(CU_ID= "SEL-22-02", Species= "Sockeye",
                             Stock = "Azuklotz", Year = 1995:2022,
                             BinLabel = "Proxy",BinPath = "Proxy",
                             ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                             ConfidenceRating2 = "Low"  ) %>% 
      left_join(bear.src,by="Year")
    
    
    # swap out records for  Azuklotz
    retro.summary.tbl.mod <- retro.summary.tbl.mod %>% dplyr::filter(!(Stock %in% c("Azuklotz")))
    retro.summary.tbl.mod <- bind_rows(retro.summary.tbl.mod,azukl.df)
    
    # Sustut -> proxy for Johansen
    sustut.src <- retro.summary.tbl %>% dplyr::filter(Stock == "Sustut") %>%
      select(Year,Stock,RapidStatus,RapidScore) %>% 
      dplyr::rename(ProxySource= Stock)
    johan.df <- data.frame(CU_ID= "SEL-22-05", Species= "Sockeye",
                           Stock = "Johansen", Year = 1995:2022,
                           BinLabel = "Proxy",BinPath = "Proxy",
                           ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                           ConfidenceRating2 = "Low"  ) %>% 
      left_join(sustut.src,by="Year")
    
    
    # swap out records for Johanson
    retro.summary.tbl.mod <- retro.summary.tbl.mod %>% dplyr::filter(!(Stock %in% c("Johansen")))
    retro.summary.tbl.mod <- bind_rows(retro.summary.tbl.mod,johan.df)
    
    
    
    write.csv(retro.summary.tbl.mod, paste0("OUTPUT/DASHBOARDS/Retro_Synoptic_Details_SkeenaMODS_",paste(datastage, collapse=""),".csv"), row.names = FALSE)   

}

############################

# ----- Get the ratios and build the dataframe for metrics
# metrics.tmp <- metrics.raw %>% rename(Compare=Value) %>%
#                                relocate(Data_Type, .after=Label)%>%
#                               mutate(Value= case_when( (Metric == "RelAbd" | Metric == "AbsAbd") ~ (Compare/LBM),
#                                                               (Metric != "RelAbd" & Metric != "AbsAbd") ~ (Compare)
#                               )
#                               ) %>%
#                               mutate(Metric = recode(Metric, RelAbd = "RelLBM", AbsAbd = "AbsLBM")) %>%
#                               rbind(metrics.raw %>% filter(Metric == "RelAbd"| Metric =="AbsAbd") %>%
#                                                     rename(Compare=Value) %>%
#                                                     mutate(Value=Compare/UBM) %>%
#                                                     mutate(Metric=recode(Metric, RelAbd = "RelUBM", AbsAbd = "AbsUBM"))
#                               ) %>%

# Check that metrics in algo.results and metrics.long line up
# compare.metrics <-  algo.results %>% select(CU_ID, Year, LongTrend) %>%
#                                      right_join(metrics.long%>%select(CU_ID, Year, Metric, Value) %>%filter(Metric =="LongTrend"), by=c("CU_ID","Year"))
# identical(compare.metrics$LongTrend, compare.metrics$Value)
# CU_13 does not align on the trend metrics - these are unstable for this CU





























