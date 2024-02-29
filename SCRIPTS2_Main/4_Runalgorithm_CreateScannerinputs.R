
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

pop.data <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_POP_SUB.csv")

# Retrospective metrics
retro.values <- read.csv("DATA_OUT/Retrospective_Metrics_Values.csv",stringsAsFactors = FALSE) %>%
                left_join(cu.lookup %>% select("CU_ID",Group), by="CU_ID" )
retro.status <- read.csv("DATA_OUT/Retrospective_Metrics_Status.csv",stringsAsFactors = FALSE)  %>%
                 left_join(cu.lookup %>% select("CU_ID",Group), by="CU_ID" )
metrics.long <- read.csv("DATA_PROCESSING/Metrics_Longform_SUB.csv")

# WSP integrated statuses
publ.status.raw <- read.csv("DATA_LOOKUP_FILES/Published_Integrated_Status_Summary.csv",stringsAsFactors = FALSE)
publ.int.status <- publ.status.raw %>%  dplyr::filter(Metric == "IntStatus") %>%
                                        select(CU_ID,Year, Status) %>% 
                                        dplyr::rename(IntStatusRaw = Status) %>%
                                        mutate(IntStatus = recode(IntStatusRaw, RedAmber = "Red", AmberGreen = "Amber")) %>%
                                        mutate(IntStatusRaw_Short = recode(IntStatusRaw, Red = "R",RedAmber = "RA", Amber = "A",AmberGreen = "AG",Green = "G"),
                                               IntStatus_Short = recode(IntStatus, Red = "R", Amber = "A",Green = "G"))

# Full metrics dummy file with all CUS (should be empty)
metrics.dummy <- read.csv("DATA_LOOKUP_FILES/METRICS_FILE_BY_CU_dummy.csv")

# Algorithm outputs IF reading these in
#algo.results <- read.csv("DATA_PROCESSING/Retro_Synoptic_Details.csv")



# ========================= RUN THE RETROSPECTIVE WITH THE RAPID STATUS FUNCTION ========================================= #

retro.lt3 <- rapid_status(data.df = retro.values, algorithm = "StateOfTheSalmon3" ,group.var = "Species") 
write.csv(retro.lt3$data,"DATA_PROCESSING/Retro_Results_LT3.csv",row.names = FALSE)


retro.summary.tbl <- retro.lt3$data %>%
                              dplyr::rename(RapidStatus = SynStatus,RapidScore = SynScore) %>%
                              select(-IntStatus,-IntScore,-ErrorScore,-ErrorType) %>%
                              left_join(retro.status %>% select(-NumStdMetrics,-Group) %>% 
                                                         rename(RelAbdCat = RelAbd, AbsAbdCat = AbsAbd, LongTrendCat = LongTrend,              
                                                                 PercChangeCat = PercChange, ProbDeclBelowLBMCat = ProbDeclBelowLBM,
                                                                 PercentileCat = Percentile, RelLBMCat = RelLBM, RelUBMCat = RelUBM,
                                                                 AbsLBMCat = AbsLBM, AbsUBMCat = AbsUBM   )      , 
                                                          by = c("CU_ID","Species","Stock","DataType","Year")) %>%
                              left_join( as.data.frame(publ.int.status),by=c("CU_ID","Year")) %>%
                              arrange(Species,Stock,Year) %>%
                              left_join(cu.data.group  %>%   
                                                 select(CU_ID,Year,SpnForAbd_Wild, SpnForTrend_Wild),
                                                 by= c("CU_ID","Year")   ) %>%
                              select(CU_ID,Species,Stock,	DataType,Year,SpnForAbd_Wild, SpnForTrend_Wild, everything()) %>%
                              left_join(qual.score.tab %>% 
                                                      dplyr::filter(Algorithm == "SotS3") %>% 
                                                      select(BinLabel, ConfidenceRating5, ConfidenceRating3, ConfidenceRating2),
                                                      by = "BinLabel")



retro.summary.tbl$IntStatus5 <- retro.summary.tbl$IntStatusRaw
retro.summary.tbl$IntStatus3 <- dplyr::recode(retro.summary.tbl$IntStatusRaw,"RedAmber" = "Red","AmberGreen" = "Amber")
retro.summary.tbl$IntStatus2 <- dplyr::recode(retro.summary.tbl$IntStatus3, "Amber" = "NotRed","Green" = "NotRed")

write.csv(retro.summary.tbl, "DATA_OUT/Retro_Synoptic_Details.csv", row.names = FALSE)   



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


bulkley.df <- data.frame(CU_ID= "SkeenaNass-15", Species= "Sockeye",
                         Stock = "Upper Bulkley Lakes", Year = 1995:2022,
                         RapidStatus = "Ext", BinLabel = "Ext",BinPath = "Ext",
                         RapidScore = NA, ConfidenceRating5 = NA, ConfidenceRating3 = NA,
                         ConfidenceRating2 = NA  )
maxan.df <- data.frame(CU_ID= "SkeenaNass-15b", Species= "Sockeye",
                         Stock = "Maxan", Year = 1995:2022,
                         RapidStatus = "Ext", BinLabel = "Ext",BinPath = "Ext",
                         RapidScore = NA, ConfidenceRating5 = NA, ConfidenceRating3 = NA,
                         ConfidenceRating2 = NA  )
retro.summary.tbl.mod <- bind_rows(retro.summary.tbl,bulkley.df,maxan.df)



# Mcdonell > use as proxy status for Aldrich and Dennis
mcdonnel.src <- retro.summary.tbl %>% dplyr::filter(Stock == "Mcdonell") %>%
                    select(Year,Stock,RapidStatus,RapidScore) %>% 
                    dplyr::rename(ProxySource= Stock)
aldrich.df <- data.frame(CU_ID= "SkeenaNass-13b", Species= "Sockeye",
                      Stock = "Aldrich", Year = 1995:2022,
                      BinLabel = "Proxy",BinPath = "Proxy",
                      ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                      ConfidenceRating2 = "Low"  ) %>% 
                      left_join(mcdonnel.src,by="Year")

dennis.df <- data.frame(CU_ID= "SkeenaNass-13c", Species= "Sockeye",
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
atna.df <- data.frame(CU_ID= "SkeenaNass-16b", Species= "Sockeye",
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

swan.df <- data.frame(CU_ID= "SkeenaNass-17b", Species= "Sockeye",
                         Stock = "Swan", Year = 1995:2022,
                         BinLabel = "Proxy",BinPath = "Proxy",
                         ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                         ConfidenceRating2 = "Low"  ) %>% 
  left_join(stephens.src,by="Year")

club.df <- data.frame(CU_ID= "SkeenaNass-17c", Species= "Sockeye",
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
damshil.df <- data.frame(CU_ID= "SkeenaNass-24b", Species= "Sockeye",
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
azukl.df <- data.frame(CU_ID= "SkeenaNass-26b", Species= "Sockeye",
                         Stock = "Azuklotz", Year = 1995:2022,
                         BinLabel = "Proxy",BinPath = "Proxy",
                         ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                         ConfidenceRating2 = "Low"  ) %>% 
  left_join(bear.src,by="Year")


# swap out records for  Damshilgwit
retro.summary.tbl.mod <- retro.summary.tbl.mod %>% dplyr::filter(!(Stock %in% c("Azuklotz")))
retro.summary.tbl.mod <- bind_rows(retro.summary.tbl.mod,azukl.df)

# Sustut -> proxy for Johansen
sustut.src <- retro.summary.tbl %>% dplyr::filter(Stock == "Sustut") %>%
  select(Year,Stock,RapidStatus,RapidScore) %>% 
  dplyr::rename(ProxySource= Stock)
johan.df <- data.frame(CU_ID= "SkeenaNass-28b", Species= "Sockeye",
                       Stock = "Johansen", Year = 1995:2022,
                       BinLabel = "Proxy",BinPath = "Proxy",
                       ConfidenceRating5 = "Low", ConfidenceRating3 = "Low",
                       ConfidenceRating2 = "Low"  ) %>% 
  left_join(sustut.src,by="Year")


# swap out records for Johanson
retro.summary.tbl.mod <- retro.summary.tbl.mod %>% dplyr::filter(!(Stock %in% c("Johansen")))
retro.summary.tbl.mod <- bind_rows(retro.summary.tbl.mod,johan.df)



write.csv(retro.summary.tbl.mod, "DATA_OUT/Retro_Synoptic_Details_SkeenaMODS.csv", row.names = FALSE)   




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


# ================================== Create the dataframes for that feed into the Scanner ============================================== #

metrics.scanner <-  metrics.long  %>%   select(-c(X, Label, DataType)) %>%
                                        filter(!Metric %in% c("ProbDeclBelowLBM", "Percentile")) %>%
                                        # mutate(Confidence =NA) %>%
                                        # rbind(
                                        #      publ.status %>% select(CU_ID,Year,IntStatus) %>%
                                        #                      pivot_longer(cols=IntStatus, names_to = "Metric", values_to="Status") %>%
                                        #                      inner_join( metrics.long %>% select(CU_ID, Species, Stock) %>% unique() , by="CU_ID") %>%
                                        #                      mutate(Compare=NA, LBM=NA, UBM=NA, Value=NA)
                                        # ) %>%
                                        rbind(
                                          retro.summary.tbl %>% select(CU_ID, Species, Stock, Year, RapidStatus, IntStatus=IntStatusRaw) %>%
                                                                mutate(RapidStatus = na_if(RapidStatus, "None")) %>%
                                                                pivot_longer(cols=c(RapidStatus, IntStatus), names_to = "Metric", values_to="Status") %>%
                                                                mutate(Compare=NA, LBM=NA, UBM=NA, Value=NA) %>%
                                                                relocate(Status, .before=Value)
                                        )%>%
                                        left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                                        left_join((retro.summary.tbl %>% select(c(CU_ID,Year,Confidence=ConfidenceRating3)) %>% 
                                                                         mutate(Metric="RapidStatus")),
                                                  by=c("CU_ID", "Year", "Metric")) %>%
                                        select(-CU_ID) %>%
                                        rename(CU_ID=CU_ID_Alt2_CULookup)%>%
                                        relocate(CU_ID, .before=Species)%>%
                                        relocate(c(Status,Confidence), .after=last_col())


# metrics.scanner <-  metrics.tmp2  %>%   filter(!Metric %in% c("ProbDeclBelowLBM", "Percentile")) %>%
#                                         select(-c(DataType, AbdMetric, ShortTrendMetric, LongTrendMetric, PercentileMetric)) %>%
#                                         rbind(
#                                             publ.status %>% select(CU_ID,Year,IntStatus) %>%
#                                                             pivot_longer(cols=IntStatus, names_to = "Metric", values_to="Status") %>%
#                                                             inner_join( metrics.raw %>% select(CU_ID, Species, Stock, Data_Type) %>% unique() , by="CU_ID") %>%
#                                                             mutate(Label="SpnForAbd_Wild", Compare=NA, LBM=NA, UBM=NA, Value=NA)
#                                         ) %>%
#                                         rbind(
#                                             sots3.trial$data %>% select(CU_ID, Species, Stock, DataType, Year, SynStatus) %>%
#                                                                  rename(Status=SynStatus, Data_Type=DataType) %>%
#                                                                  mutate(Status = na_if(Status, "None")) %>%
#                                                                  mutate(Label="SpnForAbd_Wild", Metric="SynStatus", Compare=NA, LBM=NA, UBM=NA, Value=NA)
#                                         )%>%
#                                         left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
#                                         select(-CU_ID, -Data_Type) %>%
#                                         rename(CU_ID=CU_ID_Alt2_CULookup)%>%
#                                         relocate(CU_ID, .before=Species)%>%
#                                         relocate(Status, .after=last_col())
#



# syn.statuses <-  pivot_longer(select(retro.status.df.stat, -c(ProbDeclBelowLBM, DataType, Percentile, NumStdMetrics)), cols= -c(CU_ID, Species, Stock, Year),
#                           names_to="Metric", values_to="Status")
#
# long.retro <- retro.status.df.val %>%
#                                     select(-c(ProbDeclBelowLBM, Percentile, NumStdMetrics)) %>%
#                                     pivot_longer(cols = -c(CU_ID, Species, Stock, DataType, Year),names_to="Metric", values_to="Value") %>%
#                                     left_join(statuses, by = c("CU_ID","Species","Stock","Year","Metric"))

# Could pull in the Benchmarks and comparison values from metrics.raw also if wanted
# but will require wrangling because the metrics names are different for the LBM and UBM ones
#setwd("../")
#metrics.dummy <- read.csv("DATA_LOOKUP_FILES/METRICS_FILE_BY_CU_dummy.csv")


# Metrics.dummy uses CU_IDs from NuSEDs for the tool NOT the ones used in the data prep code, so must match these
# CU_ID_Alt2_CULookup in the MAIN_CU_LOOKUP_FOR_SOS.csv

# Filter OUT the metrics data we have just calculated so this is just the EMPTY CU datasets

# Throw warning if there is data in the dummy file
if(nrow(metrics.dummy %>%  filter_at(vars(Compare, LBM, UBM, Value, Status), any_vars(!is.na(.)))) > 0){
  stop("Data present in the dummy file!! Clear this.")
}


metrics.out <- metrics.dummy %>% select(-Label) %>%
                                 filter(!CU_ID %in% metrics.scanner$CU_ID) %>%
                                 filter(!is.na(CU_ID))%>%
                                 filter(!is.na(Stock))%>%
                                 filter(!Metric == "IntStatus") %>%
                                 rbind(metrics.scanner)
# metrics.out <- metrics.dummy %>% select(-X) %>%
#                                  filter(!CU_ID %in% metrics.scanner$CU_ID) %>%
#                                  filter(!is.na(CU_ID))%>%
#                                  filter(!is.na(Stock))%>%
#                                  filter(!Metric == "IntStatus") %>%
#                                  rbind(metrics.scanner)
# #setwd("../")
#write.csv(metrics.out, "build_PStat_data/data/METRICS_FILE_BY_CU_forPSST.csv")
write.csv(metrics.out, "DATA_OUT/METRICS_FILE_BY_CU_SCANNER.csv")



# ========================= Fix escapement datasets ============================= #


# ----------- CU data file ------------------ #

# Because values were substituted into the WILD columns for Chum to get the metrics to run!
# ALSO remove the Trends data for Fraser SK

# Remove the Trends data for all CUs - no longer needed and was only different for FR SK
  # Added Oct 25 2021 to add Rel Abd metric values to time series

cu.lookup$CU_ID <- gsub("-","_", cu.lookup$CU_ID)
relabd.metric <- metrics.scanner %>% filter(Metric=="RelLBM")

# Attaching the Alternate CU_IDs from the lookup file so this match NuSEDs. This needs to be done using two different columns for matching
cu.clean <-  cu.data %>% select(-c(SpnForTrend_Total, SpnForTrend_Wild, Abd_StartYr, UseYear)) %>%
                         mutate(SpnForAbd_Wild = case_when(CU_ID %in% c("CM-02", "CM-03", "CM-04", "CM-05", "CM-06", "CM-07", "CM-08", "CM-09") ~ NA_real_,
                                                           TRUE ~ SpnForAbd_Wild)) %>%
                        rename(Escapement_Total = SpnForAbd_Total, Escapement_Wild = SpnForAbd_Wild ) %>%
                        # Added Oct 25 2021 to have the timeseries used for assessing relative abundance metrics as an option for plotting in the Scanner
                        left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                        left_join(cu.lookup %>% select(CU_ID=CU_ID_Report, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%  # Use CU_ID_Report to match to the CU_ID in CU data file where this differs from the CU_ID in the lookup file
                        mutate(CU_ID=coalesce(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%   # coalesce will take items from the first vector unless they are missing, in which case it will take items from the second vector
                        left_join(select(relabd.metric, CU_ID, Year, Compare), by=c("CU_ID","Year")) %>%
                        rename(RelAbd_metric_ts=Compare)%>%
                        relocate(c(CU_ID, DU_ID, CU_Name), .after=Species) %>%
                        select(-c(CU_ID_Report, CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                        filter(!CU_ID=="")


write.csv(cu.clean, "DATA_OUT/MERGED_FLAT_FILE_BY_CU_SCANNER.csv")
# REmove the "wild" from FR CM


# ------------  Same for the POP file ------------ #



pop.clean <- pop.data %>% select(-c(SpnForTrend_Total, SpnForTrend_Wild)) %>%
                          rename(Escapement_Total = SpnForAbd_Total, Escapement_Wild = SpnForAbd_Wild ) %>%
                          # Fix the Coho - some CUs have no hatchery origin spawners so can be put into the wild column - other CUs had hatchery which cannot be
                          # separated at teh population level (or at least HAS not been but I need to look at this and my Lynda notes to be sure)
                          # CUs with NO hatchery = Fraser Canyon (CO-05)
                          #left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                          left_join(cu.lookup %>% select(CU_ID=Pop_TimeSeriesData_CU_ID, CU_ID_Alt2_CULookup), by="CU_ID") %>% # Pop_TimeSeriesData column is the CU_ID used in the pop data file
                          #mutate(CU_ID=coalesce(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                          mutate(CU_ID=CU_ID_Alt2_CULookup) %>%
                          #select(-c(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                          select(-CU_ID_Alt2_CULookup) %>%                       
                          mutate(Escapement_Wild = case_when(CU_ID == "CO_02" ~ Escapement_Total, TRUE ~  Escapement_Wild)) # Note this is the CU_ID from the WSP process not NuSEDs

pop.data[c(5,54),]




write.csv(pop.clean, "DATA_OUT/MERGED_FLAT_FILE_BY_POP_SCANNER.csv")



#
# pop.info.adj <- pop.info %>% mutate(Comment = case_when(WSP_ts == "yes"~ "treated data used to produce CU level timeseries",
#                                                         WSP_ts == "no" ~ "raw data - do not use for analysis"))
# write.csv(pop.info.adj,"build_PStat_data-main/data/PopLookup.csv")
































