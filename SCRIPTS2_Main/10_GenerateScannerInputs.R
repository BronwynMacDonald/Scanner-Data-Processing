# June 2024
# Create data files for the Scanner

library(tidyverse)

if(!dir.exists("OUTPUT/DATA_OUT")){dir.create("OUTPUT/DATA_OUT/") }
if(!dir.exists(paste0("OUTPUT/DATA_OUT/", paste(datastage, collapse="")))){dir.create(paste0("OUTPUT/DATA_OUT/", paste(datastage, collapse=""))) }

# READ IN DATA

# Lookup files
cu.lookup <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv") %>%
                      dplyr::mutate(CU_ID = gsub("_","-",CU_ID))

# Escapement data
cu.data <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_CU_SUB.csv",stringsAsFactors = FALSE) 
pop.data <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_POP_SUB.csv")

# Retrospective metrics
retro.summary.tbl <- read_csv(paste0("OUTPUT/DASHBOARDS/Retro_Synoptic_Details_",paste(datastage, collapse=""),".csv"))
metrics.long <- read.csv(paste0("DATA_PROCESSING/FILTERED_DATA/Metrics_Longform_SUB_",paste(datastage, collapse=""),".csv"))

# Full metrics dummy file with all CUS (should be empty)
metrics.dummy <- read.csv("DATA_LOOKUP_FILES/METRICS_FILE_BY_CU_dummy.csv")




# ================================== Create the dataframes for that feed into the Scanner ============================================== #

metrics.scanner <-  metrics.long  %>%   select(-c(X, Label, DataType)) %>%
                                        filter(!Metric %in% c("ProbDeclBelowLBM", "Percentile")) %>%
                                        left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                                        select(-CU_ID) %>%
                                        rename(CU_ID=CU_ID_Alt2_CULookup) %>%
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
                                        ) %>%
                                        
                                        left_join((retro.summary.tbl %>% select(c(CU_ID,Year,Confidence=ConfidenceRating3)) %>% 
                                                                         mutate(Metric="RapidStatus")),
                                                                         by=c("CU_ID", "Year", "Metric")) %>%
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
write.csv(metrics.out, paste0("OUTPUT/DATA_OUT/",paste(datastage, collapse=""),"/METRICS_FILE_BY_CU_SCANNER_",paste(datastage, collapse=""),".csv"))



# ========================= Fix escapement datasets ============================= #


# ----------- CU data file ------------------ #

# Because values were substituted into the WILD columns for Chum to get the metrics to run!
# ALSO remove the Trends data for Fraser SK

# Remove the Trends data for all CUs - no longer needed and was only different for FR SK
# Added Oct 25 2021 to add Rel Abd metric values to time series

#cu.lookup$CU_ID <- gsub("-","_", cu.lookup$CU_ID)
relabd.metric <- metrics.scanner %>% filter(Metric=="RelLBM")

# Attaching the Alternate CU_IDs from the lookup file so this match NuSEDs. This needs to be done using two different columns for matching
cu.clean <-  cu.data %>% select(-c(SpnForTrend_Total, SpnForTrend_Wild, Abd_StartYr, UseYear)) %>%
                         mutate(SpnForAbd_Wild = case_when(CU_ID %in% c("CM-02", "CM-03", "CM-04", "CM-05", "CM-06", "CM-07", "CM-08", "CM-09") ~ NA_real_,
                                                            TRUE ~ SpnForAbd_Wild)) %>%
                         rename(Escapement_Total = SpnForAbd_Total, Escapement_Wild = SpnForAbd_Wild ) %>%
                          # Added Oct 25 2021 to have the timeseries used for assessing relative abundance metrics as an option for plotting in the Scanner
                         left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup,Data_Stage), by="CU_ID" ) %>%
                         #left_join(cu.lookup %>% select(CU_ID=CU_ID_Report, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%  # Use CU_ID_Report to match to the CU_ID in CU data file where this differs from the CU_ID in the lookup file
                         #mutate(CU_ID=coalesce(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%   # coalesce will take items from the first vector unless they are missing, in which case it will take items from the second vector
                         mutate(CU_ID=CU_ID_Alt2_CULookup) %>%                       
                         left_join(select(relabd.metric, CU_ID, Year, Compare), by=c("CU_ID","Year")) %>%
                         rename(RelAbd_metric_ts=Compare)%>%
                         relocate(c(CU_ID, DU_ID, CU_Name), .after=Species) %>%
                         #select(-c(CU_ID_Report, CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                         select(-c(CU_ID_Alt2_CULookup))%>%                       
                         filter(!CU_ID=="", Data_Stage %in% datastage) %>%
                         select(-Data_Stage)


write.csv(cu.clean, paste0("OUTPUT/DATA_OUT/",paste(datastage, collapse=""),"/MERGED_FLAT_FILE_BY_CU_SCANNER_",paste(datastage, collapse=""),".csv"))
# REmove the "wild" from FR CM


# ------------  Same for the POP file ------------ #


pop.clean <- pop.data %>% select(-c(SpnForTrend_Total, SpnForTrend_Wild)) %>%
                          rename(Escapement_Total = SpnForAbd_Total, Escapement_Wild = SpnForAbd_Wild ) %>%
                          #dplyr::filter(!is.na(CU_ID)) %>% # doing na_matches = "never" instead
                          # Fix the Coho - some CUs have no hatchery origin spawners so can be put into the wild column - other CUs had hatchery which cannot be
                          # separated at teh population level (or at least HAS not been but I need to look at this and my Lynda notes to be sure)
                          # CUs with NO hatchery = Fraser Canyon (CO-05)
                          #left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
                          left_join(cu.lookup %>% select(CU_ID=Pop_TimeSeriesData_CU_ID, CU_ID_Alt2_CULookup, Data_Stage), by="CU_ID", na_matches = "never") %>% # Pop_TimeSeriesData column is the CU_ID used in the pop data file
                          #mutate(CU_ID=coalesce(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                          mutate(CU_ID=CU_ID_Alt2_CULookup) %>%
                          #select(-c(CU_ID_Alt2_CULookup.x, CU_ID_Alt2_CULookup.y)) %>%
                          select(-CU_ID_Alt2_CULookup) %>%                       
                          mutate(Escapement_Wild = case_when(CU_ID == "CO_02" ~ Escapement_Total, TRUE ~  Escapement_Wild)) %>% # Note this is the CU_ID from the WSP process not NuSEDs
                          filter(Data_Stage %in% datastage) %>%
                          select(-Data_Stage)

write.csv(pop.clean, paste0("OUTPUT/DATA_OUT/",paste(datastage, collapse=""),"/MERGED_FLAT_FILE_BY_POP_SCANNER_",paste(datastage, collapse=""),".csv"))



# Added July 2024 fix retro.summary.table to have the correct CU_IDs -- these are now fixed in script 4

# fixed.retro <- retro.summary.tbl %>% left_join(cu.lookup %>% select(CU_ID, CU_ID_Alt2_CULookup), by="CU_ID" ) %>%
#                select(-CU_ID) %>%
#                mutate(CU_ID=CU_ID_Alt2_CULookup) %>%
#                relocate(CU_ID, .before=Species)%>%
#                select(-CU_ID_Alt2_CULookup)
# 
# write.csv(fixed.retro, paste0("OUTPUT/DASHBOARDS/Retro_Synoptic_Details_",paste(datastage, collapse=""),".csv"))
#
# pop.info.adj <- pop.info %>% mutate(Comment = case_when(WSP_ts == "yes"~ "treated data used to produce CU level timeseries",
#                                                         WSP_ts == "no" ~ "raw data - do not use for analysis"))
# write.csv(pop.info.adj,"build_PStat_data-main/data/PopLookup.csv")





