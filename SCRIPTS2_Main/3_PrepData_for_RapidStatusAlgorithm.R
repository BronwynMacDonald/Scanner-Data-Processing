
# Data prep for Running the Rapid Status Algorithm

# March 18 2023
# Change the input of rapid statuses from running the algorithm within this code to pulling in one of the algorithm output files
# Also added lines to ensure that the Metrics Dummy file is empty prior to adding in the CUs here

# Aug 4 2021
# BMac changed this to build the file GP needs to run synoptic and also the file that is read into the Scanner tool
#  USE THE dataframes built in script 3 they have the filtering by expert assessment included now!

#source('SCRIPTS2_Main/3_Generate_Synoptic_Input.R')
library(dplyr)
library(tidyr)


# if(FALSE){
# setwd("../")
# setwd("SOS-Synoptic-Status-Algorithm")
# 
# # Run SotS Learning Tree 3
# source("CART/synopticFunction_Source.R")
# 
# # retro.status.df.val <- metrics.reorg.values   # not used anymore?
# # retro.status.df.stat <- metrics.reorg.status  # not used anymore?
# # publ.status <- read.csv("DATA/Publ_Status_Reorg_Status.csv") 
# 
# setwd("../"); setwd("Scanner-Data-Processing")
# }


# Read in files

  # Lookup file
  cu.lookup <- read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE) %>%
                 dplyr::mutate(CU_ID = gsub("_","-",CU_ID))
 
  

  # Metrics flat file 
  metrics.raw <- read.csv("DATA_PROCESSING/METRICS_FILE_BY_CU_SUB.csv",stringsAsFactors = FALSE) 
    # change to the units the algorithms are using
    metrics.raw[metrics.raw$Metric == "LongTrend","Value"] <- metrics.raw[metrics.raw$Metric == "LongTrend","Value"] * 100 
    metrics.raw[metrics.raw$Metric == "LongTrend","LBM"] <- metrics.raw[metrics.raw$Metric == "LongTrend","LBM"] * 100 
    metrics.raw[metrics.raw$Metric == "LongTrend","UBM"] <- metrics.raw[metrics.raw$Metric == "LongTrend","UBM"] * 100 
  


# Get the ratios
metrics.tmp1 <- metrics.raw %>% rename(Compare=Value) %>%
                                                          #relocate(Data_Type, .after=Label) %>%
                                                          left_join(cu.lookup %>% select(CU_ID, DataType=DataQualkIdx, AbdMetric,  ShortTrendMetric, LongTrendMetric, PercentileMetric), by="CU_ID" ) %>%
                                                          # Replace Value with NA when expert defined column says this metric is not appropriate for the data
                                                          mutate(Compare = replace(Compare, ((Metric == "RelAbd" | Metric == "AbsAbd") & AbdMetric==FALSE) , NA)) %>%
                                                          mutate(Compare = replace(Compare, ((Metric == "PercChange" | Metric == "ProbDeclBelowLBM") & ShortTrendMetric==FALSE) , NA)) %>%
                                                          mutate(Compare = replace(Compare, ((Metric == "LongTrend") & LongTrendMetric==FALSE) , NA)) %>%
                                                          mutate(Compare = replace(Compare, (( Metric == "Percentile" ) & PercentileMetric==FALSE) , NA)) %>%
                                                          mutate(Value= case_when( (Metric == "RelAbd" | Metric == "AbsAbd") ~ (Compare/LBM), 
                                                                                   (Metric != "RelAbd" & Metric != "AbsAbd") ~ (Compare)
                                                          )
                                                          ) %>%
                                                          mutate(Metric = recode(Metric, RelAbd = "RelLBM", AbsAbd = "AbsLBM")) 


metrics.tmp2 <- metrics.tmp1 %>% rbind(metrics.tmp1 %>% filter(Metric == "RelLBM"| Metric =="AbsLBM") %>% 
                                                        mutate(Value=Compare/UBM) %>%
                                                        mutate(Metric=recode(Metric, RelLBM = "RelUBM", AbsLBM = "AbsUBM"))
                                                        ) %>%
                                                        mutate(Status = replace(Status, is.na(Value), NA)) %>%
                                                        select(-c(Data_Type, AbdMetric, ShortTrendMetric, LongTrendMetric, PercentileMetric)) 


# Write long format metrics to sub file
write.csv(metrics.tmp2, "DATA_PROCESSING/Metrics_Longform_SUB.csv")

std.metrics <- c("AbsLBM","AbsUBM","LongTrend","PercChange","RelLBM","RelUBM")

metrics.synoptic.values <- metrics.tmp2 %>% 
                                            select(-c(Label, Compare, LBM, UBM, Status)) %>%
                                            pivot_wider(names_from = Metric, values_from=Value)    

metrics.synoptic.status <- metrics.tmp2 %>% 
                                            select(-c(Label, Compare, LBM, UBM, Value)) %>%
                                            pivot_wider(names_from = Metric, values_from=Status) %>%
                                            mutate(RelAbd = RelUBM, AbsAbd = AbsLBM)

metrics.synoptic.values[["NumStdMetrics"]] <-  rowSums(!is.na(metrics.synoptic.values[,std.metrics]))
metrics.synoptic.status[["NumStdMetrics"]] <-  rowSums(!is.na(metrics.synoptic.status[,std.metrics]))


metrics.synoptic.values$GenAvgUsed <- metrics.synoptic.values$AbsLBM *1000 # back calculate from the Ratio relative to 1,000 BM


# Write files for running the algorithms in retro
write.csv( metrics.synoptic.values,"DATA_OUT/Retrospective_Metrics_Values.csv", row.names = FALSE)
write.csv( metrics.synoptic.status,"DATA_OUT/Retrospective_Metrics_Status.csv", row.names = FALSE)                     

# mutate(Value = case_when( ((Metric == "RelAbd" | Metric == "AbsAbd") & AbdMetric==FALSE) ~ NA_real_, 
#                           (Metric != "RelAbd" & Metric != "AbsAbd") ~ (Compare)
#                           