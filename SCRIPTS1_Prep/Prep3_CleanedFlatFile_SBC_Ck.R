#############################################
# CREATE FLAT FILE VERSIONS FOR SoS - SBC CK
#############################################

# IMPORTANT NOTE: REPLACING OKANAGAN CK DATA WITH INFO FROM SEPARATE INPUT FILE
# (FOR NOW, UNTIL THE NEW DATA FROM THE OK CK PROJECT IS PART OF THE FULL DATA FLOW)
# ONLY DOING THIS AT THE CU LEVEL, NOT THE SITE LEVEL



library(tidyverse)

# BY POP ----------------------------------------------------------------------------

# Variables matched up as follows:

# SpnForTrend_Total = TotalInfilled
# SpnForTrend_Wild = TotalInfilledAdj
# SpnForAbd_Total =  TotalInfilled
# SpnForAbd_Wild =  TotalInfilledAdj
# Recruits_Total =  NA
# Recruits_Wild =   NA


sbc.ck.bypop.raw <- read.csv("DATA_IN/SOURCES/SBC Chinook/FlatFile_For_SalmonScanner_BySite.csv",stringsAsFactors = FALSE)
names(sbc.ck.bypop.raw )


# Filtering / Renaming / Modifying:
# - remove any UseYear = FALSE (years before the pop-specific Start year used in the DFO version of the data)
# - keep all the columns, just rename a few as listed above
# - no modifications to the values, adding NA columns for recruits


# ************** BMac May 26 2021 adding in code to pull in the sites that are NOT used in the WSP data ***********
# This includes the DD sites that have data that wasn't included
# Also defined the yes/no column in the PopLookup file to tell the tool which sites are included vs not

stage_1_data <- read.csv("DATA_IN/SOURCES/SBC Chinook/Esc_Enh-Data_Stage1_MergeSources_CleanedforDB.csv")
verified_sites <- read.csv("DATA_IN/SOURCES/SBC Chinook/SBC_Chinook_VerifiedSiteLookup.csv")

# Find persistent sites (to ensure these can be identified in the PopLookup file)
P_sites<- verified_sites %>% filter(Pop_Category == "P") %>%select(POP_ID)

# Find Data deficient sites (to add into dataset)
DD_sites<- verified_sites %>% filter(Pop_Category == "DD") %>%
                              select(POP_ID, SYS_NM, CU_findex,Enhancement_Rank, StartYear)

# Pull the CU info out of the sbc data from GP so can create dataframes to bind to the sbc data
CU_info <- sbc.ck.bypop.raw %>% select(CU_Name, CU_acro, DU_ID,CU_ID) %>% unique()


# Identify the DD sites that are actually included in the CU data (North Thompson (CK-19) & Chilliwack (CK-06))
DD_included_sites <- sbc.ck.bypop.raw %>%filter(UseYear) %>% filter(!Pop_ID %in% P_sites$POP_ID)  %>% filter(!is.na(TotalInfilled)) %>%
                                         select(Pop_ID) %>% unique() %>% rename( POP_ID=Pop_ID)

stage_1_DD_data <- stage_1_data %>% filter( popID %in% DD_sites$POP_ID) %>%
                                    select(popID, rtnYear, EstClass, SumSpnEstimates_ADonly) %>%
                                    rename(POP_ID=popID)%>%
                                    left_join(DD_sites, by="POP_ID") %>%
                                    rename(Pop_ID = POP_ID, Year= rtnYear, CU_ID=CU_findex, TotalInfilled=SumSpnEstimates_ADonly,
                                           Pop_Name=SYS_NM, pNOS_Used=Enhancement_Rank ) %>%
                                    mutate(UseYear=TRUE,TotalInfilledAdj=TotalInfilled) %>%
                                    left_join(CU_info, by="CU_ID", keep=FALSE) %>%
                                  # remove Chilliwack and North Thompson from this since it's already in the data
                                    filter(!Pop_ID %in% DD_included_sites$POP_ID)




# Now add these to the Persistent sites to get all sites used in CU data
all_included <- rbind(P_sites, DD_included_sites)

# ************ what about hatchery proportions here for these non-WSP sites!


sbc.ck.bypop.cleaned <-  sbc.ck.bypop.raw  %>%
                                              filter(UseYear) %>%
                                              filter(Pop_ID %in% all_included$POP_ID) %>%  # this now only includes 'P' sites included in CU data (& Chilliwack and NThompson)
                                              bind_rows(stage_1_DD_data) %>%                   # attach the 'DD' site data NOT included in the CU data
                                              rename(SpnForTrend_Total = TotalInfilled,
                                                     SpnForTrend_Wild = TotalInfilledAdj)  %>%
                                              mutate(SpnForAbd_Total =  SpnForTrend_Total,
                                                     SpnForAbd_Wild =  SpnForTrend_Wild,
                                                     Recruits_Total =  NA, Recruits_Wild =   NA)

# sbc.ck.bypop.cleaned <-  sbc.ck.bypop.raw  %>%  filter(UseYear) %>%
#   rename(SpnForTrend_Total = TotalInfilled,
#          SpnForTrend_Wild = TotalInfilledAdj)  %>%
#   mutate(SpnForAbd_Total =  SpnForTrend_Total,
#          SpnForAbd_Wild =  SpnForTrend_Wild,
#          Recruits_Total =  NA, Recruits_Wild =   NA)

# ======== BMAC - adding the WSP yes/no column to the PopLookup file ==== #
# Pop_lookup <- read.csv("PopLookup.csv")
# Pop_lookup$WSP_ts[Pop_lookup$Pop_ID %in% all_included$POP_ID]<- "yes"
# write.csv(Pop_lookup, "PopLookup.csv")


head(sbc.ck.bypop.cleaned )
num.rec.cleaned <- dim(sbc.ck.bypop.cleaned)[1]
num.rec.raw <- dim(sbc.ck.bypop.raw)[1]

print(paste0("SBC CK By Pop: Retained Records= ",num.rec.cleaned,"/",num.rec.raw))

write.csv(sbc.ck.bypop.cleaned, "DATA_PROCESSING/Cleaned_FlatFile_ByPop_SBC_Ck.csv",row.names=FALSE)

names(sbc.ck.bypop.cleaned)



# BY CU ----------------------------------------------------------------------------

# Variables matched up as follows:

# SpnForTrend_Total = AllSitesInfilled
# SpnForTrend_Wild = AllSitesInfilledAdj
# SpnForAbd_Total =  AllSitesInfilled
# SpnForAbd_Wild =  AllSitesInfilledAdj
# Recruits_Total =  NA
# Recruits_Wild =   NA


sbc.ck.bycu.raw <- read.csv("DATA_IN/SOURCES/SBC Chinook/FlatFile_For_SalmonScanner.csv",stringsAsFactors = FALSE)
names(sbc.ck.bycu.raw )


# Filtering / Renaming / Modifying:
# - No filtering
# - rename a few as listed above, remove the one for LowUnkSites and ModHighSites
#                                 (old version of separating out wild)
# using https://dplyr.tidyverse.org/reference/select.html#details
# - no modifications to the values, adding NA columns for recruits


sbc.ck.bycu.cleaned <-  sbc.ck.bycu.raw  %>%
  rename(SpnForTrend_Total = AllSitesInfilled,
         SpnForTrend_Wild = LowUnkSitesInfilled) %>%
  mutate(SpnForAbd_Total =  SpnForTrend_Total ,
         SpnForAbd_Wild =  SpnForTrend_Wild)  %>%
        select(-starts_with("ModHighSites")) %>%
         select(-starts_with("LowUnkSites")) %>% # why have to do in 2 steps?
         mutate(Recruits_Total =  NA, Recruits_Wild =   NA)


sbc.ck.bycu.cleaned <- cbind(Species="Chinook",sbc.ck.bycu.cleaned )

head(sbc.ck.bycu.cleaned )
num.rec.cleaned <- dim(sbc.ck.bycu.cleaned)[1]
num.rec.raw <- dim(sbc.ck.bycu.raw)[1]

print(paste0("SBC CK By CU: Retained Records= ",num.rec.cleaned,"/",num.rec.raw))


#----------------------------------------------------------
# TEMPORARY PATCH FOR OKANAGAN CHINOOK


ok.ck.src <- read_csv("DATA_IN/Chinook_Okanagan_NEW.csv") %>%
                mutate(TotalSpn = NatOrigSpn + HatchOrigSpn)
ok.ck.src



ok.ck.info <- sbc.ck.bycu.cleaned %>% dplyr::filter(CU_ID == "CK-01") %>%
              select(Species, DU_ID, CU_Name, CU_acro, CU_ID) %>% unique
ok.ck.info


ok.ck.df <- left_join(ok.ck.src %>% select(CU_ID,Year, NatOrigSpn, TotalSpn) %>%
                                     dplyr::rename(SpnForTrend_Wild = NatOrigSpn,
                                                   SpnForTrend_Total = TotalSpn) %>%
                                     mutate(SpnForAbd_Total =  SpnForTrend_Total ,
                                            SpnForAbd_Wild =  SpnForTrend_Wild) ,
                      ok.ck.info, by = "CU_ID")


#view(ok.ck.df)

sbc.ck.bycu.cleaned <- sbc.ck.bycu.cleaned %>% dplyr::filter(CU_ID != "CK-01") %>%
                        bind_rows(ok.ck.df)


#----------------------------------------------------------

#ADDED because 0 values for Maria Slough  and Bessette were messing up the metric calcs (gen avg of log)
# have been debating a rule for handling this in the COSEWIC Metric WG, but no resolution

sbc.ck.bycu.cleaned$SpnForTrend_Wild[sbc.ck.bycu.cleaned$SpnForTrend_Wild==0] <- 1
sbc.ck.bycu.cleaned$SpnForAbd_Wild[sbc.ck.bycu.cleaned$SpnForAbd_Wild==0] <- 1


#----------------------------------------------------------

write.csv(sbc.ck.bycu.cleaned, "DATA_PROCESSING/Cleaned_FlatFile_ByCU_SBC_Ck.csv",row.names=FALSE)






















