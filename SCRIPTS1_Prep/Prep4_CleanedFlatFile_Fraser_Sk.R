#############################################
# CREATE FLAT FILE VERSIONS FOR SoS - Fraser Sockeye
#############################################

library(tidyverse)

# BY POP ----------------------------------------------------------------------------

# Variables matched up as follows:

# SpnForTrend_Total = eff_fem
# SpnForTrend_Wild = eff_fem
# SpnForAbd_Total =  Total
# SpnForAbd_Wild =  Total
# Recruits_Total =  rec
# Recruits_Wild =   NA

#fr.sk.bypop.raw2 <- read.csv("DATA_IN/Sockeye_Fraser_ByPop.csv",stringsAsFactors = FALSE)


# ************** Bmac changed read in file ******************************************
fr.sk.bypop.raw <- read.csv("DATA_IN/SOURCES/Fraser Sockeye/SKAll (June 2024_noEStu).csv",fileEncoding="UTF-8-BOM",stringsAsFactors = FALSE)
# *********************************************************************************
cu.lookup <- read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE) # ADDED jULY 2023


names(fr.sk.bypop.raw )

sort(unique(fr.sk.bypop.raw$est_type))


fr.sk.decoder <- read.csv("DATA_LOOKUP_FILES/Site_Info_GeneratedDecoder_Fraser_Sk.csv",
                            stringsAsFactors = FALSE) %>%
                      select(Pop_Name = AF_PopName,
                             Pop_ID = AF_POP_ID,

                             # **************************** BMac added **********************************
                             MapPOP_ID = MF_POP_ID,
                             # **************************************************************************

                             CU_ID = NF_CU_ID,
                             CU_Name.Check = NF_CUName,
                             Timing.Group = AF_Timing) %>%
                      mutate(DU_ID = NA) %>%
                      mutate(Timing.Group = tolower(Timing.Group)) %>%
                      filter(! ((is.na(Pop_ID)| Pop_ID==0) & is.na(MapPOP_ID)) ) # ************BMac added
                     #filter(!is.na(CU_Name.Check))

head(fr.sk.decoder)
# BMAc fixing an error!
fr.sk.decoder$Timing.Group[fr.sk.decoder$Pop_Name=="Celista Creek" & fr.sk.decoder$CU_ID == "L-09-03" ]  <- "t4"
fr.sk.decoder$Timing.Group[fr.sk.decoder$Pop_Name=="Celista Creek" & fr.sk.decoder$CU_ID == "L-09-02" ]  <- "t2"


 write.csv(fr.sk.decoder, "DATA_TRACKING/FR_SK_Decoder_Prep4.csv")

# Filtering / Renaming / Modifying:
# - no filtering
# - rename a most columns , drop a few
# - several modifications to the values
#       - adding NA columns for recruits
#       - convert est class to be consistent

vars.drop <- c("spnpeak","arrival","jacks","males","remarks","Estimate.Method","Survey.Start","Survey.End","Number.Inspections",
                  "females","spawn.")



# ************************************************** Bmac NOTES *************************************************
# One problem, Crazy Creek exists in 2 places - Harrison/Lillooet and also Shuswap and both have T4 timing.
# This site is being replicated wherever data exists for one of them because there are two records in the decoder.
# Must jig up a patch for now ******************************************************]
  fr.sk.decoder <- fr.sk.decoder %>% mutate(Timing.Group = ifelse(Pop_Name== "Crazy Creek" & CU_ID=="L-03-03", "t5", Timing.Group))
  fr.sk.bypop.raw <- fr.sk.bypop.raw %>% mutate( Timing.Group = ifelse(Stock.Name.stream.=="Crazy Creek" & CU.Name == "Harrison (D/S) - Late", "T5" ,Timing.Group))

  # March 24 2021 - ALSO need to account for the Early Nadina Population as part of the Nadina river - conjoin the two sites - sum them in years they overlap and put
  # in the Late Nadina cells
  # Feb 2024 - we now have a POP ID for Nadina Channel so will keep this seperate
  nadina <- fr.sk.bypop.raw %>% filter(Stock.Name.stream. %in% c("Early Nadina River","Late Nadina River")) %>%
                                select(Year, Stock.Name.stream., eff_fem, Total) %>%
                                pivot_wider(names_from = Stock.Name.stream., values_from=c(Total, eff_fem)) %>%
                                mutate(Total=rowSums(select(., c(`Total_Late Nadina River`, `Total_Early Nadina River`) ), na.rm=T)) %>%
                                mutate(eff_fem = rowSums(select(., c(`eff_fem_Late Nadina River`, `eff_fem_Early Nadina River`) ), na.rm=T)) %>%
                                select(-c(`Total_Late Nadina River`, `Total_Early Nadina River`, `eff_fem_Late Nadina River`, `eff_fem_Early Nadina River`))%>%
                                mutate(Stock.Name.stream.= "Late Nadina River")

  add.nadina <-  fr.sk.bypop.raw  %>% filter(Stock.Name.stream. == "Late Nadina River") %>%
                                      mutate(Total = nadina$Total) %>%
                                      mutate(eff_fem=nadina$eff_fem)

# ***************************************************************************************************************** END


fr.sk.bypop.cleaned <-  fr.sk.bypop.raw  %>%
                         filter(!Stock.Name.stream. %in% c("Early Nadina River","Late Nadina River" )) %>% # Feb 2024 - keep Nadina Channel in dataset
                         rbind(add.nadina) %>%
                         mutate(SpnForTrend_Wild = (males + females)*spawn./100,   # Sept 2024 - changed from Total to the EFS so this will match the CU aggregate totals
                                 SpnForAbd_Wild = (males + females)*spawn./100,
                                 SpnForAbd_Total =  (males + females)*spawn./100,
                                 SpnForTrend_Total = (males + females)*spawn./100
                                 ) %>%
                          rename(#SpnForTrend_Total = Total, #eff_fem,                    CHANGED THIS APRIL 2021 because SpnForTrend is now the RAW data for Pop_level (PROBABLY - to confirm)
                                 CU_Name = CU.Name,
                                 Pop_Name = Stock.Name.stream.,
                                 EstClass = est_type)  %>%
                          mutate(Timing.Group = tolower(Timing.Group)) %>%
                          select(-any_of(vars.drop)) %>%
                          mutate(EstClass=replace(EstClass,
                          EstClass== "Type-1: True Abundance, high resolution" , "TRUE ABUNDANCE (TYPE-1)")) %>%
                          mutate(EstClass=replace(EstClass,
                                                  EstClass== "Type-2: True Abundance, medium resolution" , "TRUE ABUNDANCE (TYPE-2)")) %>%
                          mutate(EstClass=replace(EstClass,
                                                  EstClass== "Type-3: Relative Abundance, high resolution" , "RELATIVE ABUNDANCE (TYPE-3)")) %>%
                          mutate(EstClass=replace(EstClass,
                                                  EstClass== "Type-4: Relative Abundance, medium resolution" , "RELATIVE ABUNDANCE (TYPE-4)")) %>%
                          mutate(EstClass=replace(EstClass,
                                                  EstClass== "Type-5: Relative Abundance, low resolution","")) %>%
                          mutate(EstClass=replace(EstClass,
                                                  EstClass== "Type-6: Presence or Absence","")) %>%
                          mutate(Recruits_Total =  NA, Recruits_Wild =   NA)   %>%
                          left_join(fr.sk.decoder, by=c("Pop_Name" = "Pop_Name","Timing.Group" = "Timing.Group")) %>%
                          left_join(cu.lookup %>% select(Pop_TimeSeriesData_CU_ID, CU_ID_Alt2_CULookup) %>%  # Added March 2023 to include correct CU_IDs
                                      rename(CU_ID=Pop_TimeSeriesData_CU_ID),
                                    by="CU_ID" )

# Add fix for Birkenhead & Taseko since these values get infilled/filled with NAs in the ETS - Sept 2024
  fr.sk.bypop.cleaned$SpnForAbd_Wild[fr.sk.bypop.cleaned$Pop_Name=="Birkenhead River" & fr.sk.bypop.cleaned$Year==2002] <- mean(c(
    fr.sk.bypop.cleaned$SpnForAbd_Wild[fr.sk.bypop.cleaned$Pop_Name=="Birkenhead River" & fr.sk.bypop.cleaned$Year==1998],
    fr.sk.bypop.cleaned$SpnForAbd_Wild[fr.sk.bypop.cleaned$Pop_Name=="Birkenhead River" & fr.sk.bypop.cleaned$Year==2006])
  )
  fr.sk.bypop.cleaned$SpnForAbd_Wild[fr.sk.bypop.cleaned$Pop_Name=="Taseko Lake" & fr.sk.bypop.cleaned$Year %in% c(1965,1968:1992)] <- NA
  fr.sk.bypop.cleaned <- fr.sk.bypop.cleaned %>% mutate(SpnForTrend_Wild=SpnForAbd_Wild) %>%
                                                 mutate(SpnForAbd_Total=SpnForAbd_Wild) %>%
                                                 mutate(SpnForTrend_Total=SpnForAbd_Wild)

# ****************************************************************************************************** BMAC
fr.sk.bypop.cleaned <- fr.sk.bypop.cleaned %>% mutate( Timing.Group = ifelse(Timing.Group=="t5", "t4" ,Timing.Group))
# **********************************************************************************************************
head(fr.sk.bypop.cleaned )
num.rec.cleaned <- dim(fr.sk.bypop.cleaned)[1]
num.rec.raw <- dim(fr.sk.bypop.raw)[1]



# BMac - find out which of the Stad streams aren't in the spatial data or CU definition by NuSEDs so these aren't summed in the CU total
not.in.cu <- fr.sk.bypop.cleaned  %>% filter(is.na(MapPOP_ID)) %>%
                                      group_by(CU_Name) %>%
                                      select(Pop_Name) %>%
                                      distinct(Pop_Name) %>%
                                      rename(Stock.Name.stream.=Pop_Name, CU.Name=CU_Name)


write.csv(not.in.cu, "DATA_TRACKING/Sites not in CUs.csv")

print(paste0("Fraser Sk Pop: Retained Records= ",num.rec.cleaned,"/",num.rec.raw))  # Bmac - fewer now because removed some Nadina systems

write.csv(fr.sk.bypop.cleaned, "DATA_PROCESSING/Cleaned_FlatFile_ByPop_FraserSockeye.csv",row.names=FALSE)


match.check <- unique(fr.sk.bypop.cleaned[c("CU_Name","CU_Name.Check")]) %>%
                arrange(CU_Name)

write.csv(match.check, "DATA_TRACKING/FraserSockeye_MatchingCheck.csv",row.names=FALSE)




# BY CU ----------------------------------------------------------------------------

# have to first generate the CU level summary (using code by Bronwyn MacDonald), then
# reorganize to fit into the SoS template

# Read in BMac functions
source("SCRIPTS1_Prep/SUB_GenerateFraserSockeyeCUData.R")
# main function is GetCUData(CU_Name, Update, Data)
# CU_Name appears in quotations
# Update refers to the last year of data
# Data refers to whether to pull out EFS or ETS data
# (however, ETS will not be the same as the ETS used for stock status as this file does not pull in ALL streams.)

last.yr <- range(fr.sk.bypop.raw$Year)[2] # last year for the merging and infilling

CU_Streams <- read.csv("DATA_IN/SOURCES/Fraser Sockeye/Sockeye_Fraser_CU_Streams.csv", header = TRUE)               # Defines streams in each CU
SR_Streams <- read.csv("DATA_IN/SOURCES/Fraser Sockeye/Sockeye_Fraser_SR_Streams.csv", header = TRUE)               # Defines streams in the SR data ***************** BLM add APRIL 2021 ****

Expansion_Years <- read.csv("DATA_IN/SOURCES/Fraser Sockeye/Sockeye_Fraser_Expansion_Years.csv", header = TRUE)     # Defines years being gap-filled in applicable CUs
#SK_Data  <- read.csv("DATA_IN/Sockeye_Fraser_ByPop.csv",stringsAsFactors = TRUE)

# ----------------------------------------------------------BMac added filter here to remove sites that are not in the spatial data or in the NuSEDs CU list ***
# the only difference in the numbers this makes is it removes 22 fish from Quesnel, 86 from Shuswap Late; and roughly 4,000 from EsStu over the 50+ years (mostly the 50's and 60's)
# This is necessary otherwise the site level data will not sum to the CU-level data

SK_Data <- anti_join(fr.sk.bypop.raw,not.in.cu)    # anti_join removed all of the rows that align with the not.in.cu dataframe


#cu.info.sos  <- read.csv("DATA_LOOKUP_FILES/SoS_Data_CU_Info_ForDataPrep.csv",stringsAsFactors = FALSE)


# ********************************************************************************************* BMAC ADDED
#CU.metrics.info <- read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv", stringsAsFactors = FALSE)
# ********************************************************************************************************

#cu.list.fr.sk <- unlist(cu.info.sos %>% filter(Species == "Sockeye") %>% select(Conservation_Unit))
cu.list.fr.sk <- cu.lookup %>% filter(Group %in% c("SK-Fraser_Estu", "SK-Fraser_Esum", "SK-Fraser_Sum", "SK-Fraser_RT", "SK-Fraser_Lat")) %>%
                    select(CU_Name) %>%
                    filter(CU_Name !="Chilko_ES") %>%
                    unlist()

# CUs with mean proportion method gap-filling
GapCUs <- c("Nahatlatch_ES", "Quesnel_S", "Shuswap_L", "Takla_Trem_EStu", "Takla_Trem_S_S")


if(!dir.exists("DATA_PROCESSING/FraserSockeyePrep")){dir.create("DATA_PROCESSING/FraserSockeyePrep")}

"Seymour River-Early" %in% SK_Data$Stock.Name.stream.

# test the function
test.out <- GetCUData(CU_Name = "Shuswap_L", Update = 2012, Data = "EFS",
                    cu.info.file = cu.lookup,
                    out.folder = "DATA_PROCESSING/FraserSockeyePrep/")
                    # note the "/" at the end of the folder name
test.out

cu.data.out <- data.frame()



# loop through all the CUs


cu.data.out <- data.frame(Year = numeric(),
                          SpnForAbd_Total = numeric(),
                          SpnForTrend_Total = numeric(),
                          SpnForAbd_Wild = numeric(),
                          SpnForTrend_Wild = numeric(),
                          CU_ID = character(),
                          DU_ID  = character(),
                          CU_Name = character())



# TEMP: excluding CUltus, cause it crashes things
for(cu.do in cu.list.fr.sk[cu.list.fr.sk!="Cultus_L"]){

    print("----------------------")
    print(cu.do)

    #info.sub <-   filter(cu.info.sos,Conservation_Unit == cu.do)
    info.sub <-   filter(cu.lookup, CU_Name == cu.do)

    cu.id <-  info.sub$CU_ID
    stk.id <- info.sub$STK_ID
  #  stk.name <- info.sub$StkNm

    # ***************************************************************************** BMAC
    trend.start.yr <- cu.lookup$Trends_StartYr[cu.lookup$CU_ID == cu.id]
    abd.start.yr <- cu.lookup$Abd_StartYr[cu.lookup$CU_ID == cu.id]
    #**********************************************************************************

    # ***** CHANGES APRIL 2021 - PULLING THE SUM FROM WHAT IS NOW A LIST BEING CREATED HERE
    # note the "/" at the end of the folder name
    ets.out <- GetCUData(CU_Name = cu.do, Update = last.yr, Data = "ETS",
              cu.info.file =  cu.lookup,
              out.folder = "DATA_PROCESSING/FraserSockeyePrep/")
    efs.out <- GetCUData(CU_Name = cu.do, Update = last.yr, Data = "EFS",
                         cu.info.file =  cu.lookup,
                         out.folder = "DATA_PROCESSING/FraserSockeyePrep/")


    cu.df <- left_join(as.data.frame(ets.out),as.data.frame(efs.out[,c("Year","EFS")]),by="Year") %>%

                                                    # ************************************************************** BMAC **** will change here if SpnforTrend is to be ETS
                                                    mutate(ETS = ifelse(Year < abd.start.yr, NA, ETS)) %>%
                                                    mutate(EFS = ifelse(Year < trend.start.yr, NA, EFS)) %>%
                                                    # **************************************************************

                                                    rename(SpnForAbd_Total = ETS,	SpnForTrend_Total = EFS) %>%
                                                    mutate(SpnForAbd_Wild = SpnForAbd_Total,	SpnForTrend_Wild = SpnForTrend_Total)

    dim(cu.df)

    cu.df$CU_ID <- c(cu.id)
    cu.df$CU_Name<- cu.do
    cu.df$DU_ID <- "TBD"

    cu.df$STK_ID <- stk.id
    #cu.df$STK_NAME <- stk.name

    #cu.df$TotalER	 <- NA
   # cu.df$Recruits_Wild <-  NA

    cu.data.out <- rbind(cu.data.out,cu.df)


    #  # ADDED APRIL 2021 TO GET THE INFILLED POP-LEVEL DATA
    # pop_names <- GetCUData(CU_Name = cu.do, Update = last.yr, Data = "ETS",
    #                       cu.info.file = "DATA_LOOKUP_FILES/SoS_Data_CU_Info_ForDataPrep.csv",
    #                       out.folder = "DATA_OUT/FraserSockeyePrep/")$streams
    #
    #
    # fr.sk.bypop.cleaned <- fr.sk.bypop.cleaned %>% filter(!CU_Name  == (cu.info.sos$SK_Data_CU_Name[cu.info.sos$Conservation_Unit==cu.do]) ) %>%
    #                                              rbind(filter(fr.sk.bypop.cleaned, Pop_Name %in% pop_names))
    #
    #
    # print(pop_names)
    # print(dim(fr.sk.bypop.cleaned)[1])


} # end looping through CUs


# add rec data
sr.df <- read.csv("DATA_IN/SOURCES/Fraser Sockeye/Fraser Sockeye SR Data_2023.csv", stringsAsFactors = FALSE) %>%
            select(StkID,yr,rec) %>% # ,spn) %>%
            rename(STK_ID = StkID, Year = yr, Recruits_Total = rec) %>% #, sr_spn = spn) %>%
            mutate(Recruits_Total = 10^6 * Recruits_Total) %>% #, sr_spn = 10^6 * sr_spn) %>%
            mutate(Recruits_Wild = Recruits_Total)

cu.data.out <- left_join(cu.data.out , sr.df,by=c("STK_ID","Year"))



cu.data.out <- cbind(Species = "Sockeye",cu.data.out)
dim(cu.data.out)
head(cu.data.out )




write.csv(as.matrix(cu.data.out), "DATA_PROCESSING/Cleaned_FlatFile_ByCU_FraserSockeye.csv",row.names=FALSE)


#source("SCRIPTS1_Prep/SUB_findSockeyeSRDataforPopLookup.R")

####
# Spn diagnostic plot


# pdf("DATA_TRACKING/Sockeye_Fraser_Spn_DiagnosticPlot.pdf",onefile = TRUE, height = 8.5, width = 11)
#
# for(cu.plot in sort(unique(cu.data.out$CU_ID))){
# print(cu.plot)
# plot.sub <- dplyr::filter(cu.data.out,CU_ID == cu.plot)
# plot.sub[plot.sub == Inf] <- NA
#
# y.max <- max(select(plot.sub,SpnForAbd_Total,SpnForTrend_Total,SpnForAbd_Wild,
#                     SpnForTrend_Wild,sr_spn),na.rm=TRUE) / 1000
#
#
# plot(1:5,1:5, type="n",xlim=range(plot.sub$Year),ylim=c(0,y.max),bty="n",
#         xlab="Year", ylab = "Abundance (1000 Fish)")
#
# lines(plot.sub$Year,plot.sub$SpnForTrend_Total/1000,type="o",pch=21,col="darkblue",bg="white")
# lines(plot.sub$Year,plot.sub$SpnForAbd_Total/1000,type="o",pch=19,col="darkblue")
# lines(plot.sub$Year,plot.sub$sr_spn/1000,type="p",pch=4,col="red")
#
# legend("topleft",legend = c("SpnForTrend_Total","SpnForAbd_Total","Sockdat Spn"),
#         pch=c(21,19,4),col=c("darkblue","darkblue","red"),bty="n")
#
#
# title(main=paste0(cu.plot, " - ",unique(plot.sub$CU_Name)," (",unique(plot.sub$STK_NAME),")"))
#
#
# }
#
#
# dev.off()










