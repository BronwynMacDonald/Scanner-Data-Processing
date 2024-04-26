
library(tidyverse)

# BY POP -----------------------------------------

# read in the cleaned flat files
flat.fr.sk <- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByPop_FraserSockeye.csv",stringsAsFactors = FALSE)
flat.sbc.ck <- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByPop_SBC_Ck.csv",stringsAsFactors = FALSE)
flat.fr.co<- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByPop_FraserCoho.csv",stringsAsFactors = FALSE)
flat.fr.pk<- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByPop_FraserPink_All_Nuseds.csv",stringsAsFactors = FALSE)
flat.fr.cm<- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByPop_FraserChum_all.csv",stringsAsFactors = FALSE)
#flat.isc.cm<- read.csv("DATA_PROCESSING/Cleaned_FlatFile_byPop_ISCChum.csv",stringsAsFactors = FALSE)

# find common variables
#vars.use <- intersect(intersect(intersect(intersect(intersect(names(flat.fr.sk),names(flat.sbc.ck)),names(flat.fr.co)), names(flat.fr.pk)),
#                                                              names(flat.fr.cm)), names(flat.isc.cm))
vars.use <- intersect(intersect(intersect(intersect(names(flat.fr.sk),names(flat.sbc.ck)),names(flat.fr.co)), names(flat.fr.pk)),
                      names(flat.fr.cm))#, names(flat.isc.cm))
flat.merged <- bind_rows(list(Sk_Fraser= select(flat.fr.sk,all_of(vars.use)),Ck_SBC = select(flat.sbc.ck,all_of(vars.use)),
                              Co_Fraser = select(flat.fr.co,all_of(vars.use)), Pk_Fraser = select(flat.fr.pk,vars.use),
                              Cm_Fraser = select(flat.fr.cm,all_of(vars.use))), #Cm_ISC = select(flat.isc.cm, all_of(vars.use))),
                .id = "DataSet")
head(flat.merged)
write.csv(flat.merged,"DATA_PROCESSING/MERGED_ESC_BY_POP_SUB.csv",row.names = FALSE)



# BY CU

# read in the cleaned flat files
flat.fr.sk.cu <- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByCU_FraserSockeye.csv",stringsAsFactors = FALSE)
flat.sbc.ck.cu <- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByCU_SBC_Ck.csv",stringsAsFactors = FALSE)
flat.fr.co.cu <- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByCU_FraserCoho.csv",stringsAsFactors = FALSE)
flat.fr.pk.cu <- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByCU_FraserPink.csv",stringsAsFactors = FALSE)
flat.fr.cm.cu <- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByCU_FraserChum.csv",stringsAsFactors = FALSE)
#flat.isc.cm.cu<- read.csv("DATA_PROCESSING/Cleaned_FlatFile_byCU_ISCChum.csv",stringsAsFactors = FALSE)
flat.skeenanass.sk.cu<- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByCU_SkeenaNassSockeye.csv",stringsAsFactors = FALSE)
flat.ok.sk.cu <- read.csv("DATA_PROCESSING/Cleaned_FlatFile_ByCU_OkanaganSockeye.csv",stringsAsFactors = FALSE)


flat.skeenanass.sk.cu %>% dplyr::filter(CU_Name == "Damdochax")
flat.skeenanass.sk.cu %>% dplyr::filter(CU_Name == "Alastair")

# find common variables
# vars.use <-intersect(intersect(intersect(intersect( intersect(
#                                 intersect(names(flat.fr.sk.cu),names(flat.sbc.ck.cu)),names(flat.fr.co.cu)),
#                                 names(flat.fr.pk.cu)),names(flat.fr.cm.cu)),
#                                 names(flat.isc.cm.cu)),names(flat.ok.sk.cu))
 vars.use <-intersect(intersect(intersect(intersect(
                       intersect(names(flat.fr.sk.cu),names(flat.sbc.ck.cu)),names(flat.fr.co.cu)),
                       names(flat.fr.pk.cu)),names(flat.fr.cm.cu)),names(flat.ok.sk.cu))

#                                 names(flat.isc.cm.cu)),names(flat.ok.sk.cu))
#vars.use <-intersect(intersect(intersect(
#                     intersect(names(flat.fr.sk.cu),names(flat.sbc.ck.cu)),names(flat.fr.co.cu)),
#                     names(flat.fr.pk.cu)),names(flat.fr.cm.cu))
vars.use

flat.merged.cu <- bind_rows(list(Sk_Fraser= select(flat.fr.sk.cu,any_of(vars.use)),
                              Ck_SBC = select(flat.sbc.ck.cu,any_of(vars.use)),
                              Co_Fraser= select(flat.fr.co.cu,any_of(vars.use)),
                              Pk_Fraser = select(flat.fr.pk.cu, any_of(vars.use)),
                              Cm_Fraser = select(flat.fr.cm.cu, any_of(vars.use)),
                             #Cm_ISC = select(flat.isc.cm.cu, any_of(vars.use)),
                              Sk_SkeenaNass = select(flat.skeenanass.sk.cu, any_of(vars.use)),
                              Sk_Okanagan = select(flat.ok.sk.cu, any_of(vars.use))),
                              .id = "DataSet")

sort(unique(flat.merged.cu$CU_Name))

flat.merged.cu %>% dplyr::filter(CU_Name == "Damdochax")

sum(flat.merged.cu$CU_Name=="Osoyoos",na.rm=TRUE)

# GP ADDED March 2023: Filter out any records before CU-specific start year
start.yrs.df <-  read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE) %>%
                    select(all_of(c("CU_ID_Report","Abd_StartYr")))

start.yrs.df

unique(flat.merged.cu$CU_ID)

flat.merged.cu <- flat.merged.cu %>%
                      mutate(CU_ID_Report = gsub("_","-", CU_ID)) %>%
                      left_join(start.yrs.df, by = "CU_ID_Report") %>%
                      mutate(UseYear = Year >= Abd_StartYr) %>% dplyr::filter(UseYear)
head(flat.merged.cu)


sort(unique(flat.merged.cu$CU_Name))



sort(unique(flat.merged.cu$CU_Name))
sum(flat.merged.cu$CU_Name=="Osoyoos Lake",na.rm=TRUE)



write.csv(flat.merged.cu,"DATA_PROCESSING/MERGED_ESC_BY_CU_SUB.csv",row.names = FALSE)


