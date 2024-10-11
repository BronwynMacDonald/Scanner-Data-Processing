
# IMPORTANT: This script is called within a function in script 0_RunAll.R
# that function provides a required argument: datastages

# ALSO NOTE: OPTION TO INSTALL METRICS PACKAGES HAS BEEN MOVED THERE

library("tidyverse")
library(WSPMetrics)


# read in the merged flat file and lookup file
#cu.file <- read.csv("DATA_OUT/MERGED_FLAT_FILE_BY_CU.csv",stringsAsFactors = FALSE)
cu.file.in <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_CU_SUB.csv",stringsAsFactors = FALSE)
cu.info.main <- read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE)
cyclic.cu.bm  <- read.csv("DATA_LOOKUP_FILES/FRSK_Cycle_Rel_BMs.csv", stringsAsFactors=FALSE)

# Using Package install instead
#setwd("..")
#setwd("WSP-Metrics-Code/R")
#files.sources = list.files()
#sapply(files.sources, source)
#setwd("../..")
#setwd("SOS-Data-Processing")

# fix the CU_ID ("_" vs. "-")
cu.file.in$CU_ID <- gsub("_","-",cu.file.in$CU_ID)
cu.info.main$CU_ID <- gsub("_","-",cu.info.main$CU_ID)






cu.file <- cu.file.in %>% left_join(cu.info.main %>% select(CU_ID, Data_Stage)) %>%
                          filter(Data_Stage %in% datastage)

#library(R2jags)

cu.list <- unique(cu.file[,c("Species","CU_Name","CU_ID")])
cu.list

# WARNING: THIS MUST INCLUDE AT LEAST 4 YEARS (so that  all 4 cycle-specific BM calc get used to populate outputs below; crashes otherwise!)
retro.start.use <- 1995


# clear the output
if(exists("metrics.cu.out")){rm(metrics.cu.out)}
if(exists("metrics.percchange.comp")){rm(metrics.percchange.comp)}


start.time <- proc.time()

#for(i in 55:dim(cu.list)[1]){
for(i in 1:dim(cu.list)[1]){
  #for(i in 29){
  #for(i in 55:59){

  print("----------------------------")
  print(i)
  cu.id <- cu.list[i,"CU_ID"]
  cu.name <- cu.list[i,"CU_Name"]
  cu.species <- cu.list[i,"Species"]



  print(cu.name)
  print(cu.id)



  cu.lookup.sub<- dplyr::filter(cu.info.main,CU_ID == cu.id)
  cu.lookup.sub

if( dim(cu.lookup.sub)[1]==1){ # do only if have exactly 1 matching CU_ID in the lookup file

  cu.avggen <- cu.lookup.sub$Avg_Gen
  cu.avggen

  # new fn version arguments
  #   slope.specs = list(num.gen = 3, extra.yrs = 0, filter.sides = 1, slope.smooth = TRUE,
  #                      log.transform = TRUE, out.exp = TRUE, na.rm = FALSE)

  cu.slope.specs <- list(num.gen = cu.lookup.sub$TrendNumGen,
                         extra.yrs =cu.lookup.sub$TrendExtraYears ,
                         filter.sides=1,  # backward looking
                         slope.smooth = cu.lookup.sub$TrendSmooth ,
                         log.transform = cu.lookup.sub$TrendLog,
                         out.exp = TRUE,  # exponentiate the log-smoothed series?
                         na.rm=FALSE)
  cu.slope.specs
  # new fn version arguments
  #   list(avg.type = "geomean", recent.excl = FALSE, lt.smooth = TRUE,
  #         rel.avg.type = "regular", min.lt.yrs = 20, min.perc.yrs = 20)

  cu.avg.specs <- list(avg.type =cu.lookup.sub$AvgType ,
                       recent.excl= cu.lookup.sub$AvgRecentExcl,  # use all for the long-term avg?
                       lt.smooth=cu.lookup.sub$AvgSmooth,
                       rel.avg.type = cu.lookup.sub$RelAbd_AvgData,
                       min.lt.yrs = cu.lookup.sub$LongTrendMinYears,
                       min.perc.yrs = cu.lookup.sub$LongTrendMinYears
                       # USE SAME SETTING AS LT TREND FOR NOW -> DISCUSS
                       # https://github.com/SOLV-Code/SOS-Data-Processing/issues/52
                       )
  cu.avg.specs
  # new fn version arguments
  #  metric.bm = list(RelAbd = c(NA, NA), AbsAbd = c(1000, 10000), LongTrend = c(0.5,
  #               0.75), PercChange = c(-25, -15), ProbDeclBelowLBM = c(NA, NA), Percentile = c(0.25,0.5))

  cu.bm <- list(RelAbd = cu.lookup.sub[,c("RelAbd_LBM","RelAbd_UBM")],
                AbsAbd = cu.lookup.sub[,c("AbsAbd_LBM","AbsAbd_UBM")],
                LongTrend = cu.lookup.sub[,c("LongTrend_LBM","LongTrend_UBM")],
                PercChange = cu.lookup.sub[,c("PercChange_LBM","PercChange_UBM")],
                ProbDeclBelowLBM = cu.lookup.sub[,c("ProbDeclBelowLBM_LBM","ProbDeclBelowLBM_UBM")],
                Percentile = c(0.25,0.5) #using hardwired for now-> DISCUSS
                # https://github.com/SOLV-Code/SOS-Data-Processing/issues/52
                )
  cu.bm

  data.sub <- cu.file %>% filter(CU_ID == cu.id)

  data.sub

  # GP ADDED March 2023: Now that data series were trimmed, need to add a bunch of NA years before the retro start (else the MCMC slope calc crashes)
  if(retro.start.use-10 < min(data.sub$Year)){
    yrs.add <- data.frame(CU_ID = cu.id, Year = (retro.start.use-10):min(data.sub$Year)-1)
    yrs.add

    data.sub <- bind_rows(data.sub, yrs.add) %>% arrange(Year)
    data.sub
  }




  if(cu.lookup.sub$Cyclic==TRUE) cyclic.bm.sub <- cyclic.cu.bm[cyclic.cu.bm$CU_ID==cu.id,] else(cyclic.bm.sub=NA)

  for(series.do in c("SpnForAbd_Wild","SpnForTrend_Wild" )){  #"SpnForAbd_Total","SpnForTrend_Total",

  print(series.do)



  cu.series <- data.sub[,series.do]
  cu.series[!is.finite(cu.series)] <- NA
  cu.yrs <- data.sub[,"Year"]

  cu.series
  cu.yrs




  if(sum(!is.na(cu.series))>5 &  sum(cu.series>0,na.rm=TRUE)>5){

  library(WSPMetrics)

  metrics.tmp <- WSPMetrics::calcMetrics(series.in = cu.series,
                              yrs.in = cu.yrs,
                              gen.in = cu.avggen,
                              stk.label = cu.name,
                              species.label = cu.species,
                              series.label = series.do,
                              slope.specs = cu.slope.specs,
                              avg.specs = cu.avg.specs,
                              metric.bm =  cu.bm,
                              retro.start = retro.start.use,
                              #cyclic = cu.lookup.sub$Cyclic,   #now handled below
                              #cyc.dom = cu.lookup.sub$Cyc_Dom,
                              #cyclic.bm.sub= cyclic.bm.sub,
                              tracing = TRUE)

  #calcPercChangeMCMC(cu.series)
  #cu.series




  if("WSPMetrics" %in% (.packages())){detach("package:WSPMetrics") }


    metrics.tmp <- cbind(CU_ID = cu.id, metrics.tmp)

  # ********************************* BMac changes ***********************************


#	CHECK THAT LOOKUPS AND INPUT VALUES ARE FED IN PROPERLY




	if(cu.lookup.sub$Cyclic==TRUE){

    print(paste("starting  BMAC changes for", cu.name, series.do))

        # Set to NA first (AbsAbd BMs don't need to be changed)
        metrics.tmp[metrics.tmp$Metric == "RelAbd", c("Value","LBM","UBM","Status")] <- NA
       # metrics.tmp[metrics.tmp$Metric == "AbsAbd", c("Value","Status")] <- NA   # Changed Nov 9 2021 as we decided to use the geom average for
                                                                                  # cyclic CUs, which is what is used for the regular CUs

        # Dominant cycle Benchmarks for RelAbd
       # cycs <-rep(seq(from=1,to=cu.avggen), length=length(cu.yrs))   CHANGES MAY 28 2021 to REFER TO THE CYCLE LINE INSTEAD OF YEAR SO WORKS ON RETRO
        cycs <- cu.lookup.sub$Cyc_Dom
        #cyc.yrs <-  cu.yrs[(cycs == cycs[which(cu.yrs==cu.lookup.sub$Cyc_Dom)] & cu.yrs >= retro.start.use)]
        all.cyc.yrs <- seq(from=cyclic.bm.sub$Cycle[cyclic.bm.sub$cyc == cycs], to=last(cu.yrs), by=cu.avggen)
        cyc.yrs <- all.cyc.yrs[all.cyc.yrs >= retro.start.use]

        # puts dominant BM into all rows
        metrics.tmp[metrics.tmp$Metric == "RelAbd", c("LBM","UBM")] <- cyclic.bm.sub[(cyclic.bm.sub$cyc == cycs) ,c("RelAbd_LBM","RelAbd_UBM")]


        # Most recent Dominant cycle Abundance for RelAbd metric Value
        metrics.tmp[metrics.tmp$Year %in% cyc.yrs & metrics.tmp$Metric == "RelAbd", "Value"] <- cu.series[cu.yrs%in%cyc.yrs]
        sub <- metrics.tmp %>% filter(Metric == "RelAbd") %>%
                               fill(Value, .direction="down")
        # insert early years if not filled in
        #first.cycyr <- last(cu.yrs[(cycs == cycs[which(cu.yrs==cu.lookup.sub$Cyc_Dom)] & cu.yrs < retro.start.use)])
        first.cycyr <- last(all.cyc.yrs[all.cyc.yrs < retro.start.use])

        if(is.na(first(sub$Value))) sub$Value[sub$Year<first(cyc.yrs)] <-cu.series[cu.yrs==first.cycyr]

        first.abund <- cu.series[cu.yrs==first.cycyr]
        metrics.tmp <- rbind(sub, filter(metrics.tmp, Metric != "RelAbd"))

        # AbsAbd   *********************** BMAC replaced this Oct 22 2021 to DOMINANT cycle abundance only for COSEWIC criteria BUT NO LONGER USING THIS SEE ABOVE 09/11/2021
        #metrics.tmp$Value[metrics.tmp$Metric == "AbsAbd"] <-  sub$Value  # cu.series[cu.yrs>=retro.start.use]


        # Insert Statuses
        metrics.tmp <- metrics.tmp %>%
                               mutate(Status = ifelse(Value <= LBM, "Red", ifelse(Value > UBM, "Green", "Amber")))


        print(paste("ending  BMAC changes for", cu.name, series.do))

  } # end cyclic



  # ********************************** End BMAC changes ******************************

    if(exists("metrics.cu.out")){metrics.cu.out <- rbind(metrics.cu.out,metrics.tmp)  }
    if(!exists("metrics.cu.out")){metrics.cu.out <- metrics.tmp }



    if(cu.slope.specs$slope.smooth){
      #log transform, smooth, and convert back

      library(WSPMetrics)

      trend.series <- WSPMetrics::smoothSeries(vec.in = cu.series,gen = cu.avggen,
                                   filter.sides=cu.slope.specs$filter.sides,
                                   log.transform = cu.slope.specs$log.transform,
                                   out.exp = cu.slope.specs$out.exp,
                                   na.rm=cu.slope.specs$na.rm)

    }


    if(!cu.slope.specs$slope.smooth){trend.series <- cu.series}

    if(FALSE){ # for debug only
      num.yrs.use <- (cu.avggen *cu.slope.specs$num.gen) + cu.slope.specs$extra.yrs
      num.yrs.use

    test.out  <- WSPMetrics::calcPercChangeMCMC(vec.in = log(tail(trend.series,num.yrs.use)),
                                   method = "jags",
                                   model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                                   perc.change.bm = -25,
                                   na.skip = FALSE,
                                   out.type = "short",
                                   mcmc.plots = FALSE,
                                   convergence.check = FALSE, # ??Conv check crashes on ts() ??? -> change to Rhat check
                                   logged = TRUE
                                  )

    test.out
    }




    # if("WSPMetrics" %in% (.packages())){detach("package:WSPMetrics")}
    # library(MetricsCOSEWIC)



   # for(yr.do in max(retro.start.use,min(cu.yrs)+10):max(cu.yrs)){

    # NOTE: this function has a built-in log-transform and back-conversion of the perc change
    # tmp <- MetricsCOSEWIC:: comparePercChange(
    #   du.label = cu.name,
    #   du.df = data.frame(Year = cu.yrs, Abd = trend.series),
    #   yrs.window = cu.avggen *3 ,
    #   calc.yr = yr.do,
    #   samples.out = FALSE,
    #   plot.pattern = TRUE,
    #   plot.posteriors = TRUE,
    #   plot.boxes = TRUE
    # )




    # #names(tmp )
    # compare.tmp <- cbind(Species = cu.species, Stock = cu.name, Label = series.do,
    #                      Year = yr.do, as.data.frame(tmp$Summary) %>% rownames_to_column("Var"))
    #
    # if(exists("metrics.percchange.comp")){metrics.percchange.comp <- rbind(metrics.percchange.comp ,compare.tmp)  }
    # if(!exists("metrics.percchange.comp")){metrics.percchange.comp <- compare.tmp }
    #
    # } # end looping through yrs for the retro


    # if("MetricsCOSEWIC" %in% (.packages())){detach("package:MetricsCOSEWIC") }



  } # end if have at least 5 obs
  } # end looping through series
  } # end if have  matching CU_ID in the lookup file

  } # end looping through CUs

print(paste("last row done =",i))

head(metrics.cu.out)



#------------------------------------------------------------------------------------
# clear out metrics that are not meaningful (e.g. abs BM on data for trends)
# (already skipping the _Total series above, just doing _Wild)

# adapted from BMac code
#filter( !(grepl("Abd", Label) & (grepl("LongTrend", Metric) | grepl("Perc", Metric) |  grepl("Decl", Metric)) )) %>%
#filter( !(grepl("Trend", Label) & (grepl("RelBM", Metric) | grepl("AbsBM", Metric)) ))
metrics.cu.out.cleaned  <-  rbind(
        metrics.cu.out %>% dplyr::filter(grepl("Abd", Label) & (grepl("RelAbd", Metric) | grepl("AbsAbd", Metric))),
        metrics.cu.out %>% dplyr::filter(grepl("Trend", Label) & !(grepl("RelAbd", Metric) | grepl("AbsAbd", Metric)))
                                             ) %>%
        left_join(cu.info.main %>% select(CU_ID,DataQualkIdx) %>% rename(Data_Type = DataQualkIdx) , by ="CU_ID")




#####################################
# METRIC USABILITY ADJUSTMENTS OLD

# any where data type is rel idx: change absAbd and relAbd to NA
#rel.idx.fix <-  grepl("Abd", metrics.cu.out.cleaned$Label) & grepl("Rel_Idx", metrics.cu.out.cleaned$Data_Type)
#metrics.cu.out.cleaned[ rel.idx.fix   ,c("Value","Status")] <- c(NA, NA)

# MANUAL PATCH TO DEAL WITH METRIC USABILITY (THINK THIS WAS DONE MANUALLY BEFORE)
# NEED to SET UP TO READ FROM A SPEC FILE (ALREADY HAPPENING SOMEWHERE BUT MISSING A FEW

#usability.fix.idx <- metrics.cu.out.cleaned$CU_ID %in% paste0("CK-", c("11","16","18","82"))
#metrics.cu.out.cleaned[ usability.fix.idx   ,c("Value","Status")] <- c(NA, NA)

#####################################
# METRIC USABILITY ADJUSTMENTS NEW (GP Revised 2023-05-31)
# linking it directly to cu info lookup file
# added a new column there "AbsAbdMetric" for future cases where want AbdAbd but not RelAbd (as per BMac discussion)
# for now settings in AbdMetric and AbsAbdMetric are the same

not.abd.list <- cu.info.main$CU_ID[!cu.info.main$AbdMetric]
not.absabd.list <- cu.info.main$CU_ID[!cu.info.main$AbsAbdMetric]
not.shorttrend.list <- cu.info.main$CU_ID[!cu.info.main$ShortTrendMetric]
not.longtrend.list <- cu.info.main$CU_ID[!cu.info.main$LongTrendMetric]

abd.fix.idx <- grepl("RelAbd", metrics.cu.out.cleaned$Metric) & unlist(metrics.cu.out.cleaned$CU_ID) %in%  not.abd.list
absabd.fix.idx <- grepl("AbsAbd", metrics.cu.out.cleaned$Metric) & unlist(metrics.cu.out.cleaned$CU_ID) %in%  not.absabd.list
shorttrend.fix.idx <- grepl("ShortTrend", metrics.cu.out.cleaned$Metric) & unlist(metrics.cu.out.cleaned$CU_ID) %in%  not.shorttrend.list
longtrend.fix.idx <- grepl("LongTrend", metrics.cu.out.cleaned$Metric) & unlist(metrics.cu.out.cleaned$CU_ID) %in%  not.longtrend.list

if(!dir.exists("DATA_PROCESSING/FILTERED_DATA")){dir.create("DATA_PROCESSING/FILTERED_DATA")}

write.csv(metrics.cu.out.cleaned,"DATA_PROCESSING/FILTERED_DATA/METRICS_FILE_BY_CU_PRE_CLEAN.csv",row.names=FALSE)


# GP New 2024-05-28: Extract Gen Avg  so can merge back in later (get deleted below of AbsAbd/RelAbd metrics are turned off)
gen.avg.used.df <- metrics.cu.out.cleaned %>% dplyr::filter(Metric == "RelAbd") %>% select(CU_ID, Year,Value)
write.csv(gen.avg.used.df, paste0("DATA_PROCESSING/FILTERED_DATA/GenerationalAvg_Values_",paste(datastage, collapse=""),".csv"),row.names=FALSE)




metrics.cu.out.cleaned[abd.fix.idx,c("Value","Status")] <- c(NA, NA)
metrics.cu.out.cleaned[absabd.fix.idx,c("Value","Status")] <- c(NA, NA)
metrics.cu.out.cleaned[shorttrend.fix.idx,c("Value","Status")] <- c(NA, NA)
metrics.cu.out.cleaned[longtrend.fix.idx,c("Value","Status")] <- c(NA, NA)


write.csv(metrics.cu.out.cleaned, paste0("DATA_PROCESSING/FILTERED_DATA/METRICS_FILE_BY_CU_SUB_",paste(datastage, collapse=""),".csv"), row.names=FALSE)
#write.csv(metrics.percchange.comp,"DATA_OUT/PercChange_Comparison_BY_CU.csv",row.names=FALSE)

print("Running of calcMetrics() took:")
print( proc.time() - start.time)



#library("tidyverse")
#tmp <- metrics.cu.out.cleaned %>% dplyr::filter(Stock == "Taseko_ES")

# clear the output (so it doesn't stick around in memory if you run this script from within the function in script 0)
#if(exists("metrics.cu.out")){rm(metrics.cu.out)}
#if(exists("metrics.percchange.comp")){rm(metrics.percchange.comp)}
# This didn't work, because it deletes this even if it's called within the function



