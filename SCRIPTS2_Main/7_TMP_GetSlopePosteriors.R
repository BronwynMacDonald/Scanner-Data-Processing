# this is a temporary script to generate a detailed output of 
# the posterior distribution of the slope calculation
# For now, just doing last year of Ok Ck, for Brooke's FSAR work

# This uses bits of Script 2


# General setup


library("tidyverse")
library(devtools) # Load the devtools package.
install_github("Pacific-salmon-assess/WSP-Metrics-Pkg", ref = "DEV",  dependencies = TRUE, build_vignettes = FALSE)
library(WSPMetrics)



cu.file <- read.csv("DATA_PROCESSING/MERGED_ESC_BY_CU_SUB.csv",stringsAsFactors = FALSE)
cu.info.main <- read.csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv",stringsAsFactors = FALSE)
# fix the CU_ID ("_" vs. "-")
cu.file$CU_ID <- gsub("_","-",cu.file$CU_ID)
cu.info.main$CU_ID <- gsub("_","-",cu.info.main$CU_ID)

###########
# Set up Okanagan Chinook specific

cu.id <- "CK-01"


cu.lookup.sub<- dplyr::filter(cu.info.main,CU_ID == cu.id)
cu.lookup.sub

cu.avggen <- cu.lookup.sub$Avg_Gen
cu.avggen

cu.slope.specs <- list(num.gen = cu.lookup.sub$TrendNumGen,
                       extra.yrs =cu.lookup.sub$TrendExtraYears ,
                       filter.sides=1,  # backward looking
                       slope.smooth = cu.lookup.sub$TrendSmooth ,
                       log.transform = cu.lookup.sub$TrendLog,
                       out.exp = TRUE,  # exponentiate the log-smoothed series?
                       na.rm=FALSE)
cu.slope.specs

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



# GP ADDED March 2023: Now that data series were trimmed, need to add a bunch of NA years before the retro start (else the MCMC slope calc crashes)
if(retro.start.use-10 < min(data.sub$Year)){
  yrs.add <- data.frame(CU_ID = cu.id, Year = (retro.start.use-10):min(data.sub$Year)-1)
  yrs.add
  
  data.sub <- bind_rows(data.sub, yrs.add) %>% arrange(Year)
  data.sub
}
data.sub


# DO RETROSPECTIVE STARTING 2017 (first year with 12 obs)

if(exists("retro.slopes.out")){rm(retro.slopes.out)}

slope.samples <- list(Label = "OK CK")  



for(yr.do in 2017:2023){
  
data.retro <- data.sub %>% dplyr::filter(Year <= yr.do)
data.retro


cu.series <- data.retro[,"SpnForTrend_Wild"]
cu.series[!is.finite(cu.series)] <- NA
cu.yrs <- data.retro[,"Year"]

cu.series
cu.yrs

cu.slope.specs$slope.smooth


if(cu.slope.specs$slope.smooth){

trend.series <- WSPMetrics::smoothSeries(vec.in = cu.series,gen = cu.avggen,
                                         filter.sides=cu.slope.specs$filter.sides,
                                         log.transform = cu.slope.specs$log.transform,
                                         out.exp = cu.slope.specs$out.exp,
                                         na.rm=cu.slope.specs$na.rm)
}


if(!cu.slope.specs$slope.smooth){trend.series <- cu.series}


trend.series

num.yrs.use <- (cu.avggen *cu.slope.specs$num.gen) + cu.slope.specs$extra.yrs
num.yrs.use

perc.change.out.full  <- WSPMetrics::calcPercChangeMCMC(vec.in = log(tail(trend.series,num.yrs.use)),
                                              method = "jags",
                                              model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                                              perc.change.bm = -25,
                                              na.skip = FALSE,
                                              out.type = "long", # THIS GIVES THE FULL OUTPUT!
                                              mcmc.plots = FALSE,
                                              convergence.check = FALSE, # ??Conv check crashes on ts() ??? -> change to Rhat check
                                              logged = TRUE
      )
  

names(perc.change.out.full)

quants.tmp <- quantile(perc.change.out.full$samples %>% 
                                       select(Perc_Change) %>% unlist(),probs=seq(0.05,0.95, by=0.05)) 


#names(quants.tmp) <- c("Value")  
quants.tmp <- quants.tmp %>% t() %>% as.data.frame()
quants.tmp 


if(exists("retro.slopes.out")){
  
  retro.slopes.out <- rbind(retro.slopes.out, data.frame(
    Year =  yr.do,
    perc.change = perc.change.out.full$pchange,
    prob.decl = perc.change.out.full$probdecl) %>%
      cbind(quants.tmp)
    
    

  )
}


if(!exists("retro.slopes.out")){

  retro.slopes.out <- data.frame(
     Year =  yr.do,
     perc.change = perc.change.out.full$pchange,
     prob.decl = perc.change.out.full$probdecl) %>%
    cbind(quants.tmp)

 


}




slope.samples[[as.character(yr.do)]] <- list(perc.change.out.full$samples)
  
  
  




} # end looping through years



retro.slopes.out
names(slope.samples)


write_csv(retro.slopes.out,paste0("DATA_TRACKING/Slope_Retrospective_", cu.id, ".csv"))

saveRDS(slope.samples,file=paste0("DATA_TRACKING/Slope_Retrospective_FullMCMC_", cu.id, ".RDS"))
