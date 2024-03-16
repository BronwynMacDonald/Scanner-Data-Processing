#############################################################
#  Generate summaries
# (basically some key bits from the case study paper, so we can always have the
#  the latest outputs  here, without changing anything in the tech report, and without
# having to copy files back and forth)

# NOTE: IMAGE OUTPUTS ARE NOT TRACKED ON GIT, BUT
# THE REQUIRED INPUT FILES ARE. JUST RUN THIS SCRIPT TO
# GET THE PLOTS.


####################################################################################
# GENERATE THE DASHBOARDS


library(tidyverse)
library(plotrix)


if(!dir.exists("OUTPUT/MetricsAndStatus")){dir.create("OUTPUT/MetricsAndStatus")}


#############################################################
# DASHBOARD SETTINGS
###########################################################

bm.areas <- TRUE


# Michael Arbeider's color scheme for IF Coho RPA:
# from colorbrewer2, 5 data classes, diverging, colorblind safe (2nd option):
#http://colorbrewer2.org/#type=diverging&scheme=PiYG&n=5

alpha.use <- 0.5
green.use <- rgb(184/255,225/255,134/255,alpha=alpha.use)
red.use <- rgb(241/255,182/255,218/255,alpha=alpha.use)
amber.use <- rgb(255/255,255/255,191/255,alpha=alpha.use)

alpha.use.timeline <- 1
green.use.timeline <- rgb(184/255,225/255,134/255,alpha=alpha.use.timeline)
red.use.timeline <- rgb(241/255,182/255,218/255,alpha=alpha.use.timeline)
amber.use.timeline <- rgb(255/255,255/255,191/255,alpha=alpha.use.timeline)





retro.yrs <- 1995:2021


#############################################################
# READ IN DATA
###########################################################


retro.summary.tbl <- read_csv("DATA_OUT/Retro_Synoptic_Details.csv")

# CU_IDs are now correct format and match NUSEDs. Skeena/Nass SK do no have CU_IDs
metrics.details <- read_csv("DATA_OUT/METRICS_FILE_BY_CU_SCANNER.csv") #%>%
  #left_join(cu.info %>% select(CU_ID = CU_ID_Alt2_CULookup, New_CU_ID = CU_ID), by="CU_ID" )
  #dplyr::mutate(CU_ID = gsub("_","-",CU_ID))

# CU_IDs in the lookup file do not match those in the metrics output or data anymore because those have been corrected. They match with
# CU_ID_Alt2_CULookup in the lookup file
cu.info <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv") %>%
  dplyr::mutate(CU_ID = gsub("_","-",CU_ID))

data.raw <- read.csv("DATA_OUT/MERGED_FLAT_FILE_BY_CU_SCANNER.csv",stringsAsFactors = FALSE) %>%
  #mutate(CU_ID = gsub("_","-",CU_ID)) %>%
  dplyr::filter(!is.na(CU_Name)) %>%
  left_join(cu.info %>% select(CU_Name,Group), by="CU_Name" )
head(data.raw)

sort(unique(data.raw$Group))

# CU_IDs in this file are still incorrect and match those in the lookup file
retro.values <- read.csv("DATA_OUT/Retrospective_Metrics_Values.csv",stringsAsFactors = FALSE) %>%
  left_join(cu.info %>% select("CU_ID",Group), by="CU_ID" )
names(retro.values)
sort(unique(retro.values$Group))

retro.status <- read.csv("DATA_OUT/Retrospective_Metrics_Status.csv",stringsAsFactors = FALSE)  %>%
  left_join(cu.info %>% select("CU_ID",Group), by="CU_ID" )



head(retro.status)
head(retro.values)




#############################################################
# SUBSET THE DATA
###########################################################


cu.list <-  cu.info %>%
  # dplyr::filter(Area %in% c("Nass","Skeena"))   %>%
  #select(CU_ID)
  select(CU_ID_Alt2_CULookup)
cu.list <- sort(intersect(unlist(cu.list), unique(data.raw$CU_ID)))
cu.list




for(cu.plot in cu.list){
#  for(cu.plot in "CK-04"){
  #"SkeenaNass-2"  # Meziadin

  print("_______________")
  print(cu.plot)

  cu.info.sub <- cu.info %>% dplyr::filter(CU_ID_Alt2_CULookup == cu.plot)
  cu.alt.id <- cu.info.sub$CU_ID

  #print(cu.info$CU_ID)


  cyclic.check <- cu.info.sub$Cyclic


  data.sub <- data.raw  %>% dplyr::filter(CU_ID == cu.plot)
  head(data.sub )

  retro.values.sub <-  retro.values %>% dplyr::filter(CU_ID == cu.alt.id)
  head(retro.values.sub )


  retro.summary.sub <- retro.summary.tbl %>% dplyr::filter(CU_ID == cu.alt.id)

  metrics.details.sub <- metrics.details %>% dplyr::filter(CU_ID == cu.plot)

  main.yrs.plot <- c(1960,2025)

  retro.yrs.plot <- range(retro.values$Year,2025)
  retro.yrs.plot


  # Add quick fix for Chum data which is total not wild
  if(cu.plot ==  "CM-02") data.sub$Escapement_Wild = data.sub$Escapement_Total

  ###

  #START PLOT

  if(sum(!is.na(data.sub$Escapement_Wild))>0){



    data.type <- cu.info.sub$DataQualkIdx
    data.type

        png(filename = paste0("OUTPUT/MetricsAndStatus/Dashboard_",cu.info.sub$Region,"_",gsub(" ","",cu.info.sub$Group),"_",cu.info.sub$CU_Acro,"_",cu.info.sub$CU_ID,".png"),
            width = 480*4.5, height = 480*4.8, units = "px", pointsize = 14*2.3, bg = "white",  res = NA)

        layout(mat=matrix(c(1,2,3,4,5,5),ncol=2,byrow=TRUE),heights = c(1,1,1.1))
        #layout.show(6)
        mai.vals <- c(1,2,2,2)



      #############################################################
      # PANEL 1: MAIN TIME SERIES
      ###########################################################

      #plot(1:5,1:5,main="Main retrospective time series plot")


      cu.abd <- data.sub %>% select(Year,Escapement_Wild)

      yrs.idx <- cu.abd$Year >= main.yrs.plot[1] & cu.abd$Year <= main.yrs.plot[2]


      yrs.src <- data.frame(Year=min(cu.abd$Year):max(cu.abd$Year))
      cu.abd <- left_join(yrs.src,cu.abd ,by="Year")
      head(cu.abd)

      cu.avggen <- cu.info.sub %>% select(Avg_Gen) %>% unlist()
      print(cu.avggen )

      # calculate running geomean
      gm.in <- log(cu.abd[,2])
      gm.out <- exp(stats::filter(gm.in,rep(1/cu.avggen,times =cu.avggen),sides = 1))
      gm.out

      lbm.plot <- unique(metrics.details.sub %>% dplyr::filter(Metric == "RelLBM") %>% select(LBM))
      ubm.plot <- unique(metrics.details.sub %>% dplyr::filter(Metric == "RelUBM") %>% select(UBM))
      lbm.plot
      ubm.plot
      

      ylim <- c(0, max(unlist(cu.abd[yrs.idx,2]),unlist(lbm.plot), unlist(ubm.plot),na.rm=TRUE))
      ylim

      axis.scale <- 1
      axis.scale.label <- ""

      if(ylim[2] >= 10^3 & ylim[2] < 10^4){ axis.scale <- 100; axis.scale.label <- "(100s)"   }
      if(ylim[2] >= 10^4 & ylim[2] < 10^6){ axis.scale <- 1000; axis.scale.label <- "(1000s)"   }
      if(ylim[2] >= 10^6 ){axis.scale <- 10^6; axis.scale.label <- "(Mill)"   }


      y.label.use <- paste("Wild Spn",axis.scale.label)
      y.label.use

      par(mai=mai.vals)

      plot(cu.abd$Year,cu.abd[,2]/axis.scale,col="darkblue",pch=19, lwd=3, bty="n",
           xlim = main.yrs.plot, cex=1.3, ylim = ylim/axis.scale,
           xlab = "Year", ylab = y.label.use,type="o",axes = FALSE,cex.lab=1.4
      )

      axis(1,at = seq(main.yrs.plot[1],main.yrs.plot[2],by=5),  cex.axis=1.5)
      axis(2,las=1,  cex.axis=1.5)

      # Plot BM lines
      # TWEAKED 2024-03-16: Plot lines if main plot setting bm.areas is FALSE
      # OR if have BM but area experts want to show them only for context, different from a metric
      # For some Fraser cases where AbdMetric = FALSE but values are provided for
      # variables RelAbd_LBM	and RelAbd_UBM
      if(!bm.areas |  !cu.info.sub$AbdMetric ){  
        abline(h=lbm.plot/axis.scale,col="firebrick1",lwd=3,lty=2)
        text(par("usr")[2],lbm.plot/axis.scale,"Lower\n(Ref Only)", 
             adj=c(0.5,0.5),col = "firebrick1",cex=1.2,xpd=NA)
        abline(h=ubm.plot/axis.scale,col="green",lwd=3,lty=2)
        text(par("usr")[2],ubm.plot/axis.scale,"Upper\n(Ref Only)", 
        adj=c(0.5,0.5),col = "green",cex=1.2,xpd=NA)
      }

      
      # TWEAKED 2024-03-16: as per above, only show areas if plot setting b,.areas = TRUE
      # and AbdMetric = TRUE tp indicating to use Rel Abd metric for rapid status
      if(bm.areas & cu.info.sub$AbdMetric){
        rect(par("usr")[1],lbm.plot/axis.scale, par("usr")[2],ubm.plot/axis.scale, col=amber.use,
             border = amber.use)
        rect(par("usr")[1],ubm.plot/axis.scale, par("usr")[2],par("usr")[4], col=green.use,
             border = green.use)
        rect(par("usr")[1],lbm.plot/axis.scale, par("usr")[2],par("usr")[3], col=red.use,
             border = red.use)
      }

      abline(v=1995,col="darkgrey",lty=2,lwd=4)



      # replot the series and gen avg
      lines(cu.abd$Year,cu.abd[,2]/axis.scale,col="darkblue",pch=19, lwd=3,
            cex=1.3, type="o")

      if(!cyclic.check){
        lines(cu.abd$Year,gm.out/axis.scale,type="o",col="red",lwd=4,pch=15,cex=0.8)
      }


      if(cyclic.check){
        dom.idx <-   cu.abd$Year %in% seq(cu.info.sub$Cyc_Dom_Year, max(cu.abd$Year), by =4)
        lines(cu.abd$Year[dom.idx],cu.abd[dom.idx,2]/axis.scale,col="darkblue",pch=21, bg="red",
              lwd=3,  cex=1.6, type="p")
      }


      title(main ="Relative Abundance Metric", cex.main = 1.8,col.main="darkblue",line=1)


      title(main =paste0(cu.info.sub$CU_Name," (",cu.info.sub$Group,", Data=",data.type,")"), cex.main = 2.2,col.main="darkblue",line=-2,outer=TRUE)



      #############################################################
      # PANEL 2: Log plot of full time series
      ###########################################################



      log.ticks <- log(10^c(0:8))
      log.ticks
      log.labels <- c(1,10,100,"1k","10k","100k","1M","10M","100M")

     log.yrs.plot <- c(min(cu.abd$Year),main.yrs.plot[2])

      zero.idx <- cu.abd[,2] == 0
      zero.idx[is.na(zero.idx)] <- FALSE
      cu.abd[zero.idx,2] <-0.9

      ylim.use <- range(0.9, log(10^4),log(cu.abd[,2]),na.rm=TRUE)
      ylim.use

      ticks.use <- log.ticks <= ylim.use[2]

      if(data.type == "Abs_Abd"){lbm.plot <- log(10^3); ubm.plot <- log(10^4); title.label = "Absolute Abundance Metric (Log Scale)"}
      if(data.type == "Rel_Idx"){lbm.plot <- NA; ubm.plot <- NA; title.label = "Relative Index of Abundance (Log Scale)"}

      par(mai=mai.vals)


      plot(cu.abd[,"Year"],log(cu.abd[,2]),col="darkblue",pch=19, lwd=3, bty="n",
           xlim = log.yrs.plot, cex=1.3, ylim = ylim.use,
           xlab = "Year", ylab = "Log (Wild Spn)",type="o",axes = FALSE,cex.lab=1.4
      )




      title(main = title.label, cex.main = 1.8,col.main="darkblue",line=1)

      if(!bm.areas & cu.info.sub$AbsAbdMetric){
        abline(h=lbm.plot,col="firebrick1",lwd=3,lty=2)
        text(par("usr")[2],lbm.plot,"Lower", adj=c(1,0.5),col = "firebrick1",cex=1.2)
        abline(h=ubm.plot,col="green",lwd=3,lty=2)
        text(par("usr")[2],ubm.plot,"Upper", adj=c(1,0.5),col = "green",cex=1.2)
      }

      if(bm.areas & cu.info.sub$AbsAbdMetric){
        rect(par("usr")[1],lbm.plot, par("usr")[2],ubm.plot, col=amber.use,
             border = amber.use)
        rect(par("usr")[1],ubm.plot, par("usr")[2],par("usr")[4], col=green.use,
             border = green.use)
        rect(par("usr")[1],lbm.plot, par("usr")[2],par("usr")[3], col=red.use,
             border = red.use)
      }

      abline(v=1995,col="darkgrey",lty=2,lwd=4)

      axis(1,  cex.axis=1.5)
      axis(2,at = log.ticks[ticks.use],labels = log.labels[ticks.use] ,las=1,  cex.axis=1.5)

      lines(cu.abd$Year,log(cu.abd[,2]),col="darkblue",pch=19, lwd=3, cex=1.3,type="o")

      if(!cyclic.check){lines(cu.abd$Year,log(gm.out),type="o",col="red",lwd=4,pch=15,cex=0.8)}


      if(cyclic.check){
        dom.idx <-   cu.abd$Year %in% seq(cu.info.sub$Cyc_Dom_Year, max(cu.abd$Year), by =4)
        lines(cu.abd$Year[dom.idx],log(cu.abd[dom.idx,2]),col="darkblue",pch=21, bg="red",
              lwd=3,  cex=1.6, type="p")
      }


      #############################################################
      # PANEL 3: LONG-TERM TREND
      ###########################################################


      ylim.use <- range(0,2,retro.values.sub$LongTrend/100,na.rm=TRUE)

      par(mai=mai.vals)

      plot(retro.values.sub$Year,retro.values.sub$LongTrend/100,col="darkblue",pch=19, lwd=3, bty="n",
           xlim = retro.yrs.plot, cex=1.3, ylim = ylim.use,
           xlab = "Year", ylab = "Ratio(Current/Long-term)",type="o",axes = FALSE,cex.lab=1.4
      )

      axis(1,cex.axis=1.4)
      axis(2,cex.axis=1.4,las=1)

      title(main ="Long-term Trend Metric", cex.main = 1.8,col.main="darkblue",line=1)

      if(!bm.areas){
        abline(h=cu.info.sub$LongTrend_LBM ,col="firebrick1",lwd=3,lty=2)
        text(par("usr")[2],cu.info.sub$LongTrend_LBM,"Lower",adj=1,cex=1.4,col="firebrick1",font=1,xpd=NA)

        abline(h=cu.info.sub$LongTrend_UBM,col="green",lwd=3,lty=2)
        text(par("usr")[2],cu.info.sub$LongTrend_UBM,"Upper",adj=1,cex=1.4,col="green",font=1,xpd=NA)

      }

      if(bm.areas){
        rect(par("usr")[1],cu.info.sub$LongTrend_LBM, par("usr")[2],cu.info.sub$LongTrend_UBM, col=amber.use,
             border = amber.use)
        rect(par("usr")[1],cu.info.sub$LongTrend_UBM, par("usr")[2],par("usr")[4], col=green.use,
             border = green.use)
        rect(par("usr")[1],cu.info.sub$LongTrend_LBM, par("usr")[2],par("usr")[3], col=red.use,
             border = red.use)
      }


      abline(v=1995,col="darkgrey",lty=2,lwd=4)

      text(par("usr")[2],cu.info.sub$LongTrend_UBM,"3/4",cex=1.4,font=1,adj=c(1,0),xpd=NA)
      text(par("usr")[2],cu.info.sub$LongTrend_LBM,"Half",cex=1.4,font=1,adj=c(1,0),xpd=NA)
      abline(h=1,col="darkgrey",lwd=2,lty=2); text(par("usr")[2],1,"Same",cex=1.4,font=1,adj=c(1,0),xpd=NA)
      abline(h=2,col="darkgrey",lwd=2,lty=2); text(par("usr")[2],2,"Double",cex=1.4,font=1,adj=c(1,0),xpd=NA)

      lines(retro.values.sub$Year,retro.values.sub$LongTrend/100,col="darkblue",pch=19, lwd=3, cex=1.3, type="o")





      #############################################################
      # PANEL 4: PERC CHANGE
      ###########################################################



      ylim.use <- range(-55,50,retro.values.sub$PercChange,na.rm=TRUE)

      par(mai=mai.vals)

      plot(retro.values.sub$Year,retro.values.sub$PercChange,col="darkblue",pch=19, lwd=3, bty="n",
           xlim = retro.yrs.plot, cex=1.3, ylim = ylim.use,
           xlab = "Year", ylab = "% Change - 3 Gen",type="o",axes = FALSE,cex.lab=1.4)


      axis(1,cex.axis=1.4)
      axis(2,cex.axis=1.4,las=1)

      title(main = "Percent Change Metric", cex.main = 1.8,col.main="darkblue",line=1)

      if(!bm.areas){
        abline(h=cu.info.sub$PercChange_LBM,col="firebrick1",lwd=3,lty=2)
        text(par("usr")[2],cu.info.sub$PercChange_LBM,"Lower",adj=1,cex=1.4,col="firebrick1",font=1,xpd=NA)

        abline(h=cu.info.sub$PercChange_UBM,col="green",lwd=3,lty=2)
        text(par("usr")[2],cu.info.sub$PercChange_UBM,"Upper",adj=1,cex=1.4,col="green",font=1,xpd=NA)
      }


      if(bm.areas){
        rect(par("usr")[1],cu.info.sub$PercChange_LBM, par("usr")[2],cu.info.sub$PercChange_UBM, col=amber.use,
             border = amber.use)
        rect(par("usr")[1],cu.info.sub$PercChange_UBM, par("usr")[2],par("usr")[4], col=green.use,
             border = green.use)
        rect(par("usr")[1],cu.info.sub$PercChange_LBM, par("usr")[2],par("usr")[3], col=red.use,
             border = red.use)
      }


      abline(v=1995,col="darkgrey",lty=2,lwd=4)

      abline(h=0,col="darkgrey",lwd=2,lty=2); text(par("usr")[2],0,"Same",cex=1.4,font=1,adj=c(1,0),xpd=NA)
      if(par("usr")[4] > 100){abline(h=100,col="darkgrey",lwd=2,lty=2); text(par("usr")[2],100,"Double",cex=1.4,font=1,adj=c(1,0),xpd=NA)}
      if(par("usr")[3] < -50){abline(h=-50,col="darkgrey",lwd=2,lty=2); text(par("usr")[2],-50,"Half",cex=1.4,font=1,adj=c(1,0,xpd=NA))}


      lines(retro.values.sub$Year,retro.values.sub$PercChange,col="darkblue",pch=19, lwd=3, cex=1.3, type="o")



      #############################################################
      # PANEL 5: GRID OF METRICS AND STATUSES
      ###########################################################

        par(mai=c(1,3.5,1.5,2))

        plot(1:5,1:5, type="n",xlim = range(retro.yrs.plot), ylim= c(-7.3,0.5) ,xlab="",ylab="",
             axes=FALSE)
        axis(3,at = pretty(retro.yrs),cex.axis=2)
        mtext("Metrics & Status",side=3,line=3,xpd=NA,font=2,col="darkblue",cex=1.3)
        #abline(v=pretty(retro.yrs),col="darkgrey")

        abline(h=c(-1:-7),col="darkgrey")

        #


        var.list <- c("RelLBMCat","AbsLBMCat","LongTrendCat","PercChangeCat","RapidStatus","ConfidenceRating3","IntStatusRaw")
        var.labels <- c("RelAbd", "AbsAbd", "LongTrend","PercChange","RapidStatus", "ConfRating", "IntStatus")

        for(i in 1:7){

          if(var.list[i] == "RapidStatus"){font.use <- 2}
          if(var.list[i] != "RapidStatus"){font.use <- 1}

          text(par("usr")[1],-i, var.labels[i],adj = c(1),xpd=NA,cex = 1.9,font=font.use,col="darkblue")

          var.sub <- retro.summary.sub %>% select(all_of(c("Year",var.list[i])))
          names(var.sub)[2] <- "Var"
          var.sub


          if(var.list[i] != "ConfidenceRating3"){




            red.df <- var.sub %>% dplyr::filter(Var == "Red")
            red.df

            if(dim(red.df)[1]>0){
              points(red.df$Year,-rep(i,dim(red.df)[1]),pch=22,col ="firebrick1",bg= red.use.timeline,cex=7)
              text(red.df$Year,-rep(i,dim(red.df)[1]),"R",font=font.use,col="darkblue",cex=1.8)
            }

            amber.df <- var.sub %>% dplyr::filter(Var == "Amber")
            amber.df

            if(dim(amber.df)[1]>0){
              points(amber.df$Year,-rep(i,dim(amber.df)[1]),pch=22,col ="orange",bg= amber.use.timeline,cex=7)
              text(amber.df$Year,-rep(i,dim(amber.df)[1]),"A",font=font.use,col="darkblue",cex=1.8)
            }

            green.df <- var.sub %>% dplyr::filter(Var == "Green")
            green.df

            if(dim(green.df)[1]>0){
              points(green.df$Year,-rep(i,dim(green.df)[1]),pch=22,col ="green",bg= green.use.timeline,cex=7)
              text(green.df$Year,-rep(i,dim(green.df)[1]),"G",font=font.use,col="darkblue",cex=1.8)
            }


            if(i <6){
              unk.df <- var.sub %>% dplyr::filter(is.na(Var) | Var == "None")
              unk.df

              if(dim(unk.df)[1]>0){
                points(unk.df$Year,-rep(i,dim(unk.df)[1]),pch=22,col ="darkgrey",bg= "lightgrey",cex=7)
                text(unk.df$Year,-rep(i,dim(unk.df)[1]),"?",font=font.use,col="darkblue",cex=1.8)
              }
            }


            redamber.df <- var.sub %>% dplyr::filter(Var == "RedAmber")
            redamber.df

            if(dim(redamber.df)[1]>0){
              points(redamber.df$Year,-rep(i,dim(redamber.df)[1]),pch=22,col ="firebrick1",bg= red.use.timeline,cex=7)
              text(redamber.df$Year,-rep(i,dim(redamber.df)[1]),"RA",font=font.use,col="darkblue",cex=1.8)
            }

            ambergreen.df <- var.sub %>% dplyr::filter(Var == "AmberGreen")
            ambergreen.df

            if(dim(ambergreen.df)[1]>0){
              points(ambergreen.df$Year,-rep(i,dim(ambergreen.df)[1]),pch=22,col ="green",bg= green.use.timeline,cex=7)
              text(ambergreen.df$Year,-rep(i,dim(ambergreen.df)[1]),"AG",font=font.use,col="darkblue",cex=1.8)
            }

          } # end if status, not confidence


          if(var.list[i] == "ConfidenceRating3"){

            high.df <- var.sub %>% dplyr::filter(Var == "High")
            high.df

            if(dim(high.df)[1]>0){
              points(high.df$Year,-rep(i,dim(high.df)[1]),pch=22,col ="darkblue",bg= "lightblue",cex=7)
              text(high.df$Year,-rep(i,dim(high.df)[1]),"H",font=1,col="darkblue",cex=1.8)
            }


            low.df <- var.sub %>% dplyr::filter(Var == "Low")
            low.df

            if(dim(low.df)[1]>0){
              points(low.df$Year,-rep(i,dim(low.df)[1]),pch=22,col ="darkblue",bg= "white",cex=7)
              text(low.df$Year,-rep(i,dim(low.df)[1]),"L",font=1,col="darkblue",cex=1.8)
            }

            mod.df <- var.sub %>% dplyr::filter(Var == "Moderate")
            mod.df

            if(dim(mod.df)[1]>0){
              points(mod.df$Year,-rep(i,dim(mod.df)[1]),pch=22,col ="darkblue",bg= "lightgrey",cex=7)
              text(mod.df$Year,-rep(i,dim(mod.df)[1]),"M",font=1,col="darkblue",cex=1.8)
            }



          } # end if confidence

        } # end looping through var


      dev.off()
  } # end if doing plot
} #end looping through CUs





