library(tidyverse)
library(plotrix)


if(!dir.exists("OUTPUT/DASHBOARDS/SMU_TimelinePlotsforSOPO")){dir.create("OUTPUT/DASHBOARDS/SMU_TimelinePlotsforSOPO")}
if(!dir.exists(paste0("OUTPUT/DASHBOARDS/SMU_TimelinePlotsforSOPO/", paste(datastage, collapse="")))){dir.create(paste0("OUTPUT/DASHBOARDS/SMU_TimelinePlotsforSOPO/", paste(datastage, collapse=""))) } 

if(!dir.exists("OUTPUT/DASHBOARDS/FAZ_TimelinePlotsforSOPO")){dir.create("OUTPUT/DASHBOARDS/FAZ_TimelinePlotsforSOPO")}
if(!dir.exists(paste0("OUTPUT/DASHBOARDS/FAZ_TimelinePlotsforSOPO/", paste(datastage, collapse="")))){dir.create(paste0("OUTPUT/DASHBOARDS/FAZ_TimelinePlotsforSOPO/", paste(datastage, collapse=""))) } 


cu.info <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv") %>%
  dplyr::mutate(CU_ID = gsub("_","-",CU_ID))

retro.summary.tbl <- read_csv(paste0("OUTPUT/DASHBOARDS/Retro_Synoptic_Details_",paste(datastage, collapse=""),".csv"))  

SMU_plot.specs <- read_csv("DATA_LOOKUP_FILES/SMU_TimelinePlot_SpecsforSOPO.csv") %>%
                mutate(CU_ID = gsub("_","-",CU_ID)) %>%
                left_join(cu.info %>% select(CU_ID=CU_ID_Alt2_CULookup, Data_Stage)) %>%
                filter(Data_Stage %in% datastage)

FAZ_plot.specs <- read_csv("DATA_LOOKUP_FILES/FAZ_TimelinePlot_SpecsforSOPO.csv") %>%
                  mutate(CU_ID = gsub("_","-",CU_ID)) %>%
                  left_join(cu.info %>% select(CU_ID=CU_ID_Alt2_CULookup, Data_Stage)) %>%
                  filter(Data_Stage %in% datastage)

              
#view(plot.specs)

alpha.use <- 1
green.use <- rgb(0/255,225/255,0/255,alpha=alpha.use)
red.use <- rgb(255/255,0/255,0/255,alpha=alpha.use)
amber.use <- rgb(249/255,203/255,11/255,alpha=alpha.use)

for(plot.type in c("SMU", "FAZ")){

  plot.specs <- eval(as.name(paste0(plot.type,"_plot.specs")))
  plot.list <- unique(plot.specs$Plot)
  plot.list
  
  
  for(plot.do in plot.list){
  
    specs.do <- plot.specs %>% dplyr::filter(Plot == plot.do) 
    specs.do
    specs.do$CU_ID
    
    
    #grp.labels <- specs.do %>% select(GroupIndex,Group) %>% unique()
    #grp.labels
    
    retro.yrs <- 2013:2022
    if(plot.do=="Fraser Pink") retro.yrs=2013:2023
    
    unk.end <- 2022
    
    #########################################
    
    
    
    png(filename = paste0("OUTPUT/DASHBOARDS/",paste(plot.type),"_TimelinePlotsforSOPO/",paste(datastage, collapse=""),"/SMU_TimelinePlotforSOPO_",plot.do,".png"),
        width = 480*4, height = 480*5, units = "px", pointsize = 14*3.1, bg = "white",  res = NA)
    par(mai=c(0.3,6,3,1))
    
     print(paste(plot.do,"-----------------------------"))
    
    
    plot(1:5,1:5, type="n",xlim = c(range(retro.yrs)[1]-5, range(retro.yrs)[2]+1), ylim= c(-46,0) ,xlab="",ylab="",
         axes=FALSE)
    axis(3,at = pretty(retro.yrs))
    mtext(plot.do,side=3,line=3,xpd=NA,font=2,col="darkblue",cex=1.3)
    #abline(v=pretty(retro.yrs),col="darkgrey")
    
    #abline(h=-specs.do$CUIndex,col="darkgrey")
    segments(x0=range(retro.yrs)[1], x1=range(retro.yrs)[2],  y0=-specs.do$CUIndex, y1=-specs.do$CUIndex, 
             col="darkgrey")
    
    # text(rep( (par("usr")[1])+5,length(specs.do$CUIndex)),
    #      -specs.do$CUIndex, specs.do$CU_Name,
    #      adj = c(1),xpd=NA,cex = 0.7)
    
    text(rep( retro.yrs[1]-1 ,length(specs.do$CUIndex)),
         -specs.do$CUIndex, specs.do$CU_Name,
         adj = c(1),xpd=NA,cex = 0.75)
    
    # text(rep(retro.yrs[1]-4,length(grp.labels$GroupIndex)),
    #      -grp.labels$GroupIndex, grp.labels$Group,
    #      adj = 0,xpd=NA,cex = 0.9,font=2,col="darkblue")
    
    
    
    for(cu.plot in specs.do$CU_ID){
        print(cu.plot)
        
        #if(cu.plot == "CK-33"){stop()}
          
        specs.sub <- specs.do %>% dplyr::filter(CU_ID == cu.plot)
        specs.sub
        
        #retro.sub <- retro.summary.tbl %>% dplyr::filter(CU_ID == cu.plot, Year <= 2019) %>% select(Year,RapidStatus)
        retro.sub <- retro.summary.tbl %>% dplyr::filter(CU_ID == cu.plot) %>% select(Year,RapidStatus) %>% filter(Year >=retro.yrs[1]) %>%
                                           filter(Year <= range(retro.yrs)[2])
        retro.sub
        
        red.df <- retro.sub %>% dplyr::filter(RapidStatus == "Red")
        red.df
        
        if(dim(red.df)[1]>0){
          points(red.df$Year,-rep(specs.sub$CUIndex,dim(red.df)[1]),pch=22,col ="red1",bg= red.use,cex=2.6)
          text(red.df$Year,-rep(specs.sub$CUIndex,dim(red.df)[1]),"R",font=2,col="darkblue",cex=0.8)
        }
        
        amber.df <- retro.sub %>% dplyr::filter(RapidStatus == "Amber")
        amber.df
        
        if(dim(amber.df)[1]>0){
        points(amber.df$Year,-rep(specs.sub$CUIndex,dim(amber.df)[1]),pch=22,col ="orange",bg= amber.use,cex=2.6)
        text(amber.df$Year,-rep(specs.sub$CUIndex,dim(amber.df)[1]),"A",font=2,col="darkblue",cex=0.8)
        }
        
        
        green.df <- retro.sub %>% dplyr::filter(RapidStatus == "Green")
        green.df
        
        if(dim(green.df)[1]>0){
          points(green.df$Year,-rep(specs.sub$CUIndex,dim(green.df)[1]),pch=22,col ="green",bg= green.use,cex=2.6)
          text(green.df$Year,-rep(specs.sub$CUIndex,dim(green.df)[1]),"G",font=2,col="darkblue",cex=0.8)
          }
        
        unk.df <- retro.sub %>% dplyr::filter(RapidStatus == "None")
        unk.df
        
        if(dim(retro.sub)[1] == 0){
        unk.df <- data.frame(Year = retro.yrs[1]:unk.end,RapidStatus = "None")  
        }
        
        
        if(dim(unk.df)[1]>0){
          points(unk.df$Year,-rep(specs.sub$CUIndex,dim(unk.df)[1]),pch=22,col ="darkgrey",bg= "lightgrey",cex=2.6)
          text(x = unk.df$Year,y = -rep(specs.sub$CUIndex,dim(unk.df)[1]),labels = "?",font=2,col="darkblue",cex=0.8)
        }
    
    
    
    } # end looping through CUs
    
    
    
    
    
    
    
    dev.off()
  
  
  } # end looping through plots

} # end loop over SMU and FAZ














