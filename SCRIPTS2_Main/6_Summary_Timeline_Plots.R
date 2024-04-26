library(tidyverse)
library(plotrix)


# these work
plot(1,1, main=expression('Fraser Spring 4'["2"]*' Chinook')) 
plot(1,1, main=bquote('Fraser Spring 4'['2']*' Chinook') )

plot(1,1, main=parse(text= ' "Fraser Spring 4" ["2"] * " Chinook" '))


# working
label.use  <-  ' "Fraser Spring 4" ["2"] * " Chinook" '
parse(text = label.use)
plot(1,1, main=parse(text = label.use))

# not working
tmp.spec <- read.csv("DATA_LOOKUP_FILES/tmp_specs.csv")
label.use <- tmp.spec$Group[1]
label.use
parse(text = label.use)
plot(1,1, main=parse(text = label.use))






if(!dir.exists("OUTPUT/TimelinePlots")){dir.create("OUTPUT/TimelinePlots")}


cu.info <- read_csv("DATA_LOOKUP_FILES/MOD_MAIN_CU_LOOKUP_FOR_SOS.csv") %>%
  dplyr::mutate(CU_ID = gsub("_","-",CU_ID))

retro.summary.tbl <- read_csv("DATA_OUT/Retro_Synoptic_Details_SkeenaMODS.csv")  

plot.specs <- read_csv("DATA_LOOKUP_FILES/TimelinePlot_Specs.csv") 
plot.specs
#view(plot.specs)

alpha.use <- 1
green.use <- rgb(184/255,225/255,134/255,alpha=alpha.use)
red.use <- rgb(241/255,182/255,218/255,alpha=alpha.use)
amber.use <- rgb(255/255,255/255,191/255,alpha=alpha.use)



plot.list <- unique(plot.specs$Plot)
plot.list


for(plot.do in plot.list){

specs.do <- plot.specs %>% dplyr::filter(Plot == plot.do) %>%
              mutate(CU_ID = gsub("_","-",CU_ID)) %>%
              mutate(CU_Label = paste0(CU_Acro," (",CU_ID,")"))
specs.do
specs.do$CU_ID


grp.labels <- specs.do %>% select(GroupIndex,Group) %>% unique()
grp.labels

retro.yrs <- 1995:2025

unk.end <- 2022

#########################################



png(filename = paste0("OUTPUT/TimelinePlots/TimelinePlot_",plot.do,".png"),
    width = 480*4, height = 480*5, units = "px", pointsize = 14*3.1, bg = "white",  res = NA)
par(mai=c(0.3,6,3,1))

 print(paste(plot.do,"-----------------------------"))


plot(1:5,1:5, type="n",xlim = range(retro.yrs), ylim= c(-46,-2) ,xlab="",ylab="",
     axes=FALSE)
axis(3,at = pretty(retro.yrs))
mtext(specs.do$PlotTitle ,side=3,line=2,xpd=NA,font=2,col="darkblue",cex=1.3)
#abline(v=pretty(retro.yrs),col="darkgrey")

abline(h=-specs.do$CUIndex,col="darkgrey")

text(rep(par("usr")[1],length(specs.do$CUIndex)),
     -specs.do$CUIndex, specs.do$CU_Label,
     adj = c(1),xpd=NA,cex = 0.9)

# only add these labels if have more than one group
if(length(unique(specs.do$GroupIndex))>1){
text(rep(retro.yrs[1]-4,length(grp.labels$GroupIndex)),
     -grp.labels$GroupIndex, grp.labels$Group,
     adj = 0,xpd=NA,cex = 0.9,font=2,col="darkblue")
}


for(cu.plot in specs.do$CU_ID){
print(cu.plot)

#if(cu.plot == "SkeenaNass-13b"){stop()}
  
specs.sub <- specs.do %>% dplyr::filter(CU_ID == cu.plot) 
specs.sub

#retro.sub <- retro.summary.tbl %>% dplyr::filter(CU_ID == cu.plot, Year <= 2019) %>% select(Year,RapidStatus)
retro.sub <- retro.summary.tbl %>% dplyr::filter(CU_ID == cu.plot) %>% select(Year,RapidStatus,BinLabel)
retro.sub

red.df <- retro.sub %>% dplyr::filter(RapidStatus == "Red" & BinLabel != "Proxy")
red.df

if(dim(red.df)[1]>0){
  points(red.df$Year,-rep(specs.sub$CUIndex,dim(red.df)[1]),pch=22,col ="firebrick1",bg= red.use,cex=2.6)
  text(red.df$Year,-rep(specs.sub$CUIndex,dim(red.df)[1]),"R",font=2,col="darkblue",cex=0.8)
}

red.proxy.df <- retro.sub %>% dplyr::filter(RapidStatus == "Red" & BinLabel == "Proxy")
red.proxy.df

if(dim(red.proxy.df)[1]>0){
  points(red.proxy.df$Year,-rep(specs.sub$CUIndex,dim(red.proxy.df)[1]),pch=22,col ="firebrick1",bg= red.use,cex=2.6)
  text(red.proxy.df$Year,-rep(specs.sub$CUIndex,dim(red.proxy.df)[1]),"R*",font=2,col="darkblue",cex=0.8)
}



amber.df <- retro.sub %>% dplyr::filter(RapidStatus == "Amber" & BinLabel != "Proxy")
amber.df

if(dim(amber.df)[1]>0){
points(amber.df$Year,-rep(specs.sub$CUIndex,dim(amber.df)[1]),pch=22,col ="orange",bg= amber.use,cex=2.6)
text(amber.df$Year,-rep(specs.sub$CUIndex,dim(amber.df)[1]),"A",font=2,col="darkblue",cex=0.8)
}


amber.proxy.df <- retro.sub %>% dplyr::filter(RapidStatus == "Amber" & BinLabel == "Proxy")
amber.proxy.df

if(dim(amber.proxy.df)[1]>0){
  points(amber.proxy.df$Year,-rep(specs.sub$CUIndex,dim(amber.proxy.df)[1]),pch=22,col ="orange",bg= amber.use,cex=2.6)
  text(amber.proxy.df$Year,-rep(specs.sub$CUIndex,dim(amber.proxy.df)[1]),"A*",font=2,col="darkblue",cex=0.8)
}




green.df <- retro.sub %>% dplyr::filter(RapidStatus == "Green" & BinLabel != "Proxy")
green.df

if(dim(green.df)[1]>0){
  points(green.df$Year,-rep(specs.sub$CUIndex,dim(green.df)[1]),pch=22,col ="green",bg= green.use,cex=2.6)
  text(green.df$Year,-rep(specs.sub$CUIndex,dim(green.df)[1]),"G",font=2,col="darkblue",cex=0.8)
  }



green.proxy.df <- retro.sub %>% dplyr::filter(RapidStatus == "Green" & BinLabel == "Proxy")
green.proxy.df

if(dim(green.proxy.df)[1]>0){
  points(green.proxy.df$Year,-rep(specs.sub$CUIndex,dim(green.proxy.df)[1]),pch=22,col ="green",bg= green.use,cex=2.6)
  text(green.proxy.df$Year,-rep(specs.sub$CUIndex,dim(green.proxy.df)[1]),"G*",font=2,col="darkblue",cex=0.8)
}




unk.df <- retro.sub %>% dplyr::filter(RapidStatus == "None")
unk.df

if(dim(retro.sub)[1] == 0){
unk.df <- data.frame(Year = retro.yrs[1]:unk.end,RapidStatus = "None")  
}


if(dim(unk.df)[1]>0){
  points(unk.df$Year,-rep(specs.sub$CUIndex,dim(unk.df)[1]),pch=22,col ="darkgrey",bg= "grey95",cex=2.6)
  text(x = unk.df$Year,y = -rep(specs.sub$CUIndex,dim(unk.df)[1]),labels = "?",font=1,col="darkblue",cex=0.8)
}



ext.df <- retro.sub %>% dplyr::filter(RapidStatus == "Ext")
ext.df


if(dim(ext.df)[1]>0){
  #points(ext.df$Year,-rep(specs.sub$CUIndex,dim(ext.df)[1]),pch=22,col ="darkgrey",bg= "lightgrey",cex=2.6)
  text(x = ext.df$Year,y = -rep(specs.sub$CUIndex,dim(ext.df)[1]),labels = "X",font=2,col="black",cex=0.5)
}






} # end looping through CUs







dev.off()


} # end looping through plots















