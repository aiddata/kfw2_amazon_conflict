#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("/home/aiddata/Desktop/Github/kfw2_amazon_conflict/")
#setwd("C:/Users/jflak/OneDrive/GitHub/kfw2_amazon_conflict/")

#essential spatial view packages (load and project shapefiles etc...)
library(rgdal)
library(sp)
#tools for spatial objects
library(rgeos)
library(maptools)
#tools for data manipulation
library(reshape2)
#Library that handles matching
library(MatchIt)

library(SCI)


#Run the code to build the dataset
#source('KFW_Cross-Section_Dataset_Building.R')

#clears unneeded variables and values
#rm(list = (ls()[ls()!="df_merged_shp"]))


shp_file <- "Processed_Data/shpfilecross.shp"
dta_shp = readShapePoly(shp_file)


#Eliminate non-PPTAL indigenous lands
dta_shp@data$proj_check <- 0
dta_shp@data$proj_check[is.na(dta_shp@data$reu_id)] <- 1
proj_shp <- dta_shp[dta_shp@data$proj_check !=1,]
dta_shp <- proj_shp


#Make Pre-Level Values (2003)
dta_shp$prelevel_pmean <- dta_shp$MeanP_2003
dta_shp$prelevel_pmin <- dta_shp$MinP_2003
dta_shp$prelevel_pmax <- dta_shp$MaxP_2003

dta_shp$prelevel_tmean <- dta_shp$MeanT_2003
dta_shp$prelevel_tmin <- dta_shp$MinT_2003
dta_shp$prelevel_tmax <- dta_shp$MaxT_2003

dta_shp$prelevel_ndvimean <- dta_shp$MeanL_2003
dta_shp$prelevel_ndvimax <- dta_shp$MaxL_2003

dta_shp$prelevel_iviolence <- dta_shp$ifreq2003
dta_shp$prelevel_lviolence <- dta_shp$lfreq2003


#Make Pre-Trend Values (1982-2003)
dta_shp$pretrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp$pretrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp$pretrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",1982,2003,"id")

dta_shp$pretrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp$pretrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp$pretrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",1982,2003,"id")

dta_shp$pretrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp$pretrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",1982,2003,"id")
#This is the nighttime lights pretrend. Note that it only runs 1992-2003
dta_shp$pretrend_ntl <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",1992,2003,"id")


#Make Post-Trend Values (1982-2003)
dta_shp$postrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,2014,"id")
dta_shp$postrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",1982,2014,"id")
dta_shp$postrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",1982,2014,"id")

dta_shp$postrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",2004,2014,"id")
dta_shp$postrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",2004,2014,"id")
dta_shp$postrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",2004,2014,"id")

dta_shp$postrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",2004,2014,"id")
dta_shp$postrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",2004,2014,"id")
#This is the nighttime lights postrend. Note that it only runs 1992-2003
dta_shp$postrend_ntl <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",2004,2014,"id")


#Make Pop pre-change value
dta_shp$pretrend_pop <- dta_shp$Pop_2000_x - dta_shp$Pop_1990


#Views the data
View(as.data.frame(dta_shp))
View(as.data.frame(dta_shp)[401:499])
