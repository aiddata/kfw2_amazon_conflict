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
#detach("package:MatchIt", unload=TRUE)



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
dta_shp$prelevel_iviolence[is.na(dta_shp$ifreq2003)] <- 0
dta_shp$prelevel_lviolence <- dta_shp$lfreq2003
dta_shp$prelevel_lviolence[is.na(dta_shp$lfreq2003)] <- 0


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


#Make Post-Trend Values ()
dta_shp$posttrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp$posttrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp$posttrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",2003,2014,"id")

dta_shp$posttrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp$posttrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp$posttrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",2003,2014,"id")

dta_shp$posttrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp$posttrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",2003,2014,"id")
#This is the nighttime lights posttrend.
dta_shp$posttrend_ntl <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",2003,2014,"id")

#dta_shp$posttrend_iviolence <- timeRangeTrend(dta_shp)

#Make Pop pretrend value
dta_shp$pretrend_pop <- dta_shp$Pop_2000_x - dta_shp$Pop_1990

#Make the outcome variable
#dta_shp$lviolence_outcome


#Make a binary for ever demarcated vs. never demarcated
dta_shp@data["DemBin"] <- 0
dta_shp@data$NA_check <- 0
dta_shp@data$NA_check[is.na(dta_shp@data$demend_y)] <- 1
dta_shp@data$DemBin[dta_shp@data$NA_check != 1] <- 1

#demtable <- table(dtashp@data$DemBin)
#View(demtable)


#Make a binary for treated (demarcated 2004-2008)
dta_shp@data["Treat"] <- 0
dta_shp@data$NA_list <- 1
dta_shp@data$NA_list[!is.na(dta_shp@data$demend_y) & (dta_shp@data$demend_y > 2003 & dta_shp@data$demend_y < 2009)] <- 0
dta_shp@data$Treat[dta_shp@data$NA_list == 0] <- 1


#Eliminate lands that were demarcated in years other than 2004-2008
dta_shp@data <- subset(dta_shp@data, Treat == 1 | DemBin == 0)


aVars <- c("Treat", "terrai_are", "prelevel_pmean", "prelevel_pmin", "prelevel_pmax", "prelevel_tmean",
  "prelevel_tmin", "prelevel_tmax", "prelevel_ndvimean", "prelevel_ndvimax", "prelevel_iviolence", "prelevel_lviolence", 
  "pretrend_pmean", "pretrend_pmin", "pretrend_pmax", "pretrend_tmean", "pretrend_tmin", "pretrend_tmax", 
  "pretrend_ndvimean", "pretrend_ndvimax", "pretrend_ntl", "pretrend_pop", "Slope", "Elevation", "Riv_Dist", "Road_dist",
  "posttrend_pmean", "posttrend_pmin", "posttrend_pmax", "posttrend_tmean", "posttrend_tmin", "posttrend_tmax", 
  "posttrend_ndvimean", "posttrend_ndvimax", "posttrend_ntl", "id")

#aVars <- c("Treat", "terrai_are", "prelevel_pmean", "prelevel_pmin", "prelevel_pmax", "prelevel_tmean",
#  "prelevel_tmin", "prelevel_tmax", "prelevel_ndvimean", "prelevel_ndvimax", "prelevel_iviolence", "prelevel_lviolence",
#  "pretrend_pmean", "pretrend_pmin", "pretrend_pmax", "pretrend_tmean", "pretrend_tmin", "pretrend_tmax",
#  "pretrend_ndvimean", "pretrend_ndvimax", "pretrend_ntl", "pretrend_pop", "Slope", "Elevation", "Riv_Dist", "Road_dist")


#cuts the dataset down to only complete cases (matchit won't work if there are NAs)
dta_shp <- dta_shp[complete.cases(dta_shp@data[aVars]),]


matchit.results <- matchit(Treat ~ terrai_are + prelevel_pmean + prelevel_pmin + prelevel_pmax + prelevel_tmean +
                           prelevel_tmin + prelevel_tmax + prelevel_ndvimean + prelevel_ndvimax + prelevel_iviolence + prelevel_lviolence + 
                           pretrend_pmean + pretrend_pmin + pretrend_pmax + pretrend_tmean + pretrend_tmin + pretrend_tmax + 
                           pretrend_ndvimean + pretrend_ndvimax + pretrend_ntl + pretrend_pop + Slope + Elevation + Riv_Dist + Road_dist +
                           posttrend_pmean + posttrend_pmin + posttrend_pmax + posttrend_tmean + posttrend_tmin + posttrend_tmax + 
                           posttrend_ndvimean + posttrend_ndvimax + posttrend_ntl,
                           data = dta_shp@data[aVars],
                           method = "nearest", distance="logit")
                           #caliper = 1)

#prints the matchit results
print(summary(matchit.results))

#subsets the data to only the matched data
modelData <- match.data(matchit.results)
#modelData$lfreq_tota <- subset(dta_shp$lfreq_tota, dta_shp$id)
modelData <- merge.default(modelData, dta_shp@data["id", "lfreq_tota"], by = id)


linearModel <- lm(lfreq_tota ~ Treat, 
                  data = modelData)









#Views the data
View(as.data.frame(dta_shp)[1:100])
View(as.data.frame(dta_shp)[101:200])
View(as.data.frame(dta_shp)[201:300])
View(as.data.frame(dta_shp)[301:400])
View(as.data.frame(dta_shp)[401:500])
View(as.data.frame(dta_shp)[501:504])
