
#--------------------------------
#Cross-Section Model
#Outcome:Deforestation 1995-2014
#Trtmnt: Demarcated through PPTAL 1995-2008 or never demarcated through PPTAL 1995-2008
#--------------------------------

#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
#setwd("/Users/rbtrichler/Documents/AidData/Git Repos/kfw2_amazon_conflict")
#setwd("/home/aiddata/Desktop/Github/kfw2_amazon_conflict/")
setwd("C:/Users/jflak/OneDrive/GitHub/kfw2_amazon_conflict/")

#essential spatial view packages (load and project shapefiles etc...)
library(rgdal)
library(sp)
#tools for spatial objects
library(rgeos)
library(maptools)
#tools for data manipulation
library(reshape2)
#library that handles matching
library(MatchIt)
#library that has old sci functions (like timeRangeTrend)
library(SCI)


shp_file <- "Processed_Data/shpfilecross.shp"
dta_shp = readShapePoly(shp_file)


#Eliminate non-PPTAL indigenous lands
dta_shp@data$proj_check <- 0
dta_shp@data$proj_check[is.na(dta_shp@data$reu_id)] <- 1
proj_shp <- dta_shp[dta_shp@data$proj_check !=1,]
dta_shp <- proj_shp

#View(as.data.frame(dta_shp@data[1:100]))
#View(as.data.frame(dta_shp@data[101:200]))
#View(as.data.frame(dta_shp@data[201:300]))
#View(as.data.frame(dta_shp@data[301:400]))
#View(as.data.frame(dta_shp@data[401:471]))

#print(colnames(dta_shp@data))


####Creating Pre, Post, etc. Variables####
#Make Pre-Level Values (1995)
dta_shp$prelevel_pmean <- dta_shp$MeanP_1995
dta_shp$prelevel_pmin <- dta_shp$MinP_1995
dta_shp$prelevel_pmax <- dta_shp$MaxP_1995

dta_shp$prelevel_tmean <- dta_shp$MeanT_1995
dta_shp$prelevel_tmin <- dta_shp$MinT_1995
dta_shp$prelevel_tmax <- dta_shp$MaxT_1995

dta_shp$prelevel_ndvimean <- dta_shp$MeanL_1995
dta_shp$prelevel_ndvimax <- dta_shp$MaxL_1995

dta_shp$prelevel_ntl <- dta_shp$ntl_1995

#fills in 0s for NAs in lfreq_tota and ifreq_tota
dta_shp$lfreq_tota[is.na(dta_shp$lfreq_tota)] <- 0
dta_shp$ifreq_tota[is.na(dta_shp$ifreq_tota)] <- 0

#fills in 0s for NAs in other lfreq and ifreq variables
dta_shp$lfreq2003[is.na(dta_shp$lfreq2003)] <- 0
dta_shp$lfreq2004[is.na(dta_shp$lfreq2004)] <- 0
dta_shp$lfreq2005[is.na(dta_shp$lfreq2005)] <- 0
dta_shp$lfreq2006[is.na(dta_shp$lfreq2006)] <- 0
dta_shp$lfreq2007[is.na(dta_shp$lfreq2007)] <- 0
dta_shp$lfreq2008[is.na(dta_shp$lfreq2008)] <- 0
dta_shp$lfreq2009[is.na(dta_shp$lfreq2009)] <- 0
dta_shp$lfreq2010[is.na(dta_shp$lfreq2010)] <- 0
dta_shp$lfreq2011[is.na(dta_shp$lfreq2011)] <- 0
dta_shp$lfreq2012[is.na(dta_shp$lfreq2012)] <- 0
dta_shp$lfreq2013[is.na(dta_shp$lfreq2013)] <- 0
dta_shp$lfreq2014[is.na(dta_shp$lfreq2014)] <- 0

dta_shp$ifreq2003[is.na(dta_shp$ifreq2003)] <- 0
dta_shp$ifreq2004[is.na(dta_shp$ifreq2004)] <- 0
dta_shp$ifreq2005[is.na(dta_shp$ifreq2005)] <- 0
dta_shp$ifreq2006[is.na(dta_shp$ifreq2006)] <- 0
dta_shp$ifreq2007[is.na(dta_shp$ifreq2007)] <- 0
dta_shp$ifreq2008[is.na(dta_shp$ifreq2008)] <- 0
dta_shp$ifreq2009[is.na(dta_shp$ifreq2009)] <- 0
dta_shp$ifreq2010[is.na(dta_shp$ifreq2010)] <- 0
dta_shp$ifreq2011[is.na(dta_shp$ifreq2011)] <- 0
dta_shp$ifreq2012[is.na(dta_shp$ifreq2012)] <- 0
dta_shp$ifreq2013[is.na(dta_shp$ifreq2013)] <- 0
dta_shp$ifreq2014[is.na(dta_shp$ifreq2014)] <- 0


#Make Pre-Trend Values (1982-1995)
dta_shp$pretrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"id")
dta_shp$pretrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",1982,1995,"id")
dta_shp$pretrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",1982,1995,"id")

dta_shp$pretrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,1995,"id")
dta_shp$pretrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",1982,1995,"id")
dta_shp$pretrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",1982,1995,"id")

dta_shp$pretrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,1995,"id")
dta_shp$pretrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",1982,1995,"id")
#This is the nighttime lights pretrend. Note that it only runs 1992-1995, not 1982-1995
dta_shp$pretrend_ntl <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",1992,1995,"id")


#Make Post-Trend Values
dta_shp$posttrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",1995,2014,"id")
dta_shp$posttrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",1995,2014,"id")
dta_shp$posttrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",1995,2014,"id")

dta_shp$posttrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",1995,2014,"id")
dta_shp$posttrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",1995,2014,"id")
dta_shp$posttrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",1995,2014,"id")

dta_shp$posttrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",1995,2014,"id")
dta_shp$posttrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",1995,2014,"id")

#no ntl data for 2014 yet, so posttrend only goes through 2013
dta_shp$posttrend_ntl <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",1995,2013,"id")

#Make Pop pretrend value
dta_shp$pretrend_pop <- dta_shp$Pop_2000_y - dta_shp$Pop_1990

#Make ndvi differences for 1995-2014
dta_shp$diff_ndvimax <- dta_shp$MaxL_2014 - dta_shp$MaxL_1995


#Make lviolence and iviolence posttrends
dta_shp$posttrend_lviolence <- timeRangeTrend(dta_shp,"lfreq[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp$posttrend_iviolence <- timeRangeTrend(dta_shp,"ifreq[0-9][0-9][0-9][0-9]",2003,2014,"id")

#print(names(dta_shp@data))


#Make a binary for ever demarcated vs. never demarcated
dta_shp@data["DemBin"] <- 0
dta_shp@data$NA_check <- 0
dta_shp@data$NA_check[is.na(dta_shp@data$demend_y)] <- 1
dta_shp@data$DemBin[dta_shp@data$NA_check != 1] <- 1

# demtable <- table(dta_shp@data$DemBin)
# View(demtable)


#Make a binary for treated (demarcated 1995-2008)
dta_shp@data["Treat"] <- 0
dta_shp@data$NA_list <- 1
dta_shp@data$NA_list[!is.na(dta_shp@data$demend_y) & (dta_shp@data$demend_y > 1994 & dta_shp@data$demend_y < 2009)] <- 0
dta_shp@data$Treat[dta_shp@data$NA_list == 0] <- 1


#Eliminate lands that were demarcated in years other than 1995-2008
dta_shp@data <- subset(dta_shp@data, Treat == 1 | DemBin == 0)


#aVars <- c("Treat", "terrai_are", "prelevel_pmean", "prelevel_pmin", "prelevel_pmax", "prelevel_tmean",
#  "prelevel_tmin", "prelevel_tmax", "prelevel_ndvimean", "prelevel_ndvimax", "prelevel_iviolence", "prelevel_lviolence", 
#  "pretrend_pmean", "pretrend_pmin", "pretrend_pmax", "pretrend_tmean", "pretrend_tmin", "pretrend_tmax", 
#  "pretrend_ndvimean", "pretrend_ndvimax", "pretrend_ntl", "pretrend_pop", "Slope", "Elevation", "Riv_Dist", "Road_dist",
#  "posttrend_pmean", "posttrend_pmin", "posttrend_pmax", "posttrend_tmean", "posttrend_tmin", "posttrend_tmax", 
#  "posttrend_ndvimean", "posttrend_ndvimax", "posttrend_ntl", "id")

aVars <- c("Treat", "terrai_are", "prelevel_pmean", "prelevel_pmin", "prelevel_pmax", "prelevel_tmean",
           "prelevel_tmin", "prelevel_tmax", "prelevel_ndvimean", "prelevel_ndvimax", "prelevel_ntl",
           "pretrend_pmean", "pretrend_pmin", "pretrend_pmax", "pretrend_tmean", "pretrend_tmin", "pretrend_tmax",
           "pretrend_ndvimean", "pretrend_ndvimax", "pretrend_ntl", "pretrend_pop", "Slope", "Elevation", "Riv_Dist", "Road_dist", "Pop_2000_y", "id")


#cuts the dataset down to only complete cases (matchit won't work if there are NAs)
#dta_shp1 <- dta_shp[complete.cases(dta_shp@data[aVars]),]
#dta_shp<-dta_shp1

#matchit.results <- matchit(Treat ~ terrai_are + prelevel_pmean + prelevel_pmin + prelevel_pmax + prelevel_tmean +
#                           prelevel_tmin + prelevel_tmax + prelevel_ndvimean + prelevel_ndvimax + prelevel_iviolence + prelevel_lviolence + 
#                           pretrend_pmean + pretrend_pmin + pretrend_pmax + pretrend_tmean + pretrend_tmin + pretrend_tmax + 
#                           pretrend_ndvimean + pretrend_ndvimax + pretrend_ntl + pretrend_pop + Slope + Elevation + Riv_Dist + Road_dist +
#                           posttrend_pmean + posttrend_pmin + posttrend_pmax + posttrend_tmean + posttrend_tmin + posttrend_tmax + 
#                           posttrend_ndvimean + posttrend_ndvimax + posttrend_ntl,
#                           data = dta_shp@data[aVars],
#                           method = "nearest", distance="logit")

####MatchIt####
matchit.results <- matchit(Treat ~ terrai_are + prelevel_pmean + prelevel_pmin + prelevel_pmax + prelevel_tmean +
                             prelevel_tmin + prelevel_tmax + prelevel_ndvimean + prelevel_ndvimax + prelevel_ntl + 
                             pretrend_pmean + pretrend_pmin + pretrend_pmax + pretrend_tmean + pretrend_tmin + pretrend_tmax + 
                             pretrend_ndvimean + pretrend_ndvimax + pretrend_ntl + pretrend_pop + Slope + Elevation + Riv_Dist + Road_dist + Pop_2000_y,
                           data = dta_shp@data[aVars],
                           method = "nearest", distance="logit")


#prints the matchit results
print(summary(matchit.results))

#makes a new dataframe with the matched pair ids, to identify each pair
df_pairs <- as.data.frame(matchit.results$match.matrix)
df_pairs$treated_obs <- as.numeric(rownames(df_pairs))
rownames(df_pairs) <- NULL
colnames(df_pairs)[1] <- "untreated_obs"
df_pairs$untreated_obs <- as.numeric(as.character(df_pairs$untreated_obs))
df_pairs$pair_id <- as.numeric(rownames(df_pairs))
df_pairs <- data.frame(id = c(df_pairs$untreated_obs, df_pairs$treated_obs), pair_id = c(df_pairs$pair_id, df_pairs$pair_id))
#View(df_pairs)

#df_test <- data.frame(x = c(df_pairs$untreated_obs, df_pairs$treated_obs), y = c(df_pairs$pair_id, df_pairs$pair_id))
#View(df_test)

#print(names(dta_shp@data))

#subsets the data to only the matched data
modelData <- match.data(matchit.results)
dta_shp_subset <- dta_shp
dta_shp_subset@data$id_present <- (dta_shp_subset$id %in% modelData$id)
dta_shp_subset@data <- dta_shp_subset@data[dta_shp_subset$id_present == TRUE,]
dta_shp@data <- dta_shp_subset@data

#print(names(dta_shp@data))

#adds id, lfreq_tota, and pair_id variables to modelData, as well as all the posttrends
modelData <- merge.default(modelData, dta_shp@data[c("posttrend_pmean", "posttrend_pmin", "posttrend_pmax", "posttrend_tmean", "posttrend_tmin", "posttrend_tmax", 
                                                     "posttrend_ntl", "id", "lfreq_tota", "ifreq_tota", "posttrend_lviolence", "posttrend_iviolence", "posttrend_ndvimean", "posttrend_ndvimax", "diff_ndvimax")], by = "id")
modelData$id <- modelData$id - 1
modelData <- merge.default(modelData, df_pairs, by = "id")


####Start of Models####
model_treat_only <- lm(diff_ndvimax ~ Treat,
                       data = modelData)
print(summary(model_treat_only))
print('______________________________________________________________________________________', quote = FALSE)


model_treat_FE <- lm(diff_ndvimax ~ Treat + factor(pair_id),
                     data = modelData)
print(summary(model_treat_FE))
print('______________________________________________________________________________________', quote = FALSE)

model_prelevel <- lm(diff_ndvimax ~ Treat + terrai_are + 
                         prelevel_pmean + prelevel_pmin + prelevel_pmax + prelevel_tmean +
                         prelevel_tmin + prelevel_tmax + prelevel_ndvimean + prelevel_ndvimax + prelevel_ntl +
                         Slope + Elevation + Riv_Dist + Road_dist + Pop_2000_y + factor(pair_id),
                       data = modelData)
print(summary(model_prelevel))
print('______________________________________________________________________________________', quote = FALSE)

#This model includes all of the covariates including the violence prelevels, 
#but I think that the violence prelevels should probably be taken out since they are almost all zeros
model_all_covars <- lm(diff_ndvimax ~ Treat + terrai_are + 
                         prelevel_pmean + prelevel_pmin + prelevel_pmax + prelevel_tmean +
                         prelevel_tmin + prelevel_tmax + prelevel_ndvimean + prelevel_ndvimax + prelevel_ntl +
                         Slope + Elevation + Riv_Dist + Road_dist +
                         posttrend_pmean + posttrend_pmin + posttrend_pmax + posttrend_tmean + posttrend_tmin + posttrend_tmax + 
                         posttrend_ndvimean + posttrend_ndvimax + posttrend_iviolence + posttrend_lviolence + posttrend_ntl + Pop_2000_y + factor(pair_id),
                       data = modelData)
print(summary(model_all_covars))
print('______________________________________________________________________________________', quote = FALSE)


#Views the data
#View(as.data.frame(dta_shp)[1:100])
#View(as.data.frame(dta_shp)[101:200])
#View(as.data.frame(dta_shp)[201:300])
#View(as.data.frame(dta_shp)[301:400])
#View(as.data.frame(dta_shp)[401:500])
#View(as.data.frame(dta_shp)[501:504])
