
#--------------------------------
#Cross-Section Model
#Outcome:Deforestation 2003-2014
#Trtmnt: Years prior to or after demarcation
#--------------------------------

#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("/Users/rbtrichler/Documents/AidData/Git Repos/kfw2_amazon_conflict")
#setwd("/home/aiddata/Desktop/Github/kfw2_amazon_conflict/")
#setwd("C:/Users/jflak/OneDrive/GitHub/kfw2_amazon_conflict/")

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
#stargazer for visualizations
library(stargazer)


shp_file <- "Processed_Data/shpfilecross.shp"
dta_shp = readShapePoly(shp_file)

#View(as.data.frame(dta_shp@data[1:100]))
#View(as.data.frame(dta_shp@data[101:200]))
#View(as.data.frame(dta_shp@data[201:300]))
#View(as.data.frame(dta_shp@data[301:400]))
#View(as.data.frame(dta_shp@data[401:470]))

#print(colnames(dta_shp@data))


####Creating Pre, Post, etc. Variables####
#Make Pre-Level Values (1988)
dta_shp$prelevel_pmean <- dta_shp$MeanP_1988
dta_shp$prelevel_pmin <- dta_shp$MinP_1988
dta_shp$prelevel_pmax <- dta_shp$MaxP_1988

dta_shp$prelevel_tmean <- dta_shp$MeanT_1988
dta_shp$prelevel_tmin <- dta_shp$MinT_1988
dta_shp$prelevel_tmax <- dta_shp$MaxT_1988

dta_shp$prelevel_ndvimean <- dta_shp$MeanL_1988
dta_shp$prelevel_ndvimax <- dta_shp$MaxL_1988

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


#Make Pre-Trend Values (1982-1988)
dta_shp$pretrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1988,"id")
dta_shp$pretrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",1982,1988,"id")
dta_shp$pretrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",1982,1988,"id")

dta_shp$pretrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,1988,"id")
dta_shp$pretrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",1982,1988,"id")
dta_shp$pretrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",1982,1988,"id")

dta_shp$pretrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,1988,"id")
dta_shp$pretrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",1982,1988,"id")


#Make Post-Trend Values
dta_shp$posttrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",1988,2014,"id")
dta_shp$posttrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",1988,2014,"id")
dta_shp$posttrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",1988,2014,"id")

dta_shp$posttrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",1988,2014,"id")
dta_shp$posttrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",1988,2014,"id")
dta_shp$posttrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",1988,2014,"id")

dta_shp$posttrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",1988,2014,"id")
dta_shp$posttrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",1988,2014,"id")

dta_shp$posttrend_pop<-timeRangeTrend(dta_shp,"Pop_[0-9][0-9][0-9][0-9]","1990","2010","id")

#no ntl data for 2014 yet, so posttrend only goes through 2013
#drop 4 obs with no ntl data, which also have no demarcation data so would be dropped out anyway
dta_shp <- dta_shp[!is.na(dta_shp$ntl_1992),]
dta_shp$posttrend_ntl <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",1992,2013, "id")

#Make ndvi differences for 1988-2014
dta_shp$diff_ndvimax <- dta_shp$MaxL_2014 - dta_shp$MaxL_1988

#Make lviolence and iviolence posttrends
dta_shp$posttrend_lviolence <- timeRangeTrend(dta_shp,"lfreq[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp$posttrend_iviolence <- timeRangeTrend(dta_shp,"ifreq[0-9][0-9][0-9][0-9]",2003,2014,"id")


#Make a binary for ever demarcated (DemBin=1) vs. never demarcated (DemBin=0)
#use data from FUNAI in either 2011 (dem_y11) or 2016 (dem_y16) since including PPTAL and non-PPTAL
#more demarcation data exists in dem_y16 than dem_y11
dta_shp@data["DemBin"] <- 0
dta_shp@data$DemBin[!is.na(dta_shp@data$dem_y16)] <- 1
table(dta_shp@data$DemBin)

#Make a binary for treated (demarcated 1988 or later)
#use dem_y11 or dem_y16 because we're including all indigenous lands, not just those demarcated through PPTAL
#Treat=1 if demarcated 1988 or later, 0=never demarcated 
dta_shp@data["Treat"] <- 0
dta_shp@data$Treat[(dta_shp@data$DemBin==1)&(dta_shp@data$dem_y16>1987)]<-1
#table(dta_shp@data$Treat)

#Eliminate lands that were demarcated in years before 1988, so dataset only includes never demarcated and demarcated 1988 or later
dta_shp<- subset(dta_shp, Treat == 1 | DemBin == 0)

aVars <- c("Treat", "area_587","prelevel_pmean", "prelevel_pmin", "prelevel_pmax", "prelevel_tmean",
           "prelevel_tmin", "prelevel_tmax", "prelevel_ndvimean", "prelevel_ndvimax",
           "pretrend_pmean", "pretrend_pmin", "pretrend_pmax", "pretrend_tmean", "pretrend_tmin", "pretrend_tmax",
           "pretrend_ndvimean", "pretrend_ndvimax", "Slope", "Elevation", "Riv_Dist", "Road_dist", "Pop_1990", "id","U___F")


#cuts the dataset down to only complete cases (matchit won't work if there are NAs)
#this step should not eliminate any cases
dta_shp1 <- dta_shp[complete.cases(dta_shp@data[aVars]),]
dta_shp<-dta_shp1

#--------------
####MatchIt####
#--------------

## WITHOUT REPLACEMENT

#constrain matches to within state (represented by "U___F" variable)
matchit.results <- matchit(Treat ~ area_587+prelevel_pmean + prelevel_pmin + prelevel_pmax + prelevel_tmean +
                             prelevel_tmin + prelevel_tmax + prelevel_ndvimean + prelevel_ndvimax + 
                             pretrend_pmean + pretrend_pmin + pretrend_pmax + pretrend_tmean + pretrend_tmin + pretrend_tmax + 
                             pretrend_ndvimean + pretrend_ndvimax + Slope + Elevation + Riv_Dist + Road_dist + Pop_1990,
                           data = dta_shp@data[aVars],
                           method = "nearest", distance="logit",exact="U___F",discard="both")


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
df_pairs$id <- df_pairs$id + 1
#View(df_pairs)

#subsets the data to only the matched data
modelData <- match.data(matchit.results)
dta_shp_subset <- dta_shp
dta_shp_subset@data$id_present <- (dta_shp_subset$id %in% modelData$id)
dta_shp_subset@data <- dta_shp_subset@data[dta_shp_subset$id_present == TRUE,]

#print(names(dta_shp@data))

#adds id, lfreq_tota, and pair_id variables to modelData, as well as all the posttrends
modelData <- merge.default(modelData, dta_shp_subset@data[c("posttrend_pmean", "posttrend_pmin", "posttrend_pmax", "posttrend_tmean", "posttrend_tmin", "posttrend_tmax", 
                                                     "posttrend_ntl", "id", "lfreq_tota", "ifreq_tota", "posttrend_ndvimean", "posttrend_ndvimax", "diff_ndvimax")], by = "id")
modelData <- merge.default(modelData, df_pairs, by = "id")

## WITH REPLACEMENT

#constrain matches to within state
matchit.replace <- matchit(Treat ~ area_587+prelevel_pmean + prelevel_pmin + prelevel_pmax + prelevel_tmean +
                             prelevel_tmin + prelevel_tmax + prelevel_ndvimean + prelevel_ndvimax + 
                             pretrend_pmean + pretrend_pmin + pretrend_pmax + pretrend_tmean + pretrend_tmin + pretrend_tmax + 
                             pretrend_ndvimean + pretrend_ndvimax + Slope + Elevation + Riv_Dist + Road_dist + Pop_1990,
                           data = dta_shp@data[aVars],
                           method = "nearest", distance="logit",replace=TRUE,exact="U___F",discard="both")

#prints the matchit results
print(summary(matchit.replace))

#don't need to create pair ids because can't use pair fixed effects when matching with replacement
#will use weights generated in match.data instead
#create subset of dta_shp with only the matched data
modelData_rep <- match.data(matchit.replace)
dta_shp_subset_rep <- dta_shp
dta_shp_subset_rep@data$id_present <- (dta_shp_subset_rep$id %in% modelData_rep$id)
dta_shp_subset_rep@data <- dta_shp_subset_rep@data[dta_shp_subset_rep$id_present == TRUE,]

modelData_rep <- merge.default(modelData_rep, dta_shp_subset_rep@data[c("posttrend_pmean", "posttrend_pmin", "posttrend_pmax", 
                                                                        "posttrend_tmean", "posttrend_tmin", "posttrend_tmax", 
                                                                        "posttrend_ntl","posttrend_pop", "id", "lfreq_tota", "ifreq_tota", 
                                                                        "posttrend_lviolence", "posttrend_iviolence", 
                                                                        "posttrend_ndvimean", "posttrend_ndvimax", "diff_ndvimax")], by = "id")


#---------------------
####Start of Models####
#---------------------

## WITHOUT REPLACEMENT
model_treat_only <- lm(diff_ndvimax ~ Treat,
                       data = modelData)
print(summary(model_treat_only))
print('______________________________________________________________________________________', quote = FALSE)


model_treat_FE <- lm(diff_ndvimax ~ Treat + factor(pair_id),
                     data = modelData)
print(summary(model_treat_FE))
print('______________________________________________________________________________________', quote = FALSE)

model2<-lm(diff_ndvimax ~ Treat + area_587 +
             prelevel_ndvimax + pretrend_ndvimax+
             Pop_1990 + prelevel_tmean + prelevel_pmean +
             posttrend_tmean + posttrend_pmean+posttrend_ntl+posttrend_pop+
             Slope +Elevation + Riv_Dist + Road_dist+
             factor(pair_id), 
           data=modelData)
print(summary(model2))


model3<-lm(diff_ndvimax ~ Treat + area_587+
             prelevel_ndvimax + pretrend_ndvimax+
             Pop_1990 + prelevel_tmean + prelevel_pmean +
             posttrend_tmean + posttrend_pmean+posttrend_ntl+posttrend_pop+
             lfreq_tota + ifreq_tota+
             Slope +Elevation + Riv_Dist + Road_dist+
             factor(pair_id), data=modelData)
print(summary(model3))

## WITH REPLACEMENT

model1R <- lm(diff_ndvimax ~ Treat,
              data = modelData_rep)
print(summary(model1R))
print('______________________________________________________________________________________', quote = FALSE)


model1.1R <- lm(diff_ndvimax ~ Treat,
                data = modelData_rep, weights=(distance))
print(summary(model1.1R))
print('______________________________________________________________________________________', quote = FALSE)

model2R<-lm(diff_ndvimax ~ Treat + area_587 +
             prelevel_ndvimax + pretrend_ndvimax+
             Pop_1990 + prelevel_tmean + prelevel_pmean +
             posttrend_tmean + posttrend_pmean+posttrend_ntl+posttrend_pop+
             Slope +Elevation + Riv_Dist + Road_dist, 
           data=modelData_rep, weights=(distance))
print(summary(model2R))


model3<-lm(diff_ndvimax ~ Treat + area_587+
             prelevel_ndvimax + pretrend_ndvimax+
             Pop_1990 + prelevel_tmean + prelevel_pmean +
             posttrend_tmean + posttrend_pmean+posttrend_ntl+posttrend_pop+
             lfreq_tota + ifreq_tota+
             Slope +Elevation + Riv_Dist + Road_dist,
           data=modelData_rep, weights=(distance))
print(summary(model3))





#Views the data
#View(as.data.frame(dta_shp)[1:100])
#View(as.data.frame(dta_shp)[101:200])
#View(as.data.frame(dta_shp)[201:300])
#View(as.data.frame(dta_shp)[301:400])
#View(as.data.frame(dta_shp)[401:500])
#View(as.data.frame(dta_shp)[501:504])





#scratch-------

#add in area and perimeter for all lands (not just PPTAL) and then merge it into the processed cross-section data
src_Shp <- readShapePoly("/Users/rbtrichler/Documents/AidData/Git Repos/KFW_Amazon/input_data/terra_indigenaPolygon_id.shp")
src_Shp1<-src_Shp@data[,c("terrai_are","terrai_per","id")]
src_Shp1$area_587<-as.numeric(gsub(" Ha","",src_Shp1$terrai_are))
src_Shp1$perim_587<-as.numeric(gsub(" Ha","",src_Shp1$terrai_per))
src_Shp1<-src_Shp1[,3:5]
dta_shp2 <- merge(dta_shp, src_Shp1, by="id")
dta_shp<-dta_shp2
writePolyShape(dta_shp2,"/Users/rbtrichler/Documents/AidData/Git Repos/kfw2_amazon_conflict/Processed_Data/shpfilecross.shp")

