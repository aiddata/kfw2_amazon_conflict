
#--------------------------------
#Cross-Section Model
#Outcome:Deforestation 1995-2014
#Trtmnt: Demarcated through PPTAL 1995-2008 or never demarcated through PPTAL 1995-2008
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

#Make ndvi differences for 1995-2014
dta_shp$diff_ndvimax <- dta_shp$MaxL_2014 - dta_shp$MaxL_1995

#Make lviolence and iviolence posttrends
dta_shp$posttrend_lviolence <- timeRangeTrend(dta_shp,"lfreq[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp$posttrend_iviolence <- timeRangeTrend(dta_shp,"ifreq[0-9][0-9][0-9][0-9]",2003,2014,"id")

#print(names(dta_shp@data))


#Make a binary for ever demarcated (Treat=1) vs. never demarcated(Treat=0)
dta_shp@data["Treat"] <- 0
dta_shp@data$NA_check <- 0
dta_shp@data$NA_check[is.na(dta_shp@data$demend_y)] <- 1
dta_shp@data$Treat[dta_shp@data$NA_check != 1] <- 1

table(dta_shp@data$Treat)


#Identify set of variables used for 1st stage matching equation
aVars <- c("Treat", "terrai_are", "prelevel_pmean", "prelevel_pmin", "prelevel_pmax", "prelevel_tmean",
           "prelevel_tmin", "prelevel_tmax", "prelevel_ndvimean", "prelevel_ndvimax", "prelevel_ntl",
           "pretrend_pmean", "pretrend_pmin", "pretrend_pmax", "pretrend_tmean", "pretrend_tmin", "pretrend_tmax",
           "pretrend_ndvimean", "pretrend_ndvimax", "pretrend_ntl", "Pop_1995", "Slope", "Elevation", "Riv_Dist", 
           "Road_dist", "id","UF")


#cuts the dataset down to only complete cases (matchit won't work if there are NAs)
#dta_shp1 <- dta_shp[complete.cases(dta_shp@data[aVars]),]
#dta_shp<-dta_shp1

#---------------
####MatchIt####
#---------------

## WITHOUT REPLACEMENT ##
matchit.results <- matchit(Treat ~ terrai_are + prelevel_pmean  + prelevel_tmean +
                             pretrend_pmean + pretrend_pmin + pretrend_pmax + pretrend_tmean + pretrend_tmin + pretrend_tmax + 
                             prelevel_ndvimax + pretrend_ndvimean+ pretrend_ndvimax+
                             prelevel_ntl + pretrend_ntl+
                             Pop_1995 + Slope + Elevation + Riv_Dist + Road_dist,
                           data = dta_shp@data[aVars],
                           method = "nearest", distance="logit", exact="UF",discard="both")

#prints the matchit results
print(summary(matchit.results))

#makes a new dataframe with the matched pair ids, to identify each pair
#dataset includes treatment units that were not matched
df_pairs <- as.data.frame(matchit.results$match.matrix)
#copies 1st column rownames, which are rownames from dta_shp of treated observations (106 units)
df_pairs$treated_obs <- as.numeric(rownames(df_pairs))
#drops rownames column
rownames(df_pairs) <- NULL
#rename column that identifies control unit matched to each treated unit
#if value in "untreated_obs" is NA, then that treatment unit was not matched
#what does it mean if value in "untreated_obs" is -1? They aren't matched either
colnames(df_pairs)[1] <- "untreated_obs"
df_pairs$untreated_obs <- as.numeric(as.character(df_pairs$untreated_obs))
#Create 3rd column "pair_id" which assigns id to each matched set of treatment and control units
df_pairs$pair_id <- as.numeric(rownames(df_pairs))
#replicate pair_id so it's associated separately with treatment and control rowname, which is now "id" field
#dataset still includes treatment units that were not matched
df_pairs <- data.frame(id = c(df_pairs$untreated_obs, df_pairs$treated_obs), pair_id = c(df_pairs$pair_id, df_pairs$pair_id))
#df_pairs$id comes from dta_shp$rownames, which is off from dta_shp$id by 1, so add 1 to df_pairs$id so that it matches dta_shp$id
#will need this for merge later
df_pairs$id <- df_pairs$id + 1
#View(df_pairs)

#df_test <- data.frame(x = c(df_pairs$untreated_obs, df_pairs$treated_obs), y = c(df_pairs$pair_id, df_pairs$pair_id))
#View(df_test)

#create subset of dta_shp with only the matched data
modelData_norep <- match.data(matchit.results)
dta_shp_subset <- dta_shp
dta_shp_subset@data$id_present <- (dta_shp_subset$id %in% modelData_norep$id)
dta_shp_subset@data <- dta_shp_subset@data[dta_shp_subset$id_present == TRUE,]

#adds id, lfreq_tota, and pair_id variables to modelData, as well as all the posttrends
modelData_norep <- merge.default(modelData_norep, dta_shp_subset@data[c("posttrend_pmean", "posttrend_pmin", "posttrend_pmax", 
                                                     "posttrend_tmean", "posttrend_tmin", "posttrend_tmax", 
                                                     "posttrend_ntl", "id", "lfreq_tota", "ifreq_tota", 
                                                     "posttrend_lviolence", "posttrend_iviolence", 
                                                     "posttrend_ndvimean", "posttrend_ndvimax", "diff_ndvimax")], by = "id")
modelData_norep <- merge.default(modelData_norep, df_pairs, by = "id")


## WITH REPLACEMENT ##

#Run matchit
matchit.replace <- matchit(Treat ~ terrai_are + prelevel_pmean  + prelevel_tmean +
                             pretrend_pmean + pretrend_pmin + pretrend_pmax + pretrend_tmean + pretrend_tmin + pretrend_tmax + 
                             prelevel_ndvimax + pretrend_ndvimean+ pretrend_ndvimax+
                             prelevel_ntl + pretrend_ntl+
                             Pop_1995 + Slope + Elevation + Riv_Dist + Road_dist,
                           data = dta_shp@data[aVars],
                           method = "nearest", distance="logit",replace=TRUE, exact="UF",discard="both")


#prints the matchit results
print(summary(matchit.replace))

#don't need to create pair ids because can't use pair fixed effects when matching with replacement
#will use weights generated in match.data instead
#create subset of dta_shp with only the matched data
modelData_rep <- match.data(matchit.replace)
dta_shp_subset_rep <- dta_shp
dta_shp_subset_rep@data$id_present <- (dta_shp_subset_rep$id %in% modelData_rep$id)
dta_shp_subset_rep@data <- dta_shp_subset_rep@data[dta_shp_subset_rep$id_present == TRUE,]

#adds id, lfreq_tota, and pair_id variables to modelData, as well as all the posttrends
modelData_rep <- merge.default(modelData_rep, dta_shp_subset_rep@data[c("posttrend_pmean", "posttrend_pmin", "posttrend_pmax", 
                                                            "posttrend_tmean", "posttrend_tmin", "posttrend_tmax", 
                                                            "posttrend_ntl", "id", "lfreq_tota", "ifreq_tota", 
                                                            "posttrend_lviolence", "posttrend_iviolence", 
                                                            "posttrend_ndvimean", "posttrend_ndvimax", "diff_ndvimax")], by = "id")


#-----------------------
####Start of Models####
#----------------------

##Without replacement

model1 <- lm(diff_ndvimax ~ Treat,
                       data = modelData_norep)
print(summary(model1))
print('______________________________________________________________________________________', quote = FALSE)


model1.1 <- lm(diff_ndvimax ~ Treat + factor(pair_id),
                     data = modelData_norep)
print(summary(model1.1))
print('______________________________________________________________________________________', quote = FALSE)


model2<-lm(diff_ndvimax ~ Treat + terrai_are +
           prelevel_ndvimax + pretrend_ndvimax+
             Pop_1995 + prelevel_tmean + prelevel_pmean + prelevel_ntl+
             posttrend_tmean + posttrend_pmean+posttrend_ntl+
             Slope +Elevation + Riv_Dist + Road_dist+
             factor(pair_id), 
           data=modelData_norep)
print(summary(model2))


model3<-lm(diff_ndvimax ~ Treat + terrai_are+
           prelevel_ndvimax + pretrend_ndvimax+
           Pop_1995 + prelevel_tmean + prelevel_pmean + prelevel_ntl+
           posttrend_tmean + posttrend_pmean+posttrend_ntl+
           lfreq_tota + ifreq_tota+
           Slope +Elevation + Riv_Dist + Road_dist+
           factor(pair_id), data=modelData_norep)
print(summary(model3))

## With Replacement

model1R <- lm(diff_ndvimax ~ Treat,
                       data = modelData_rep)
print(summary(model1R))
print('______________________________________________________________________________________', quote = FALSE)


model1.1R <- lm(diff_ndvimax ~ Treat,
                     data = modelData_rep, weights=(distance))
print(summary(model1.1R))
print('______________________________________________________________________________________', quote = FALSE)


model2R<-lm(diff_ndvimax ~ Treat + terrai_are +
             prelevel_ndvimax + pretrend_ndvimax+
             Pop_1995 + prelevel_tmean + prelevel_pmean + prelevel_ntl+
             posttrend_tmean + posttrend_pmean+posttrend_ntl+
             Slope +Elevation + Riv_Dist + Road_dist, 
           data=modelData_rep, weights=(distance))
print(summary(model2R))


model3R<-lm(diff_ndvimax ~ Treat + terrai_are+
             prelevel_ndvimax + pretrend_ndvimax+
             Pop_1995 + prelevel_tmean + prelevel_pmean + prelevel_ntl+
             posttrend_tmean + posttrend_pmean+posttrend_ntl+
             lfreq_tota + ifreq_tota+
             Slope +Elevation + Riv_Dist + Road_dist, 
            data=modelData_rep, weights=(distance))
print(summary(model3R))
            
           
#--------------
# STARGAZER
#--------------

stargazer(model1.1,model2, model3, model1.1R,model2R,model3R,
          omit=c("factor"),
#           keep=c("Treat","terrai_are","Pop_1990","MeanT_1995","post_trend_temp","MeanP_1995",
#                  "post_trend_precip","Slope","Elevation","Riv_Dist","Road_dist"),
#           covariate.labels=c("Treatment", "Pre-Trend NDVI", "Baseline NDVI", "Area (hectares)","Baseline Population Density",
#                              "Baseline Temperature", "Temperature Trends", "Precipitation Trends","Baseline Precipitation", 
#                              "Slope", "Elevation", "Distance to River", "Distance to Road"),
          dep.var.labels=c("Change in Max NDVI 1995-2014"),
          title="Regression Results", type="html", omit.stat=c("f","ser"), align=TRUE)




