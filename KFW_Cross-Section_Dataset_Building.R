library(maptools)
library(reshape)
library(splitstackshape)
library(ggplot2)

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("C:/Users/jflak/OneDrive/GitHub/")

#clear variables and values
rm(list = ls())


#loads the shape file
shpfile = "KFW_Amazon/processed_data/kfw_analysis_inputs.shp"
shpfile_working = readShapePoly(shpfile)

df_shpfile <- as.data.frame(shpfile_working)

#load saved data
load("kfw2_amazon_conflict/Interim Data/Land Violence Data.Rda")
load("kfw2_amazon_conflict/Interim Data/Individual Violence Data.Rda")
load("kfw2_amazon_conflict/Interim Data/Demarcation Date Data.Rda")
load("kfw2_amazon_conflict/Interim Data/Land Violence Overlap.Rda")
load("kfw2_amazon_conflict/Interim Data/Individual Violence Overlap.Rda")


#renames data_iviolence$ad_id to id_ad
names(data_iviolence)[names(data_iviolence)=="ad_id"] <- "id_ad"
#renames lviolence_uniqueid to ad_id so the the id names match
names(data_lviolence)[names(data_lviolence)=="ad_id"] <- "id_ad"

data_iviolence$year <- as.integer(as.character(data_iviolence$year))
data_lviolence$year <- as.integer(as.character(data_lviolence$year))


#counts frequency of each id_ad in the data (number of incidences of violence per unit)
#iviolence_overlap$freq <- table(data_iviolence$id_ad)
#lviolence_overlap$freq <- table(data_lviolence$id_ad)


data_cross_i <- subset(iviolence_overlap, select = c("id_ad", "freq"))
data_cross_l <- subset(lviolence_overlap, select = c("id_ad", "freq"))

names(data_cross_i)[names(data_cross_i)=="freq"] <- "ifreq_total"
names(data_cross_l)[names(data_cross_l)=="freq"] <- "lfreq_total"



data_cross_i$ifreq2003 <- NA
data_cross_i$ifreq2004 <- NA
data_cross_i$ifreq2005 <- NA
data_cross_i$ifreq2006 <- NA
data_cross_i$ifreq2007 <- NA
data_cross_i$ifreq2008 <- NA
data_cross_i$ifreq2009 <- NA
data_cross_i$ifreq2010 <- NA
data_cross_i$ifreq2011 <- NA
data_cross_i$ifreq2012 <- NA
data_cross_i$ifreq2013 <- NA
data_cross_i$ifreq2014 <- NA


#data_cross_i$ifreq2003[which(data_iviolence$id_ad[year = 2003] != NULL)] <- 1
#data_cross_i$ifreq2003[data_cross_i$id_ad == 5001] <- 1
#data_cross_i$ifreq2003[which(data_cross_i$id_ad == data_iviolence$id_ad[data_iviolence$year == 2003 & !is.null(data_cross_i$id_ad)])] <- 1

#temp_test <- as.vector(which(data_cross_i$id_ad == data_iviolence$id_ad[data_iviolence$year == 2003 & !is.null(data_cross_i$id_ad)]))

#data_cross_i$ifreq2003[which(data_cross_i$id_ad == data_iviolence$id_ad[data_iviolence$year == 2003 & !is.null(data_cross_i)])] <- 1



#data_cross_i$ifreq2003[data_cross_i$id_ad >= 5100] <- 1
#data_cross_i$ifreq2003[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2003])] <- 1
data_cross_i$ifreq2003[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2003])] <- table(data_iviolence$id_ad[data_iviolence$year == 2003])
data_cross_i$ifreq2004[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2004])] <- table(data_iviolence$id_ad[data_iviolence$year == 2004])
data_cross_i$ifreq2005[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2005])] <- table(data_iviolence$id_ad[data_iviolence$year == 2005])
data_cross_i$ifreq2006[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2006])] <- table(data_iviolence$id_ad[data_iviolence$year == 2006])
data_cross_i$ifreq2007[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2007])] <- table(data_iviolence$id_ad[data_iviolence$year == 2007])
data_cross_i$ifreq2008[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2008])] <- table(data_iviolence$id_ad[data_iviolence$year == 2008])
data_cross_i$ifreq2009[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2009])] <- table(data_iviolence$id_ad[data_iviolence$year == 2009])
data_cross_i$ifreq2010[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2010])] <- table(data_iviolence$id_ad[data_iviolence$year == 2010])
data_cross_i$ifreq2011[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2011])] <- table(data_iviolence$id_ad[data_iviolence$year == 2011])
data_cross_i$ifreq2012[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2012])] <- table(data_iviolence$id_ad[data_iviolence$year == 2012])
data_cross_i$ifreq2013[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2013])] <- table(data_iviolence$id_ad[data_iviolence$year == 2013])
data_cross_i$ifreq2014[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2014])] <- table(data_iviolence$id_ad[data_iviolence$year == 2014])

data_cross_l$lfreq2003[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2003])] <- table(data_lviolence$id_ad[data_lviolence$year == 2003])
data_cross_l$lfreq2004[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2004])] <- table(data_lviolence$id_ad[data_lviolence$year == 2004])
data_cross_l$lfreq2005[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2005])] <- table(data_lviolence$id_ad[data_lviolence$year == 2005])
data_cross_l$lfreq2006[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2006])] <- table(data_lviolence$id_ad[data_lviolence$year == 2006])
data_cross_l$lfreq2007[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2007])] <- table(data_lviolence$id_ad[data_lviolence$year == 2007])
data_cross_l$lfreq2008[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2008])] <- table(data_lviolence$id_ad[data_lviolence$year == 2008])
data_cross_l$lfreq2009[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2009])] <- table(data_lviolence$id_ad[data_lviolence$year == 2009])
data_cross_l$lfreq2010[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2010])] <- table(data_lviolence$id_ad[data_lviolence$year == 2010])
data_cross_l$lfreq2011[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2011])] <- table(data_lviolence$id_ad[data_lviolence$year == 2011])
data_cross_l$lfreq2012[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2012])] <- table(data_lviolence$id_ad[data_lviolence$year == 2012])
data_cross_l$lfreq2013[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2013])] <- table(data_lviolence$id_ad[data_lviolence$year == 2013])
data_cross_l$lfreq2014[is.element(data_cross_l$id_ad, data_lviolence$id_ad[data_lviolence$year == 2014])] <- table(data_lviolence$id_ad[data_lviolence$year == 2014])



data_cross_merged <- merge(data_cross_i, data_cross_l, by = "id_ad", all = TRUE)
data_cross_merged <- merge(data_cross_merged, data_demdates, by = "id_ad", all = TRUE)


#data_cross_i$has2003 <- length(which())

iviolence_count2003 <- table(data_iviolence$id_ad[data_iviolence$year == 2003])


temp_val <- sum(data_iviolence$id_ad == 5001)
temp_vector <- sum(data_iviolence$id_ad == data_iviolence$id_ad[1:length(data_iviolence)])














































#iviolence_overlap$freq2003 <- length(which(data_iviolence$id_ad == iviolence_overlap$id_ad))



#data_cross <- as.data.frame(data_demdates$id_ad)
#names(data_cross)[names(data_cross)=="data_demdates$id_ad"] <- "id_ad"
#data_cross["ifreq2003"] <- NA


#data_cross$ifreq2003[match(iviolence_overlap$id_ad, data_demdates$id_ad)]
#data_cross$ifreq2003[data_cross$id_ad == iviolence_overlap$id_ad] <- 5



#data_cross <- as.data.frame(iviolence_overlap$id_ad)

#data_cross = data_demdates
#data_cross$id_ad <- data_iviolence$id_ad


#data_cross$ifreq2003 <- sum(data_cross$id_ad == data_iviolence$id_ad)
#data_cross$ifreq2003 <- table(data_iviolence$id_ad)

#iviolence_overlap$ifreq2003 <- table(data_iviolence$id_ad, exclude = NULL)
#iviolence_overlap$ifreq2003 <- sum(data_iviolence$id_ad == iviolence_overlap$id_ad)

#data_cross$ifreq2004 <- table(data_iviolence$id_ad[year = 2004])
#data_cross$ifreq2005 <- table(data_iviolence$id_ad[year = 2005])
#data_cross$ifreq2006 <- table(data_iviolence$id_ad[year = 2006])
#data_cross$ifreq2007 <- table(data_iviolence$id_ad[year = 2007])
#data_cross$ifreq2008 <- table(data_iviolence$id_ad[year = 2008])
#data_cross$ifreq2009 <- table(data_iviolence$id_ad[year = 2009])
#data_cross$ifreq2010 <- table(data_iviolence$id_ad[year = 2010])
#data_cross$ifreq2011 <- table(data_iviolence$id_ad[year = 2011])
#data_cross$ifreq2012 <- table(data_iviolence$id_ad[year = 2012])
#data_cross$ifreq2013 <- table(data_iviolence$id_ad[year = 2013])
#data_cross$ifreq2014 <- table(data_iviolence$id_ad[year = 2014])

#data_cross$lfreq2003 <- table(data_lviolence$id_ad[year = 2003])
#data_cross$lfreq2004 <- table(data_lviolence$id_ad[year = 2004])
#data_cross$lfreq2005 <- table(data_lviolence$id_ad[year = 2005])
#data_cross$lfreq2006 <- table(data_lviolence$id_ad[year = 2006])
#data_cross$lfreq2007 <- table(data_lviolence$id_ad[year = 2007])
#data_cross$lfreq2008 <- table(data_lviolence$id_ad[year = 2008])
#data_cross$lfreq2009 <- table(data_lviolence$id_ad[year = 2009])
#data_cross$lfreq2010 <- table(data_lviolence$id_ad[year = 2010])
#data_cross$lfreq2011 <- table(data_lviolence$id_ad[year = 2011])
#data_cross$lfreq2012 <- table(data_lviolence$id_ad[year = 2012])
#data_cross$lfreq2013 <- table(data_lviolence$id_ad[year = 2013])
#data_cross$lfreq2014 <- table(data_lviolence$id_ad[year = 2014])


#counts frequency of each id_ad in the data (number of incidences of violence per unit)
#iviolence_overlap$freq <- table(data_iviolence$id_ad)
#lviolence_overlap$freq <- table(data_lviolence$id_ad)

#iviolence_overlap$freq2003 <- table(data_iviolence$id_ad)
#lviolence_overlap$freq2003 <- iviolence_overlap$freq2003 - 
