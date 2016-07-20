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

#makes a dataframe of the shapefile data
df_shpfile <- as.data.frame(shpfile_working)

#load saved data
load("kfw2_amazon_conflict/Interim Data/Land Violence Data.Rda")
load("kfw2_amazon_conflict/Interim Data/Individual Violence Data.Rda")
load("kfw2_amazon_conflict/Interim Data/Demarcation Date Data.Rda")
load("kfw2_amazon_conflict/Interim Data/Land Violence Overlap.Rda")
load("kfw2_amazon_conflict/Interim Data/Individual Violence Overlap.Rda")

#reads the population and nighttime lights data from file
data_pop_nlights <- read.csv(file = "kfw2_amazon_conflict/Raw and Original Data/merge_terra_indigenaPolygon_id_thin.csv", header=TRUE, sep = ',')


#EXAMPLE for counting frequency of violence - from Preliminary_Delimitation_and_Conflict_Data_Analysis.R
#counts frequency of each id_ad in the data (number of incidences of violence per unit)
#iviolence_overlap$freq <- table(data_iviolence$id_ad)
#lviolence_overlap$freq <- table(data_lviolence$id_ad)


#renames data_iviolence$ad_id to id_ad
names(data_iviolence)[names(data_iviolence)=="ad_id"] <- "id_ad"
#renames lviolence_uniqueid to ad_id so the the id names match
names(data_lviolence)[names(data_lviolence)=="ad_id"] <- "id_ad"

#changes the year variables in data_iviolence and data_lviolence from factors to integers
data_iviolence$year <- as.integer(as.character(data_iviolence$year))
data_lviolence$year <- as.integer(as.character(data_lviolence$year))


#TEST for counting violence incidents for one year - not necessary code for final product
#iviolence_count2003 <- table(data_iviolence$id_ad[data_iviolence$year == 2003])


#Creates new dataframes with iviolence and lviolence data and appropriately renames "freq" variable in each
data_cross_i <- subset(iviolence_overlap, select = c("id_ad", "freq"))
data_cross_l <- subset(lviolence_overlap, select = c("id_ad", "freq"))
names(data_cross_i)[names(data_cross_i)=="freq"] <- "ifreq_total"
names(data_cross_l)[names(data_cross_l)=="freq"] <- "lfreq_total"


#TEST for creating new columns with violence counts - not necessary code for final product
#data_cross_i$ifreq2003[data_cross_i$id_ad >= 5100] <- 1
#data_cross_i$ifreq2003[is.element(data_cross_i$id_ad, data_iviolence$id_ad[data_iviolence$year == 2003])] <- 1


#Counts the number of incidences of violence for each community in each year, and makes new columns for each year
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


#Creates a merged dataset with the individual and land data, as well as the demarcation dates
data_cross_merged <- merge(data_cross_i, data_cross_l, by = "id_ad", all = TRUE)
data_cross_merged <- merge(data_cross_merged, data_demdates, by = "id_ad", all = TRUE)


#renames data_pop_nlights$id to data_pop_nlights$id_587
names(data_pop_nlights)[names(data_pop_nlights)=="id"] <- "id_587"

#Merges previous merged dataset with data_pop_nlights
data_cross_merged <- merge(data_cross_merged, data_pop_nlights, by = "id_587")

#Renames gpw4_*e to Pop_*e (in data_cross_merged)
names(data_cross_merged) <- gsub("gpw4_*", "Pop_", names(data_cross_merged))
#Removes the "e" from the end of the Pop variables
names(data_cross_merged) <- gsub("(Pop_....)e", "\\1", names(data_cross_merged))
#Renames ncc4_*e to ntl_*e (in data_cross_merged)
names(data_cross_merged) <- gsub("ncc4_", "ntl_", names(data_cross_merged))
#Removes the "e" from the end of the ntl variables
names(data_cross_merged) <- gsub("(ntl_....)e", "\\1", names(data_cross_merged))


#Views relevant dataframes
View(data_cross_merged)
View(data_cross_merged[101:132])




