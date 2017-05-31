#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
#setwd("/home/aiddata/Desktop/Github/kfw2_amazon_conflict/")
setwd("C:/Users/jflak/OneDrive/GitHub/kfw2_amazon_conflict/")
#setwd("/Users/rbtrichler/Documents/AidData/Git Repos/kfw2_amazon_conflict")


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
#df_dta_shp = as.data.frame(dta_shp)

#deletes the Pop_2000_x variable and renames Pop_2000_y to Pop_2000
dta_shp@data$Pop_2000_x <- NULL
names(dta_shp@data)[names(dta_shp@data)=="Pop_2000_y"] <- "Pop_2000"

#fills in 0s for NAs in lfreq_tota and ifreq_tota
dta_shp@data$lfreq_tota[is.na(dta_shp@data$lfreq_tota)] <- 0
dta_shp@data$ifreq_tota[is.na(dta_shp@data$ifreq_tota)]<-0

#Impute 2014 ntl value
dta_shp@data$ntl_2013[is.na(dta_shp@data$ntl_2013)] <- 5555
dta_shp@data$ntl2014trend <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",2009,2013,"id")
dta_shp@data$ntl2014trend[dta_shp@data$ntl2014trend == "NaN"] <- NA
dta_shp@data$ntl_2014 <- (dta_shp@data$ntl2014trend + dta_shp@data$ntl_2013)
dta_shp@data$ntl_2014[dta_shp@data$ntl_2014 < 0] <- 0
dta_shp@data$ntl_2013[dta_shp@data$ntl_2013 == 5555] <- NA


#Fills in the previous Pop value for the missing Pop values
dta_shp@data$Pop_2003 <- dta_shp@data$Pop_2000
dta_shp@data$Pop_2004 <- dta_shp@data$Pop_2000

dta_shp@data$Pop_2006 <- dta_shp@data$Pop_2005
dta_shp@data$Pop_2007 <- dta_shp@data$Pop_2005
dta_shp@data$Pop_2008 <- dta_shp@data$Pop_2005
dta_shp@data$Pop_2009 <- dta_shp@data$Pop_2005

dta_shp@data$Pop_2011 <- dta_shp@data$Pop_2010
dta_shp@data$Pop_2012 <- dta_shp@data$Pop_2010
dta_shp@data$Pop_2013 <- dta_shp@data$Pop_2010
dta_shp@data$Pop_2014 <- dta_shp@data$Pop_2010


#Turn dta_shp@data into dataframe
df<-dta_shp@data

#Drop all variables for years outside of 2003-2014
df<- df[, -grep("(19[0-9][0-9])", names(df))]
df<- df[, -grep("(2000)", names(df))]
df<- df[, -grep("(2001)", names(df))]
df<- df[, -grep("(2002)", names(df))]
df<- df[, -grep("(2015)", names(df))]
df<- df[, -grep("(2020)", names(df))]

#Drop all AvgD and AvgDist varialbes (many NAs, not used for analysis)
df<-df[,-grep("(AvgD)",names(df))]

#Add underscore to ifreq and lfreq variables to match other variables with yearly measures

for (i in 2:length(df))
{
  colnames(df)[i] <- sub("lfreq","lfreq_",colnames(df)[i])
}

for (i in 2:length(df))
{
  colnames(df)[i] <- sub("ifreq","ifreq_",colnames(df)[i])
}


#Orders the dataframe columns alphabetically - DO NOT REMOVE, this is key to getting reshape to work correctly
df <- as.data.frame(df)[order(names(df))]

#List the names of the variables in dta_shp
#print(names(df))

#shows what variables are taken by reshape in varying
print(grep("(.+)[0-9][0-9][0-9][0-9]$", names(df), value = TRUE))

#-----------------------------------
#Reshapes the dataframe from wide to long
#-----------------------------------

# panel_data <- reshape(dta_shp@data, dir = "long", varying = grep("(.+)[0-9][0-9][0-9][0-9]$", names(dta_shp), value = TRUE),
#                       v.names = c("Pop_", "MeanL_", "MaxL_", "MeanT_", "MaxT_", "MinT_", "MeanP_", "MaxP_", "MinP_",
#                                   "ifreq", "lfreq", "ntl_"), times = 1982:2020)

panel_data<-reshape(df,dir="long",varying=grep("(.+)[0-9][0-9][0-9][0-9]$", names(df), value = TRUE),sep="_")

View(as.data.frame(panel_data)[1:100])
View(as.data.frame(panel_data)[101:150])

#Rename variable "time" to "year"
names(panel_data)[names(panel_data)=="time"]="year"

#Replace NAs in ifreq and lfreq with 0
panel_data$ifreq[is.na(panel_data$ifreq)]<-0
panel_data$lfreq[is.na(panel_data$lfreq)]<-0

#Save panel data
write.csv(panel_data,"C:/Users/jflak/OneDrive/GitHub/kfw2_amazon_conflict/Processed_Data/panel_data.csv")



#View(as.data.frame(dta_shp)[1:100])
#View(as.data.frame(dta_shp)[101:200])
#View(as.data.frame(dta_shp)[201:300])
#View(as.data.frame(dta_shp)[301:400])
#View(as.data.frame(dta_shp)[401:469])