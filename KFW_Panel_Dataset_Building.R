#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
#setwd("/home/aiddata/Desktop/Github/kfw2_amazon_conflict/")
#setwd("C:/Users/jflak/OneDrive/GitHub/kfw2_amazon_conflict/")
setwd("/Users/rbtrichler/Documents/AidData/Git Repos/kfw2_amazon_conflict")


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

#fixes a few mistakes in variable names (if they're not already fixed)
names(dta_shp@data)[names(dta_shp@data)=="ifreq11"] <- "ifreq2011"
names(dta_shp@data)[names(dta_shp@data)=="lfreq11"] <- "lfreq2011"
names(dta_shp@data)[names(dta_shp@data)=="ntl_11"] <- "ntl_2011"

#fills in 0s for NAs in lfreq_tota
dta_shp@data$lfreq_tota[is.na(dta_shp@data$lfreq_tota)] <- 0

#Impute 2014 ntl value
dta_shp@data$ntl_2013[is.na(dta_shp@data$ntl_2013)] <- 5555
dta_shp@data$ntl2014trend <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",2009,2013,"id")
dta_shp@data$ntl2014trend[dta_shp@data$ntl2014trend == "NaN"] <- NA
dta_shp@data$ntl_2014 <- (dta_shp@data$ntl2014trend + dta_shp@data$ntl_2013)
dta_shp@data$ntl_2014[dta_shp@data$ntl_2014 < 0] <- 0
dta_shp@data$ntl_2013[dta_shp@data$ntl_2013 == 5555] <- NA


#Fills in the previous Pop value for the missing Pop values
dta_shp@data$Pop_1991 <- dta_shp@data$Pop_1990
dta_shp@data$Pop_1992 <- dta_shp@data$Pop_1990
dta_shp@data$Pop_1993 <- dta_shp@data$Pop_1990
dta_shp@data$Pop_1994 <- dta_shp@data$Pop_1990

dta_shp@data$Pop_1996 <- dta_shp@data$Pop_1995
dta_shp@data$Pop_1997 <- dta_shp@data$Pop_1995
dta_shp@data$Pop_1998 <- dta_shp@data$Pop_1995
dta_shp@data$Pop_1999 <- dta_shp@data$Pop_1995

dta_shp@data$Pop_2001 <- dta_shp@data$Pop_2000
dta_shp@data$Pop_2002 <- dta_shp@data$Pop_2000
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

dta_shp@data$Pop_2016 <- dta_shp@data$Pop_2015
dta_shp@data$Pop_2017 <- dta_shp@data$Pop_2015
dta_shp@data$Pop_2018 <- dta_shp@data$Pop_2015
dta_shp@data$Pop_2019 <- dta_shp@data$Pop_2015


#Fills in some variables to fix an error that reshape throws
dta_shp@data$ifreq1982 <- NA
dta_shp@data$ifreq1983 <- NA
dta_shp@data$ifreq1984 <- NA
dta_shp@data$ifreq1985 <- NA
dta_shp@data$ifreq1986 <- NA
dta_shp@data$ifreq1987 <- NA
dta_shp@data$ifreq1988 <- NA
dta_shp@data$ifreq1989 <- NA
dta_shp@data$ifreq1990 <- NA
dta_shp@data$ifreq1991 <- NA
dta_shp@data$ifreq1992 <- NA
dta_shp@data$ifreq1993 <- NA
dta_shp@data$ifreq1994 <- NA
dta_shp@data$ifreq1995 <- NA
dta_shp@data$ifreq1996 <- NA
dta_shp@data$ifreq1997 <- NA
dta_shp@data$ifreq1998 <- NA
dta_shp@data$ifreq1999 <- NA
dta_shp@data$ifreq2000 <- NA
dta_shp@data$ifreq2001 <- NA
dta_shp@data$ifreq2002 <- NA

dta_shp@data$ifreq2015 <- NA
dta_shp@data$ifreq2016 <- NA
dta_shp@data$ifreq2017 <- NA
dta_shp@data$ifreq2018 <- NA
dta_shp@data$ifreq2019 <- NA
dta_shp@data$ifreq2020 <- NA

dta_shp@data$lfreq1982 <- NA
dta_shp@data$lfreq1983 <- NA
dta_shp@data$lfreq1984 <- NA
dta_shp@data$lfreq1985 <- NA
dta_shp@data$lfreq1986 <- NA
dta_shp@data$lfreq1987 <- NA
dta_shp@data$lfreq1988 <- NA
dta_shp@data$lfreq1989 <- NA
dta_shp@data$lfreq1990 <- NA
dta_shp@data$lfreq1991 <- NA
dta_shp@data$lfreq1992 <- NA
dta_shp@data$lfreq1993 <- NA
dta_shp@data$lfreq1994 <- NA
dta_shp@data$lfreq1995 <- NA
dta_shp@data$lfreq1996 <- NA
dta_shp@data$lfreq1997 <- NA
dta_shp@data$lfreq1998 <- NA
dta_shp@data$lfreq1999 <- NA
dta_shp@data$lfreq2000 <- NA
dta_shp@data$lfreq2001 <- NA
dta_shp@data$lfreq2002 <- NA

dta_shp@data$lfreq2015 <- NA
dta_shp@data$lfreq2016 <- NA
dta_shp@data$lfreq2017 <- NA
dta_shp@data$lfreq2018 <- NA
dta_shp@data$lfreq2019 <- NA
dta_shp@data$lfreq2020 <- NA

dta_shp@data$MaxL_2015 <- NA
dta_shp@data$MaxL_2016 <- NA
dta_shp@data$MaxL_2017 <- NA
dta_shp@data$MaxL_2018 <- NA
dta_shp@data$MaxL_2019 <- NA
dta_shp@data$MaxL_2020 <- NA

dta_shp@data$MaxP_2015 <- NA
dta_shp@data$MaxP_2016 <- NA
dta_shp@data$MaxP_2017 <- NA
dta_shp@data$MaxP_2018 <- NA
dta_shp@data$MaxP_2019 <- NA
dta_shp@data$MaxP_2020 <- NA

dta_shp@data$MaxT_2015 <- NA
dta_shp@data$MaxT_2016 <- NA
dta_shp@data$MaxT_2017 <- NA
dta_shp@data$MaxT_2018 <- NA
dta_shp@data$MaxT_2019 <- NA
dta_shp@data$MaxT_2020 <- NA

dta_shp@data$MeanL_2015 <- NA
dta_shp@data$MeanL_2016 <- NA
dta_shp@data$MeanL_2017 <- NA
dta_shp@data$MeanL_2018 <- NA
dta_shp@data$MeanL_2019 <- NA
dta_shp@data$MeanL_2020 <- NA

dta_shp@data$MeanP_2015 <- NA
dta_shp@data$MeanP_2016 <- NA
dta_shp@data$MeanP_2017 <- NA
dta_shp@data$MeanP_2018 <- NA
dta_shp@data$MeanP_2019 <- NA
dta_shp@data$MeanP_2020 <- NA

dta_shp@data$MeanT_2015 <- NA
dta_shp@data$MeanT_2016 <- NA
dta_shp@data$MeanT_2017 <- NA
dta_shp@data$MeanT_2018 <- NA
dta_shp@data$MeanT_2019 <- NA
dta_shp@data$MeanT_2020 <- NA

dta_shp@data$MinP_2015 <- NA
dta_shp@data$MinP_2016 <- NA
dta_shp@data$MinP_2017 <- NA
dta_shp@data$MinP_2018 <- NA
dta_shp@data$MinP_2019 <- NA
dta_shp@data$MinP_2020 <- NA

dta_shp@data$MinT_2015 <- NA
dta_shp@data$MinT_2016 <- NA
dta_shp@data$MinT_2017 <- NA
dta_shp@data$MinT_2018 <- NA
dta_shp@data$MinT_2019 <- NA
dta_shp@data$MinT_2020 <- NA

dta_shp@data$ntl_1982 <- NA
dta_shp@data$ntl_1983 <- NA
dta_shp@data$ntl_1984 <- NA
dta_shp@data$ntl_1985 <- NA
dta_shp@data$ntl_1986 <- NA
dta_shp@data$ntl_1987 <- NA
dta_shp@data$ntl_1988 <- NA
dta_shp@data$ntl_1989 <- NA
dta_shp@data$ntl_1990 <- NA
dta_shp@data$ntl_1991 <- NA

#dta_shp@data$ntl_2014 <- NA
dta_shp@data$ntl_2015 <- NA
dta_shp@data$ntl_2016 <- NA
dta_shp@data$ntl_2017 <- NA
dta_shp@data$ntl_2018 <- NA
dta_shp@data$ntl_2019 <- NA
dta_shp@data$ntl_2020 <- NA

dta_shp@data$Pop_1982 <- NA
dta_shp@data$Pop_1983 <- NA
dta_shp@data$Pop_1984 <- NA
dta_shp@data$Pop_1985 <- NA
dta_shp@data$Pop_1986 <- NA
dta_shp@data$Pop_1987 <- NA
dta_shp@data$Pop_1988 <- NA
dta_shp@data$Pop_1989 <- NA


#Orders the dataframe columns alphabetically
#dta_shp@data <- as.data.frame(dta_shp@data)[order(names(dta_shp@data))]

#List the names of the variables in dta_shp
#print(names(dta_shp@data))

#shows what variables are taken by reshape in varying
#print(grep("(.+)[0-9][0-9][0-9][0-9]$", names(dta_shp), value = TRUE))

#Reshapes the dataframe from wide to long
panel_data <- reshape(dta_shp@data, dir = "long", varying = grep("(.+)[0-9][0-9][0-9][0-9]$", names(dta_shp), value = TRUE),
                      v.names = c("Pop_", "MeanL_", "MaxL_", "MeanT_", "MaxT_", "MinT_", "MeanP_", "MaxP_", "MinP_",
                                  "ifreq", "lfreq", "ntl_"), times = 1982:2020)
#scratch
panel_data1 <- reshape(data, dir = "long", varying = grep("(.+)[0-9][0-9][0-9][0-9]$", names(data), value = TRUE),
                      v.names = c("Pop_", "MeanL_", "MaxL_", "MeanT_", "MaxT_", "MinT_", "MeanP_", "MaxP_", "MinP_",
                                  "ifreq", "lfreq", "ntl_"), idvar="id",timevar="year")

data<-dta_shp@data

all_reshape <- c(Pop,MeanL, MaxL, MeanT,MaxT,MinT,MeanP,MaxP,MinP,ifreq,lfreq,ntl)
DFa4 <- reshape(data, varying=c("Pop", "MeanL", "MaxL", "MeanT", "MaxT", "MinT", "MeanP", "MaxP", "MinP",
                              "ntl"),direction="long", idvar="id", sep="_", timevar="year")


all_reshape <- c(PCloss, mean_ln, minairTemp, maxairTemp, meanairTemp, minPre, maxPre, meanPre, MinDist, DecayDist, ProjCount, DecayDist100)
DFa4 <- reshape(DFa3, varying=all_reshape,direction="long", idvar="ID", sep="_", timevar="Year")
#------

View(as.data.frame(panel_data)[1:100])
View(as.data.frame(panel_data)[101:166])

panel_data2<-panel_data[panel_data$time>=2003,]
panel_data2<-panel_data2[panel_data2$time<=2014,]

#Rename variable "time" to "year"
names(panel_data)[names(panel_data)=="time"]="year"

write.csv(panel_data,"/Users/rbtrichler/Documents/AidData/Git Repos/kfw2_amazon_conflict/Processed_Data/panel_data.csv")

#View(as.data.frame(dta_shp)[1:100])
#View(as.data.frame(dta_shp)[101:200])
#View(as.data.frame(dta_shp)[201:300])
#View(as.data.frame(dta_shp)[301:400])
#View(as.data.frame(dta_shp)[401:469])