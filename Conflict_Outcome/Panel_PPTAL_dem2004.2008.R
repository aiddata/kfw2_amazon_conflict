
#--------------------------------
#Panel Model
#Outcome:Yearly Count of Land Conflict, 2003-2014
#Trtmnt: PPTAL lands demarcated between 2004 and 2008
#--------------------------------

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
library(multiwayvcov)
library(lmtest)

#load in full panel dataset from processed data folder
panel_data<-read.csv("Processed_Data/panel_data.csv")

#---------------
#Formatting Panel Dataset
#---------------

#subset to PPTAL lands demarcated between 2004 and 2008
panel_data_sub<-panel_data[!is.na(panel_data$demend_y),]
panel_data_sub<-panel_data_sub[panel_data_sub$demend_y>=2004,]
#there should be 23 lands for each year using check below
#table(panel_data_sub$year)

#Remove years of panel dataset outside of 2003-2014(years when record of land conflict outcome exists)
panel_data_sub1<-panel_data[panel_data$year>=2003,]
panel_data_sub1<-panel_data_sub1[panel_data_sub1$year<=2014,]
#table(panel_data_sub1$year)

panel_data<-panel_data_sub1

#Create treatment binary that turns from 0 to 1 in year of demarcation
panel_data$trt_dem<-NA
panel_data$trt_dem[panel_data$year>=panel_data$demend_y]<-1
panel_data$trt_dem[panel_data$year<panel_data$demend_y]<-0

#test treatment binary
#panel_data_sort<-panel_data[order(panel_data$id),]
View(as.data.frame(panel_data_sort)[,100:168])

#---------------------
#Models
#---------------------

Model1<- lm(lfreq ~ trt_dem + factor(reu_id), data=panel_data)
cluster1 <- cluster.vcov(Model1, cbind(panel_data$year, panel_data$reu_id), force_posdef=TRUE)
CMREG1 <- coeftest(Model1, cluster1)

Model2<- lm(lfreq ~ trt_dem + 
              MaxL_ + Pop_ + 
              MeanT_ + MaxT_ + MinT_ +
              MeanP_ + MaxP_ + MinP_ +
              ifreq + ntl_ +
              factor(reu_id),
              data=panel_data)
cluster2 <- cluster.vcov(Model2, cbind(panel_data$year, panel_data$reu_id), force_posdef=TRUE)
CMREG2 <- coeftest(Model2, cluster2)

Model3<- lm(lfreq ~ trt_dem + 
              MaxL_ + Pop_ + 
              MeanT_ + MaxT_ + MinT_ +
              MeanP_ + MaxP_ + MinP_ +
              ifreq + ntl_ +
              year + factor(reu_id),
            data=panel_data)
cluster3 <- cluster.vcov(Model3, cbind(panel_data$year, panel_data$reu_id), force_posdef=TRUE)
CMREG3 <- coeftest(Model3, cluster3)

Model4<- lm(lfreq ~ trt_dem + 
              MaxL_ + Pop_ + 
              MeanT_ + MaxT_ + MinT_ +
              MeanP_ + MaxP_ + MinP_ +
              ifreq + ntl_ +
              factor(year) + factor(reu_id),
            data=panel_data)
cluster4 <- cluster.vcov(Model4, cbind(panel_data$year, panel_data$reu_id), force_posdef=TRUE)
CMREG4 <- coeftest(Model4, cluster4)

