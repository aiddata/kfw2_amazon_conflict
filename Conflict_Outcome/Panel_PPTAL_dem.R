
#--------------------------------
#Panel Model
#Outcome:Yearly Count of Land Conflict, 2003-2014
#Trtmnt: PPTAL lands ever demarcated 
#--------------------------------

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
library(multiwayvcov)
library(lmtest)
library(stargazer)

#load in full panel dataset from processed data folder
panel_data<-read.csv("Processed_Data/panel_data.csv")

#---------------
#Formatting Panel Dataset
#---------------

#subset to PPTAL lands ever demarcated 
panel_data_sub<-panel_data[!is.na(panel_data$demend_y),]
#subset to all lands demarcated before 2003, so treatment var in panel is years post demarcation
panel_data_sub<-panel_data_sub[panel_data_sub$demend_y<2003,]
#there should be 66 lands for each year using check below
#table(panel_data_sub$year)

panel_data<-panel_data_sub

#Create treatment variable that measures years after demarcation
panel_data$trt_dem<-NA
panel_data$trt_dem<-panel_data$year-panel_data$demend_y

#test treatment var
panel_data_sort<-panel_data[order(panel_data$id),]
View(as.data.frame(panel_data_sort)[,100:149])

#---------------------
#Models
#---------------------

Model1<- lm(lfreq ~ trt_dem + factor(id), data=panel_data)
cluster1 <- cluster.vcov(Model1, cbind(panel_data$year, panel_data$id), force_posdef=TRUE)
CMREG1 <- coeftest(Model1, cluster1)

Model2<- lm(lfreq ~ trt_dem + ifreq+
              Pop + 
              ntl +
              factor(id),
            data=panel_data)
cluster2 <- cluster.vcov(Model2, cbind(panel_data$year, panel_data$id), force_posdef=TRUE)
CMREG2 <- coeftest(Model2, cluster2)

Model3<- lm(lfreq ~ trt_dem + ifreq+
              Pop + 
              ntl+
              year + factor(id),
            data=panel_data)
cluster3 <- cluster.vcov(Model3, cbind(panel_data$year, panel_data$id), force_posdef=TRUE)
CMREG3 <- coeftest(Model3, cluster3)

Model4<- lm(lfreq ~ trt_dem + ifreq+
              Pop + 
              ntl +
              factor(year) + factor(id),
            data=panel_data)
cluster4 <- cluster.vcov(Model4, cbind(panel_data$year, panel_data$id), force_posdef=TRUE)
CMREG4 <- coeftest(Model4, cluster4)



#-------------------
#Stargazer
#------------------

stargazer(CMREG1,CMREG2,CMREG3,CMREG4,
          type="html", align=TRUE,
          omit=c("factor"),
          omit.stat=c("f","ser"),
          covariate.labels=c("Years Since Demarcation","Individual Violence",
                             "Population","Nighttime Lights","Year"),
          add.lines=list(c("Observations","792","792","792","792"),
                         c("Community Fixed Effects?","Yes","Yes","Yes","Yes"),
                         c("Year Fixed Effects?","No","No","No","Yes")),
          title="PPTAL Regression Results: Demarcation Pre-2003",
          dep.var.labels=c("Land Conflict"))

