
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

#load in full panel dataset from processed data folder
panel_data<-read.csv("Processed_Data/panel_data.csv")

#