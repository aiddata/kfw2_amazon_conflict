#clear variables and values
rm(list=ls())

#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
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
#df_dta_shp = as.data.frame(dta_shp)

#deletes the Pop_2000_x variable and renames Pop_2000_y to Pop_2000
dta_shp@data$Pop_2000_x <- NULL
names(dta_shp@data)[names(dta_shp@data)=="Pop_2000_y"] <- "Pop_2000"

#fixes a few mistakes in variable names
names(dta_shp@data)[names(dta_shp@data)=="ifreq11"] <- "ifreq2011"
names(dta_shp@data)[names(dta_shp@data)=="lfreq11"] <- "lfreq2011"
names(dta_shp@data)[names(dta_shp@data)=="ntl_11"] <- "ntl_2011"

####Creating Pre, Post, etc. Variables####
#Make Pre-Level Values (2003)
dta_shp@data$prelevel_pmean <- dta_shp@data$MeanP_2003
dta_shp@data$prelevel_pmin <- dta_shp@data$MinP_2003
dta_shp@data$prelevel_pmax <- dta_shp@data$MaxP_2003

dta_shp@data$prelevel_tmean <- dta_shp@data$MeanT_2003
dta_shp@data$prelevel_tmin <- dta_shp@data$MinT_2003
dta_shp@data$prelevel_tmax <- dta_shp@data$MaxT_2003

dta_shp@data$prelevel_ndvimean <- dta_shp@data$MeanL_2003
dta_shp@data$prelevel_ndvimax <- dta_shp@data$MaxL_2003

dta_shp@data$prelevel_ntl <- dta_shp@data$ntl_2003

dta_shp@data$prelevel_iviolence <- dta_shp@data$ifreq2003
dta_shp@data$prelevel_iviolence[is.na(dta_shp@data$ifreq2003)] <- 0
dta_shp@data$prelevel_lviolence <- dta_shp@data$lfreq2003
dta_shp@data$prelevel_lviolence[is.na(dta_shp@data$lfreq2003)] <- 0


#fills in 0s for NAs in lfreq_tota
dta_shp@data$lfreq_tota[is.na(dta_shp@data$lfreq_tota)] <- 0

#fills in 0s for NAs in ntl (but doesn't work right now)
#dta_shp@data[names(dta_shp@data) == "ntl[0-9][0-9][0-9][0-9]"][is.na(dta_shp@data$lfreq_tota)] <- 0


#Make Pre-Trend Values (1982-2003)
dta_shp@data$pretrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp@data$pretrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp@data$pretrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",1982,2003,"id")

dta_shp@data$pretrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp@data$pretrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp@data$pretrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",1982,2003,"id")

dta_shp@data$pretrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,2003,"id")
dta_shp@data$pretrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",1982,2003,"id")
#This is the nighttime lights pretrend. Note that it only runs 1992-2003, not 1982-2003
dta_shp@data$pretrend_ntl <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",1992,2003,"id")


#Make Post-Trend Values
dta_shp@data$posttrend_pmean <- timeRangeTrend(dta_shp,"MeanP_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp@data$posttrend_pmin <- timeRangeTrend(dta_shp,"MinP_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp@data$posttrend_pmax <- timeRangeTrend(dta_shp,"MaxP_[0-9][0-9][0-9][0-9]",2003,2014,"id")

dta_shp@data$posttrend_tmean <- timeRangeTrend(dta_shp,"MeanT_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp@data$posttrend_tmin <- timeRangeTrend(dta_shp,"MinT_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp@data$posttrend_tmax <- timeRangeTrend(dta_shp,"MaxT_[0-9][0-9][0-9][0-9]",2003,2014,"id")

dta_shp@data$posttrend_ndvimean <- timeRangeTrend(dta_shp,"MeanL_[0-9][0-9][0-9][0-9]",2003,2014,"id")
dta_shp@data$posttrend_ndvimax <- timeRangeTrend(dta_shp,"MaxL_[0-9][0-9][0-9][0-9]",2003,2014,"id")

#no ntl data for 2014 yet, so posttrend only goes through 2013
dta_shp@data$posttrend_ntl <- timeRangeTrend(dta_shp,"ntl_[0-9][0-9][0-9][0-9]",2003,2013,"id")

#Make Pop pretrend value
dta_shp@data$pretrend_pop <- dta_shp@data$Pop_2000 - dta_shp@data$Pop_1990



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

dta_shp@data$ntl_2014 <- NA
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
dta_shp@data <- as.data.frame(dta_shp@data)[order(names(dta_shp@data))]

#List the names of the variables in dta_shp
#print(names(dta_shp@data))

#shows what variables are taken by reshape in varying
#print(grep("(.+)[0-9][0-9][0-9][0-9]$", names(dta_shp), value = TRUE))

#Reshapes the dataframe from wide to long
dta_shp@data <- reshape(dta_shp@data, dir = "long", varying = grep("(.+)[0-9][0-9][0-9][0-9]$", names(dta_shp), value = TRUE), v.names = c("Pop_", "MeanL_", "MaxL_", "MeanT_", "MaxT_", "MinT_", "MeanP_", "MaxP_", "MinP_", "ifreq", "lfreq", "ntl_"), times = 1982:2020)



View(as.data.frame(dta_shp)[1:100])
View(as.data.frame(dta_shp)[101:165])



#View(as.data.frame(dta_shp)[1:100])
#View(as.data.frame(dta_shp)[101:200])
#View(as.data.frame(dta_shp)[201:300])
#View(as.data.frame(dta_shp)[301:400])
#View(as.data.frame(dta_shp)[401:469])



#melt.data.frame(df_dta_shp)

#tmp = reshape(df_dta_shp, dir = "long", varying = 4:60 , v.names = "Pop_")
#View(tmp[1:100])
#View(tmp[101:200])
#View(tmp[201:300])
#View(tmp[301:400])
#View(tmp[401:466])

#tmp2 <- melt(df_dta_shp, id=c("SP_ID", "id", "SP_ID_1", "Slope", "Elevation", "UrbTravTim", "Riv_Dist", "Road_dist", "reu_id", "terrai_nom", "terrai_are", "terrai_per",
#                      "stagename", "stagenum", "UF", "pop", "idstart_m", "idstart_y", "idplan_m", "idplan_y",
#                      "idend_m",   "idend_y",   "delplan_m", "delplan_y", "delend_m",  "delend_y",  "demplan_m",
#                      "demplan_y", "demend_m",  "demend_y",  "apprplan_m", "apprplan_y", "apprend_m", "apprend_y",
#                      "regplan_m", "regplan_y", "regend_m",  "regend_y",  "enforce_st", "enforce_en", "enforce_to",
#                      "Ag_Pot_Maj", "X1997_Perc", "X2011_Perc", "X2013_Perc", "Cons_AOD",  "Biodiv_Pri", "Clim_Zone",
#                      "ClimChgZon", "AvgD_FedCU", "AvgD_StaCU", "AvgD_Log",  "AvgDist0_9", "AvgDist2_0", "AvgDist5_0",
#                      "AvgDist6_0", "AvgDist7_0", "AvgDist8_0", "AvgDist9_0", "AvgDist10_", "AvgDist11_", "AvgDist12_",
#                      "AvgDist_15", "AvgD_Rail", "AvgD_Mine", "AvgD_City", "AvgD_MajCi", "Cons_Ecosy", "AreaForRes",
#                      "AreaOvLR",  "DomSoil",   "ConsTrdPop", "DomVegetat", "id_ad",     "ifreq_tota", "id_733",    "Terra_Indi", "Duplicate_", "id_683",   
#                      "Grupo_Indi", "Municipio", "U___F",     "Superf__Ha", "Situa",     "C__R",      "id11",     
#                      "del11",     "dem11",     "appr11",    "reg11",     "exreg11",   "Modificati", "ter_nome", 
#                      "municipio_", "uf_sigla",  "superficie", "fase_ti",   "modalidade", "id16",      "tit_em_est",
#                      "del16",     "tit_del",   "dem16",     "tit_dem",   "appr16",    "tit_appr",  "reg16",    
#                      "tit_reg",   "id_diff",   "del_diff",  "dem_diff",  "appr_diff", "reg_diff",  "id_d16",   
#                      "id_m16",    "id_y16",    "id_d11",    "id_m11",    "id_y11",    "del_d16",   "del_m16",  
#                      "del_y16",   "del_d11",   "del_m11",   "del_y11",   "dem_d16",   "dem_m16",   "dem_y16",  
#                      "dem_d11",   "dem_m11",   "dem_y11",   "appr_d16",  "appr_m16",  "appr_y16",  "appr_d11", 
#                      "appr_m11",  "appr_y11",  "reg_d16",   "reg_m16",   "reg_y16",   "reg_d11",   "reg_m11",  
#                      "reg_y11",   "exreg_d11", "exreg_m11", "exreg_y11", "Pop_2000_x",  "dflc_e",    "dcfu_e",    "dcsu_e",    "dfmi_e",    "dfrw_e"))

#View(tmp2[1:100])
#View(tmp2[101:154])
#print(colnames(tmp2))
