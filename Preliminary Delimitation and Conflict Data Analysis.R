#This section is to help make the code work "out of the box" from  Github


#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("C:/Users/jflak/OneDrive/GitHub/kfw2_amazon_conflict")


#Naming Guide for the following code (up until the violence data manipulation section)
#
#The stages are, in order, identificada, delimitada, declarada, homologada, and regularizada. The names of values and variables
#are regularized based on these base names for the stages, which always come at the beginning of a value or variable name.
#The variable "Reg.SPU" in the 2011 dataset is renamed to "reg_extra2011" because it is "extra", it does not have a
#counterpart in the 2016 dataset, unlike the other variables.
#Every value and variable that corresponds to the 2011 Bonn Researchers dataset ends in "2011" while every value and variable
#that corresponds to the 2016 MLG dataset ends in "2016".

##########################################Start of Code#############################


#clear variables and values
rm(list = ls())


#reads the data from the files
data2011 <- read.csv(file = "Raw and Original Data/DemStages_FUNAI2011_BonnResearchers.csv", header=TRUE, sep = ',')
data2016 <- read.csv(file = "Raw and Original Data/DemStages_FUNAI_2016_fromMLG.csv", header=TRUE, sep = ',')


print("START OF TABLES")


#renames terrai_codigo to id_733 so the the id names match
names(data2016)[names(data2016)=="terrai_codigo"] <- "id_733"


#rename all of the demarcation stages from the 2016 dataset for clarity and ease of manipulation
names(data2016)[names(data2016)=="data_em_estudo"] <- "identificada2016"
names(data2016)[names(data2016)=="data_delimitada"] <- "delimitada2016"
names(data2016)[names(data2016)=="data_declarada"] <- "declarada2016"
names(data2016)[names(data2016)=="data_homologada"] <- "homologada2016"
names(data2016)[names(data2016)=="data_regularizada"] <- "regularizada2016"

#rename all of the demarcation stages from the 2011 dataset for clarity and ease of manipulation
names(data2011)[names(data2011)=="Identificada"] <- "identificada2011"
names(data2011)[names(data2011)=="Delimitada"] <- "delimitada2011"
names(data2011)[names(data2011)=="Declarada_Demarcada"] <- "declarada2011"
names(data2011)[names(data2011)=="Holologada_Homologado"] <- "homologada2011"
names(data2011)[names(data2011)=="Reg..CRI"] <- "regularizada2011"
names(data2011)[names(data2011)=="Reg..SPU"] <- "reg_extra2011"


#merges the two data frames by id_733 (the id variable that they have in common)
data_merged <- merge(data2011, data2016, by="id_733")


#changes the demarcation stage variables to date variables
data_merged$identificada2011 <- as.Date(data_merged$identificada2011, format= "%m/%d/%Y")
data_merged$identificada2016 <- as.Date(data_merged$identificada2016, format= "%m/%d/%Y")
data_merged$delimitada2011 <- as.Date(data_merged$delimitada2011, format= "%m/%d/%Y")
data_merged$delimitada2016 <- as.Date(data_merged$delimitada2016, format= "%m/%d/%Y")
data_merged$declarada2011 <- as.Date(data_merged$declarada2011, format= "%m/%d/%Y")
data_merged$declarada2016 <- as.Date(data_merged$declarada2016, format= "%m/%d/%Y")
data_merged$homologada2011 <- as.Date(data_merged$homologada2011, format= "%m/%d/%Y")
data_merged$homologada2016 <- as.Date(data_merged$homologada2016, format= "%m/%d/%Y")
data_merged$regularizada2011 <- as.Date(data_merged$regularizada2011, format= "%m/%d/%Y")
data_merged$regularizada2016 <- as.Date(data_merged$regularizada2016, format= "%m/%d/%Y")
data_merged$reg_extra2011 <- as.Date(data_merged$reg_extra2011, format= "%m/%d/%Y")


#counts number of non-blank observations and stores it in new value for each stage
identificada_count2011 <- length(data_merged$identificada2011) - sum(is.na(data_merged$identificada2011))
identificada_count2016 <- length(data_merged$identificada2016) - sum(is.na(data_merged$identificada2016))
delimitada_count2011 <- length(data_merged$delimitada2011) - sum(is.na(data_merged$delimitada2011))
delimitada_count2016 <- length(data_merged$delimitada2016) - sum(is.na(data_merged$delimitada2016))
declarada_count2011 <- length(data_merged$declarada2011) - sum(is.na(data_merged$declarada2011))
declarada_count2016 <- length(data_merged$declarada2016) - sum(is.na(data_merged$declarada2016))
homologada_count2011 <- length(data_merged$homologada2011) - sum(is.na(data_merged$homologada2011))
homologada_count2016 <- length(data_merged$homologada2016) - sum(is.na(data_merged$homologada2016))
regularizada_count2011 <- length(data_merged$regularizada2011) - sum(is.na(data_merged$regularizada2011))
regularizada_count2016 <- length(data_merged$regularizada2016) - sum(is.na(data_merged$regularizada2016))
reg_extra_count2011 <- length(data_merged$reg_extra2011) - sum(is.na(data_merged$reg_extra2011))


#counts number of non-blank observations for each stage (in either dataset)
identificada_count_any <- length(data_merged$identificada2011) - sum(is.na(data_merged$identificada2011) & is.na(data_merged$identificada2016))
delimitada_count_any <- length(data_merged$delimitada2011) - sum(is.na(data_merged$delimitada2011) & is.na(data_merged$delimitada2016))
declarada_count_any <- length(data_merged$declarada2011) - sum(is.na(data_merged$declarada2011) & is.na(data_merged$declarada2016))
homologada_count_any <- length(data_merged$homologada2011) - sum(is.na(data_merged$homologada2011) & is.na(data_merged$homologada2016))
regularizada_count_any <- length(data_merged$regularizada2011) - sum(is.na(data_merged$regularizada2011) & is.na(data_merged$regularizada2016))


#finds percentage of non-blank observations and stores it in new value for each stage
identificada_ratio2011 <- 100*(length(data_merged$identificada2011) - sum(is.na(data_merged$identificada2011))) / length(data_merged$identificada2011)
identificada_ratio2016 <- 100*(length(data_merged$identificada2016) - sum(is.na(data_merged$identificada2016))) / length(data_merged$identificada2016)
delimitada_ratio2011 <- 100*(length(data_merged$delimitada2011) - sum(is.na(data_merged$delimitada2011))) / length(data_merged$delimitada2011)
delimitada_ratio2016 <- 100*(length(data_merged$delimitada2016) - sum(is.na(data_merged$delimitada2016))) / length(data_merged$delimitada2016)
declarada_ratio2011 <- 100*(length(data_merged$declarada2011) - sum(is.na(data_merged$declarada2011))) / length(data_merged$declarada2011)
declarada_ratio2016 <- 100*(length(data_merged$declarada2016) - sum(is.na(data_merged$declarada2016))) / length(data_merged$declarada2016)
homologada_ratio2011 <- 100*(length(data_merged$homologada2011) - sum(is.na(data_merged$homologada2011))) / length(data_merged$homologada2011)
homologada_ratio2016 <- 100*(length(data_merged$homologada2016) - sum(is.na(data_merged$homologada2016))) / length(data_merged$homologada2016)
regularizada_ratio2011 <- 100*(length(data_merged$regularizada2011) - sum(is.na(data_merged$regularizada2011))) / length(data_merged$regularizada2011)
regularizada_ratio2016 <- 100*(length(data_merged$regularizada2016) - sum(is.na(data_merged$regularizada2016))) / length(data_merged$regularizada2016)
reg_extra_ratio2011 <- 100*(length(data_merged$reg_extra2011) - sum(is.na(data_merged$reg_extra2011))) / length(data_merged$reg_extra2011)


#checks whether the observations in each variable are the same, makes an indicator variable TRUE if different, FALSE if the same
data_merged$identificada_diff <- data_merged$identificada2016!=data_merged$identificada2011
data_merged$delimitada_diff <- data_merged$delimitada2016!=data_merged$delimitada2011
data_merged$declarada_diff <- data_merged$declarada2016!=data_merged$declarada2011
data_merged$homologada_diff <- data_merged$homologada2016!=data_merged$homologada2011
data_merged$regularizada_diff <- data_merged$regularizada2016!=data_merged$regularizada2011


#counts and stores in new values the number of differences between the two datasets for each stage
identificada_diff_val <- length(which(data_merged$identificada_diff))
delimitada_diff_val <- length(which(data_merged$delimitada_diff))
declarada_diff_val <- length(which(data_merged$declarada_diff))
homologada_diff_val <- length(which(data_merged$homologada_diff))
regularizada_diff_val <- length(which(data_merged$regularizada_diff))


#counts and stores in new values the agreement between the two datasets for each stage
identificada_same_val <- length(which(!data_merged$identificada_diff))
delimitada_same_val <- length(which(!data_merged$delimitada_diff))
declarada_same_val <- length(which(!data_merged$declarada_diff))
homologada_same_val <- length(which(!data_merged$homologada_diff))
regularizada_same_val <- length(which(!data_merged$regularizada_diff))


#counts number of non-blank observations that have map data for each stage
identificada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$identificada2011) | is.na(data_merged$id_587))
identificada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$identificada2016) | is.na(data_merged$id_587))
delimitada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$delimitada2011) | is.na(data_merged$id_587))
delimitada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$delimitada2016) | is.na(data_merged$id_587))
declarada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$declarada2011) | is.na(data_merged$id_587))
declarada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$declarada2016) | is.na(data_merged$id_587))
homologada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$homologada2011) | is.na(data_merged$id_587))
homologada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$homologada2016) | is.na(data_merged$id_587))
regularizada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$regularizada2011) | is.na(data_merged$id_587))
regularizada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$regularizada2016) | is.na(data_merged$id_587))
reg_extra_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$reg_extra2011) | is.na(data_merged$id_587))


#counts number of non-blank mapped observations for each stage (in either dataset)
identificada_count_any_map <- length(data_merged$identificada2011) - sum((is.na(data_merged$identificada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$identificada2016) | is.na(data_merged$id_587)))
delimitada_count_any_map <- length(data_merged$delimitada2011) - sum((is.na(data_merged$delimitada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$delimitada2016) | is.na(data_merged$id_587)))
declarada_count_any_map <- length(data_merged$declarada2011) - sum((is.na(data_merged$declarada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$declarada2016) | is.na(data_merged$id_587)))
homologada_count_any_map <- length(data_merged$homologada2011) - sum((is.na(data_merged$homologada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$homologada2016) | is.na(data_merged$id_587)))
regularizada_count_any_map <- length(data_merged$regularizada2011) - sum((is.na(data_merged$regularizada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$regularizada2016) | is.na(data_merged$id_587)))


#Concept note table 1####
print("Concept Note Table 1")
table_conceptnote1 <- matrix(c(identificada_count_any, identificada_count_any_map,
                               delimitada_count_any, delimitada_count_any_map,
                               declarada_count_any, declarada_count_any_map,
                               homologada_count_any, homologada_count_any_map,
                               regularizada_count_any, regularizada_count_any_map),
                             ncol = 2, byrow = TRUE)
colnames(table_conceptnote1) <- c("Counts of Communities with Demarcation Information", "Counts of Communities with Demarcation Information and Map Information")
rownames(table_conceptnote1) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_conceptnote1 <- as.table(table_conceptnote1)
print(table_conceptnote1)
print("------------------------------------------------------------------------------------------")


#counts number of observations that are the same and have map data
identificada_same_map <- length(which(!data_merged$identificada_diff & !is.na(data_merged$id_587)))
delimitada_same_map <- length(which(!data_merged$delimitada_diff & !is.na(data_merged$id_587)))
declarada_same_map <- length(which(!data_merged$declarada_diff & !is.na(data_merged$id_587)))
homologada_same_map <- length(which(!data_merged$homologada_diff & !is.na(data_merged$id_587)))
regularizada_same_map <- length(which(!data_merged$regularizada_diff & !is.na(data_merged$id_587)))


#counts number of observations that are different and have map data
identificada_diff_map <- length(which((data_merged$identificada_diff) & is.na(data_merged$id_587)))
delimitada_diff_map <- length(which((data_merged$delimitada_diff) & is.na(data_merged$id_587)))
declarada_diff_map <- length(which((data_merged$declarada_diff) & is.na(data_merged$id_587)))
homologada_diff_map <- length(which((data_merged$homologada_diff) & is.na(data_merged$id_587)))
regularizada_diff_map <- length(which((data_merged$regularizada_diff) & is.na(data_merged$id_587)))


#changes the indicator variables from TRUE/FALSE to 1/0
data_merged$identificada_diff <- as.numeric(data_merged$identificada_diff)
data_merged$delimitada_diff <- as.numeric(data_merged$delimitada_diff)
data_merged$declarada_diff <- as.numeric(data_merged$declarada_diff)
data_merged$homologada_diff <- as.numeric(data_merged$homologada_diff)
data_merged$regularizada_diff <- as.numeric(data_merged$regularizada_diff)


#seperates the data into day, month, year columns for each stage
data_merged$identificada_day2016 <- as.numeric(format(data_merged$identificada2016, format = "%d"))
data_merged$identificada_month2016 <- as.numeric(format(data_merged$identificada2016, format = "%m"))
data_merged$identificada_year2016 <- as.numeric(format(data_merged$identificada2016, format = "%Y"))

data_merged$identificada_day2011 <- as.numeric(format(data_merged$identificada2011, format = "%d"))
data_merged$identificada_month2011 <- as.numeric(format(data_merged$identificada2011, format = "%m"))
data_merged$identificada_year2011 <- as.numeric(format(data_merged$identificada2011, format = "%Y"))

data_merged$delimitada_day2016 <- as.numeric(format(data_merged$delimitada2016, format = "%d"))
data_merged$delimitada_month2016 <- as.numeric(format(data_merged$delimitada2016, format = "%m"))
data_merged$delimitada_year2016 <- as.numeric(format(data_merged$delimitada2016, format = "%Y"))

data_merged$delimitada_day2011 <- as.numeric(format(data_merged$delimitada2011, format = "%d"))
data_merged$delimitada_month2011 <- as.numeric(format(data_merged$delimitada2011, format = "%m"))
data_merged$delimitada_year2011 <- as.numeric(format(data_merged$delimitada2011, format = "%Y"))

data_merged$declarada_day2016 <- as.numeric(format(data_merged$declarada2016, format = "%d"))
data_merged$declarada_month2016 <- as.numeric(format(data_merged$declarada2016, format = "%m"))
data_merged$declarada_year2016 <- as.numeric(format(data_merged$declarada2016, format = "%Y"))

data_merged$declarada_day2011 <- as.numeric(format(data_merged$declarada2011, format = "%d"))
data_merged$declarada_month2011 <- as.numeric(format(data_merged$declarada2011, format = "%m"))
data_merged$declarada_year2011 <- as.numeric(format(data_merged$declarada2011, format = "%Y"))

data_merged$homologada_day2016 <- as.numeric(format(data_merged$homologada2016, format = "%d"))
data_merged$homologada_month2016 <- as.numeric(format(data_merged$homologada2016, format = "%m"))
data_merged$homologada_year2016 <- as.numeric(format(data_merged$homologada2016, format = "%Y"))

data_merged$homologada_day2011 <- as.numeric(format(data_merged$homologada2011, format = "%d"))
data_merged$homologada_month2011 <- as.numeric(format(data_merged$homologada2011, format = "%m"))
data_merged$homologada_year2011 <- as.numeric(format(data_merged$homologada2011, format = "%Y"))

data_merged$regularizada_day2016 <- as.numeric(format(data_merged$regularizada2016, format = "%d"))
data_merged$regularizada_month2016 <- as.numeric(format(data_merged$regularizada2016, format = "%m"))
data_merged$regularizada_year2016 <- as.numeric(format(data_merged$regularizada2016, format = "%Y"))

data_merged$regularizada_day2011 <- as.numeric(format(data_merged$regularizada2011, format = "%d"))
data_merged$regularizada_month2011 <- as.numeric(format(data_merged$regularizada2011, format = "%m"))
data_merged$regularizada_year2011 <- as.numeric(format(data_merged$regularizada2011, format = "%Y"))

data_merged$reg_extra_day2011 <- as.numeric(format(data_merged$reg_extra2011, format = "%d"))
data_merged$reg_extra_month2011 <- as.numeric(format(data_merged$reg_extra2011, format = "%m"))
data_merged$reg_extra_year2011 <- as.numeric(format(data_merged$reg_extra2011, format = "%Y"))


#creates a table comparing 2011 and 2016 data####
print("Comparison of 2011 and 2016 data, with Agreement:")
table_one <- matrix(c(identificada_count2011, identificada_map_count2011, identificada_count2016, identificada_map_count2016, identificada_same_val, identificada_diff_val, identificada_same_map, identificada_diff_map,
                      delimitada_count2011, delimitada_map_count2011, delimitada_count2016, delimitada_map_count2016, delimitada_same_val, delimitada_diff_val, delimitada_same_map, delimitada_diff_map,
                      declarada_count2011, declarada_map_count2011, declarada_count2016, declarada_map_count2016, declarada_same_val, declarada_diff_val, declarada_same_map, declarada_diff_map,
                      homologada_count2011, homologada_map_count2011, homologada_count2016, homologada_map_count2016, homologada_same_val, homologada_diff_val, homologada_same_map, homologada_diff_map,
                      regularizada_count2011, regularizada_map_count2011, regularizada_count2016, regularizada_map_count2016, regularizada_same_val, regularizada_diff_val, regularizada_same_map, regularizada_diff_map),
                    ncol = 8, byrow = TRUE)
colnames(table_one) <- c("~2011", "~2011 Mapped", "~2016", "~2016 Mapped", "~Agreement", "~Disagreement", "~Mapped Agreement", "~Mapped Disagreement")
rownames(table_one) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_one <- as.table(table_one)
print(table_one)

#==================================================================================================

####################################################Start of PPTAL Manipulation###################################

#reads in PPTAL data
dataPPTAL <- read.csv(file = "Raw and Original Data/PPTAL_151lands_demdates.csv", header=TRUE, sep = ',')


#renames id to id_587 in the PPTAL data so the id names match
names(dataPPTAL)[names(dataPPTAL)=="id"] <- "id_587"


#merges data_merged with PPTAL data by id_587
data_merged <- merge(data_merged, dataPPTAL, by = "id_587")


#counts number of non-blank observations for each stage in PPTAL data
id_count <- length(data_merged$idstart_m) - sum(is.na(data_merged$idstart_m))
del_count <- length(data_merged$delplan_m) - sum(is.na(data_merged$delplan_m))
dem_count <- length(data_merged$demplan_m) - sum(is.na(data_merged$demplan_m))
appr_count <- length(data_merged$apprend_m) - sum(is.na(data_merged$apprend_m))
reg_count <- length(data_merged$regplan_m) - sum(is.na(data_merged$regplan_m))


#checks whether the observations in PPTAL and 2011 data are the same, makes an indicator variable TRUE if different, FALSE if the same
data_merged$id_diff_2011 <- data_merged$identificada_month2011!=data_merged$idstart_m | data_merged$identificada_year2011!=data_merged$idstart_y
data_merged$del_diff_2011 <- data_merged$delimitada_month2011!=data_merged$delplan_m | data_merged$delimitada_year2011!=data_merged$delplan_y
data_merged$dem_diff_2011 <- data_merged$declarada_month2011!=data_merged$demplan_m | data_merged$declarada_year2011!=data_merged$demplan_y
data_merged$appr_diff_2011 <- data_merged$homologada_month2011!=data_merged$apprend_m | data_merged$homologada_year2011!=data_merged$apprend_y
data_merged$reg_diff_2011 <- data_merged$regularizada_month2011!=data_merged$regplan_m | data_merged$regularizada_year2011!=data_merged$regplan_y

#checks whether the observations in PPTAL and 2016 data are the same, makes an indicator variable TRUE if different, FALSE if the same
data_merged$id_diff_2016 <- data_merged$identificada_month2016!=data_merged$idstart_m | data_merged$identificada_year2016!=data_merged$idstart_y
data_merged$del_diff_2016 <- data_merged$delimitada_month2016!=data_merged$delplan_m | data_merged$delimitada_year2016!=data_merged$delplan_y
data_merged$dem_diff_2016 <- data_merged$declarada_month2016!=data_merged$demplan_m | data_merged$declarada_year2016!=data_merged$demplan_y
data_merged$appr_diff_2016 <- data_merged$homologada_month2016!=data_merged$apprend_m | data_merged$homologada_year2016!=data_merged$apprend_y
data_merged$reg_diff_2016 <- data_merged$regularizada_month2016!=data_merged$regplan_m | data_merged$regularizada_year2016!=data_merged$regplan_y


#counts and stores in new values the number of differences between the two datasets for each stage
id_diff_val_2011 <- length(which(data_merged$id_diff_2011))
del_diff_val_2011 <- length(which(data_merged$del_diff_2011))
dem_diff_val_2011 <- length(which(data_merged$dem_diff_2011))
appr_diff_val_2011 <- length(which(data_merged$appr_diff_2011))
reg_diff_val_2011 <- length(which(data_merged$reg_diff_2011))

id_diff_val_2016 <- length(which(data_merged$id_diff_2016))
del_diff_val_2016 <- length(which(data_merged$del_diff_2016))
dem_diff_val_2016 <- length(which(data_merged$dem_diff_2016))
appr_diff_val_2016 <- length(which(data_merged$appr_diff_2016))
reg_diff_val_2016 <- length(which(data_merged$reg_diff_2016))


#counts and stores in new values the number of observations that agree between the two datasets for each stage
id_same_val_2011 <- length(which(!data_merged$id_diff_2011))
del_same_val_2011 <- length(which(!data_merged$del_diff_2011))
dem_same_val_2011 <- length(which(!data_merged$dem_diff_2011))
appr_same_val_2011 <- length(which(!data_merged$appr_diff_2011))
reg_same_val_2011 <- length(which(!data_merged$reg_diff_2011))

id_same_val_2016 <- length(which(!data_merged$id_diff_2016))
del_same_val_2016 <- length(which(!data_merged$del_diff_2016))
dem_same_val_2016 <- length(which(!data_merged$dem_diff_2016))
appr_same_val_2016 <- length(which(!data_merged$appr_diff_2016))
reg_same_val_2016 <- length(which(!data_merged$reg_diff_2016))

#-----------------------------------------------------------------------------

#checks whether the month observations in PPTAL and 2011 data are the same, makes an indicator variable TRUE if different, FALSE if the same
data_merged$id_diff_m2011 <- data_merged$identificada_month2011!=data_merged$idstart_m
data_merged$del_diff_m2011 <- data_merged$delimitada_month2011!=data_merged$delplan_m
data_merged$dem_diff_m2011 <- data_merged$declarada_month2011!=data_merged$demplan_m
data_merged$appr_diff_m2011 <- data_merged$homologada_month2011!=data_merged$apprend_m
data_merged$reg_diff_m2011 <- data_merged$regularizada_month2011!=data_merged$regplan_m

#checks whether the month observations in PPTAL and 2016 data are the same, makes an indicator variable TRUE if different, FALSE if the same
data_merged$id_diff_m2016 <- data_merged$identificada_month2016!=data_merged$idstart_m
data_merged$del_diff_m2016 <- data_merged$delimitada_month2016!=data_merged$delplan_m
data_merged$dem_diff_m2016 <- data_merged$declarada_month2016!=data_merged$demplan_m
data_merged$appr_diff_m2016 <- data_merged$homologada_month2016!=data_merged$apprend_m
data_merged$reg_diff_m2016 <- data_merged$regularizada_month2016!=data_merged$regplan_m


#counts and stores in new values the number of month differences between the two datasets for each stage
id_diff_val_m2011 <- length(which(data_merged$id_diff_m2011))
del_diff_val_m2011 <- length(which(data_merged$del_diff_m2011))
dem_diff_val_m2011 <- length(which(data_merged$dem_diff_m2011))
appr_diff_val_m2011 <- length(which(data_merged$appr_diff_m2011))
reg_diff_val_m2011 <- length(which(data_merged$reg_diff_m2011))

id_diff_val_m2016 <- length(which(data_merged$id_diff_m2016))
del_diff_val_m2016 <- length(which(data_merged$del_diff_m2016))
dem_diff_val_m2016 <- length(which(data_merged$dem_diff_m2016))
appr_diff_val_m2016 <- length(which(data_merged$appr_diff_m2016))
reg_diff_val_m2016 <- length(which(data_merged$reg_diff_m2016))


#counts and stores in new values the number of month observations that agree between the two datasets for each stage
id_same_val_m2011 <- length(which(!data_merged$id_diff_m2011))
del_same_val_m2011 <- length(which(!data_merged$del_diff_m2011))
dem_same_val_m2011 <- length(which(!data_merged$dem_diff_m2011))
appr_same_val_m2011 <- length(which(!data_merged$appr_diff_m2011))
reg_same_val_m2011 <- length(which(!data_merged$reg_diff_m2011))

id_same_val_m2016 <- length(which(!data_merged$id_diff_m2016))
del_same_val_m2016 <- length(which(!data_merged$del_diff_m2016))
dem_same_val_m2016 <- length(which(!data_merged$dem_diff_m2016))
appr_same_val_m2016 <- length(which(!data_merged$appr_diff_m2016))
reg_same_val_m2016 <- length(which(!data_merged$reg_diff_m2016))

#-----------------------------------------------------------------------------

#checks whether the year observations in PPTAL and 2011 data are the same, makes an indicator variable TRUE if different, FALSE if the same
data_merged$id_diff_y2011 <- data_merged$identificada_year2011!=data_merged$idstart_y
data_merged$del_diff_y2011 <- data_merged$delimitada_year2011!=data_merged$delplan_y
data_merged$dem_diff_y2011 <- data_merged$declarada_year2011!=data_merged$demplan_y
data_merged$appr_diff_y2011 <- data_merged$homologada_year2011!=data_merged$apprend_y
data_merged$reg_diff_y2011 <- data_merged$regularizada_year2011!=data_merged$regplan_y

#checks whether the year observations in PPTAL and 2016 data are the same, makes an indicator variable TRUE if different, FALSE if the same
data_merged$id_diff_y2016 <- data_merged$identificada_year2016!=data_merged$idstart_y
data_merged$del_diff_y2016 <- data_merged$delimitada_year2016!=data_merged$delplan_y
data_merged$dem_diff_y2016 <- data_merged$declarada_year2016!=data_merged$demplan_y
data_merged$appr_diff_y2016 <- data_merged$homologada_year2016!=data_merged$apprend_y
data_merged$reg_diff_y2016 <- data_merged$regularizada_year2016!=data_merged$regplan_y


#counts and stores in new values the number of year differences between the two datasets for each stage
id_diff_val_y2011 <- length(which(data_merged$id_diff_y2011))
del_diff_val_y2011 <- length(which(data_merged$del_diff_y2011))
dem_diff_val_y2011 <- length(which(data_merged$dem_diff_y2011))
appr_diff_val_y2011 <- length(which(data_merged$appr_diff_y2011))
reg_diff_val_y2011 <- length(which(data_merged$reg_diff_y2011))

id_diff_val_y2016 <- length(which(data_merged$id_diff_y2016))
del_diff_val_y2016 <- length(which(data_merged$del_diff_y2016))
dem_diff_val_y2016 <- length(which(data_merged$dem_diff_y2016))
appr_diff_val_y2016 <- length(which(data_merged$appr_diff_y2016))
reg_diff_val_y2016 <- length(which(data_merged$reg_diff_y2016))


#counts and stores in new values the number of year observations that agree between the two datasets for each stage
id_same_val_y2011 <- length(which(!data_merged$id_diff_y2011))
del_same_val_y2011 <- length(which(!data_merged$del_diff_y2011))
dem_same_val_y2011 <- length(which(!data_merged$dem_diff_y2011))
appr_same_val_y2011 <- length(which(!data_merged$appr_diff_y2011))
reg_same_val_y2011 <- length(which(!data_merged$reg_diff_y2011))

id_same_val_y2016 <- length(which(!data_merged$id_diff_y2016))
del_same_val_y2016 <- length(which(!data_merged$del_diff_y2016))
dem_same_val_y2016 <- length(which(!data_merged$dem_diff_y2016))
appr_same_val_y2016 <- length(which(!data_merged$appr_diff_y2016))
reg_same_val_y2016 <- length(which(!data_merged$reg_diff_y2016))

#-----------------------------------------------------------------------------

#counts the number of observations that are not blank in BOTH the datasets
id_present_2011 <- length(data_merged$idstart_m) - sum(is.na(data_merged$idstart_m) | is.na(data_merged$identificada2011))
del_present_2011 <- length(data_merged$delplan_m) - sum(is.na(data_merged$delplan_m) | is.na(data_merged$delimitada2011))
dem_present_2011 <- length(data_merged$demplan_m) - sum(is.na(data_merged$demplan_m) | is.na(data_merged$declarada2011))
appr_present_2011 <- length(data_merged$apprend_m) - sum(is.na(data_merged$apprend_m) | is.na(data_merged$homologada2011))
reg_present_2011 <- length(data_merged$regplan_m) - sum(is.na(data_merged$regplan_m) | is.na(data_merged$regularizada2011))

id_present_2016 <- length(data_merged$idstart_m) - sum(is.na(data_merged$idstart_m) | is.na(data_merged$identificada2016))
del_present_2016 <- length(data_merged$delplan_m) - sum(is.na(data_merged$delplan_m) | is.na(data_merged$delimitada2016))
dem_present_2016 <- length(data_merged$demplan_m) - sum(is.na(data_merged$demplan_m) | is.na(data_merged$declarada2016))
appr_present_2016 <- length(data_merged$apprend_m) - sum(is.na(data_merged$apprend_m) | is.na(data_merged$homologada2016))
reg_present_2016 <- length(data_merged$regplan_m) - sum(is.na(data_merged$regplan_m) | is.na(data_merged$regularizada2016))


#counts the number of observations that are present in 2011 or 2016 but NOT PPTAL
id_present_only_2011 <- length(data_merged$idstart_m) - sum(!is.na(data_merged$idstart_m) | is.na(data_merged$identificada2011))
del_present_only_2011 <- length(data_merged$delplan_m) - sum(!is.na(data_merged$delplan_m) | is.na(data_merged$delimitada2011))
dem_present_only_2011 <- length(data_merged$demplan_m) - sum(!is.na(data_merged$demplan_m) | is.na(data_merged$declarada2011))
appr_present_only_2011 <- length(data_merged$apprend_m) - sum(!is.na(data_merged$apprend_m) | is.na(data_merged$homologada2011))
reg_present_only_2011 <- length(data_merged$regplan_m) - sum(!is.na(data_merged$regplan_m) | is.na(data_merged$regularizada2011))

id_present_only_2016 <- length(data_merged$idstart_m) - sum(!is.na(data_merged$idstart_m) | is.na(data_merged$identificada2016))
del_present_only_2016 <- length(data_merged$delplan_m) - sum(!is.na(data_merged$delplan_m) | is.na(data_merged$delimitada2016))
dem_present_only_2016 <- length(data_merged$demplan_m) - sum(!is.na(data_merged$demplan_m) | is.na(data_merged$declarada2016))
appr_present_only_2016 <- length(data_merged$apprend_m) - sum(!is.na(data_merged$apprend_m) | is.na(data_merged$homologada2016))
reg_present_only_2016 <- length(data_merged$regplan_m) - sum(!is.na(data_merged$regplan_m) | is.na(data_merged$regularizada2016))


#creates a table comparing agreement of PPTAL, 2011 and 2016 data####
print("------------------------------------------------------------------------------------------")
print("Comparison of PPTAL, 2011, and 2016 data, with Agreement, and Agreement of Month and Year:")
table_two <- matrix(c(id_count, id_same_val_2011, id_diff_val_2011, id_same_val_m2011, id_diff_val_m2011,id_same_val_y2011, id_diff_val_y2011, id_present_2011, id_present_only_2011, id_same_val_2016, id_diff_val_2016, id_same_val_m2016, id_diff_val_m2016, id_same_val_y2016, id_diff_val_y2016, id_present_2016, id_present_only_2016,
                      del_count, del_same_val_2011, del_diff_val_2011, del_same_val_m2011, del_diff_val_m2011, del_same_val_y2011, del_diff_val_y2011, del_present_2011, del_present_only_2011, del_same_val_2016, del_diff_val_2016, del_same_val_m2016, del_diff_val_m2016, del_same_val_y2016, del_diff_val_y2016, del_present_2016, del_present_only_2016,
                      dem_count, dem_same_val_2011, dem_diff_val_2011, dem_same_val_m2011, dem_diff_val_m2011, dem_same_val_y2011, dem_diff_val_y2011, dem_present_2011, dem_present_only_2011, dem_same_val_2016, dem_diff_val_2016, dem_same_val_m2016, dem_diff_val_m2016, dem_same_val_y2016, dem_diff_val_y2016, dem_present_2016, dem_present_only_2016,
                      appr_count, appr_same_val_2011, appr_diff_val_2011, appr_same_val_m2011, appr_diff_val_m2011, appr_same_val_y2011, appr_diff_val_y2011, appr_present_2011, appr_present_only_2011, appr_same_val_2016, appr_diff_val_2016, appr_same_val_m2016, appr_diff_val_m2016, appr_same_val_y2016, appr_diff_val_y2016, appr_present_2016, appr_present_only_2016,
                      reg_count, reg_same_val_2011, reg_diff_val_2011, reg_same_val_m2011, reg_diff_val_m2011, reg_same_val_y2011, reg_diff_val_y2011, reg_present_2011, reg_present_only_2011, reg_same_val_2016, reg_diff_val_2016, reg_same_val_m2016, reg_diff_val_m2016, reg_same_val_y2016, reg_diff_val_y2016, reg_present_2016, reg_present_only_2016),
                    ncol = 17, byrow = TRUE)
colnames(table_two) <- c("~PPTAL", "~PPTAL 2011 Agree", "~PPTAL 2011 Disagree", "~PPTAL 2011 M Agree", "~PPTAL 2011 M Disagree", "~PPTAL 2011 Y Agree", "~PPTAL 2011 Y Disagree", "~Present PPTAL 2011", "~Present 2011, not PPTAL",
                         "~PPTAL 2016 Agree", "~PPTAL 2016 Disagree", "~PPTAL 2016 M Agree", "~PPTAL 2016 M Disagree", "~PPTAL 2016 Y Agree", "~PPTAL 2016 Y Disagree", "~Present PPTAL 2016", "~Present 2016, not PPTAL")
rownames(table_two) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_two <- as.table(table_two)
print(table_two)


#creates a table comparing PPTAL, 2011 and 2016 data (# of Observations)####
print("------------------------------------------------------------------------------------------")
print("Comparison of PPTAL, 2011 and 2016 data (# of Observations):")
table_three <- matrix(c(id_count, identificada_count2011, identificada_map_count2011, identificada_count2016, identificada_map_count2016, 
                        del_count, delimitada_count2011, delimitada_map_count2011, delimitada_count2016, delimitada_map_count2016,
                        dem_count, declarada_count2011, declarada_map_count2011, declarada_count2016, declarada_map_count2016,
                        appr_count, homologada_count2011, homologada_map_count2011, homologada_count2016, homologada_map_count2016,
                        reg_count, regularizada_count2011, regularizada_map_count2011, regularizada_count2016, regularizada_map_count2016),
                      ncol = 5, byrow = TRUE)
colnames(table_three) <- c("~PPTAL Observations", "~2011 Observations", "~2011 Mapped Observations", "~2016 Observations",
                           "~2016 Mapped Observations")
rownames(table_three) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_three <- as.table(table_three)
print(table_three)
print("======================================================================================")


#############################################Start of Violence Data Manipulation###############################


#reads the violence data from the files
data_iviolence <- read.csv(file = "Raw and Original Data/Final_IndividualViolence_CIMI (1).csv", header=TRUE, sep = ',')
data_lviolence <- read.csv(file = "Raw and Original Data/Final_LandViolence_CIMI_new (2).csv", header=TRUE, sep = ',')


#drops rows with missing ad_id
data_iviolence$ad_id <- as.numeric(as.character(data_iviolence$ad_id))
data_lviolence$ad_id <- as.numeric(as.character(data_lviolence$ad_id))
data_iviolence <- data_iviolence[!(is.na(data_iviolence$ad_id)),]
data_lviolence <- data_lviolence[!(is.na(data_lviolence$ad_id)),]


#creates new vectors of unique ad_ids from both violence datasets
iviolence_uniqueid <- sort(unique(data_iviolence$ad_id))
lviolence_uniqueid <- sort(unique(data_lviolence$ad_id))


#gets number of unique areas affected by violence
iviolence_uniqueid_val <- length(iviolence_uniqueid)
lviolence_uniqueid_val <- length(lviolence_uniqueid)


#creates new dataframes for finding violence overlap with other datasets
iviolence_overlap <- as.data.frame(iviolence_uniqueid)
lviolence_overlap <- as.data.frame(lviolence_uniqueid)


#prints vectors of unique ad_ids
#print(iviolence_uniqueid)
#print(lviolence_uniqueid)


#renames iviolence_uniqueid to ad_id so the the id names match
names(iviolence_overlap)[names(iviolence_overlap)=="iviolence_uniqueid"] <- "id_ad"
#renames lviolence_uniqueid to ad_id so the the id names match
names(lviolence_overlap)[names(lviolence_overlap)=="lviolence_uniqueid"] <- "id_ad"


#changes the demarcation stage variables in 2011 dataset to date variables
data2011$identificada2011 <- as.Date(data2011$identificada2011, format= "%m/%d/%Y")
data2011$delimitada2011 <- as.Date(data2011$delimitada2011, format= "%m/%d/%Y")
data2011$declarada2011 <- as.Date(data2011$declarada2011, format= "%m/%d/%Y")
data2011$homologada2011 <- as.Date(data2011$homologada2011, format= "%m/%d/%Y")
data2011$regularizada2011 <- as.Date(data2011$regularizada2011, format= "%m/%d/%Y")
data2011$reg_extra2011 <- as.Date(data2011$reg_extra2011, format= "%m/%d/%Y")


#merges violence data and 2011 data (dropping rows that have no corresponding id_ad)
iviolence_overlap_11 <- merge(iviolence_overlap, data2011, by = "id_ad")
lviolence_overlap_11 <- merge(lviolence_overlap, data2011, by = "id_ad")


#merges 2011/2016 data (dropping rows that have no corresponding id_733)
data_merged_11_16 <- merge(data2011, data2016, by="id_733")


#changes the demarcation stage variables in 2011/2016 merged dataset to date variables
data_merged_11_16$identificada2011 <- as.Date(data_merged_11_16$identificada2011, format= "%m/%d/%Y")
data_merged_11_16$identificada2016 <- as.Date(data_merged_11_16$identificada2016, format= "%m/%d/%Y")
data_merged_11_16$delimitada2011 <- as.Date(data_merged_11_16$delimitada2011, format= "%m/%d/%Y")
data_merged_11_16$delimitada2016 <- as.Date(data_merged_11_16$delimitada2016, format= "%m/%d/%Y")
data_merged_11_16$declarada2011 <- as.Date(data_merged_11_16$declarada2011, format= "%m/%d/%Y")
data_merged_11_16$declarada2016 <- as.Date(data_merged_11_16$declarada2016, format= "%m/%d/%Y")
data_merged_11_16$homologada2011 <- as.Date(data_merged_11_16$homologada2011, format= "%m/%d/%Y")
data_merged_11_16$homologada2016 <- as.Date(data_merged_11_16$homologada2016, format= "%m/%d/%Y")
data_merged_11_16$regularizada2011 <- as.Date(data_merged_11_16$regularizada2011, format= "%m/%d/%Y")
data_merged_11_16$regularizada2016 <- as.Date(data_merged_11_16$regularizada2016, format= "%m/%d/%Y")
data_merged_11_16$reg_extra2011 <- as.Date(data_merged_11_16$reg_extra2011, format= "%m/%d/%Y")


#merges violence data and 2011/2016 merged data (dropping rows that have no corresponding id_ad)
iviolence_overlap_16 <- merge(iviolence_overlap, data_merged_11_16, by = "id_ad")
lviolence_overlap_16 <- merge(lviolence_overlap, data_merged_11_16, by = "id_ad")


#merges violence data and PPTAL data (dropping rows that have no corresponding id_ad)
iviolence_overlap_PPTAL <- merge(iviolence_overlap, data_merged, by = "id_ad")
lviolence_overlap_PPTAL <- merge(lviolence_overlap, data_merged, by = "id_ad")


#finds overlap of violence data and mapped data
iviolence_overlap_11_map <- iviolence_overlap_11[(!is.na(iviolence_overlap_11$id_587)),]
lviolence_overlap_11_map <- lviolence_overlap_11[(!is.na(lviolence_overlap_11$id_587)),]
iviolence_overlap_16_map <- iviolence_overlap_16[(!is.na(iviolence_overlap_16$id_587)),]
lviolence_overlap_16_map <- lviolence_overlap_16[(!is.na(lviolence_overlap_16$id_587)),]


#fills in values for whether there is overlap for each dataset into columns in iviolence_overlap
iviolence_overlap$overlap_11 <- iviolence_overlap$id_ad %in% iviolence_overlap_11$id_ad
iviolence_overlap$overlap_16 <- iviolence_overlap$id_ad %in% iviolence_overlap_16$id_ad
iviolence_overlap$overlap_PPTAL <- iviolence_overlap$id_ad %in% iviolence_overlap_PPTAL$id_ad
iviolence_overlap$overlap_any <- (iviolence_overlap$id_ad %in% iviolence_overlap_11$id_ad) | (iviolence_overlap$id_ad %in% iviolence_overlap_16$id_ad) | (iviolence_overlap$id_ad %in% iviolence_overlap_PPTAL$id_ad)
iviolence_overlap$overlap_11_map <- iviolence_overlap$id_ad %in% iviolence_overlap_11_map$id_ad
iviolence_overlap$overlap_16_map <- iviolence_overlap$id_ad %in% iviolence_overlap_16_map$id_ad
iviolence_overlap$overlap_any_map <- (iviolence_overlap$id_ad %in% iviolence_overlap_11_map$id_ad) | (iviolence_overlap$id_ad %in% iviolence_overlap_16_map$id_ad) | (iviolence_overlap$id_ad %in% iviolence_overlap_PPTAL$id_ad)

#fills in values for whether there is overlap for each dataset into columns in lviolence_overlap
lviolence_overlap$overlap_11 <- lviolence_overlap$id_ad %in% lviolence_overlap_11$id_ad
lviolence_overlap$overlap_16 <- lviolence_overlap$id_ad %in% lviolence_overlap_16$id_ad
lviolence_overlap$overlap_PPTAL <- lviolence_overlap$id_ad %in% lviolence_overlap_PPTAL$id_ad
lviolence_overlap$overlap_any <- (lviolence_overlap$id_ad %in% lviolence_overlap_11$id_ad) | (lviolence_overlap$id_ad %in% lviolence_overlap_16$id_ad) | (lviolence_overlap$id_ad %in% lviolence_overlap_PPTAL$id_ad)
lviolence_overlap$overlap_11_map <- lviolence_overlap$id_ad %in% lviolence_overlap_11_map$id_ad
lviolence_overlap$overlap_16_map <- lviolence_overlap$id_ad %in% lviolence_overlap_16_map$id_ad
lviolence_overlap$overlap_any_map <- (lviolence_overlap$id_ad %in% lviolence_overlap_11_map$id_ad) | (lviolence_overlap$id_ad %in% lviolence_overlap_16_map$id_ad) | (lviolence_overlap$id_ad %in% lviolence_overlap_PPTAL$id_ad)

########################Start of Finding Individual Stage Overlap###############################

#finding identificada overlap for violence data and each other dataset
iviolence_overlap$overlap_11_id <- !is.na(iviolence_overlap_11$identificada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)])
iviolence_overlap$overlap_16_id <- !is.na(iviolence_overlap_16$identificada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)])
iviolence_overlap$overlap_PPTAL_id <- !is.na(iviolence_overlap_PPTAL$idstart_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_id <- !is.na(iviolence_overlap_11$identificada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)]) | !is.na(iviolence_overlap_16$identificada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)]) | !is.na(iviolence_overlap_PPTAL$idstart_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])

#finding identificada overlap for violence data and each other mapped dataset
iviolence_overlap$overlap_11_id_map <- !is.na(iviolence_overlap_11_map$identificada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)])
iviolence_overlap$overlap_16_id_map <- !is.na(iviolence_overlap_16_map$identificada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)])
iviolence_overlap$overlap_PPTAL_id_map <- !is.na(iviolence_overlap_PPTAL$idstart_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_id_map <- !is.na(iviolence_overlap_11_map$identificada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)]) | !is.na(iviolence_overlap_16_map$identificada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)]) | !is.na(iviolence_overlap_PPTAL$idstart_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])


#finding delimitada overlap for violence data and each other dataset
iviolence_overlap$overlap_11_del <- !is.na(iviolence_overlap_11$delimitada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)])
iviolence_overlap$overlap_16_del <- !is.na(iviolence_overlap_16$delimitada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)])
iviolence_overlap$overlap_PPTAL_del <- !is.na(iviolence_overlap_PPTAL$delplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_del <- !is.na(iviolence_overlap_11$delimitada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)]) | !is.na(iviolence_overlap_16$delimitada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)]) | !is.na(iviolence_overlap_PPTAL$delplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])

#finding delimitada overlap for violence data and each other mapped dataset
iviolence_overlap$overlap_11_del_map <- !is.na(iviolence_overlap_11_map$delimitada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)])
iviolence_overlap$overlap_16_del_map <- !is.na(iviolence_overlap_16_map$delimitada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)])
iviolence_overlap$overlap_PPTAL_del_map <- !is.na(iviolence_overlap_PPTAL$delplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_del_map <- !is.na(iviolence_overlap_11_map$delimitada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)]) | !is.na(iviolence_overlap_16_map$delimitada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)]) | !is.na(iviolence_overlap_PPTAL$delplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])


#finding declarada overlap for violence data and each other dataset
iviolence_overlap$overlap_11_dem <- !is.na(iviolence_overlap_11$declarada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)])
iviolence_overlap$overlap_16_dem <- !is.na(iviolence_overlap_16$declarada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)])
iviolence_overlap$overlap_PPTAL_dem <- !is.na(iviolence_overlap_PPTAL$demplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_dem <- !is.na(iviolence_overlap_11$declarada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)]) | !is.na(iviolence_overlap_16$declarada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)]) | !is.na(iviolence_overlap_PPTAL$demplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])

#finding declarada overlap for violence data and each other mapped dataset
iviolence_overlap$overlap_11_dem_map <- !is.na(iviolence_overlap_11_map$declarada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)])
iviolence_overlap$overlap_16_dem_map <- !is.na(iviolence_overlap_16_map$declarada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)])
iviolence_overlap$overlap_PPTAL_dem_map <- !is.na(iviolence_overlap_PPTAL$demplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_dem_map <- !is.na(iviolence_overlap_11_map$declarada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)]) | !is.na(iviolence_overlap_16_map$declarada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)]) | !is.na(iviolence_overlap_PPTAL$demplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])


#finding homologada overlap for violence data and each other dataset
iviolence_overlap$overlap_11_appr <- !is.na(iviolence_overlap_11$homologada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)])
iviolence_overlap$overlap_16_appr <- !is.na(iviolence_overlap_16$homologada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)])
iviolence_overlap$overlap_PPTAL_appr <- !is.na(iviolence_overlap_PPTAL$apprend_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_appr <- !is.na(iviolence_overlap_11$homologada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)]) | !is.na(iviolence_overlap_16$homologada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)]) | !is.na(iviolence_overlap_PPTAL$apprend_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])

#finding homologada overlap for violence data and each other mapped dataset
iviolence_overlap$overlap_11_appr_map <- !is.na(iviolence_overlap_11_map$homologada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)])
iviolence_overlap$overlap_16_appr_map <- !is.na(iviolence_overlap_16_map$homologada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)])
iviolence_overlap$overlap_PPTAL_appr_map <- !is.na(iviolence_overlap_PPTAL$apprend_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_appr_map <- !is.na(iviolence_overlap_11_map$homologada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)]) | !is.na(iviolence_overlap_16_map$homologada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)]) | !is.na(iviolence_overlap_PPTAL$apprend_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])


#finding regularizada overlap for violence data and each other dataset
iviolence_overlap$overlap_11_reg <- !is.na(iviolence_overlap_11$regularizada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)])
iviolence_overlap$overlap_16_reg <- !is.na(iviolence_overlap_16$regularizada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)])
iviolence_overlap$overlap_PPTAL_reg <- !is.na(iviolence_overlap_PPTAL$regplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_reg <- !is.na(iviolence_overlap_11$regularizada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11$id_ad)]) | !is.na(iviolence_overlap_16$regularizada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16$id_ad)]) | !is.na(iviolence_overlap_PPTAL$regplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])

#finding regularizada overlap for violence data and each other mapped dataset
iviolence_overlap$overlap_11_reg_map <- !is.na(iviolence_overlap_11_map$regularizada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)])
iviolence_overlap$overlap_16_reg_map <- !is.na(iviolence_overlap_16_map$regularizada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)])
iviolence_overlap$overlap_PPTAL_reg_map <- !is.na(iviolence_overlap_PPTAL$regplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])
iviolence_overlap$overlap_any_reg_map <- !is.na(iviolence_overlap_11_map$regularizada2011[match(iviolence_overlap$id_ad, iviolence_overlap_11_map$id_ad)]) | !is.na(iviolence_overlap_16_map$regularizada2016[match(iviolence_overlap$id_ad, iviolence_overlap_16_map$id_ad)]) | !is.na(iviolence_overlap_PPTAL$regplan_m[match(iviolence_overlap$id_ad, iviolence_overlap_PPTAL$id_ad)])


##################Counting the Overlap for each stage for individual violence###########
ioverlap_11_id_val <- length(which(iviolence_overlap$overlap_11_id))
ioverlap_16_id_val <- length(which(iviolence_overlap$overlap_16_id))
ioverlap_PPTAL_id_val <- length(which(iviolence_overlap$overlap_PPTAL_id))
ioverlap_11_id_map_val <- length(which(iviolence_overlap$overlap_11_id_map))
ioverlap_16_id_map_val <- length(which(iviolence_overlap$overlap_16_id_map))
ioverlap_any_id_val <- length(which(iviolence_overlap$overlap_any_id))
ioverlap_any_id_map_val <- length(which(iviolence_overlap$overlap_any_id_map))

ioverlap_11_del_val <- length(which(iviolence_overlap$overlap_11_del))
ioverlap_16_del_val <- length(which(iviolence_overlap$overlap_16_del))
ioverlap_PPTAL_del_val <- length(which(iviolence_overlap$overlap_PPTAL_del))
ioverlap_11_del_map_val <- length(which(iviolence_overlap$overlap_11_del_map))
ioverlap_16_del_map_val <- length(which(iviolence_overlap$overlap_16_del_map))
ioverlap_any_del_val <- length(which(iviolence_overlap$overlap_any_del))
ioverlap_any_del_map_val <- length(which(iviolence_overlap$overlap_any_del_map))

ioverlap_11_dem_val <- length(which(iviolence_overlap$overlap_11_dem))
ioverlap_16_dem_val <- length(which(iviolence_overlap$overlap_16_dem))
ioverlap_PPTAL_dem_val <- length(which(iviolence_overlap$overlap_PPTAL_dem))
ioverlap_11_dem_map_val <- length(which(iviolence_overlap$overlap_11_dem_map))
ioverlap_16_dem_map_val <- length(which(iviolence_overlap$overlap_16_dem_map))
ioverlap_any_dem_val <- length(which(iviolence_overlap$overlap_any_dem))
ioverlap_any_dem_map_val <- length(which(iviolence_overlap$overlap_any_dem_map))

ioverlap_11_appr_val <- length(which(iviolence_overlap$overlap_11_appr))
ioverlap_16_appr_val <- length(which(iviolence_overlap$overlap_16_appr))
ioverlap_PPTAL_appr_val <- length(which(iviolence_overlap$overlap_PPTAL_appr))
ioverlap_11_appr_map_val <- length(which(iviolence_overlap$overlap_11_appr_map))
ioverlap_16_appr_map_val <- length(which(iviolence_overlap$overlap_16_appr_map))
ioverlap_any_appr_val <- length(which(iviolence_overlap$overlap_any_appr))
ioverlap_any_appr_map_val <- length(which(iviolence_overlap$overlap_any_appr_map))

ioverlap_11_reg_val <- length(which(iviolence_overlap$overlap_11_reg))
ioverlap_16_reg_val <- length(which(iviolence_overlap$overlap_16_reg))
ioverlap_PPTAL_reg_val <- length(which(iviolence_overlap$overlap_PPTAL_reg))
ioverlap_11_reg_map_val <- length(which(iviolence_overlap$overlap_11_reg_map))
ioverlap_16_reg_map_val <- length(which(iviolence_overlap$overlap_16_reg_map))
ioverlap_any_reg_val <- length(which(iviolence_overlap$overlap_any_reg))
ioverlap_any_reg_map_val <- length(which(iviolence_overlap$overlap_any_reg_map))


ioverlap_any_any_val <- length(which(iviolence_overlap$overlap_any))
ioverlap_any_any_val_map <- length(which(iviolence_overlap$overlap_any_map))


#finding identificada overlap for violence data and each other dataset
lviolence_overlap$overlap_11_id <- !is.na(lviolence_overlap_11$identificada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)])
lviolence_overlap$overlap_16_id <- !is.na(lviolence_overlap_16$identificada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)])
lviolence_overlap$overlap_PPTAL_id <- !is.na(lviolence_overlap_PPTAL$idstart_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_id <- !is.na(lviolence_overlap_11$identificada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)]) | !is.na(lviolence_overlap_16$identificada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)]) | !is.na(lviolence_overlap_PPTAL$idstart_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])

#finding identificada overlap for violence data and each other mapped dataset
lviolence_overlap$overlap_11_id_map <- !is.na(lviolence_overlap_11_map$identificada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)])
lviolence_overlap$overlap_16_id_map <- !is.na(lviolence_overlap_16_map$identificada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)])
lviolence_overlap$overlap_PPTAL_id_map <- !is.na(lviolence_overlap_PPTAL$idstart_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_id_map <- !is.na(lviolence_overlap_11_map$identificada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)]) | !is.na(lviolence_overlap_16_map$identificada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)]) | !is.na(lviolence_overlap_PPTAL$idstart_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])


#finding delimitada overlap for violence data and each other dataset
lviolence_overlap$overlap_11_del <- !is.na(lviolence_overlap_11$delimitada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)])
lviolence_overlap$overlap_16_del <- !is.na(lviolence_overlap_16$delimitada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)])
lviolence_overlap$overlap_PPTAL_del <- !is.na(lviolence_overlap_PPTAL$delplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_del <- !is.na(lviolence_overlap_11$delimitada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)]) | !is.na(lviolence_overlap_16$delimitada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)]) | !is.na(lviolence_overlap_PPTAL$delplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])

#finding delimitada overlap for violence data and each other mapped dataset
lviolence_overlap$overlap_11_del_map <- !is.na(lviolence_overlap_11_map$delimitada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)])
lviolence_overlap$overlap_16_del_map <- !is.na(lviolence_overlap_16_map$delimitada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)])
lviolence_overlap$overlap_PPTAL_del_map <- !is.na(lviolence_overlap_PPTAL$delplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_del_map <- !is.na(lviolence_overlap_11_map$delimitada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)]) | !is.na(lviolence_overlap_16_map$delimitada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)]) | !is.na(lviolence_overlap_PPTAL$delplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])


#finding declarada overlap for violence data and each other dataset
lviolence_overlap$overlap_11_dem <- !is.na(lviolence_overlap_11$declarada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)])
lviolence_overlap$overlap_16_dem <- !is.na(lviolence_overlap_16$declarada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)])
lviolence_overlap$overlap_PPTAL_dem <- !is.na(lviolence_overlap_PPTAL$demplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_dem <- !is.na(lviolence_overlap_11$declarada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)]) | !is.na(lviolence_overlap_16$declarada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)]) | !is.na(lviolence_overlap_PPTAL$demplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])

#finding declarada overlap for violence data and each other mapped dataset
lviolence_overlap$overlap_11_dem_map <- !is.na(lviolence_overlap_11_map$declarada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)])
lviolence_overlap$overlap_16_dem_map <- !is.na(lviolence_overlap_16_map$declarada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)])
lviolence_overlap$overlap_PPTAL_dem_map <- !is.na(lviolence_overlap_PPTAL$demplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_dem_map <- !is.na(lviolence_overlap_11_map$declarada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)]) | !is.na(lviolence_overlap_16_map$declarada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)]) | !is.na(lviolence_overlap_PPTAL$demplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])


#finding homologada overlap for violence data and each other dataset
lviolence_overlap$overlap_11_appr <- !is.na(lviolence_overlap_11$homologada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)])
lviolence_overlap$overlap_16_appr <- !is.na(lviolence_overlap_16$homologada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)])
lviolence_overlap$overlap_PPTAL_appr <- !is.na(lviolence_overlap_PPTAL$apprend_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_appr <- !is.na(lviolence_overlap_11$homologada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)]) | !is.na(lviolence_overlap_16$homologada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)]) | !is.na(lviolence_overlap_PPTAL$apprend_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])

#finding homologada overlap for violence data and each other mapped dataset
lviolence_overlap$overlap_11_appr_map <- !is.na(lviolence_overlap_11_map$homologada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)])
lviolence_overlap$overlap_16_appr_map <- !is.na(lviolence_overlap_16_map$homologada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)])
lviolence_overlap$overlap_PPTAL_appr_map <- !is.na(lviolence_overlap_PPTAL$apprend_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_appr_map <- !is.na(lviolence_overlap_11_map$homologada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)]) | !is.na(lviolence_overlap_16_map$homologada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)]) | !is.na(lviolence_overlap_PPTAL$apprend_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])


#finding regularizada overlap for violence data and each other dataset
lviolence_overlap$overlap_11_reg <- !is.na(lviolence_overlap_11$regularizada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)])
lviolence_overlap$overlap_16_reg <- !is.na(lviolence_overlap_16$regularizada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)])
lviolence_overlap$overlap_PPTAL_reg <- !is.na(lviolence_overlap_PPTAL$regplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_reg <- !is.na(lviolence_overlap_11$regularizada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11$id_ad)]) | !is.na(lviolence_overlap_16$regularizada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16$id_ad)]) | !is.na(lviolence_overlap_PPTAL$regplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])

#finding regularizada overlap for violence data and each other mapped dataset
lviolence_overlap$overlap_11_reg_map <- !is.na(lviolence_overlap_11_map$regularizada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)])
lviolence_overlap$overlap_16_reg_map <- !is.na(lviolence_overlap_16_map$regularizada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)])
lviolence_overlap$overlap_PPTAL_reg_map <- !is.na(lviolence_overlap_PPTAL$regplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])
lviolence_overlap$overlap_any_reg_map <- !is.na(lviolence_overlap_11_map$regularizada2011[match(lviolence_overlap$id_ad, lviolence_overlap_11_map$id_ad)]) | !is.na(lviolence_overlap_16_map$regularizada2016[match(lviolence_overlap$id_ad, lviolence_overlap_16_map$id_ad)]) | !is.na(lviolence_overlap_PPTAL$regplan_m[match(lviolence_overlap$id_ad, lviolence_overlap_PPTAL$id_ad)])

##################Counting the Overlap for each stage for land violence###########
loverlap_11_id_val <- length(which(lviolence_overlap$overlap_11_id))
loverlap_16_id_val <- length(which(lviolence_overlap$overlap_16_id))
loverlap_PPTAL_id_val <- length(which(lviolence_overlap$overlap_PPTAL_id))
loverlap_11_id_map_val <- length(which(lviolence_overlap$overlap_11_id_map))
loverlap_16_id_map_val <- length(which(lviolence_overlap$overlap_16_id_map))
loverlap_any_id_val <- length(which(lviolence_overlap$overlap_any_id))
loverlap_any_id_map_val <- length(which(lviolence_overlap$overlap_any_id_map))

loverlap_11_del_val <- length(which(lviolence_overlap$overlap_11_del))
loverlap_16_del_val <- length(which(lviolence_overlap$overlap_16_del))
loverlap_PPTAL_del_val <- length(which(lviolence_overlap$overlap_PPTAL_del))
loverlap_11_del_map_val <- length(which(lviolence_overlap$overlap_11_del_map))
loverlap_16_del_map_val <- length(which(lviolence_overlap$overlap_16_del_map))
loverlap_any_del_val <- length(which(lviolence_overlap$overlap_any_del))
loverlap_any_del_map_val <- length(which(lviolence_overlap$overlap_any_del_map))

loverlap_11_dem_val <- length(which(lviolence_overlap$overlap_11_dem))
loverlap_16_dem_val <- length(which(lviolence_overlap$overlap_16_dem))
loverlap_PPTAL_dem_val <- length(which(lviolence_overlap$overlap_PPTAL_dem))
loverlap_11_dem_map_val <- length(which(lviolence_overlap$overlap_11_dem_map))
loverlap_16_dem_map_val <- length(which(lviolence_overlap$overlap_16_dem_map))
loverlap_any_dem_val <- length(which(lviolence_overlap$overlap_any_dem))
loverlap_any_dem_map_val <- length(which(lviolence_overlap$overlap_any_dem_map))

loverlap_11_appr_val <- length(which(lviolence_overlap$overlap_11_appr))
loverlap_16_appr_val <- length(which(lviolence_overlap$overlap_16_appr))
loverlap_PPTAL_appr_val <- length(which(lviolence_overlap$overlap_PPTAL_appr))
loverlap_11_appr_map_val <- length(which(lviolence_overlap$overlap_11_appr_map))
loverlap_16_appr_map_val <- length(which(lviolence_overlap$overlap_16_appr_map))
loverlap_any_appr_val <- length(which(lviolence_overlap$overlap_any_appr))
loverlap_any_appr_map_val <- length(which(lviolence_overlap$overlap_any_appr_map))

loverlap_11_reg_val <- length(which(lviolence_overlap$overlap_11_reg))
loverlap_16_reg_val <- length(which(lviolence_overlap$overlap_16_reg))
loverlap_PPTAL_reg_val <- length(which(lviolence_overlap$overlap_PPTAL_reg))
loverlap_11_reg_map_val <- length(which(lviolence_overlap$overlap_11_reg_map))
loverlap_16_reg_map_val <- length(which(lviolence_overlap$overlap_16_reg_map))
loverlap_any_reg_val <- length(which(lviolence_overlap$overlap_any_reg))
loverlap_any_reg_map_val <- length(which(lviolence_overlap$overlap_any_reg_map))


loverlap_any_any_val <- length(which(lviolence_overlap$overlap_any))
loverlap_any_any_val_map <- length(which(lviolence_overlap$overlap_any_map))


#creates a table of individual violence overlap with stage data####
print("Individual Violence Overlap with Stage Data:")
table_four <- matrix(c(ioverlap_11_id_val, ioverlap_16_id_val, ioverlap_PPTAL_id_val, ioverlap_11_id_map_val, ioverlap_16_id_map_val, ioverlap_any_id_val, ioverlap_any_id_map_val,
                       ioverlap_11_del_val, ioverlap_16_del_val, ioverlap_PPTAL_del_val, ioverlap_11_del_map_val, ioverlap_16_del_map_val, ioverlap_any_del_val, ioverlap_any_del_map_val,
                       ioverlap_11_dem_val, ioverlap_16_dem_val, ioverlap_PPTAL_dem_val, ioverlap_11_dem_map_val, ioverlap_16_dem_map_val, ioverlap_any_dem_val, ioverlap_any_dem_map_val,
                       ioverlap_11_appr_val, ioverlap_16_appr_val, ioverlap_PPTAL_appr_val, ioverlap_11_appr_map_val, ioverlap_16_appr_map_val, ioverlap_any_appr_val, ioverlap_any_appr_map_val,
                       ioverlap_11_reg_val, ioverlap_16_reg_val, ioverlap_PPTAL_reg_val, ioverlap_11_reg_map_val, ioverlap_16_reg_map_val, ioverlap_any_reg_val, ioverlap_any_reg_map_val),
                     ncol = 7, byrow = TRUE)
colnames(table_four) <- c("~2011", "~2016", "~PPTAL", "~2011 Mapped", "~2016 Mapped", "~Any", "~Any Mapped")
rownames(table_four) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_four <- as.table(table_four)
print(table_four)
print("---------------------------------------------------------------------------------------------------------------")

#creates a table of land violence overlap with stage data####
print("Land Violence Overlap with Stage Data:")
table_five <- matrix(c(loverlap_11_id_val, loverlap_16_id_val, loverlap_PPTAL_id_val, loverlap_11_id_map_val, loverlap_16_id_map_val, loverlap_any_id_val, loverlap_any_id_map_val,
                       loverlap_11_del_val, loverlap_16_del_val, loverlap_PPTAL_del_val, loverlap_11_del_map_val, loverlap_16_del_map_val, loverlap_any_del_val, loverlap_any_del_map_val,
                       loverlap_11_dem_val, loverlap_16_dem_val, loverlap_PPTAL_dem_val, loverlap_11_dem_map_val, loverlap_16_dem_map_val, loverlap_any_dem_val, loverlap_any_dem_map_val,
                       loverlap_11_appr_val, loverlap_16_appr_val, loverlap_PPTAL_appr_val, loverlap_11_appr_map_val, loverlap_16_appr_map_val, loverlap_any_appr_val, loverlap_any_appr_map_val,
                       loverlap_11_reg_val, loverlap_16_reg_val, loverlap_PPTAL_reg_val, loverlap_11_reg_map_val, loverlap_16_reg_map_val, loverlap_any_reg_val, loverlap_any_reg_map_val),
                     ncol = 7, byrow = TRUE)
colnames(table_five) <- c("~2011", "~2016", "~PPTAL", "~2011 Mapped", "~2016 Mapped", "~Any", "~Any Mapped")
rownames(table_five) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_five <- as.table(table_five)
print(table_five)
print("---------------------------------------------------------------------------------------------------------------")



#counts frequency of each ad_id in the data (number of incidences of violence per unit)
iviolence_overlap$freq <- table(data_iviolence$ad_id)
lviolence_overlap$freq <- table(data_lviolence$ad_id)


#counts number of ad_ids with 1-5+ incidences of violence for mapped id (individual)####
ifreq_11_id_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_id_map == TRUE] == 1))
ifreq_16_id_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_id_map == TRUE] == 1))
ifreq_PPTAL_id_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_id_map == TRUE] == 1))

ifreq_11_id_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_id_map == TRUE] == 2))
ifreq_16_id_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_id_map == TRUE] == 2))
ifreq_PPTAL_id_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_id_map == TRUE] == 2))

ifreq_11_id_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_id_map == TRUE] == 3))
ifreq_16_id_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_id_map == TRUE] == 3))
ifreq_PPTAL_id_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_id_map == TRUE] == 3))

ifreq_11_id_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_id_map == TRUE] == 4))
ifreq_16_id_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_id_map == TRUE] == 4))
ifreq_PPTAL_id_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_id_map == TRUE] == 4))

ifreq_11_id_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_id_map == TRUE] >= 5))
ifreq_16_id_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_id_map == TRUE] >= 5))
ifreq_PPTAL_id_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_id_map == TRUE] >= 5))


#counts number of ad_ids with 1-5+ incidences of violence for mapped del
ifreq_11_del_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_del_map == TRUE] == 1))
ifreq_16_del_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_del_map == TRUE] == 1))
ifreq_PPTAL_del_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_del_map == TRUE] == 1))

ifreq_11_del_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_del_map == TRUE] == 2))
ifreq_16_del_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_del_map == TRUE] == 2))
ifreq_PPTAL_del_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_del_map == TRUE] == 2))

ifreq_11_del_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_del_map == TRUE] == 3))
ifreq_16_del_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_del_map == TRUE] == 3))
ifreq_PPTAL_del_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_del_map == TRUE] == 3))

ifreq_11_del_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_del_map == TRUE] == 4))
ifreq_16_del_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_del_map == TRUE] == 4))
ifreq_PPTAL_del_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_del_map == TRUE] == 4))

ifreq_11_del_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_del_map == TRUE] >= 5))
ifreq_16_del_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_del_map == TRUE] >= 5))
ifreq_PPTAL_del_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_del_map == TRUE] >= 5))


#counts number of ad_ids with 1-5+ incidences of violence for mapped dem
ifreq_11_dem_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_dem_map == TRUE] == 1))
ifreq_16_dem_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_dem_map == TRUE] == 1))
ifreq_PPTAL_dem_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_dem_map == TRUE] == 1))

ifreq_11_dem_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_dem_map == TRUE] == 2))
ifreq_16_dem_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_dem_map == TRUE] == 2))
ifreq_PPTAL_dem_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_dem_map == TRUE] == 2))

ifreq_11_dem_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_dem_map == TRUE] == 3))
ifreq_16_dem_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_dem_map == TRUE] == 3))
ifreq_PPTAL_dem_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_dem_map == TRUE] == 3))

ifreq_11_dem_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_dem_map == TRUE] == 4))
ifreq_16_dem_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_dem_map == TRUE] == 4))
ifreq_PPTAL_dem_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_dem_map == TRUE] == 4))

ifreq_11_dem_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_dem_map == TRUE] >= 5))
ifreq_16_dem_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_dem_map == TRUE] >= 5))
ifreq_PPTAL_dem_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_dem_map == TRUE] >= 5))


#counts number of ad_ids with 1-5+ incidences of violence for mapped appr
ifreq_11_appr_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_appr_map == TRUE] == 1))
ifreq_16_appr_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_appr_map == TRUE] == 1))
ifreq_PPTAL_appr_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_appr_map == TRUE] == 1))

ifreq_11_appr_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_appr_map == TRUE] == 2))
ifreq_16_appr_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_appr_map == TRUE] == 2))
ifreq_PPTAL_appr_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_appr_map == TRUE] == 2))

ifreq_11_appr_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_appr_map == TRUE] == 3))
ifreq_16_appr_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_appr_map == TRUE] == 3))
ifreq_PPTAL_appr_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_appr_map == TRUE] == 3))

ifreq_11_appr_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_appr_map == TRUE] == 4))
ifreq_16_appr_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_appr_map == TRUE] == 4))
ifreq_PPTAL_appr_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_appr_map == TRUE] == 4))

ifreq_11_appr_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_appr_map == TRUE] >= 5))
ifreq_16_appr_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_appr_map == TRUE] >= 5))
ifreq_PPTAL_appr_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_appr_map == TRUE] >= 5))


#counts number of ad_ids with 1-5+ incidences of violence for mapped reg
ifreq_11_reg_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_reg_map == TRUE] == 1))
ifreq_16_reg_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_reg_map == TRUE] == 1))
ifreq_PPTAL_reg_map1 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_reg_map == TRUE] == 1))

ifreq_11_reg_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_reg_map == TRUE] == 2))
ifreq_16_reg_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_reg_map == TRUE] == 2))
ifreq_PPTAL_reg_map2 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_reg_map == TRUE] == 2))

ifreq_11_reg_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_reg_map == TRUE] == 3))
ifreq_16_reg_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_reg_map == TRUE] == 3))
ifreq_PPTAL_reg_map3 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_reg_map == TRUE] == 3))

ifreq_11_reg_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_reg_map == TRUE] == 4))
ifreq_16_reg_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_reg_map == TRUE] == 4))
ifreq_PPTAL_reg_map4 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_reg_map == TRUE] == 4))

ifreq_11_reg_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_11_reg_map == TRUE] >= 5))
ifreq_16_reg_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_16_reg_map == TRUE] >= 5))
ifreq_PPTAL_reg_map5 <- length(which(iviolence_overlap$freq[iviolence_overlap$overlap_PPTAL_reg_map == TRUE] >= 5))



#counts number of ad_ids with 1-5+ incidences of violence for mapped id (land)####
lfreq_11_id_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_id_map == TRUE] == 1))
lfreq_16_id_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_id_map == TRUE] == 1))
lfreq_PPTAL_id_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_id_map == TRUE] == 1))

lfreq_11_id_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_id_map == TRUE] == 2))
lfreq_16_id_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_id_map == TRUE] == 2))
lfreq_PPTAL_id_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_id_map == TRUE] == 2))

lfreq_11_id_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_id_map == TRUE] == 3))
lfreq_16_id_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_id_map == TRUE] == 3))
lfreq_PPTAL_id_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_id_map == TRUE] == 3))

lfreq_11_id_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_id_map == TRUE] == 4))
lfreq_16_id_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_id_map == TRUE] == 4))
lfreq_PPTAL_id_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_id_map == TRUE] == 4))

lfreq_11_id_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_id_map == TRUE] >= 5))
lfreq_16_id_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_id_map == TRUE] >= 5))
lfreq_PPTAL_id_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_id_map == TRUE] >= 5))


#counts number of ad_ids with 1-5+ incidences of violence for mapped del
lfreq_11_del_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_del_map == TRUE] == 1))
lfreq_16_del_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_del_map == TRUE] == 1))
lfreq_PPTAL_del_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_del_map == TRUE] == 1))

lfreq_11_del_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_del_map == TRUE] == 2))
lfreq_16_del_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_del_map == TRUE] == 2))
lfreq_PPTAL_del_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_del_map == TRUE] == 2))

lfreq_11_del_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_del_map == TRUE] == 3))
lfreq_16_del_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_del_map == TRUE] == 3))
lfreq_PPTAL_del_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_del_map == TRUE] == 3))

lfreq_11_del_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_del_map == TRUE] == 4))
lfreq_16_del_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_del_map == TRUE] == 4))
lfreq_PPTAL_del_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_del_map == TRUE] == 4))

lfreq_11_del_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_del_map == TRUE] >= 5))
lfreq_16_del_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_del_map == TRUE] >= 5))
lfreq_PPTAL_del_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_del_map == TRUE] >= 5))


#counts number of ad_ids with 1-5+ incidences of violence for mapped dem
lfreq_11_dem_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_dem_map == TRUE] == 1))
lfreq_16_dem_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_dem_map == TRUE] == 1))
lfreq_PPTAL_dem_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_dem_map == TRUE] == 1))

lfreq_11_dem_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_dem_map == TRUE] == 2))
lfreq_16_dem_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_dem_map == TRUE] == 2))
lfreq_PPTAL_dem_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_dem_map == TRUE] == 2))

lfreq_11_dem_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_dem_map == TRUE] == 3))
lfreq_16_dem_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_dem_map == TRUE] == 3))
lfreq_PPTAL_dem_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_dem_map == TRUE] == 3))

lfreq_11_dem_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_dem_map == TRUE] == 4))
lfreq_16_dem_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_dem_map == TRUE] == 4))
lfreq_PPTAL_dem_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_dem_map == TRUE] == 4))

lfreq_11_dem_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_dem_map == TRUE] >= 5))
lfreq_16_dem_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_dem_map == TRUE] >= 5))
lfreq_PPTAL_dem_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_dem_map == TRUE] >= 5))


#counts number of ad_ids with 1-5+ incidences of violence for mapped appr
lfreq_11_appr_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_appr_map == TRUE] == 1))
lfreq_16_appr_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_appr_map == TRUE] == 1))
lfreq_PPTAL_appr_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_appr_map == TRUE] == 1))

lfreq_11_appr_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_appr_map == TRUE] == 2))
lfreq_16_appr_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_appr_map == TRUE] == 2))
lfreq_PPTAL_appr_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_appr_map == TRUE] == 2))

lfreq_11_appr_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_appr_map == TRUE] == 3))
lfreq_16_appr_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_appr_map == TRUE] == 3))
lfreq_PPTAL_appr_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_appr_map == TRUE] == 3))

lfreq_11_appr_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_appr_map == TRUE] == 4))
lfreq_16_appr_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_appr_map == TRUE] == 4))
lfreq_PPTAL_appr_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_appr_map == TRUE] == 4))

lfreq_11_appr_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_appr_map == TRUE] >= 5))
lfreq_16_appr_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_appr_map == TRUE] >= 5))
lfreq_PPTAL_appr_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_appr_map == TRUE] >= 5))


#counts number of ad_ids with 1-5+ incidences of violence for mapped reg
lfreq_11_reg_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_reg_map == TRUE] == 1))
lfreq_16_reg_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_reg_map == TRUE] == 1))
lfreq_PPTAL_reg_map1 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_reg_map == TRUE] == 1))

lfreq_11_reg_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_reg_map == TRUE] == 2))
lfreq_16_reg_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_reg_map == TRUE] == 2))
lfreq_PPTAL_reg_map2 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_reg_map == TRUE] == 2))

lfreq_11_reg_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_reg_map == TRUE] == 3))
lfreq_16_reg_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_reg_map == TRUE] == 3))
lfreq_PPTAL_reg_map3 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_reg_map == TRUE] == 3))

lfreq_11_reg_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_reg_map == TRUE] == 4))
lfreq_16_reg_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_reg_map == TRUE] == 4))
lfreq_PPTAL_reg_map4 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_reg_map == TRUE] == 4))

lfreq_11_reg_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_11_reg_map == TRUE] >= 5))
lfreq_16_reg_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_16_reg_map == TRUE] >= 5))
lfreq_PPTAL_reg_map5 <- length(which(lviolence_overlap$freq[lviolence_overlap$overlap_PPTAL_reg_map == TRUE] >= 5))


#############################Start of Frequency Tables####################################


#creates a table of frequencies of incidences of violence for 2011 mapped (individual)
print("Individual Violence frequencies of incidences (2011):")
table_six <- matrix(c(ifreq_11_id_map1, ifreq_11_id_map2, ifreq_11_id_map3, ifreq_11_id_map4, ifreq_11_id_map5,
                      ifreq_11_del_map1, ifreq_11_del_map2, ifreq_11_del_map3, ifreq_11_del_map4, ifreq_11_del_map5,
                      ifreq_11_dem_map1, ifreq_11_dem_map2, ifreq_11_dem_map3, ifreq_11_dem_map4, ifreq_11_dem_map5,
                      ifreq_11_appr_map1, ifreq_11_appr_map2, ifreq_11_appr_map3, ifreq_11_appr_map4, ifreq_11_appr_map5,
                      ifreq_11_reg_map1, ifreq_11_reg_map2, ifreq_11_reg_map3, ifreq_11_reg_map4, ifreq_11_reg_map5),
                    ncol = 5, byrow = TRUE)
colnames(table_six) <- c("1", "2", "3", "4", "5+")
rownames(table_six) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_six <- as.table(table_six)
print(table_six)
print("---------------------------------------------------------------------------------------------------------------")

#creates a table of frequencies of incidences of violence for 2016 mapped (individual)
print("Individual Violence frequencies of incidences (2016):")
table_seven <- matrix(c(ifreq_16_id_map1, ifreq_16_id_map2, ifreq_16_id_map3, ifreq_16_id_map4, ifreq_16_id_map5,
                        ifreq_16_del_map1, ifreq_16_del_map2, ifreq_16_del_map3, ifreq_16_del_map4, ifreq_16_del_map5,
                        ifreq_16_dem_map1, ifreq_16_dem_map2, ifreq_16_dem_map3, ifreq_16_dem_map4, ifreq_16_dem_map5,
                        ifreq_16_appr_map1, ifreq_16_appr_map2, ifreq_16_appr_map3, ifreq_16_appr_map4, ifreq_16_appr_map5,
                        ifreq_16_reg_map1, ifreq_16_reg_map2, ifreq_16_reg_map3, ifreq_16_reg_map4, ifreq_16_reg_map5),
                      ncol = 5, byrow = TRUE)
colnames(table_seven) <- c("1", "2", "3", "4", "5+")
rownames(table_seven) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_seven <- as.table(table_seven)
print(table_seven)
print("---------------------------------------------------------------------------------------------------------------")

#creates a table of frequencies of incidences of violence for PPTAL (individual)
print("Individual Violence frequencies of incidences (PPTAL):")
table_eight <- matrix(c(ifreq_PPTAL_id_map1, ifreq_PPTAL_id_map2, ifreq_PPTAL_id_map3, ifreq_PPTAL_id_map4, ifreq_PPTAL_id_map5,
                        ifreq_PPTAL_del_map1, ifreq_PPTAL_del_map2, ifreq_PPTAL_del_map3, ifreq_PPTAL_del_map4, ifreq_PPTAL_del_map5,
                        ifreq_PPTAL_dem_map1, ifreq_PPTAL_dem_map2, ifreq_PPTAL_dem_map3, ifreq_PPTAL_dem_map4, ifreq_PPTAL_dem_map5,
                        ifreq_PPTAL_appr_map1, ifreq_PPTAL_appr_map2, ifreq_PPTAL_appr_map3, ifreq_PPTAL_appr_map4, ifreq_PPTAL_appr_map5,
                        ifreq_PPTAL_reg_map1, ifreq_PPTAL_reg_map2, ifreq_PPTAL_reg_map3, ifreq_PPTAL_reg_map4, ifreq_PPTAL_reg_map5),
                      ncol = 5, byrow = TRUE)
colnames(table_eight) <- c("1", "2", "3", "4", "5+")
rownames(table_eight) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_eight <- as.table(table_eight)
print(table_eight)
print("---------------------------------------------------------------------------------------------------------------")

#==================================================================================================================


#creates a table of frequencies of incidences of violence for 2011 mapped (land)
print("Land Violence frequencies of incidences (2011):")
table_nine <- matrix(c(lfreq_11_id_map1, lfreq_11_id_map2, lfreq_11_id_map3, lfreq_11_id_map4, lfreq_11_id_map5,
                       lfreq_11_del_map1, lfreq_11_del_map2, lfreq_11_del_map3, lfreq_11_del_map4, lfreq_11_del_map5,
                       lfreq_11_dem_map1, lfreq_11_dem_map2, lfreq_11_dem_map3, lfreq_11_dem_map4, lfreq_11_dem_map5,
                       lfreq_11_appr_map1, lfreq_11_appr_map2, lfreq_11_appr_map3, lfreq_11_appr_map4, lfreq_11_appr_map5,
                       lfreq_11_reg_map1, lfreq_11_reg_map2, lfreq_11_reg_map3, lfreq_11_reg_map4, lfreq_11_reg_map5),
                     ncol = 5, byrow = TRUE)
colnames(table_nine) <- c("1", "2", "3", "4", "5+")
rownames(table_nine) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_nine <- as.table(table_nine)
print(table_nine)
print("---------------------------------------------------------------------------------------------------------------")

#creates a table of frequencies of incidences of violence for 2016 mapped (land)
print("Land Violence frequencies of incidences (2016):")
table_ten <- matrix(c(lfreq_16_id_map1, lfreq_16_id_map2, lfreq_16_id_map3, lfreq_16_id_map4, lfreq_16_id_map5,
                      lfreq_16_del_map1, lfreq_16_del_map2, lfreq_16_del_map3, lfreq_16_del_map4, lfreq_16_del_map5,
                      lfreq_16_dem_map1, lfreq_16_dem_map2, lfreq_16_dem_map3, lfreq_16_dem_map4, lfreq_16_dem_map5,
                      lfreq_16_appr_map1, lfreq_16_appr_map2, lfreq_16_appr_map3, lfreq_16_appr_map4, lfreq_16_appr_map5,
                      lfreq_16_reg_map1, lfreq_16_reg_map2, lfreq_16_reg_map3, lfreq_16_reg_map4, lfreq_16_reg_map5),
                    ncol = 5, byrow = TRUE)
colnames(table_ten) <- c("1", "2", "3", "4", "5+")
rownames(table_ten) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_ten <- as.table(table_ten)
print(table_ten)
print("---------------------------------------------------------------------------------------------------------------")

#creates a table of frequencies of incidences of violence for PPTAL (land)
print("Land Violence frequencies of incidences (PPTAL):")
table_eleven <- matrix(c(lfreq_PPTAL_id_map1, lfreq_PPTAL_id_map2, lfreq_PPTAL_id_map3, lfreq_PPTAL_id_map4, lfreq_PPTAL_id_map5,
                         lfreq_PPTAL_del_map1, lfreq_PPTAL_del_map2, lfreq_PPTAL_del_map3, lfreq_PPTAL_del_map4, lfreq_PPTAL_del_map5,
                         lfreq_PPTAL_dem_map1, lfreq_PPTAL_dem_map2, lfreq_PPTAL_dem_map3, lfreq_PPTAL_dem_map4, lfreq_PPTAL_dem_map5,
                         lfreq_PPTAL_appr_map1, lfreq_PPTAL_appr_map2, lfreq_PPTAL_appr_map3, lfreq_PPTAL_appr_map4, lfreq_PPTAL_appr_map5,
                         lfreq_PPTAL_reg_map1, lfreq_PPTAL_reg_map2, lfreq_PPTAL_reg_map3, lfreq_PPTAL_reg_map4, lfreq_PPTAL_reg_map5),
                       ncol = 5, byrow = TRUE)
colnames(table_eleven) <- c("1", "2", "3", "4", "5+")
rownames(table_eleven) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_eleven <- as.table(table_eleven)
print(table_eleven)
print("---------------------------------------------------------------------------------------------------------------")




#finds the total number of individual violence incidents that have any demarcation data
ifreq_any_id_val <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_id == TRUE])
ifreq_any_del_val <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_del == TRUE])
ifreq_any_dem_val <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_dem == TRUE])
ifreq_any_appr_val <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_appr == TRUE])
ifreq_any_reg_val <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_reg == TRUE])

ifreq_any_any_val <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any == TRUE])

#finds the total number of individual violence incidents that have any demarcation data
ifreq_any_id_val_map <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_id_map == TRUE])
ifreq_any_del_val_map <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_del_map == TRUE])
ifreq_any_dem_val_map <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_dem_map == TRUE])
ifreq_any_appr_val_map <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_appr_map == TRUE])
ifreq_any_reg_val_map <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_reg_map == TRUE])

ifreq_any_any_val_map <- sum(iviolence_overlap$freq[iviolence_overlap$overlap_any_map == TRUE])

#finds the total number of land violence incidents that have any demarcation data
lfreq_any_id_val <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_id == TRUE])
lfreq_any_del_val <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_del == TRUE])
lfreq_any_dem_val <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_dem == TRUE])
lfreq_any_appr_val <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_appr == TRUE])
lfreq_any_reg_val <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_reg == TRUE])

lfreq_any_any_val <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any == TRUE])

#finds the total number of land violence incidents that have any demarcation data
lfreq_any_id_val_map <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_id_map == TRUE])
lfreq_any_del_val_map <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_del_map == TRUE])
lfreq_any_dem_val_map <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_dem_map == TRUE])
lfreq_any_appr_val_map <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_appr_map == TRUE])
lfreq_any_reg_val_map <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_reg_map == TRUE])

lfreq_any_any_val_map <- sum(lviolence_overlap$freq[lviolence_overlap$overlap_any_map == TRUE])


#Concept note table 2####
print("Table of frequency of individual violence")
table_conceptnote2 <- matrix(c(ifreq_any_id_val, ifreq_any_id_val_map,
                               ifreq_any_del_val, ifreq_any_del_val_map,
                               ifreq_any_dem_val, ifreq_any_dem_val_map,
                               ifreq_any_appr_val, ifreq_any_appr_val_map,
                               ifreq_any_reg_val, ifreq_any_reg_val_map),
                             ncol = 2, byrow = TRUE)
colnames(table_conceptnote2) <- c("Counts of Individual Violence Incidents", "Counts of Individual Violence Incidents - Mapped")
rownames(table_conceptnote2) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_conceptnote2 <- as.table(table_conceptnote2)
print(table_conceptnote2)
print("------------------------------------------------------------------------------------------")


#Concept note table 3####
print("Table of frequency of land violence")
table_conceptnote3 <- matrix(c(lfreq_any_id_val, lfreq_any_id_val_map,
                               lfreq_any_del_val, lfreq_any_del_val_map,
                               lfreq_any_dem_val, lfreq_any_dem_val_map,
                               lfreq_any_appr_val, lfreq_any_appr_val_map,
                               lfreq_any_reg_val, lfreq_any_reg_val_map),
                             ncol = 2, byrow = TRUE)
colnames(table_conceptnote3) <- c("Counts of Land Violence Incidents", "Counts of Land Violence Incidents - Mapped")
rownames(table_conceptnote3) <- c("Identificada", "Delimitada", "Declarada", "Homologada", "Regularizada")
table_conceptnote3 <- as.table(table_conceptnote3)
print(table_conceptnote3)
print("------------------------------------------------------------------------------------------")



print("END OF TABLES")


#finding number of commmunities with individual violence
#ifreq_any_id <- length(iviolence_overlap$freq[iviolence_overlap$overlap_any_id])
#ifreq_any_del <- length(iviolence_overlap$freq[iviolence_overlap$overlap_any_del])
#ifreq_any_dem <- length(iviolence_overlap$freq[iviolence_overlap$overlap_any_dem])
#ifreq_any_appr <- length(iviolence_overlap$freq[iviolence_overlap$overlap_any_appr])
#ifreq_any_reg <- length(iviolence_overlap$freq[iviolence_overlap$overlap_any_reg])


#used for getting the tables into the clipboard (one at a time) and copying them into Excel

#write.table(table_four, "clipboard", sep = "\t")
#write.table(table_five, "clipboard", sep = "\t")
#write.table(table_six, "clipboard", sep = "\t")
#write.table(table_seven, "clipboard", sep = "\t")
#write.table(table_eight, "clipboard", sep = "\t")
#write.table(table_nine, "clipboard", sep = "\t")
#write.table(table_ten, "clipboard", sep = "\t")
#write.table(table_eleven, "clipboard", sep = "\t")
#write.table(table_conceptnote1, "clipboard", sep = "\t")
#write.table(table_conceptnote2, "clipboard", sep = "\t")
#write.table(table_conceptnote3, "clipboard", sep = "\t")

print(ioverlap_any_any_val)
print(ioverlap_any_any_val_map)
print(loverlap_any_any_val)
print(loverlap_any_any_val_map)

print(ifreq_any_any_val)
print(ifreq_any_any_val_map)
print(lfreq_any_any_val)
print(lfreq_any_any_val_map)

#New Tasks to find specific demarcation between dates numbers
#Number of PPTAL communities demarcated between 2004 and 2008
PPTAL_demplan_2004_2008 <- length(data_merged$id_587[(data_merged$demplan_y > 2003) & (data_merged$demplan_y < 2009)])
print(paste0("PPTAL_demplan_2004_2008: ", PPTAL_demplan_2004_2008))

PPTAL_demend_2004_2008 <- length(data_merged$id_587[(data_merged$demend_y > 2003) & (data_merged$demend_y < 2009)])
print(paste0("PPTAL_demend_2004_2008: ", PPTAL_demend_2004_2008))






#merges the two data frames by id_733 (the id variable that they have in common)
data_merged <- merge(data2011, data2016, by="id_733")


#changes the demarcation stage variables to date variables
data_merged$identificada2011 <- as.Date(data_merged$identificada2011, format= "%m/%d/%Y")
data_merged$identificada2016 <- as.Date(data_merged$identificada2016, format= "%m/%d/%Y")
data_merged$delimitada2011 <- as.Date(data_merged$delimitada2011, format= "%m/%d/%Y")
data_merged$delimitada2016 <- as.Date(data_merged$delimitada2016, format= "%m/%d/%Y")
data_merged$declarada2011 <- as.Date(data_merged$declarada2011, format= "%m/%d/%Y")
data_merged$declarada2016 <- as.Date(data_merged$declarada2016, format= "%m/%d/%Y")
data_merged$homologada2011 <- as.Date(data_merged$homologada2011, format= "%m/%d/%Y")
data_merged$homologada2016 <- as.Date(data_merged$homologada2016, format= "%m/%d/%Y")
data_merged$regularizada2011 <- as.Date(data_merged$regularizada2011, format= "%m/%d/%Y")
data_merged$regularizada2016 <- as.Date(data_merged$regularizada2016, format= "%m/%d/%Y")
data_merged$reg_extra2011 <- as.Date(data_merged$reg_extra2011, format= "%m/%d/%Y")


#counts number of non-blank observations and stores it in new value for each stage
identificada_count2011 <- length(data_merged$identificada2011) - sum(is.na(data_merged$identificada2011))
identificada_count2016 <- length(data_merged$identificada2016) - sum(is.na(data_merged$identificada2016))
delimitada_count2011 <- length(data_merged$delimitada2011) - sum(is.na(data_merged$delimitada2011))
delimitada_count2016 <- length(data_merged$delimitada2016) - sum(is.na(data_merged$delimitada2016))
declarada_count2011 <- length(data_merged$declarada2011) - sum(is.na(data_merged$declarada2011))
declarada_count2016 <- length(data_merged$declarada2016) - sum(is.na(data_merged$declarada2016))
homologada_count2011 <- length(data_merged$homologada2011) - sum(is.na(data_merged$homologada2011))
homologada_count2016 <- length(data_merged$homologada2016) - sum(is.na(data_merged$homologada2016))
regularizada_count2011 <- length(data_merged$regularizada2011) - sum(is.na(data_merged$regularizada2011))
regularizada_count2016 <- length(data_merged$regularizada2016) - sum(is.na(data_merged$regularizada2016))
reg_extra_count2011 <- length(data_merged$reg_extra2011) - sum(is.na(data_merged$reg_extra2011))


#counts number of non-blank observations for each stage (in either dataset)
identificada_count_any <- length(data_merged$identificada2011) - sum(is.na(data_merged$identificada2011) & is.na(data_merged$identificada2016))
delimitada_count_any <- length(data_merged$delimitada2011) - sum(is.na(data_merged$delimitada2011) & is.na(data_merged$delimitada2016))
declarada_count_any <- length(data_merged$declarada2011) - sum(is.na(data_merged$declarada2011) & is.na(data_merged$declarada2016))
homologada_count_any <- length(data_merged$homologada2011) - sum(is.na(data_merged$homologada2011) & is.na(data_merged$homologada2016))
regularizada_count_any <- length(data_merged$regularizada2011) - sum(is.na(data_merged$regularizada2011) & is.na(data_merged$regularizada2016))


#finds percentage of non-blank observations and stores it in new value for each stage
identificada_ratio2011 <- 100*(length(data_merged$identificada2011) - sum(is.na(data_merged$identificada2011))) / length(data_merged$identificada2011)
identificada_ratio2016 <- 100*(length(data_merged$identificada2016) - sum(is.na(data_merged$identificada2016))) / length(data_merged$identificada2016)
delimitada_ratio2011 <- 100*(length(data_merged$delimitada2011) - sum(is.na(data_merged$delimitada2011))) / length(data_merged$delimitada2011)
delimitada_ratio2016 <- 100*(length(data_merged$delimitada2016) - sum(is.na(data_merged$delimitada2016))) / length(data_merged$delimitada2016)
declarada_ratio2011 <- 100*(length(data_merged$declarada2011) - sum(is.na(data_merged$declarada2011))) / length(data_merged$declarada2011)
declarada_ratio2016 <- 100*(length(data_merged$declarada2016) - sum(is.na(data_merged$declarada2016))) / length(data_merged$declarada2016)
homologada_ratio2011 <- 100*(length(data_merged$homologada2011) - sum(is.na(data_merged$homologada2011))) / length(data_merged$homologada2011)
homologada_ratio2016 <- 100*(length(data_merged$homologada2016) - sum(is.na(data_merged$homologada2016))) / length(data_merged$homologada2016)
regularizada_ratio2011 <- 100*(length(data_merged$regularizada2011) - sum(is.na(data_merged$regularizada2011))) / length(data_merged$regularizada2011)
regularizada_ratio2016 <- 100*(length(data_merged$regularizada2016) - sum(is.na(data_merged$regularizada2016))) / length(data_merged$regularizada2016)
reg_extra_ratio2011 <- 100*(length(data_merged$reg_extra2011) - sum(is.na(data_merged$reg_extra2011))) / length(data_merged$reg_extra2011)


#checks whether the observations in each variable are the same, makes an indicator variable TRUE if different, FALSE if the same
data_merged$identificada_diff <- data_merged$identificada2016!=data_merged$identificada2011
data_merged$delimitada_diff <- data_merged$delimitada2016!=data_merged$delimitada2011
data_merged$declarada_diff <- data_merged$declarada2016!=data_merged$declarada2011
data_merged$homologada_diff <- data_merged$homologada2016!=data_merged$homologada2011
data_merged$regularizada_diff <- data_merged$regularizada2016!=data_merged$regularizada2011


#counts and stores in new values the number of differences between the two datasets for each stage
identificada_diff_val <- length(which(data_merged$identificada_diff))
delimitada_diff_val <- length(which(data_merged$delimitada_diff))
declarada_diff_val <- length(which(data_merged$declarada_diff))
homologada_diff_val <- length(which(data_merged$homologada_diff))
regularizada_diff_val <- length(which(data_merged$regularizada_diff))


#counts and stores in new values the agreement between the two datasets for each stage
identificada_same_val <- length(which(!data_merged$identificada_diff))
delimitada_same_val <- length(which(!data_merged$delimitada_diff))
declarada_same_val <- length(which(!data_merged$declarada_diff))
homologada_same_val <- length(which(!data_merged$homologada_diff))
regularizada_same_val <- length(which(!data_merged$regularizada_diff))


#counts number of non-blank observations that have map data for each stage
identificada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$identificada2011) | is.na(data_merged$id_587))
identificada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$identificada2016) | is.na(data_merged$id_587))
delimitada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$delimitada2011) | is.na(data_merged$id_587))
delimitada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$delimitada2016) | is.na(data_merged$id_587))
declarada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$declarada2011) | is.na(data_merged$id_587))
declarada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$declarada2016) | is.na(data_merged$id_587))
homologada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$homologada2011) | is.na(data_merged$id_587))
homologada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$homologada2016) | is.na(data_merged$id_587))
regularizada_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$regularizada2011) | is.na(data_merged$id_587))
regularizada_map_count2016 <- length(data_merged$id_587) - sum(is.na(data_merged$regularizada2016) | is.na(data_merged$id_587))
reg_extra_map_count2011 <- length(data_merged$id_587) - sum(is.na(data_merged$reg_extra2011) | is.na(data_merged$id_587))


#counts number of non-blank mapped observations for each stage (in either dataset)
identificada_count_any_map <- length(data_merged$identificada2011) - sum((is.na(data_merged$identificada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$identificada2016) | is.na(data_merged$id_587)))
delimitada_count_any_map <- length(data_merged$delimitada2011) - sum((is.na(data_merged$delimitada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$delimitada2016) | is.na(data_merged$id_587)))
declarada_count_any_map <- length(data_merged$declarada2011) - sum((is.na(data_merged$declarada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$declarada2016) | is.na(data_merged$id_587)))
homologada_count_any_map <- length(data_merged$homologada2011) - sum((is.na(data_merged$homologada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$homologada2016) | is.na(data_merged$id_587)))
regularizada_count_any_map <- length(data_merged$regularizada2011) - sum((is.na(data_merged$regularizada2011) | is.na(data_merged$id_587)) & (is.na(data_merged$regularizada2016) | is.na(data_merged$id_587)))



#counts number of observations that are the same and have map data
identificada_same_map <- length(which(!data_merged$identificada_diff & !is.na(data_merged$id_587)))
delimitada_same_map <- length(which(!data_merged$delimitada_diff & !is.na(data_merged$id_587)))
declarada_same_map <- length(which(!data_merged$declarada_diff & !is.na(data_merged$id_587)))
homologada_same_map <- length(which(!data_merged$homologada_diff & !is.na(data_merged$id_587)))
regularizada_same_map <- length(which(!data_merged$regularizada_diff & !is.na(data_merged$id_587)))


#counts number of observations that are different and have map data
identificada_diff_map <- length(which((data_merged$identificada_diff) & is.na(data_merged$id_587)))
delimitada_diff_map <- length(which((data_merged$delimitada_diff) & is.na(data_merged$id_587)))
declarada_diff_map <- length(which((data_merged$declarada_diff) & is.na(data_merged$id_587)))
homologada_diff_map <- length(which((data_merged$homologada_diff) & is.na(data_merged$id_587)))
regularizada_diff_map <- length(which((data_merged$regularizada_diff) & is.na(data_merged$id_587)))


#changes the indicator variables from TRUE/FALSE to 1/0
data_merged$identificada_diff <- as.numeric(data_merged$identificada_diff)
data_merged$delimitada_diff <- as.numeric(data_merged$delimitada_diff)
data_merged$declarada_diff <- as.numeric(data_merged$declarada_diff)
data_merged$homologada_diff <- as.numeric(data_merged$homologada_diff)
data_merged$regularizada_diff <- as.numeric(data_merged$regularizada_diff)


#seperates the data into day, month, year columns for each stage
data_merged$identificada_day2016 <- as.numeric(format(data_merged$identificada2016, format = "%d"))
data_merged$identificada_month2016 <- as.numeric(format(data_merged$identificada2016, format = "%m"))
data_merged$identificada_year2016 <- as.numeric(format(data_merged$identificada2016, format = "%Y"))

data_merged$identificada_day2011 <- as.numeric(format(data_merged$identificada2011, format = "%d"))
data_merged$identificada_month2011 <- as.numeric(format(data_merged$identificada2011, format = "%m"))
data_merged$identificada_year2011 <- as.numeric(format(data_merged$identificada2011, format = "%Y"))

data_merged$delimitada_day2016 <- as.numeric(format(data_merged$delimitada2016, format = "%d"))
data_merged$delimitada_month2016 <- as.numeric(format(data_merged$delimitada2016, format = "%m"))
data_merged$delimitada_year2016 <- as.numeric(format(data_merged$delimitada2016, format = "%Y"))

data_merged$delimitada_day2011 <- as.numeric(format(data_merged$delimitada2011, format = "%d"))
data_merged$delimitada_month2011 <- as.numeric(format(data_merged$delimitada2011, format = "%m"))
data_merged$delimitada_year2011 <- as.numeric(format(data_merged$delimitada2011, format = "%Y"))

data_merged$declarada_day2016 <- as.numeric(format(data_merged$declarada2016, format = "%d"))
data_merged$declarada_month2016 <- as.numeric(format(data_merged$declarada2016, format = "%m"))
data_merged$declarada_year2016 <- as.numeric(format(data_merged$declarada2016, format = "%Y"))

data_merged$declarada_day2011 <- as.numeric(format(data_merged$declarada2011, format = "%d"))
data_merged$declarada_month2011 <- as.numeric(format(data_merged$declarada2011, format = "%m"))
data_merged$declarada_year2011 <- as.numeric(format(data_merged$declarada2011, format = "%Y"))

data_merged$homologada_day2016 <- as.numeric(format(data_merged$homologada2016, format = "%d"))
data_merged$homologada_month2016 <- as.numeric(format(data_merged$homologada2016, format = "%m"))
data_merged$homologada_year2016 <- as.numeric(format(data_merged$homologada2016, format = "%Y"))

data_merged$homologada_day2011 <- as.numeric(format(data_merged$homologada2011, format = "%d"))
data_merged$homologada_month2011 <- as.numeric(format(data_merged$homologada2011, format = "%m"))
data_merged$homologada_year2011 <- as.numeric(format(data_merged$homologada2011, format = "%Y"))

data_merged$regularizada_day2016 <- as.numeric(format(data_merged$regularizada2016, format = "%d"))
data_merged$regularizada_month2016 <- as.numeric(format(data_merged$regularizada2016, format = "%m"))
data_merged$regularizada_year2016 <- as.numeric(format(data_merged$regularizada2016, format = "%Y"))

data_merged$regularizada_day2011 <- as.numeric(format(data_merged$regularizada2011, format = "%d"))
data_merged$regularizada_month2011 <- as.numeric(format(data_merged$regularizada2011, format = "%m"))
data_merged$regularizada_year2011 <- as.numeric(format(data_merged$regularizada2011, format = "%Y"))

data_merged$reg_extra_day2011 <- as.numeric(format(data_merged$reg_extra2011, format = "%d"))
data_merged$reg_extra_month2011 <- as.numeric(format(data_merged$reg_extra2011, format = "%m"))
data_merged$reg_extra_year2011 <- as.numeric(format(data_merged$reg_extra2011, format = "%Y"))


#Number of 2016 communities demarcated between 2004 and 2014
FUNAI2016_dem_2004_2014 <- length(data_merged$id_733[which((data_merged$declarada_year2016 > 2003) & (data_merged$declarada_year2016 < 2015))])
print(paste0("FUNAI2016_dem_2004_2014: ", FUNAI2016_dem_2004_2014))

#Number of 2016 communities approved between 2004 and 2014
FUNAI2016_appr_2004_2014 <- length(data_merged$id_733[which((data_merged$homologada_year2016 > 2003) & (data_merged$homologada_year2016 < 2015))])
print(paste0("FUNAI2016_appr_2004_2014: ", FUNAI2016_appr_2004_2014))

#Number of 2016 communities registered between 2004 and 2014
FUNAI2016_reg_2004_2014 <- length(data_merged$id_733[which((data_merged$regularizada_year2016 > 2003) & (data_merged$regularizada_year2016 < 2015))])
print(paste0("FUNAI2016_reg_2004_2014: ", FUNAI2016_reg_2004_2014))


#Number of 2016 communities ever demarcated
FUNAI2016_dem_ever <- length(data_merged$id_733[!is.na(data_merged$declarada_year2016)])
print(paste0("FUNAI2016_dem_ever: ", FUNAI2016_dem_ever))

#Number of 2016 communities ever demarcated
FUNAI2016_appr_ever <- length(data_merged$id_733[!is.na(data_merged$homologada_year2016)])
print(paste0("FUNAI2016_appr_ever: ", FUNAI2016_appr_ever))

#Number of 2016 communities ever demarcated
FUNAI2016_reg_ever <- length(data_merged$id_733[!is.na(data_merged$regularizada_year2016)])
print(paste0("FUNAI2016_reg_ever: ", FUNAI2016_reg_ever))


#Saving the important datasets - NOTE: Saves them in working directory
#data_demdates <- data_merged
#save(data_demdates, file = "Demarcation Date Data.Rda")
#save(data_iviolence, file = "Individual Violence Data.Rda")
#save(data_lviolence, file = "Land Violence Data.Rda")
#save(iviolence_overlap, file = "Individual Violence Overlap.Rda")
#save(lviolence_overlap, file = "Land Violence Overlap.Rda")


#Opens some of the data in the viewer

#opens PPTAL data in the viewer
#View(dataPPTAL)
#View(dataPPTAL[101:200])
#View(dataPPTAL[201:300])
#View(dataPPTAL[301:341])

#View(data_merged)

#View(data_merged[324:423])

#opens violence data in the viewer
#View(data_iviolence)
#View(data_lviolence)
#View(iviolence_overlap)
#View(lviolence_overlap)



