#This file includes the code from the example that Dan created for MatchIt

library(MatchIt)
library(devtools)
detach("package:MatchIt", unload=TRUE)
#load_all("~/Desktop/Github/MatchIt/R")
# library(devtools)
# install_github("itpir/matchit")


#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("/home/aiddata/Desktop/Github/kfw2_amazon_conflict")


#load saved data
load("Interim Data/Land_Violence_Data.Rda")
load("Interim Data/Individual_Violence_Data.Rda")
load("Interim Data/Demarcation_Date_Data.Rda")
load("Interim Data/Land_Violence_Overlap.Rda")
load("Interim Data/Individual_Violence_Overlap.Rda")

#Make up a treatment
data_iviolence["treatment.status"] <- 0
data_iviolence["treatment.status"][data_iviolence$Case.No < 1000,] <- 1

example_dta <- data_iviolence[complete.cases(data_iviolence),]

nonspatial_matchit <- matchit(treatment.status ~ ad_id, data=example_dta ,
                              method = "nearest", distance = "logit",
                              caliper=0.5)

#Examine our balance
summary(nonspatial_matchit)

#Subset our data to only include matches
matchedData <- match.data(nonspatial_matchit)


spatial.opts <- list(decay.model = "threshold",
                     threshold = 1000)

spatial_matchit <- matchit(treatment.status ~ ad_id, data= example_dta,
                           method = "nearest", distance = "logit",
                           caliper=0.5, spatial.options=spatial.opts)
