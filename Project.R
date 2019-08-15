getwd()
setwd("Users/muhammadhussein/Desktop/R")

#-----------------------------Installation of packages---------------------------#
# install.packages("class")
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("dplyr")
# install.packages("shiny")


#-----------------------------Library---------------------------#
library(class)
library(ggplot2)
library(caret)
library(dplyr)
library(shiny)

#reading data set
iowaTrain <- read.csv("train.csv", header = TRUE, sep = ',')

#removing variables from set
iowaTrain$Id <- NULL
iowaTrain$Street <- NULL
iowaTrain$Alley <- NULL
iowaTrain$LotFrontage <- NULL
iowaTrain$LotConfig <- NULL
iowaTrain$YearRemodAdd <- NULL
iowaTrain$MasVnrArea <- NULL
iowaTrain$BsmtExposure <- NULL
iowaTrain$BsmtFinSF1 <- NULL
iowaTrain$BsmtFinSF2 <- NULL
iowaTrain$TotalBsmtSF <- NULL
iowaTrain$PoolArea <- NULL
iowaTrain$GarageYrBlt <- NULL
iowaTrain$PoolArea <- NULL
iowaTrain$PoolQC <- NULL
iowaTrain$MiscFeature <- NULL
iowaTrain$X3SsnPorch <- NULL
iowaTrain$MiscVal <- NULL
iowaTrain$WoodDeckSF <- NULL
iowaTrain$OpenPorchSF <- NULL
iowaTrain$EnclosedPorch <- NULL
iowaTrain$ScreenPorch <- NULL
iowaTrain$FireplaceQu <- NULL
iowaTrain$GarageCars <- NULL
iowaTrain$GarageFinish <- NULL
iowaTrain$GarageCond <- NULL
iowaTrain$SaleType <- NULL
iowaTrain$SaleCondition <- NULL
iowaTrain$Fence <- NULL

remove_1 <- iowaTrain[,]

no_results <- remove_1[,]

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }

#check if there are any N/A's in the set
any(is.na(iowaTrain))
#check the amount of N/A's in the set
sum(is.na(iowaTrain))
na.cols = which(colSums(is.na(iowaTrain)) > 0)
sort(colSums(sapply(iowaTrain[na.cols], is.na)), decreasing = TRUE)

na.omit(iowaTrain)

# i <- sapply(iowaTrain, is.factor)
# iowaTrain[i] <- lapply(iowaTrain, as.integer)
# # convert factors to integers in test set
# # i <- sapply(df_test, is.factor)
# # df_test[i] <- lapply(df_test[i], as.integer)
# 
# 
#-------------------Electrical variable -----------
# table(iowaTrain$Electrical)
# index <- which(iowaTrain$Electrical == "SBrkr")
# iowaTrain[index, 'Electrical'] <- "SB"
# index <- which(iowaTrain$Electrical == "Mix")
# iowaTrain[index, 'Electrical'] <- "MX"
# index <- which(iowaTrain$Electrical == "FuseA")
# iowaTrain[index, 'Electrical'] <- "FA"
# index <- which(iowaTrain$Electrical == "FuseF")
# iowaTrain[index, 'Electrical'] <- "FF"
# index <- which(iowaTrain$Electrical == "FuseP")
# iowaTrain[index, 'Electrical'] <- "FP"
# table(iowaTrain$Electrical)



# look at median sale price by lot configuration
iowaTrain %>% 
  group_by(LotConfig) %>% 
  dplyr::summarise(n = n(), 
                   median_saleprice = median(SalePrice))
