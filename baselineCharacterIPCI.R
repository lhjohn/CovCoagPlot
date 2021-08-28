library(magrittr)
library(dplyr)

covSummary1 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_1/covSummary.rds")
covSummary2 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_2/covSummary.rds")
covSummary3 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_3/covSummary.rds")
covSummary4 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_4/covSummary.rds")
covSummary5 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_5/covSummary.rds")
covSummary6 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_6/covSummary.rds")

covSummary9 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_9/covSummary.rds")
covSummary10 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_10/covSummary.rds")
covSummary11 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_11/covSummary.rds")
covSummary12 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_12/covSummary.rds")
covSummary13 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_13/covSummary.rds")
covSummary14 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_14/covSummary.rds")

covSummary17 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_17/covSummary.rds")
covSummary18 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_18/covSummary.rds")
covSummary19 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_19/covSummary.rds")
covSummary20 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_20/covSummary.rds")
covSummary21 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_21/covSummary.rds")
covSummary22 <- readRDS("~/Data/Coagulopathy/IPCI/Characterisation/CovCoagCharacterResults/Analysis_22/covSummary.rds")


age1 <- covSummary1 %>%
  dplyr::filter(analysisId == 3, covariateId >= 13003)
sum(age1$CovariateCount)
age1 <- covSummary1 %>%
  dplyr::filter(analysisId == 3, covariateId < 13003)
sum(age1$CovariateCount)

age2 <- covSummary2 %>%
  dplyr::filter(analysisId == 3, covariateId >= 13003)
sum(age2$CovariateCount)
age2 <- covSummary2 %>%
  dplyr::filter(analysisId == 3, covariateId < 13003)
sum(age2$CovariateCount)


age3 <- covSummary3 %>%
  dplyr::filter(analysisId == 3, covariateId >= 13003)
sum(age3$CovariateCount)
age3 <- covSummary3 %>%
  dplyr::filter(analysisId == 3, covariateId < 13003)
sum(age3$CovariateCount)


age4 <- covSummary4 %>%
  dplyr::filter(analysisId == 3, covariateId >= 13003)
sum(age4$CovariateCount)
age4 <- covSummary4 %>%
  dplyr::filter(analysisId == 3, covariateId < 13003)
sum(age4$CovariateCount)


age5 <- covSummary5 %>%
  dplyr::filter(analysisId == 3, covariateId >= 13003)
sum(age5$CovariateCount)
age5 <- covSummary5 %>%
  dplyr::filter(analysisId == 3, covariateId < 13003)
sum(age5$CovariateCount)

age6 <- covSummary6 %>%
  dplyr::filter(analysisId == 3, covariateId >= 13003)
sum(age6$CovariateCount)
age6 <- covSummary6 %>%
  dplyr::filter(analysisId == 3, covariateId < 13003)
sum(age6$CovariateCount)
