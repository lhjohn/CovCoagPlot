library(magrittr)
options(scipen=999)
library(heatmaply)
library(gridExtra)

analysis_range <- seq(1, 8, 1)

get_eval <- function(path){
  plpResult <- PatientLevelPrediction::loadPlpResult(path)
  prediction <- plpResult$prediction
  evaluationStatistics <- plpResult$performanceEvaluation$evaluationStatistics
  
  auc_test <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "test", Metric == "AUC.auc") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  auc_test_lb <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "test", Metric == "AUC.auc_lb95ci") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  auc_test_ub <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "test", Metric == "AUC.auc_ub95ci") %>%
    dplyr::select(Value) %>%
    as.numeric()
    
  eavg_test <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "test", Metric == "Emean.Eavg") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  pred_0_64 <- prediction %>%
    dplyr::filter(ageYear >=0, ageYear <= 64, indexes == -1)
  pred_65_150 <- prediction %>%
    dplyr::filter(ageYear >=65, ageYear <= 150, indexes == -1)
  
  pred_male <- prediction %>%
    dplyr::filter(gender == 8507, indexes == -1)
  pred_female <- prediction %>%
    dplyr::filter(gender == 8532, indexes == -1)
  
  pred_0_64_eval <- PatientLevelPrediction::evaluatePlp(pred_0_64)
  auc_0_64 <- pred_0_64_eval$evaluationStatistics$AUC$auc
  auc_0_64_lb <- pred_0_64_eval$evaluationStatistics$AUC$auc_lb95ci
  auc_0_64_ub <- pred_0_64_eval$evaluationStatistics$AUC$auc_ub95ci
  eavg_0_64 <- pred_0_64_eval$evaluationStatistics$Emean
  
  pred_65_150_eval <- PatientLevelPrediction::evaluatePlp(pred_65_150)
  auc_65_150 <- pred_65_150_eval$evaluationStatistics$AUC$auc
  auc_65_150_lb <- pred_65_150_eval$evaluationStatistics$AUC$auc_lb95ci
  auc_65_150_ub <- pred_65_150_eval$evaluationStatistics$AUC$auc_ub95ci
  eavg_65_150 <- pred_65_150_eval$evaluationStatistics$Emean
  
  pred_male_eval <- PatientLevelPrediction::evaluatePlp(pred_male)
  auc_male <- pred_male_eval$evaluationStatistics$AUC$auc
  auc_male_lb <- pred_male_eval$evaluationStatistics$AUC$auc_lb95ci
  auc_male_ub <- pred_male_eval$evaluationStatistics$AUC$auc_ub95ci
  eavg_male <- pred_male_eval$evaluationStatistics$Emean
  
  pred_female_eval <- PatientLevelPrediction::evaluatePlp(pred_female)
  auc_female <- pred_female_eval$evaluationStatistics$AUC$auc
  auc_female_lb <- pred_female_eval$evaluationStatistics$AUC$auc_lb95ci
  auc_female_ub <- pred_female_eval$evaluationStatistics$AUC$auc_ub95ci
  eavg_female <- pred_female_eval$evaluationStatistics$Emean
  
  result <- data.frame(auc=auc_test,
                       auc_lb=auc_test_lb,
                       auc_ub=auc_test_ub,
                       auc_0_64=auc_0_64,
                       auc_0_64_lb=auc_0_64_lb,
                       auc_0_64_ub=auc_0_64_ub,
                       auc_65_150=auc_65_150,
                       auc_65_150_lb=auc_65_150_lb,
                       auc_65_150_ub=auc_65_150_ub,
                       auc_male=auc_male,
                       auc_male_lb=auc_male_lb,
                       auc_male_ub=auc_male_ub,
                       auc_female=auc_female,
                       auc_female_lb=auc_female_lb,
                       auc_female_ub=auc_female_ub,
                       eavg=eavg_test,
                       eavg_0_64=eavg_0_64,
                       eavg_65_150=eavg_65_150,
                       eavg_male=eavg_male,
                       eavg_female=eavg_female)
  
  return(result)
}

base_eval <- data.frame(
  auc=numeric(0),
  auc_lb=numeric(0),
  auc_ub=numeric(0),
  auc_0_64=numeric(0),
  auc_0_64_lb=numeric(0),
  auc_0_64_ub=numeric(0),
  auc_65_150=numeric(0),
  auc_65_150_lb=numeric(0),
  auc_65_150_ub=numeric(0),
  auc_male=numeric(0),
  auc_male_lb=numeric(0),
  auc_male_ub=numeric(0),
  auc_female=numeric(0),
  auc_female_lb=numeric(0),
  auc_female_ub=numeric(0),
  eavg=numeric(0),
  eavg_0_64=numeric(0),
  eavg_65_150=numeric(0),
  eavg_male=numeric(0),
  eavg_female=numeric(0))

ema_eval <- data.frame(auc=numeric(0),
                       auc_lb=numeric(0),
                       auc_ub=numeric(0),
                       auc_0_64=numeric(0),
                       auc_0_64_lb=numeric(0),
                       auc_0_64_ub=numeric(0),
                       auc_65_150=numeric(0),
                       auc_65_150_lb=numeric(0),
                       auc_65_150_ub=numeric(0),
                       auc_male=numeric(0),
                       auc_male_lb=numeric(0),
                       auc_male_ub=numeric(0),
                       auc_female=numeric(0),
                       auc_female_lb=numeric(0),
                       auc_female_ub=numeric(0),
                       eavg=numeric(0),
                       eavg_0_64=numeric(0),
                       eavg_65_150=numeric(0),
                       eavg_male=numeric(0),
                       eavg_female=numeric(0))

full_eval <- data.frame(auc=numeric(0),
                       auc_lb=numeric(0),
                       auc_ub=numeric(0),
                       auc_0_64=numeric(0),
                       auc_0_64_lb=numeric(0),
                       auc_0_64_ub=numeric(0),
                       auc_65_150=numeric(0),
                       auc_65_150_lb=numeric(0),
                       auc_65_150_ub=numeric(0),
                       auc_male=numeric(0),
                       auc_male_lb=numeric(0),
                       auc_male_ub=numeric(0),
                       auc_female=numeric(0),
                       auc_female_lb=numeric(0),
                       auc_female_ub=numeric(0),
                       eavg=numeric(0),
                       eavg_0_64=numeric(0),
                       eavg_65_150=numeric(0),
                       eavg_male=numeric(0),
                       eavg_female=numeric(0))
# base_eavg <- data.frame(eavg=numeric(0),
#                         eavg_0_64=numeric(0),
#                         eavg_65_150=numeric(0))

for (i in analysis_range) {
  # in case only every nth analysis needs to be loaded, not relevant anymore
  # therefore set to 1 == 0
  if(i %% 1 == 0) {
    path <- paste0("~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/Analysis_", i, "/plpResult")
    # auc_row <- data.frame(auc=0,
    #                       auc_lb=0,
    #                       auc_ub=0,
    #                   auc_0_64=0,
    #                   auc_0_64_lb=0,
    #                   auc_0_64_ub=0,
    #                   auc_65_150=0,
    #                   auc_65_150_lb=0,
    #                   auc_65_150_ub=0,
    #                   eavg=0,
    #                   eavg_0_64=0,
    #                   eavg_65_150=0)
    # eavg_row <- data.frame(eavg=0,
    #                        eavg_0_64=0,
    #                        eavg_65_150=0)
    tryCatch(
      {
        eval_row <- get_eval(path)
        # eavg_row <- get_eavg(path)
      }, error=function(cond) {
      }
    )
    base_eval <- rbind(base_eval, eval_row)
    # base_eavg <- rbind(base_eavg, eavg_row)
  }
}

for (i in analysis_range) {
  # in case only every nth analysis needs to be loaded, not relevant anymore
  # therefore set to 1 == 0
  if(i %% 1 == 0) {
    path <- paste0("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_", i, "/plpResult")
    # auc_row <- data.frame(auc=0,
    #                       auc_lb=0,
    #                       auc_ub=0,
    #                       auc_0_64=0,
    #                       auc_0_64_lb=0,
    #                       auc_0_64_ub=0,
    #                       auc_65_150=0,
    #                       auc_65_150_lb=0,
    #                       auc_65_150_ub=0,
    #                       eavg=0,
    #                       eavg_0_64=0,
    #                       eavg_65_150=0)
    # eavg_row <- data.frame(eavg=0,
    #                        eavg_0_64=0,
    #                        eavg_65_150=0)
    tryCatch(
      {
        eval_row <- get_eval(path)
        # eavg_row <- get_eavg(path)
      }, error=function(cond) {
      }
    )
    ema_eval <- rbind(ema_eval, eval_row)
    # base_eavg <- rbind(base_eavg, eavg_row)
  }
}

for (i in analysis_range) {
  # in case only every nth analysis needs to be loaded, not relevant anymore
  # therefore set to 1 == 0
  if(i %% 1 == 0) {
    path <- paste0("~/Data/Coagulopathy/CPRD Aurum/FullOutput_25p/Analysis_", i, "/plpResult")
    # auc_row <- data.frame(auc=0,
    #                       auc_lb=0,
    #                       auc_ub=0,
    #                       auc_0_64=0,
    #                       auc_0_64_lb=0,
    #                       auc_0_64_ub=0,
    #                       auc_65_150=0,
    #                       auc_65_150_lb=0,
    #                       auc_65_150_ub=0,
    #                       eavg=0,
    #                       eavg_0_64=0,
    #                       eavg_65_150=0)
    # eavg_row <- data.frame(eavg=0,
    #                        eavg_0_64=0,
    #                        eavg_65_150=0)
    tryCatch(
      {
        eval_row <- get_eval(path)
        # eavg_row <- get_eavg(path)
      }, error=function(cond) {
      }
    )
    full_eval <- rbind(full_eval, eval_row)
    # base_eavg <- rbind(base_eavg, eavg_row)
  }
}

base_eval_mat <- base_eval
ema_eval_mat <- ema_eval
full_eval_mat <- full_eval
# base_eavg_mat <- base_eavg

# auc
base_eval_mat$auc[base_eval_mat$auc == 0] <- NA
base_eval_mat$auc_0_64[base_eval_mat$auc_0_64 == 0] <- NA
base_eval_mat$auc_65_150[base_eval_mat$auc_65_150 == 0] <- NA
base_eval_mat$auc_male[base_eval_mat$auc_male == 0] <- NA
base_eval_mat$auc_female[base_eval_mat$auc_female == 0] <- NA


base_eval_mat$auc[base_eval_mat$auc < 0.5] <- 0.5
base_eval_mat$auc_0_64[base_eval_mat$auc_0_64 < 0.5] <- 0.5
base_eval_mat$auc_65_150[base_eval_mat$auc_65_150 < 0.5] <- 0.5
base_eval_mat$auc_male[base_eval_mat$auc_male < 0.5] <- 0.5
base_eval_mat$auc_female[base_eval_mat$auc_female < 0.5] <- 0.5

ema_eval_mat$auc[ema_eval_mat$auc == 0] <- NA
ema_eval_mat$auc_0_64[ema_eval_mat$auc_0_64 == 0] <- NA
ema_eval_mat$auc_65_150[ema_eval_mat$auc_65_150 == 0] <- NA
ema_eval_mat$auc_male[ema_eval_mat$auc_male == 0] <- NA
ema_eval_mat$auc_female[ema_eval_mat$auc_female == 0] <- NA

ema_eval_mat$auc[ema_eval_mat$auc < 0.5] <- 0.5
ema_eval_mat$auc_0_64[ema_eval_mat$auc_0_64 < 0.5] <- 0.5
ema_eval_mat$auc_65_150[ema_eval_mat$auc_65_150 < 0.5] <- 0.5
ema_eval_mat$auc_male[ema_eval_mat$auc_male < 0.5] <- 0.5
ema_eval_mat$auc_female[ema_eval_mat$auc_female < 0.5] <- 0.5

full_eval_mat$auc[full_eval_mat$auc == 0] <- NA
full_eval_mat$auc_0_64[full_eval_mat$auc_0_64 == 0] <- NA
full_eval_mat$auc_65_150[full_eval_mat$auc_65_150 == 0] <- NA
full_eval_mat$auc_male[full_eval_mat$auc_male == 0] <- NA
full_eval_mat$auc_female[full_eval_mat$auc_female == 0] <- NA

full_eval_mat$auc[full_eval_mat$auc < 0.5] <- 0.5
full_eval_mat$auc_0_64[full_eval_mat$auc_0_64 < 0.5] <- 0.5
full_eval_mat$auc_65_150[full_eval_mat$auc_65_150 < 0.5] <- 0.5
full_eval_mat$auc_male[full_eval_mat$auc_male < 0.5] <- 0.5
full_eval_mat$auc_female[full_eval_mat$auc_female < 0.5] <- 0.5


# calibration
base_eval_mat$eavg[base_eval_mat$eavg == 0] <- NA
base_eval_mat$eavg_0_64[base_eval_mat$eavg_0_64 == 0] <- NA
base_eval_mat$eavg_65_150[base_eval_mat$eavg_65_150 == 0] <- NA
base_eval_mat$eavg_male[base_eval_mat$eavg_male == 0] <- NA
base_eval_mat$eavg_female[base_eval_mat$eavg_female == 0] <- NA

ema_eval_mat$eavg[ema_eval_mat$eavg == 0] <- NA
ema_eval_mat$eavg_0_64[ema_eval_mat$eavg_0_64 == 0] <- NA
ema_eval_mat$eavg_65_150[ema_eval_mat$eavg_65_150 == 0] <- NA
ema_eval_mat$eavg_male[ema_eval_mat$eavg_male == 0] <- NA
ema_eval_mat$eavg_female[ema_eval_mat$eavg_female == 0] <- NA

full_eval_mat$eavg[full_eval_mat$eavg == 0] <- NA
full_eval_mat$eavg_0_64[full_eval_mat$eavg_0_64 == 0] <- NA
full_eval_mat$eavg_65_150[full_eval_mat$eavg_65_150 == 0] <- NA
full_eval_mat$eavg_male[full_eval_mat$eavg_male == 0] <- NA
full_eval_mat$eavg_female[full_eval_mat$eavg_female == 0] <- NA


ref_base <- read.csv("~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/settings.csv")
ref_base <- ref_base[order(ref_base$analysisId), ]

# outcome <- ref_base$outcomeName[seq(1, nrow(ref_base), 3)]
outcome <- ref_base$outcomeName

if (length(base_eval <= 8)) {
  outcome <- outcome[1:8]
  outcome_base <- paste0(outcome, " ", rep("30 days", 8))
} else {
outcome_base <- paste0(outcome, " ", c(rep("30 days", 8),
                                       rep("60 days", 8),
                                       rep("90 days", 8)))
}

outcome_base <- gsub("narrow ", "", as.character(outcome_base))
outcome_base <- gsub("MI or IS", "ATE", as.character(outcome_base))

# remove stroke and death outcome for EMA report
# also remove ischemic stroke
# base_eval_mat <- base_eval_mat[-c(2, 7, 8, 10, 15, 16, 18, 23, 24),]
# base_eval_mat <- base_eval_mat[-c(2, 7, 8, seq(9, 24, 1))]
base_eval_mat <- base_eval_mat %>%
  dplyr::slice(c(1, 3, 4, 5, 6))
ema_eval_mat <- ema_eval_mat %>%
  dplyr::slice(c(1, 3, 4, 5, 6))
full_eval_mat <- full_eval_mat %>%
  dplyr::slice(c(1, 3, 4, 5, 6))
# base_eavg_mat <- base_eavg_mat[-c(2, 7, 8, 10, 15, 16, 18, 23, 24),]
# removing DTH and STR using the following subset
# outcome_base <- outcome_base[-c(2, 7, 8, 10, 15, 16, 18, 23, 24)]
outcome_base <- outcome_base[-c(2, 7, 8, seq(9, 24, 1))]

full_data_base <- data.frame(base_eval_mat,
                        outcome_base = outcome_base)
full_data_ema <- data.frame(ema_eval_mat,
                        outcome_base = outcome_base)
full_data_full <- data.frame(full_eval_mat,
                            outcome_base = outcome_base)
# labels for all outcomes except STR and DTH
# full_data <- full_data %>%
#   mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days", "MI 60 days", "MI 90 days",
#                                                          "MI or IS 30 days", "MI or IS 60 days", "MI or IS 90 days",
#                                                          "PE 30 days", "PE 60 days", "PE 90 days",
#                                                          "DVT narrow 30 days", "DVT narrow 60 days", "DVT narrow 90 days",
#                                                          "VTE narrow 30 days", "VTE narrow 60 days", "VTE narrow 90 days"))) %>%
#   arrange(outcome_base)

# labels for all outcomes except STR and DTH
# full_data <- full_data %>%
#   dplyr::mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days", "MI 60 days", "MI 90 days",
#                                                          "MI or IS 30 days", "MI or IS 60 days", "MI or IS 90 days",
#                                                          "PE 30 days", "PE 60 days", "PE 90 days",
#                                                          "DVT 30 days", "DVT 60 days", "DVT 90 days",
#                                                          "VTE 30 days", "VTE 60 days", "VTE 90 days"))) %>%
#   dplyr::arrange(outcome_base)
full_data_base <- full_data_base %>%
  dplyr::mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days",
                                                                "ATE 30 days",
                                                                "PE 30 days",
                                                                "DVT 30 days",
                                                                "VTE 30 days"))) %>%
  dplyr::arrange(outcome_base)

full_data_ema<- full_data_ema %>%
  dplyr::mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days",
                                                                "ATE 30 days",
                                                                "PE 30 days",
                                                                "DVT 30 days",
                                                                "VTE 30 days"))) %>%
  dplyr::arrange(outcome_base)

full_data_full<- full_data_full %>%
  dplyr::mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days",
                                                                "ATE 30 days",
                                                                "PE 30 days",
                                                                "DVT 30 days",
                                                                "VTE 30 days"))) %>%
  dplyr::arrange(outcome_base)
# create cellnotes
cellnotes_base_auc <- paste0(format(round(full_data_base$auc, 2), nsmall = 2),"\n (",
                             format(round(full_data_base$auc_lb, 2), nsmall = 2), ", ",
                             format(round(full_data_base$auc_ub, 2), nsmall = 2), ")")
cellnotes_base_auc_0_64 <- paste0(format(round(full_data_base$auc_0_64, 2), nsmall = 2),"\n (",
                                  format(round(full_data_base$auc_0_64_lb, 2), nsmall = 2), ", ",
                                  format(round(full_data_base$auc_0_64_ub, 2), nsmall = 2), ")")
cellnotes_base_auc_65_150 <- paste0(format(round(full_data_base$auc_65_150, 2), nsmall = 2),"\n (",
                                    format(round(full_data_base$auc_65_150_lb, 2), nsmall = 2), ", ",
                                    format(round(full_data_base$auc_65_150_ub, 2), nsmall = 2), ")")
cellnotes_base_auc_male <- paste0(format(round(full_data_base$auc_male, 2), nsmall = 2),"\n (",
                                    format(round(full_data_base$auc_male_lb, 2), nsmall = 2), ", ",
                                    format(round(full_data_base$auc_male_ub, 2), nsmall = 2), ")")
cellnotes_base_auc_female <- paste0(format(round(full_data_base$auc_female, 2), nsmall = 2),"\n (",
                                  format(round(full_data_base$auc_female_lb, 2), nsmall = 2), ", ",
                                  format(round(full_data_base$auc_female_ub, 2), nsmall = 2), ")")
cellnotes_base <- data.frame(cellnotes_base_auc = cellnotes_base_auc,
                             cellnotes_base_auc_0_64 = cellnotes_base_auc_0_64,
                             cellnotes_base_auc_65_150 = cellnotes_base_auc_65_150,
                             cellnotes_base_auc_male = cellnotes_base_auc_male,
                             cellnotes_base_auc_female = cellnotes_base_auc_female)

cellnotes_ema_auc <- paste0(format(round(full_data_ema$auc, 2), nsmall = 2),"\n (",
                             format(round(full_data_ema$auc_lb, 2), nsmall = 2), ", ",
                             format(round(full_data_ema$auc_ub, 2), nsmall = 2), ")")
cellnotes_ema_auc_0_64 <- paste0(format(round(full_data_ema$auc_0_64, 2), nsmall = 2),"\n (",
                                  format(round(full_data_ema$auc_0_64_lb, 2), nsmall = 2), ", ",
                                  format(round(full_data_ema$auc_0_64_ub, 2), nsmall = 2), ")")
cellnotes_ema_auc_65_150 <- paste0(format(round(full_data_ema$auc_65_150, 2), nsmall = 2),"\n (",
                                    format(round(full_data_ema$auc_65_150_lb, 2), nsmall = 2), ", ",
                                    format(round(full_data_ema$auc_65_150_ub, 2), nsmall = 2), ")")
cellnotes_ema_auc_male <- paste0(format(round(full_data_ema$auc_male, 2), nsmall = 2),"\n (",
                                  format(round(full_data_ema$auc_male_lb, 2), nsmall = 2), ", ",
                                  format(round(full_data_ema$auc_male_ub, 2), nsmall = 2), ")")
cellnotes_ema_auc_female <- paste0(format(round(full_data_ema$auc_female, 2), nsmall = 2),"\n (",
                                    format(round(full_data_ema$auc_female_lb, 2), nsmall = 2), ", ",
                                    format(round(full_data_ema$auc_female_ub, 2), nsmall = 2), ")")
cellnotes_ema <- data.frame(cellnotes_ema_auc = cellnotes_ema_auc,
                             cellnotes_ema_auc_0_64 = cellnotes_ema_auc_0_64,
                             cellnotes_ema_auc_65_150 = cellnotes_ema_auc_65_150,
                             cellnotes_ema_auc_male = cellnotes_ema_auc_male,
                             cellnotes_ema_auc_female = cellnotes_ema_auc_female)

cellnotes_full_auc <- paste0(format(round(full_data_full$auc, 2), nsmall = 2),"\n (",
                            format(round(full_data_full$auc_lb, 2), nsmall = 2), ", ",
                            format(round(full_data_full$auc_ub, 2), nsmall = 2), ")")
cellnotes_full_auc_0_64 <- paste0(format(round(full_data_full$auc_0_64, 2), nsmall = 2),"\n (",
                                 format(round(full_data_full$auc_0_64_lb, 2), nsmall = 2), ", ",
                                 format(round(full_data_full$auc_0_64_ub, 2), nsmall = 2), ")")
cellnotes_full_auc_65_150 <- paste0(format(round(full_data_full$auc_65_150, 2), nsmall = 2),"\n (",
                                   format(round(full_data_full$auc_65_150_lb, 2), nsmall = 2), ", ",
                                   format(round(full_data_full$auc_65_150_ub, 2), nsmall = 2), ")")
cellnotes_full_auc_male <- paste0(format(round(full_data_full$auc_male, 2), nsmall = 2),"\n (",
                                 format(round(full_data_full$auc_male_lb, 2), nsmall = 2), ", ",
                                 format(round(full_data_full$auc_male_ub, 2), nsmall = 2), ")")
cellnotes_full_auc_female <- paste0(format(round(full_data_full$auc_female, 2), nsmall = 2),"\n (",
                                   format(round(full_data_full$auc_female_lb, 2), nsmall = 2), ", ",
                                   format(round(full_data_full$auc_female_ub, 2), nsmall = 2), ")")
cellnotes_full <- data.frame(cellnotes_full_auc = cellnotes_full_auc,
                            cellnotes_full_auc_0_64 = cellnotes_full_auc_0_64,
                            cellnotes_full_auc_65_150 = cellnotes_full_auc_65_150,
                            cellnotes_full_auc_male = cellnotes_full_auc_male,
                            cellnotes_full_auc_female = cellnotes_full_auc_female)

axis_label_font_size <- 24
tick_font_size <- 18
cell_note_font_size <- 18

auc_base_plot <- full_data_base %>%
  dplyr::select(c(auc, auc_0_64, auc_65_150, auc_male, auc_female)) %>%
  heatmaply::heatmaply(dendrogram = "none",
                       ylab = "Outcome",
                       xlab = "Target", 
                       main = " ",
                       plot_method = "plotly",
                       colorbar_len = 1,
                       cellnote = cellnotes_base,
                       cellnote_size = cell_note_font_size,
                       draw_cellnote = TRUE,
                       digits = 2L,
                       cellnote_textposition = "middle center",
                       scale = "none",
                       na.value = "grey50",
                       limits = c(0.5, 1.0),
                       # margins = c(60,100,NA,NA),
                       # grid_color = "white",
                       # grid_width = 0.00002,
                       titleX = TRUE,
                       titleY = TRUE,
                       hide_colorbar = FALSE,
                       branches_lwd = NULL,
                       label_names = c("Outcome", "Target", "AUROC"),
                       # fontsize_row = 8,
                       # fontsize_col = 8,
                       labCol = c("All", "Age <65", "Age >=65", "Male", "Female"),
                       column_text_angle = 0,
                       grid_gap = 3,
                       labRow = full_data_base$outcome_base,
                       key.title = "AUROC",
                       heatmap_layers = theme(axis.line=element_blank())) %>%
  layout(
    font = list(
      size = axis_label_font_size),
    xaxis = list(tickfont = list(size = tick_font_size)), 
    yaxis = list(tickfont = list(size = tick_font_size))
  )

auc_ema_plot <- full_data_ema %>%
  dplyr::select(c(auc, auc_0_64, auc_65_150, auc_male, auc_female)) %>%
  heatmaply::heatmaply(dendrogram = "none",
                       ylab = "Outcome",
                       xlab = "Target", 
                       main = " ",
                       plot_method = "plotly",
                       colorbar_len=1,
                       cellnote = cellnotes_ema,
                       cellnote_size = cell_note_font_size,
                       draw_cellnote = TRUE,
                       digits = 2L,
                       cellnote_textposition = "middle center",
                       scale = "none",
                       na.value = "grey50",
                       limits = c(0.5, 1.0),
                       # margins = c(60,100,NA,NA),
                       # grid_color = "white",
                       # grid_width = 0.002,
                       titleX = TRUE,
                       titleY = FALSE,
                       hide_colorbar = FALSE,
                       branches_lwd = NULL,
                       label_names = c("Outcome", "Target", "AUROC"),
                       # fontsize_row = 8,
                       # fontsize_col = 8,
                       labCol = c("All", "Age <65", "Age >=65", "Male", "Female"),
                       column_text_angle = 0,
                       grid_gap = 3,
                       labRow = full_data_ema$outcome_base,
                       key.title = "AUROC",
                       showticklabels = c(TRUE, FALSE),
                       heatmap_layers = theme(axis.line=element_blank())) %>%
  layout(
    font = list(
      size = axis_label_font_size),
    xaxis = list(tickfont = list(size = tick_font_size)), 
    yaxis = list(tickfont = list(size = tick_font_size)),
    grid = list(
      xgap = 0.2,
      ygap = 0.2
    )
  )

auc_full_plot <- full_data_full %>%
  dplyr::select(c(auc, auc_0_64, auc_65_150, auc_male, auc_female)) %>%
  heatmaply::heatmaply(dendrogram = "none",
                       ylab = "Outcome",
                       xlab = "Target", 
                       main = " ",
                       plot_method = "plotly",
                       colorbar_len=1,
                       cellnote = cellnotes_full,
                       cellnote_size = cell_note_font_size,
                       draw_cellnote = TRUE,
                       digits = 2L,
                       cellnote_textposition = "middle center",
                       scale = "none",
                       na.value = "grey50",
                       limits = c(0.5, 1.0),
                       # margins = c(60,100,NA,NA),
                       # grid_color = "white",
                       # grid_width = 0.002,
                       titleX = TRUE,
                       titleY = FALSE,
                       hide_colorbar = FALSE,
                       branches_lwd = NULL,
                       label_names = c("Outcome", "Target", "AUROC"),
                       # fontsize_row = 8,
                       # fontsize_col = 8,
                       labCol = c("All", "Age <65", "Age >=65", "Male", "Female"),
                       column_text_angle = 0,
                       grid_gap = 3,
                       labRow = full_data_full$outcome_base,
                       key.title = "AUROC",
                       showticklabels = c(TRUE, FALSE),
                       heatmap_layers = theme(axis.line=element_blank())) %>%
  layout(
    font = list(
      size = axis_label_font_size),
    xaxis = list(tickfont = list(size = tick_font_size)), 
    yaxis = list(tickfont = list(size = tick_font_size)),
    grid = list(
      xgap = 0.2,
      ygap = 0.2
    )
  )

eavg_base_plot <- full_data_base %>%
  dplyr::select(c(eavg, eavg_0_64, eavg_65_150, eavg_male, eavg_female)) %>%
  heatmaply::heatmaply(dendrogram = "none",
                       ylab = "Outcome",
                       xlab = "Target", 
                       main = " ",
                       plot_method = "plotly",
                       colorbar_len=1,
                       colors = viridis(n = 256,  option = "magma", direction = -1),
                       # cellnote = TRUE,
                       draw_cellnote = TRUE,
                       cellnote_size = cell_note_font_size,
                       digits = 5L,
                       cellnote_textposition = "middle center",
                       scale = "none",
                       na.value = "grey50",
                       limits = c(0, 0.005),
                       # margins = c(60,100,NA,NA),
                       # grid_color = "white",
                       # grid_width = 0.00002,
                       titleX = TRUE,
                       titleY = TRUE,
                       hide_colorbar = FALSE,
                       branches_lwd = NULL,
                       label_names = c("Outcome", "Target", "AUROC"),
                       # fontsize_row = 8,
                       # fontsize_col = 8,
                       labCol = c("All", "Age <65", "Age >=65", "Male", "Female"),
                       column_text_angle = 0,
                       grid_gap = 3,
                       labRow = full_data_base$outcome_base,
                       key.title = "E<sub>avg</sub>",
                       heatmap_layers = theme(axis.line=element_blank())) %>%
  layout(
    font = list(
      size = axis_label_font_size),
    xaxis = list(
      tickfont = list(size = tick_font_size)), 
    yaxis = list(
      tickfont = list(size = tick_font_size))
  )

eavg_ema_plot <- full_data_ema %>%
  dplyr::select(c(eavg, eavg_0_64, eavg_65_150, eavg_male, eavg_female)) %>%
  heatmaply::heatmaply(dendrogram = "none",
                       ylab = "Outcome",
                       xlab = "Target", 
                       main = " ",
                       plot_method = "plotly",
                       colorbar_len=1,
                       colors = viridis(n = 256,  option = "magma", direction = -1),
                       # cellnote = TRUE,
                       cellnote_size = cell_note_font_size,
                       draw_cellnote = TRUE,
                       digits = 5L,
                       cellnote_textposition = "middle center",
                       scale = "none",
                       na.value = "grey50",
                       limits = c(0, 0.005),
                       # margins = c(60,100,NA,NA),
                       # grid_color = "white",
                       # grid_width = 0.00002,
                       titleX = TRUE,
                       titleY = FALSE,
                       hide_colorbar = FALSE,
                       branches_lwd = NULL,
                       label_names = c("Outcome", "Target", "AUROC"),
                       # fontsize_row = 8,
                       # fontsize_col = 8,
                       labCol = c("All", "Age <65", "Age >=65", "Male", "Female"),
                       column_text_angle = 0,
                       grid_gap = 3,
                       labRow = full_data_ema$outcome_base,
                       key.title = "E<sub>avg</sub>",
                       showticklabels = c(TRUE, FALSE),
                       heatmap_layers = theme(axis.line=element_blank())) %>%
  layout(
    font = list(
      size = axis_label_font_size),
    xaxis = list(
      tickfont = list(size = tick_font_size)), 
    yaxis = list(
      tickfont = list(size = tick_font_size))
  )

eavg_full_plot <- full_data_full %>%
  dplyr::select(c(eavg, eavg_0_64, eavg_65_150, eavg_male, eavg_female)) %>%
  heatmaply::heatmaply(dendrogram = "none",
                       ylab = "Outcome",
                       xlab = "Target", 
                       main = " ",
                       plot_method = "plotly",
                       colorbar_len=1,
                       colors = viridis(n = 256,  option = "magma", direction = -1),
                       # cellnote = TRUE,
                       cellnote_size = cell_note_font_size,
                       draw_cellnote = TRUE,
                       digits = 5L,
                       cellnote_textposition = "middle center",
                       scale = "none",
                       na.value = "grey50",
                       limits = c(0, 0.005),
                       # margins = c(60,100,NA,NA),
                       # grid_color = "white",
                       # grid_width = 0.00002,
                       titleX = TRUE,
                       titleY = FALSE,
                       hide_colorbar = FALSE,
                       branches_lwd = NULL,
                       label_names = c("Outcome", "Target", "AUROC"),
                       # fontsize_row = 8,
                       # fontsize_col = 8,
                       labCol = c("All", "Age <65", "Age >=65", "Male", "Female"),
                       column_text_angle = 0,
                       grid_gap = 3,
                       labRow = full_data_full$outcome_base,
                       key.title = "E<sub>avg</sub>",
                       showticklabels = c(TRUE, FALSE),
                       heatmap_layers = theme(axis.line=element_blank())) %>%
  layout(
    font = list(
      size = axis_label_font_size),
    xaxis = list(
      tickfont = list(size = tick_font_size)), 
    yaxis = list(
      tickfont = list(size = tick_font_size))
  )

# auc_base_plot$width <- 900
# auc_base_plot$height <- 500
# 
# auc_pars_plot$width <- 900
# auc_pars_plot$height <- 500

auc_plots<- subplot(auc_base_plot, auc_ema_plot, auc_full_plot, margin = .025, titleX = TRUE, titleY = TRUE)
auc_plots <- auc_plots %>%
  layout(
    annotations = list(
      list(x = 0.16 , y = 1.094, text = "Baseline models", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.86 , y = 1.094, text = "Parsimonious models", showarrow = F, xref='paper', yref='paper')),
    margin = list(
      l = 200,
      r = 0,
      b = 75,
      t = 150,
      pad = 0
    )
  )
# 
# auc_plots$width <-  1800
# auc_plots$height <- 700
# print(auc_plots)
orca(auc_plots, file = "./output/cprd_models_auc_agesex.png", width = 1800, height = 530)


# r$colorbar_len <- 0.9
# print(auc_plots)

eavg_plots <- subplot(eavg_base_plot, eavg_ema_plot, eavg_full_plot, margin = 0.025, titleX = TRUE, titleY = TRUE)
eavg_plots <- eavg_plots %>%
  layout(
    annotations = list(
      list(x = 0.16 , y = 1.094, text = "Baseline models", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.86 , y = 1.094, text = "Parsimonious models", showarrow = F, xref='paper', yref='paper')),
    margin = list(
      l = 200,
      r = 0,
      b = 75,
      t = 150,
      pad = 0
    )
  )

eavg_plots$width <-  1800
eavg_plots$height <- 700
# r$colorbar_len <- 0.9


orca(eavg_plots, file = "./output/cprd_models_eavg_agesex.png", width = 1800, height = 530) #height was 420

# export(eavg_plots, file = "./output/cprd_models_eavg_agesex.png")

# orca(auc_plots, file = "./output/cprd_models_auc.png", scale = 1)

# export(q, file = "base25_models_eavg.png")

