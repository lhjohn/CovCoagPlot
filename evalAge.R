library(magrittr)
options(scipen=999)

get_auc <- function(path){
  plpResult <- PatientLevelPrediction::loadPlpResult(path)
  prediction <- plpResult$prediction
  evaluationStatistics <- plpResult$performanceEvaluation$evaluationStatistics
  
  auc_test <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "test", Metric == "AUC.auc") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  pred_0_64 <- prediction %>%
    dplyr::filter(ageYear >=0, ageYear <= 64, indexes == -1)
  pred_65_150 <- prediction %>%
    dplyr::filter(ageYear >=65, ageYear <= 150, indexes == -1)
  
  pred_0_64_eval <- PatientLevelPrediction::evaluatePlp(pred_0_64)
  auc_0_64 <- pred_0_64_eval$evaluationStatistics$AUC$auc
  
  pred_65_150_eval <- PatientLevelPrediction::evaluatePlp(pred_65_150)
  auc_65_150 <- pred_65_150_eval$evaluationStatistics$AUC$auc
  
  result <- data.frame(auc=auc_test, auc_0_64=auc_0_64, auc_65_150=auc_65_150)
  
  return(result)
}

get_eavg <- function(path){
  plpResult <- PatientLevelPrediction::loadPlpResult(path)
  prediction <- plpResult$prediction
  evaluationStatistics <- plpResult$performanceEvaluation$evaluationStatistics
  
  eavg_test <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "test", Metric == "Emean.Eavg") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  pred_0_64 <- prediction %>%
    dplyr::filter(ageYear >=0, ageYear <= 64, indexes == -1)
  pred_65_150 <- prediction %>%
    dplyr::filter(ageYear >=65, ageYear <= 150, indexes == -1)
  
  pred_0_64_eval <- PatientLevelPrediction::evaluatePlp(pred_0_64)
  eavg_0_64 <- pred_0_64_eval$evaluationStatistics$Emean
  
  pred_65_150_eval <- PatientLevelPrediction::evaluatePlp(pred_65_150)
  eavg_65_150 <- pred_65_150_eval$evaluationStatistics$Emean
  
  result <- data.frame(eavg=eavg_test, eavg_0_64=eavg_0_64, eavg_65_150=eavg_65_150)
  
  return(result)
}
# path <- "~/Downloads/BaseOutput/Analysis_1/plpResult"
# test <- auc("~/Downloads/BaseOutput/Analysis_1/plpResult")

base_auc <- data.frame(auc=numeric(0),
                   auc_0_64=numeric(0),
                   auc_65_150=numeric(0))
base_eavg <- data.frame(eavg=numeric(0),
                        eavg_0_64=numeric(0),
                        eavg_65_150=numeric(0))

for (i in 1:72) {
  if(i %% 3 == 1) {
    path <- paste0("~/Downloads/BaseOutput/Analysis_", i, "/plpResult")
    auc_row <- data.frame(auc=0,
                      auc_0_64=0,
                      auc_65_150=0)
    eavg_row <- data.frame(eavg=0,
                           eavg_0_64=0,
                           eavg_65_150=0)
    tryCatch(
      {
        auc_row <- get_auc(path)
        eavg_row <- get_eavg(path)
      }, error=function(cond) {
      }
    )
    base_auc <- rbind(base_auc, auc_row)
    base_eavg <- rbind(base_eavg, eavg_row)
  }
}

base_auc_mat <- base_auc
base_eavg_mat <- base_eavg

base_auc_mat[base_auc_mat == 0] <- NA
base_auc_mat[base_auc_mat < 0.5] <- 0.5

base_eavg_mat[base_eavg_mat == 0] <- NA

ref_base <- read.csv("~/Downloads/BaseOutput/settings.csv")
ref_base <- ref_base[order(ref_base$analysisId), ]

outcome <- ref_base$outcomeName[seq(1, nrow(ref_base), 3)]
outcome_base <- paste0(outcome, " ", c(rep("30 days", 8),
                                       rep("60 days", 8),
                                       rep("90 days", 8)))
p <- heatmaply::heatmaply(base_auc_mat,
                          dendrogram = "none",
                          ylab = "Outcome and setting",
                          xlab = "Target", 
                          main = "CPRD Aurum (internal) - Discrimination [AUROC]",
                          # cellnote = TRUE,
                          draw_cellnote = TRUE,
                          digits = 2L,
                          cellnote_textposition = "middle center",
                          scale = "none",
                          na.value = "grey50",
                          limits = c(0.5, 1.0),
                          # margins = c(60,100,NA,NA),
                          grid_color = "white",
                          grid_width = 0.00002,
                          titleX = TRUE,
                          hide_colorbar = FALSE,
                          branches_lwd = NULL,
                          label_names = c("Outcome", "Target:", "AUROC"),
                          fontsize_row = 8,
                          fontsize_col = 8,
                          labCol = c("All ages", "Age <65", "Age >=65"),
                          column_text_angle = 0,
                          labRow = outcome_base,
                          heatmap_layers = theme(axis.line=element_blank()))

q <- heatmaply::heatmaply(base_eavg_mat,
                          dendrogram = "none",
                          ylab = "Outcome and setting",
                          xlab = "Target", 
                          main = "CPRD Aurum (internal) - Calibration [Eavg]",
                          # cellnote = TRUE,
                          colors = viridis(n = 256,  option = "plasma"),
                          draw_cellnote = TRUE,
                          digits = 5L,
                          cellnote_textposition = "middle center",
                          scale = "none",
                          limits = c(0, 0.005),
                          na.value = "grey50",
                          # margins = c(60,100,NA,NA),
                          grid_color = "white",
                          grid_width = 0.00002,
                          titleX = TRUE,
                          hide_colorbar = FALSE,
                          branches_lwd = NULL,
                          label_names = c("Outcome", "Target:", "AUROC"),
                          fontsize_row = 8,
                          fontsize_col = 8,
                          labCol = c("All ages", "Age <65", "Age >=65"),
                          column_text_angle = 0,
                          labRow = outcome_base,
                          heatmap_layers = theme(axis.line=element_blank()))

p$width <- 500
p$height <- 900

q$width <- 500
q$height <- 900

export(p, file = "base_models_auc.png")
export(q, file = "base_models_eavg.png")

print(q)
print(p)
