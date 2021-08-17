library(magrittr)

auc <- function(path){
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
# path <- "~/Downloads/BaseOutput/Analysis_1/plpResult"
# test <- auc("~/Downloads/BaseOutput/Analysis_1/plpResult")

base <- data.frame(auc=numeric(0),
                   auc_0_64=numeric(0),
                   auc_65_150=numeric(0))

for (i in 1:72) {
  path <- paste0("~/Downloads/BaseOutput/Validation/IPCI/Analysis_", i, "/validationResult.rds")
  tryCatch(
    {
      base_ipci[i] <- as.numeric(data.frame(readRDS(path)$performanceEvaluation$evaluationStatistics)["AUC.auc", "Value"])
    }, error=function(cond) {
    }
  )
}



for (i in 1:72) {
  if(i %% 3 == 1) {
    path <- paste0("~/Downloads/BaseOutput/Analysis_", i, "/plpResult")
    row <- data.frame(auc=0,
                      auc_0_64=0,
                      auc_65_150=0)
    tryCatch(
      {
        row <- auc(path)
      }, error=function(cond) {
      }
    )
    base <- rbind(base, row) 
  }
}

base_matrix <- base
base_matrix[base_matrix == 0] <- NA
base_matrix[base_matrix < 0.5] <- 0.5
# base_matrix <- base_matrix[base_matrix < 0.5] <- NA

ref_base <- read.csv("~/Downloads/BaseOutput/settings.csv")
ref_base <- ref_base[order(ref_base$analysisId), ]

outcome <- ref_base$outcomeName[seq(1, nrow(ref_base), 3)]
outcome_base <- paste0(outcome, " ", c(rep("30 days", 8),
                                       rep("60 days", 8),
                                       rep("90 days", 8)))
p <- heatmaply::heatmaply(base_matrix,
                          dendrogram = "none",
                          ylab = "Outcome and setting",
                          xlab = "Target", 
                          main = "CPRD Aurum (internal)",
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

p$width <- 500
p$height <- 900

export(p, file = "base_models.png")
print(p)
