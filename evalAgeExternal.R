library(magrittr)
options(scipen=999)
library(heatmaply)

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

for (i in 1:24) {
  # in case only every nth analysis needs to be loaded, not relevant anymore
  # therefore set to 1 == 0
  if(i %% 1 == 0) {
    path <- paste0("~/Downloads/BaseOutput_25p/Analysis_", i, "/plpResult")
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

ref_base <- read.csv("~/Downloads/BaseOutput_25p/settings.csv")
ref_base <- ref_base[order(ref_base$analysisId), ]

# outcome <- ref_base$outcomeName[seq(1, nrow(ref_base), 3)]
outcome <- ref_base$outcomeName
outcome_base <- paste0(outcome, " ", c(rep("30 days", 8),
                                       rep("60 days", 8),
                                       rep("90 days", 8)))

outcome_base <- gsub("narrow ", "", as.character(outcome_base))

# remove stroke and death outcome for EMA report
# also remove ischemic stroke
base_auc_mat <- base_auc_mat[-c(2, 7, 8, 10, 15, 16, 18, 23, 24),]
base_eavg_mat <- base_eavg_mat[-c(2, 7, 8, 10, 15, 16, 18, 23, 24),]
outcome_base <- outcome_base[-c(2, 7, 8, 10, 15, 16, 18, 23, 24)]

full_data <- data.frame(base_auc_mat,
                        base_eavg_mat,
                        outcome_base = outcome_base)
# full_data <- full_data %>%
#   mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days", "MI 60 days", "MI 90 days",
#                                                          "MI or IS 30 days", "MI or IS 60 days", "MI or IS 90 days",
#                                                          "PE 30 days", "PE 60 days", "PE 90 days",
#                                                          "DVT narrow 30 days", "DVT narrow 60 days", "DVT narrow 90 days",
#                                                          "VTE narrow 30 days", "VTE narrow 60 days", "VTE narrow 90 days"))) %>%
#   arrange(outcome_base)
full_data <- full_data %>%
  dplyr::mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days", "MI 60 days", "MI 90 days",
                                                                "MI or IS 30 days", "MI or IS 60 days", "MI or IS 90 days",
                                                                "PE 30 days", "PE 60 days", "PE 90 days",
                                                                "DVT 30 days", "DVT 60 days", "DVT 90 days",
                                                                "VTE 30 days", "VTE 60 days", "VTE 90 days"))) %>%
  dplyr::arrange(outcome_base)


p <- full_data %>%
  dplyr::select(c(auc, auc_0_64, auc_65_150)) %>%
  heatmaply::heatmaply(dendrogram = "none",
                       ylab = "Outcome and setting",
                       xlab = "Target", 
                       main = " ",
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
                       titleY = TRUE,
                       hide_colorbar = FALSE,
                       branches_lwd = NULL,
                       label_names = c("Outcome", "Target:", "AUROC"),
                       fontsize_row = 8,
                       fontsize_col = 8,
                       labCol = c("All ages", "Age <65", "Age >=65"),
                       column_text_angle = 0,
                       labRow = full_data$outcome_base,
                       key.title = "AUROC",
                       heatmap_layers = theme(axis.line=element_blank()))

q <- full_data %>%
  dplyr::select(c(eavg, eavg_0_64, eavg_65_150)) %>%
  heatmaply::heatmaply(base_eavg_mat,
                       dendrogram = "none",
                       ylab = "Outcome and setting",
                       xlab = "Target", 
                       main = "  ",
                       # cellnote = TRUE,
                       colors = viridis(n = 256,  option = "magma", direction = -1),
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
                       labRow = full_data$outcome_base,
                       showticklabels = c(TRUE, TRUE),
                       titleY = TRUE,
                       key.title = "E<sub>avg</sub>",
                       heatmap_layers = theme(axis.line=element_blank()))

p$width <- 500
p$height <- 475

q$width <- 500
q$height <- 475

# r <- subplot(p, q, margin = .05, titleX = TRUE, titleY = TRUE)
# r <- r %>% layout(annotations = list(
#   list(x = 0.15 , y = 1.05, text = "AUROC", showarrow = F, xref='paper', yref='paper'),
#   list(x = 0.82 , y = 1.05, text = "Eavg", showarrow = F, xref='paper', yref='paper'))
# )
# print(r)

print(q)
print(p)

export(p, file = "base25_models_auc.png")
export(q, file = "base25_models_eavg.png")

