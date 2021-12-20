library(magrittr)
options(scipen=999)
library(heatmaply)

# # IPCI
# path_base <- paste0("~/Data/Coagulopathy/IPCI/ValidationBase/IPCI/Analysis_", i, "/validationResult.rds")
# path_ema <- paste0("~/Data/Coagulopathy/IPCI/ValidationEma/IPCI/Analysis_", i, "/validationResult.rds")

# SIDIAP


get_eval_ext <- function(path, plpResult = FALSE){
  # plpResult <- PatientLevelPrediction::loadPlpResult(path)
  if(plpResult == FALSE){
    validationResult <- readRDS(path)
    prediction <- validationResult$prediction
    evaluationStatistics <- validationResult$performanceEvaluation$evaluationStatistics
  } else {
    plpResult <- PatientLevelPrediction::loadPlpResult(path)
    prediction <- plpResult$prediction
    evaluationStatistics <- plpResult$performanceEvaluation$evaluationStatistics
    
  }
  auc_test <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "recalibrationInTheLarge", Metric == "AUC.auc") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  auc_test_lb <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "recalibrationInTheLarge", Metric == "AUC.auc_lb95ci") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  auc_test_ub <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "recalibrationInTheLarge", Metric == "AUC.auc_ub95ci") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  eavg_test <- data.frame(evaluationStatistics) %>%
    dplyr::filter(Eval == "recalibrationInTheLarge", Metric == "Emean.Eavg") %>%
    dplyr::select(Value) %>%
    as.numeric()
  
  # set value to be mean prediction, as this is what evaluatePlp() will use
  prediction$value <- prediction$recalibrationInTheLargeValue
  
  pred_0_64 <- prediction %>%
    dplyr::filter(ageYear >=0, ageYear <= 64)
  pred_65_150 <- prediction %>%
    dplyr::filter(ageYear >=65, ageYear <= 150)
  
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
  
  result <- data.frame(auc=auc_test, auc_lb=auc_test_lb, auc_ub=auc_test_ub,
                       auc_0_64=auc_0_64, auc_0_64_lb=auc_0_64_lb, auc_0_64_ub=auc_0_64_ub,
                       auc_65_150=auc_65_150, auc_65_150_lb=auc_65_150_lb, auc_65_150_ub=auc_65_150_ub,
                       eavg=eavg_test, eavg_0_64=eavg_0_64, eavg_65_150=eavg_65_150)
  
  return(result)
}

base_eval <- data.frame(auc=numeric(0),
                        auc_lb=numeric(0),
                        auc_ub=numeric(0),
                        auc_0_64=numeric(0),
                        auc_0_64_lb=numeric(0),
                        auc_0_64_ub=numeric(0),
                        auc_65_150=numeric(0),
                        auc_65_150_lb=numeric(0),
                        auc_65_150_ub=numeric(0),
                        eavg=numeric(0),
                        eavg_0_64=numeric(0),
                        eavg_65_150=numeric(0))
ema_eval <- data.frame(auc=numeric(0),
                       auc_lb=numeric(0),
                       auc_ub=numeric(0),
                       auc_0_64=numeric(0),
                       auc_0_64_lb=numeric(0),
                       auc_0_64_ub=numeric(0),
                       auc_65_150=numeric(0),
                       auc_65_150_lb=numeric(0),
                       auc_65_150_ub=numeric(0),
                       eavg=numeric(0),
                       eavg_0_64=numeric(0),
                       eavg_65_150=numeric(0))
# base_eavg <- data.frame(eavg=numeric(0),
#                         eavg_0_64=numeric(0),
#                         eavg_65_150=numeric(0))

for (i in 1:24) {
  # in case only every nth analysis needs to be loaded, not relevant anymore
  # therefore set to 1 == 0
  if(i %% 1 == 0) {
    # path_base <- paste0("~/Data/Coagulopathy/IPCI/ValidationBase/IPCI/Analysis_", i, "/validationResult.rds")
    path_base <- paste0("~/Data/Coagulopathy/SIDIAP/ValidationBase/Analysis_", i, "/validationResult.rds")
    
    auc_row <- data.frame(auc=0,
                          auc_lb=0,
                          auc_ub=0,
                          auc_0_64=0,
                          auc_0_64_lb=0,
                          auc_0_64_ub=0,
                          auc_65_150=0,
                          auc_65_150_lb=0,
                          auc_65_150_ub=0,
                          eavg=0,
                          eavg_0_64=0,
                          eavg_65_150=0)
    tryCatch(
      {
        eval_row <- get_eval_ext(path_base)
        # eavg_row <- get_eavg(path)
      }, error=function(cond) {
      }
    )
    base_eval <- rbind(base_eval, eval_row)
    # base_eavg <- rbind(base_eavg, eavg_row)
  }
}

for (i in 1:24) {
  # in case only every nth analysis needs to be loaded, not relevant anymore
  # therefore set to 1 == 0
  if(i %% 1 == 0) {
    path_ema <- paste0("~/Data/Coagulopathy/SIDIAP/ValidationEma/Analysis_", i, "/validationResult.rds")
    # path_ema <- paste0("~/Data/Coagulopathy/IPCI/ValidationEma/IPCI/Analysis_", i, "/validationResult.rds")
    
    auc_row <- data.frame(auc=0,
                          auc_lb=0,
                          auc_ub=0,
                          auc_0_64=0,
                          auc_0_64_lb=0,
                          auc_0_64_ub=0,
                          auc_65_150=0,
                          auc_65_150_lb=0,
                          auc_65_150_ub=0,
                          eavg=0,
                          eavg_0_64=0,
                          eavg_65_150=0)
    tryCatch(
      {
        eval_row <- get_eval_ext(path_ema)
        # eavg_row <- get_eavg(path)
      }, error=function(cond) {
      }
    )
    ema_eval <- rbind(ema_eval, eval_row)
    # base_eavg <- rbind(base_eavg, eavg_row)
  }
}

base_eval_mat <- base_eval
base_eval_mat_true <- base_eval
ema_eval_mat <- ema_eval
ema_eval_mat_true <- ema_eval
# base_eavg_mat <- base_eavg

base_eval_mat$auc[base_eval_mat$auc == 0] <- NA
base_eval_mat$auc_0_64[base_eval_mat$auc_0_64 == 0] <- NA
base_eval_mat$auc_65_150[base_eval_mat$auc_65_150 == 0] <- NA

base_eval_mat$auc[base_eval_mat$auc < 0.5] <- 0.5
base_eval_mat$auc_0_64[base_eval_mat$auc_0_64 < 0.5] <- 0.5
base_eval_mat$auc_65_150[base_eval_mat$auc_65_150 < 0.5] <- 0.5

ema_eval_mat$auc[ema_eval_mat$auc == 0] <- NA
ema_eval_mat$auc_0_64[ema_eval_mat$auc_0_64 == 0] <- NA
ema_eval_mat$auc_65_150[ema_eval_mat$auc_65_150 == 0] <- NA

ema_eval_mat$auc[ema_eval_mat$auc < 0.5] <- 0.5
ema_eval_mat$auc_0_64[ema_eval_mat$auc_0_64 < 0.5] <- 0.5
ema_eval_mat$auc_65_150[ema_eval_mat$auc_65_150 < 0.5] <- 0.5

base_eval_mat$eavg[base_eval_mat$eavg == 0] <- NA
base_eval_mat$eavg_0_64[base_eval_mat$eavg_0_64 == 0] <- NA
base_eval_mat$eavg_65_150[base_eval_mat$eavg_65_150 == 0] <- NA

ema_eval_mat$eavg[ema_eval_mat$eavg == 0] <- NA
ema_eval_mat$eavg_0_64[ema_eval_mat$eavg_0_64 == 0] <- NA
ema_eval_mat$eavg_65_150[ema_eval_mat$eavg_65_150 == 0] <- NA


ref_base <- read.csv("~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/settings.csv")
ref_base <- ref_base[order(ref_base$analysisId), ]

# outcome <- ref_base$outcomeName[seq(1, nrow(ref_base), 3)]
outcome <- ref_base$outcomeName
outcome_base <- paste0(outcome, " ", c(rep("30 days", 8),
                                       rep("60 days", 8),
                                       rep("90 days", 8)))

outcome_base <- gsub("narrow ", "", as.character(outcome_base))

# remove stroke and death outcome for EMA report
# also remove ischemic stroke
# base_eval_mat <- base_eval_mat[-c(2, 7, 8, 10, 15, 16, 18, 23, 24),]
# base_eval_mat <- base_eval_mat[-c(2, 7, 8, seq(9, 24, 1))]
base_eval_mat <- base_eval_mat %>%
  dplyr::slice(c(1, 3, 4, 5, 6))
ema_eval_mat <- ema_eval_mat %>%
  dplyr::slice(c(1, 3, 4, 5, 6))

# true values
base_eval_mat_true <- base_eval_mat_true %>%
  dplyr::slice(c(1, 3, 4, 5, 6))
ema_eval_mat_true <- ema_eval_mat_true %>%
  dplyr::slice(c(1, 3, 4, 5, 6))
# base_eavg_mat <- base_eavg_mat[-c(2, 7, 8, 10, 15, 16, 18, 23, 24),]
# removing DTH and STR using the following subset
# outcome_base <- outcome_base[-c(2, 7, 8, 10, 15, 16, 18, 23, 24)]
outcome_base <- outcome_base[-c(2, 7, 8, seq(9, 24, 1))]

full_data_base <- data.frame(base_eval_mat,
                             outcome_base = outcome_base)
full_data_ema <- data.frame(ema_eval_mat,
                            outcome_base = outcome_base)

full_data_base_true <- data.frame(base_eval_mat_true,
                             outcome_base = outcome_base)
full_data_ema_true <- data.frame(ema_eval_mat_true,
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
                                                                "MI or IS 30 days",
                                                                "PE 30 days",
                                                                "DVT 30 days",
                                                                "VTE 30 days"))) %>%
  dplyr::arrange(outcome_base)

full_data_ema<- full_data_ema %>%
  dplyr::mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days",
                                                                "MI or IS 30 days",
                                                                "PE 30 days",
                                                                "DVT 30 days",
                                                                "VTE 30 days"))) %>%
  dplyr::arrange(outcome_base)

# trye
full_data_base_true <- full_data_base_true %>%
  dplyr::mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days",
                                                                "MI or IS 30 days",
                                                                "PE 30 days",
                                                                "DVT 30 days",
                                                                "VTE 30 days"))) %>%
  dplyr::arrange(outcome_base)

full_data_ema_true <- full_data_ema_true %>%
  dplyr::mutate(outcome_base =  factor(outcome_base, levels = c("MI 30 days",
                                                                "MI or IS 30 days",
                                                                "PE 30 days",
                                                                "DVT 30 days",
                                                                "VTE 30 days"))) %>%
  dplyr::arrange(outcome_base)




# create cellnotes
cellnotes_base_auc <- paste0(format(round(full_data_base_true$auc, 2), nsmall = 2),"\n (",
                             format(round(full_data_base$auc_lb, 2), nsmall = 2), ", ",
                             format(round(full_data_base$auc_ub, 2), nsmall = 2), ")")
cellnotes_base_auc_0_64 <- paste0(format(round(full_data_base_true$auc_0_64, 2), nsmall = 2),"\n (",
                                  format(round(full_data_base$auc_0_64_lb, 2), nsmall = 2), ", ",
                                  format(round(full_data_base$auc_0_64_ub, 2), nsmall = 2), ")")
cellnotes_base_auc_65_150 <- paste0(format(round(full_data_base_true$auc_65_150, 2), nsmall = 2),"\n (",
                                    format(round(full_data_base$auc_65_150_lb, 2), nsmall = 2), ", ",
                                    format(round(full_data_base$auc_65_150_ub, 2), nsmall = 2), ")")
cellnotes_base <- data.frame(cellnotes_base_auc = cellnotes_base_auc,
                             cellnotes_base_auc_0_64 = cellnotes_base_auc_0_64,
                             cellnotes_base_auc_65_150 = cellnotes_base_auc_65_150)

cellnotes_ema_auc <- paste0(format(round(full_data_ema_true$auc, 2), nsmall = 2),"\n (",
                            format(round(full_data_ema$auc_lb, 2), nsmall = 2), ", ",
                            format(round(full_data_ema$auc_ub, 2), nsmall = 2), ")")
cellnotes_ema_auc_0_64 <- paste0(format(round(full_data_ema_true$auc_0_64, 2), nsmall = 2),"\n (",
                                 format(round(full_data_ema$auc_0_64_lb, 2), nsmall = 2), ", ",
                                 format(round(full_data_ema$auc_0_64_ub, 2), nsmall = 2), ")")
cellnotes_ema_auc_65_150 <- paste0(format(round(full_data_ema_true$auc_65_150, 2), nsmall = 2),"\n (",
                                   format(round(full_data_ema$auc_65_150_lb, 2), nsmall = 2), ", ",
                                   format(round(full_data_ema$auc_65_150_ub, 2), nsmall = 2), ")")
cellnotes_ema <- data.frame(cellnotes_ema_auc = cellnotes_ema_auc,
                            cellnotes_ema_auc_0_64 = cellnotes_ema_auc_0_64,
                            cellnotes_ema_auc_65_150 = cellnotes_ema_auc_65_150)

axis_label_font_size <- 24
tick_font_size <- 18
cell_note_font_size <- 18

auc_base_plot <- full_data_base %>%
  dplyr::select(c(auc, auc_0_64, auc_65_150)) %>%
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
                       labCol = c("All ages", "Age <65", "Age >=65"),
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
  dplyr::select(c(auc, auc_0_64, auc_65_150)) %>%
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
                       labCol = c("All ages", "Age <65", "Age >=65"),
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

eavg_base_plot <- full_data_base %>%
  dplyr::select(c(eavg, eavg_0_64, eavg_65_150)) %>%
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
                       labCol = c("All ages", "Age <65", "Age >=65"),
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
  dplyr::select(c(eavg, eavg_0_64, eavg_65_150)) %>%
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
                       labCol = c("All ages", "Age <65", "Age >=65"),
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


# auc_base_plot$width <- 900
# auc_base_plot$height <- 500
# 
# auc_pars_plot$width <- 900
# auc_pars_plot$height <- 500

auc_plots<- subplot(auc_base_plot, auc_ema_plot, margin = .1, titleX = TRUE, titleY = TRUE)
auc_plots <- auc_plots %>%
  layout(
    annotations = list(
      list(x = 0.11 , y = 1.094, text = "Baseline models", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.92 , y = 1.094, text = "Parsimonious models", showarrow = F, xref='paper', yref='paper'))
  )

auc_plots$width <-  1400
auc_plots$height <- 420
print(auc_plots)

# r$colorbar_len <- 0.9
# print(auc_plots)

eavg_plots<- subplot(eavg_base_plot, eavg_ema_plot, margin = .075, titleX = TRUE, titleY = TRUE)
eavg_plots <- eavg_plots %>%
  layout(
    annotations = list(
      list(x = 0.11 , y = 1.094, text = "Baseline models", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.92 , y = 1.094, text = "Parsimonious models", showarrow = F, xref='paper', yref='paper'))
  )

eavg_plots$width <-  1400
eavg_plots$height <- 420
# r$colorbar_len <- 0.9
print(eavg_plots)
# 
export(auc_plots, file = "./output/cprd_models_on_sidiap_auc_.png")
export(eavg_plots, file = "./output/cprd_models_on_sidiap_eavg_.png")

# orca(auc_plots, file = "./output/cprd_models_auc.png", scale = 1)

# export(q, file = "base25_models_eavg.png")

