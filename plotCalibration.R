# prediction1 <- prediction[-(2)]
# attr(prediction1, "metaData") <- attr(prediction, "metaData")
# test <- PatientLevelPrediction::evaluatePlp(prediction1)
library(ggplot2)
library(ggpubr)

plotSparseCalibration2 <- function(evaluation,
                                   smooth = "loess",
                                   span = 1,
                                   nKnots = 5,
                                   scatter = T,
                                   bins = 10,
                                   zoom =  "data",
                                   sample = T,
                                   fileName = NULL,
                                   type = NULL) {
  
  ind <- 1:nrow(evaluation$calibrationSummary)
  
  if(is.null(type)){
    if(!is.null(evaluation$calibrationSummary$Eval)){
      ind <- evaluation$calibrationSummary$Eval%in%c('test','validation')
    }
  } else{
    ind <- evaluation$calibrationSummary$Eval == type
  }
  # use calibrationSummary
  sparsePred <- evaluation$calibrationSummary[ind,]
  
  limVal <- max(max(sparsePred$averagePredictedProbability),max(sparsePred$observedIncidence))
  
  smooth_plot <- ggplot2::ggplot(data = sparsePred, ggplot2::aes(x = averagePredictedProbability, 
                                                                 y = observedIncidence)) +
    ggplot2::stat_smooth(ggplot2::aes(color = "Loess", linetype = "Loess"),
                         method = "loess",
                         se = TRUE,
                         #span = span,
                         size = 1,
                         show.legend = F) +
    ggplot2::geom_segment(ggplot2::aes(x = 0,
                                       xend = 1,
                                       y = 0,
                                       yend = 1,
                                       color = "Ideal",
                                       linetype = "Ideal")) +
    ggplot2::coord_cartesian(xlim = c(0,limVal),
                             ylim = c(0,limVal)) + 
    ggplot2::scale_linetype_manual(name = "Models",
                                   values = c(Loess = "solid",
                                              Ideal = "dashed")) + 
    ggplot2::scale_color_manual(name = "Models", values = c(Loess = "blue", Ideal = "red")) + 
    ggplot2::labs(x = "Predicted Probability", y = "Observed Probability")
  
  # construct the plot grid
  if (scatter) {
    smooth_plot <- smooth_plot + ggplot2::geom_point(data = sparsePred,
                                                     ggplot2::aes(x = averagePredictedProbability,
                                                                  y = observedIncidence),
                                                     color = "black",
                                                     size = 2)
  }
  
  # Histogram object detailing the distibution of event/noevent for each probability interval
  
  popData1 <- sparsePred[,c('averagePredictedProbability', 'PersonCountWithOutcome')]
  popData1$Label <- "Outcome"
  colnames(popData1) <- c('averagePredictedProbability','PersonCount',"Label")
  popData2 <- sparsePred[,c('averagePredictedProbability', 'PersonCountAtRisk')]
  popData2$Label <- "No Outcome"
  popData2$PersonCountAtRisk <- -1*(popData2$PersonCountAtRisk -popData1$PersonCount)
  colnames(popData2) <- c('averagePredictedProbability','PersonCount',"Label")
  popData <- rbind(popData1, popData2)
  popData$averagePredictedProbability <- factor(popData$averagePredictedProbability)
  hist_plot <- ggplot2::ggplot(popData, ggplot2::aes(y = averagePredictedProbability, x = PersonCount, 
                                                     fill = Label)) + 
    ggplot2::geom_bar(data = subset(popData,Label == "Outcome"), stat = "identity") + 
    ggplot2::geom_bar(data = subset(popData,Label == "No Outcome"), stat = "identity") + 
    ggplot2::geom_bar(stat = "identity") + 
    ggplot2::scale_x_continuous(labels = abs) + 
    #ggplot2::scale_fill_brewer(palette = "Set1") + 
    ggplot2::coord_flip( ) +
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank())
  
  # testting whether this is installed in shinydeploy
  plot <- gridExtra::grid.arrange(smooth_plot,
                                  hist_plot,
                                  ncol = 1,
                                  heights=c(2,1))
  
  #plot <- cowplot::plot_grid(smooth_plot,
  #                           hist_plot,
  #                           ncol = 1,
  #                           axis = "lr",
  #                           align = "v",
  #                           rel_heights = c(1, 0.6))
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}



result1 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_1/plpResult")
result2 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_2/plpResult")
result3 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_3/plpResult")
result4 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_4/plpResult")
result5 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_5/plpResult")
result6 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_6/plpResult")
result7 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_7/plpResult")
result8 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_8/plpResult")

MI <- PatientLevelPrediction::plotSparseCalibration(result1$performanceEvaluation)
IS <- PatientLevelPrediction::plotSparseCalibration(result2$performanceEvaluation)
MIIS <- PatientLevelPrediction::plotSparseCalibration(result3$performanceEvaluation)
PE <- PatientLevelPrediction::plotSparseCalibration(result4$performanceEvaluation)
DVT <- PatientLevelPrediction::plotSparseCalibration(result5$performanceEvaluation)
VTE <- PatientLevelPrediction::plotSparseCalibration(result6$performanceEvaluation)
DTH <- PatientLevelPrediction::plotSparseCalibration(result7$performanceEvaluation)
STR <- PatientLevelPrediction::plotSparseCalibration(result8$performanceEvaluation)

MI_SM <- PatientLevelPrediction::plotSmoothCalibration(result1, scatter = TRUE,zoom = "data")
IS_SM <- PatientLevelPrediction::plotSmoothCalibration(result2, scatter = TRUE, zoom = "data")
MIIS_SM <- PatientLevelPrediction::plotSmoothCalibration(result3, scatter = TRUE, zoom = "data")
PE_SM <- PatientLevelPrediction::plotSmoothCalibration(result4, scatter = TRUE, zoom = "data")
DVT_SM <- PatientLevelPrediction::plotSmoothCalibration(result5, scatter = TRUE, zoom = "data")
VTE_SM <- PatientLevelPrediction::plotSmoothCalibration(result6, scatter = TRUE, zoom = "data")
# DTH_SM <- PatientLevelPrediction::plotSmoothCalibration(result7, zoom = "data")
# STR_SM <- PatientLevelPrediction::plotSmoothCalibration(result8, zoom = "data")


sparseCalibration <- ggarrange(MI, (MIIS + ylab(NULL)), PE, DVT, VTE,
          labels = c("MI", "MI or IS", "PE", "DVT", "VTE"),
          ncol = 3, nrow = 2)

smoothCalibration <- ggarrange(MI_SM , MIIS_SM , PE_SM, DVT_SM , VTE_SM ,
                               labels = c("MI", "MI or IS", "PE", "DVT", "VTE"),
                               ncol = 3, nrow = 2)

ggsave(plot = sparseCalibration, filename = "./output/sparseCalibration.png", width = 15, height = 8)
ggsave(plot = smoothCalibration, filename = "./output/smoothCalibration.png", width = 15, height = 8)

