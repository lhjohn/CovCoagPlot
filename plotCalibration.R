# prediction1 <- prediction[-(2)]
# attr(prediction1, "metaData") <- attr(prediction, "metaData")
# test <- PatientLevelPrediction::evaluatePlp(prediction1)
library(ggplot2)
library(ggpubr)
library(gridExtra)

plotDemoSum <- function(evaluation, type='test', main = "", fileName=NULL, outcome = "", yLab = "Fraction"){
  if (!all(is.na(evaluation$demographicSummary$averagePredictedProbability))){
    if(is.null(evaluation$demographicSummary$Eval)){
      evaluation$demographicSummary$Eval <- type
    }
    ind <- evaluation$demographicSummary$Eval==type
    x<- evaluation$demographicSummary[ind,colnames(evaluation$demographicSummary)%in%c('ageGroup','genGroup','averagePredictedProbability',
                                                                                       'PersonCountAtRisk', 'PersonCountWithOutcome')]
    
    
    # remove -1 values:
    x$averagePredictedProbability[is.na(x$averagePredictedProbability)] <- 0
    x <- x[x$PersonCountWithOutcome != -1,]
    if(nrow(x)==0){
      return(NULL)
    }
    
    x$observed <- x$PersonCountWithOutcome/x$PersonCountAtRisk
    
    
    x <- x[,colnames(x)%in%c('ageGroup','genGroup','averagePredictedProbability','observed')]
    
    # if age or gender missing add 
    if(sum(colnames(x)=='ageGroup')==1 && sum(colnames(x)=='genGroup')==0  ){
      x$genGroup = rep('Non', nrow(x))
      evaluation$demographicSummary$genGroup = rep('Non', nrow(evaluation$demographicSummary))
    } 
    if(sum(colnames(x)=='ageGroup')==0 && sum(colnames(x)=='genGroup')==1  ){
      x$ageGroup = rep('-1', nrow(x))
      evaluation$demographicSummary$ageGroup = rep('-1', nrow(evaluation$demographicSummary))
      
    }
    
    x <- reshape2::melt(x, id.vars=c('ageGroup','genGroup'))
    
    # 1.96*StDevPredictedProbability
    ci <- evaluation$demographicSummary[ind,colnames(evaluation$demographicSummary)%in%c('ageGroup','genGroup','averagePredictedProbability','StDevPredictedProbability')]
    ci$StDevPredictedProbability[is.na(ci$StDevPredictedProbability)] <- 1
    ci$lower <- ci$averagePredictedProbability-1.96*ci$StDevPredictedProbability
    ci$lower[ci$lower <0] <- 0
    ci$upper <- ci$averagePredictedProbability+1.96*ci$StDevPredictedProbability
    ci$upper[ci$upper >1] <- max(ci$upper[ci$upper <1])
    
    x$age <- gsub('Age group:','', x$ageGroup)
    x$age <- factor(x$age,levels=c(" 0-4"," 5-9"," 10-14",
                                   " 15-19"," 20-24"," 25-29"," 30-34"," 35-39"," 40-44",
                                   " 45-49"," 50-54"," 55-59"," 60-64"," 65-69"," 70-74",
                                   " 75-79"," 80-84"," 85-89"," 90-94"," 95-99","-1"),ordered=TRUE)
    
    
    
    x <- merge(x, ci[,c('ageGroup','genGroup','lower','upper')], by=c('ageGroup','genGroup'))
    x <- x[!is.na(x$value),]
    
    plot <- ggplot2::ggplot(data=x, 
                            ggplot2::aes(x=.data$age, 
                                         group=interaction(.data$variable,.data$genGroup))) +
      
      ggplot2::geom_line(ggplot2::aes(y=.data$value, group=.data$variable,
                                      color=.data$variable,
                                      linetype = .data$variable))+
      ggplot2::geom_ribbon(data=x[x$variable!='observed',],
                           ggplot2::aes(ymin=.data$lower, ymax=.data$upper
                                        , group=.data$genGroup), 
                           fill="blue", alpha=0.2) +
      ggplot2::facet_grid(.~ .data$genGroup, scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::ggtitle(main) +
      #ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(yLab) +
      ggplot2::scale_x_discrete("Age group") +
      ggplot2::scale_color_manual(values = c("royalblue4","red"),
                                  guide = ggplot2::guide_legend(title = outcome),
                                  labels = c("Expected", "Observed")) +
      
      ggplot2::guides(linetype=FALSE)
    
    if (!is.null(fileName))
      ggplot2::ggsave(fileName, plot, width = 7, height = 4.5, dpi = 400)
    return(plot)
  }
}

result1 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_1/plpResult")
result2 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_2/plpResult")
result3 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_3/plpResult")
result4 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_4/plpResult")
result5 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_5/plpResult")
result6 <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/EmaOutput_25p/Analysis_6/plpResult")


# restul1_sidiap <- readRDS("~/Data/Coagulopathy/SIDIAP/ValidationEma/Analysis_1/performanceEvaluation/")
# DVT <- plotDemoSum(result5$performanceEvaluation, outcome = "DVT", main = "CPRD Aurum") +
#   ggplot2::theme_light() +
#   ggplot2::theme(axis.title.x=element_blank())
# demoSum <- grid.arrange(MI, MIIS, PE, DVT, VTE, nrow = 5)
# ggsave(plot = demoSum, filename = "./output/demoSum.png", width = 16, height = 8)

MI <- plotDemoSum(result1$performanceEvaluation, outcome = "MI", main = "CPRD Aurum") +
  ggplot2::theme_light()
ggsave(plot = MI, filename = "./output/calibration/MI_cprd_cal.png", width = 15, height = 2)

MIIS <- plotDemoSum(result3$performanceEvaluation, outcome = "ATE", main = "CPRD Aurum") +
  ggplot2::theme_light()
ggsave(plot = MIIS, filename = "./output/calibration/MIIS_cprd_cal.png", width = 15, height = 2)

PE <- plotDemoSum(result4$performanceEvaluation, outcome = "PE", main = "CPRD Aurum") +
  ggplot2::theme_light()
ggsave(plot = PE, filename = "./output/calibration/PE_cprd_cal.png", width = 15, height = 2)

DVT <- plotDemoSum(result5$performanceEvaluation, outcome = "DVT", main = "CPRD Aurum") +
  ggplot2::theme_light()
ggsave(plot = DVT, filename = "./output/calibration/DVT_cprd_cal.png", width = 15, height = 2)

VTE <- plotDemoSum(result6$performanceEvaluation, outcome = "VTE", main = "CPRD Aurum") +
  ggplot2::theme_light()
ggsave(plot = VTE, filename = "./output/calibration/VTE_cprd_cal.png", width = 15, height = 2)


# 
# sparseCalibration <- ggarrange(MI, (MIIS + ylab(NULL)), PE, DVT, VTE,
#           labels = c("MI", "MI or IS", "PE", "DVT", "VTE"),
#           ncol = 3, nrow = 2)
# ggsave(plot = sparseCalibration, filename = "./output/sparseCalibration.png", width = 15, height = 8)

# result1_sidiap <- read.csv("~/Data/Coagulopathy/SIDIAP/ValidationEma/Analysis_1/performanceEvaluation/demographicSummary.csv")
result1_val_sidiap <- readRDS("~/Data/Coagulopathy/SIDIAP/ValidationEma/Analysis_1/validationResult.rds")
result3_val_sidiap <- readRDS("~/Data/Coagulopathy/SIDIAP/ValidationEma/Analysis_3/validationResult.rds")
result4_val_sidiap <- readRDS("~/Data/Coagulopathy/SIDIAP/ValidationEma/Analysis_4/validationResult.rds")
result5_val_sidiap <- readRDS("~/Data/Coagulopathy/SIDIAP/ValidationEma/Analysis_5/validationResult.rds")
result6_val_sidiap <- readRDS("~/Data/Coagulopathy/SIDIAP/ValidationEma/Analysis_6/validationResult.rds")

# evaluation <- list(demographicSummary = result1_sidiap)
MI_sidiap <- plotDemoSum(result1_val_sidiap$performanceEvaluation, outcome = "MI", main = "SIDIAP", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = MI_sidiap, filename = "./output/calibration/MI_sidiap_cal.png", width = 17, height = 2)

MIIS_sidiap <- plotDemoSum(result3_val_sidiap$performanceEvaluation, outcome = "ATE", main = "SIDIAP", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = MIIS_sidiap, filename = "./output/calibration/MIIS_sidiap_cal.png", width = 17, height = 2)

PE_sidiap <- plotDemoSum(result4_val_sidiap$performanceEvaluation, outcome = "PE", main = "SIDIAP", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = PE_sidiap, filename = "./output/calibration/PE_sidiap_cal.png", width = 17, height = 2)

DVT_sidiap <- plotDemoSum(result5_val_sidiap$performanceEvaluation, outcome = "DVT", main = "SIDIAP", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = DVT_sidiap, filename = "./output/calibration/DVT_sidiap_cal.png", width = 17, height = 2)

VTE_sidiap <- plotDemoSum(result6_val_sidiap$performanceEvaluation, outcome = "VTE", main = "SIDIAP", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = VTE_sidiap, filename = "./output/calibration/VTE_sidiap_cal.png", width = 17, height = 2)
# 
# MI_sidiap <- plotDemoSum(result1_val_sidiap$performanceEvaluation, outcome = "MI", yLab = " ", type = "recalibrationInTheLarge") +
#   ggplot2::theme_light() +
#   ggplot2::theme(axis.title.x=element_blank())
# 

result1_val_ipci <- readRDS("~/Data/Coagulopathy/IPCI/ValidationEma/IPCI/Analysis_1/validationResult.rds")
result3_val_ipci <- readRDS("~/Data/Coagulopathy/IPCI/ValidationEma/IPCI/Analysis_3/validationResult.rds")
result4_val_ipci <- readRDS("~/Data/Coagulopathy/IPCI/ValidationEma/IPCI/Analysis_4/validationResult.rds")
result5_val_ipci <- readRDS("~/Data/Coagulopathy/IPCI/ValidationEma/IPCI/Analysis_5/validationResult.rds")
result6_val_ipci <- readRDS("~/Data/Coagulopathy/IPCI/ValidationEma/IPCI/Analysis_6/validationResult.rds")

# evaluation <- list(demographicSummary = result1_ipci)
MI_ipci <- plotDemoSum(result1_val_ipci$performanceEvaluation, outcome = "MI", main = "IPCI", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = MI_ipci, filename = "./output/calibration/MI_ipci_cal.png", width = 17, height = 2)

MIIS_ipci <- plotDemoSum(result3_val_ipci$performanceEvaluation, outcome = "ATE", main = "IPCI", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = MIIS_ipci, filename = "./output/calibration/MIIS_ipci_cal.png", width = 17, height = 2)

PE_ipci <- plotDemoSum(result4_val_ipci$performanceEvaluation, outcome = "PE", main = "IPCI", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = PE_ipci, filename = "./output/calibration/PE_ipci_cal.png", width = 17, height = 2)

DVT_ipci <- plotDemoSum(result5_val_ipci$performanceEvaluation, outcome = "DVT", main = "IPCI", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = DVT_ipci, filename = "./output/calibration/DVT_ipci_cal.png", width = 17, height = 2)

VTE_ipci <- plotDemoSum(result6_val_ipci$performanceEvaluation, outcome = "VTE", main = "IPCI", type = "recalibrationInTheLarge") +
  ggplot2::theme_light()
ggsave(plot = VTE_ipci, filename = "./output/calibration/VTE_ipci_cal.png", width = 17, height = 2)