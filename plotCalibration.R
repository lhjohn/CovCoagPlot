# prediction1 <- prediction[-(2)]
# attr(prediction1, "metaData") <- attr(prediction, "metaData")
# test <- PatientLevelPrediction::evaluatePlp(prediction1)
library(ggplot2)
library(ggpubr)

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

