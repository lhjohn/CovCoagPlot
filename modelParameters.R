library(mltools)
library(data.table)
library(heatmaply)
# plpResult <- PatientLevelPrediction::loadPlpResult("~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/Analysis_1/plpResult")
plpResultsPath1 <- "~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/Analysis_1/plpResult"
plpResultsPath3 <- "~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/Analysis_3/plpResult"
plpResultsPath4 <- "~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/Analysis_4/plpResult"
plpResultsPath5 <- "~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/Analysis_5/plpResult"
plpResultsPath6 <- "~/Data/Coagulopathy/CPRD Aurum/BaseOutput_25p/Analysis_6/plpResult"

# plpResult$model$model$coefficients
# remove sex coefficient
logit <- function(t) {
  return(1/(1+exp(-t)))
}

risk <- function(plpResultsPath, male = 1, female = 0) {
  plpResult <- PatientLevelPrediction::loadPlpResult(plpResultsPath)
  model <- plpResult$model$model$coefficients
  
  insert <- setNames(0.000, "4003")
  model <- c(model[1:2], insert, model[3:length(model)])
  
  # create one-hot encoded dummy data frame, without the last column (sex)
  ageLen <- length(names(model[1:(length(model)-1)]))
  data <- data.frame(matrix(ncol = length(names(model[2:ageLen]))))
  data <- one_hot(as.data.table(factor(names(model[2:ageLen]),
                                       levels = names(model[2:ageLen]))))
  colnames(data) <- names(model[2:(length(model)-1)])
  
  intercept <- model["(Intercept)"]
  sexCoeff <- model["8507001"]
  predictors <- model[2:(length(model)-1)]
  
  pred <- data.frame(predMale = numeric(0),
                     predFemale = numeric(0))
  
  for (i in 1:length(predictors)) {
    predMale <- logit(intercept+sum(data[i]*predictors)+male*sexCoeff)
    predFemale <- logit(intercept+sum(data[i]*predictors)+female*sexCoeff)
    
    pred <- rbind(pred,
                  data.frame(predMale = predMale,
                             predFemale = predFemale))
  }
  pred$age <- names(data)
  rownames(pred) <- names(data)
  
  return(pred)
}

risk1 <- risk(plpResultsPath1)
risk3 <- risk(plpResultsPath3)
risk4 <- risk(plpResultsPath4)
risk5 <- risk(plpResultsPath5)
risk6 <- risk(plpResultsPath6)

ageLab <- c("15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44",
            "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74",
            "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95 - 99", "100 - 104",
            "105 - 109")

plotHeat <- function(risk, yTick, yLab = F, xLab = F) {
  axis_label_font_size <- 28
  tick_font_size <- 20
  
  return(heatmaply::heatmaply(risk[c("predMale", "predFemale")],
                       dendrogram = "none",
                       ylab = "Age group",
                       xlab = "Sex", 
                       main = " ",
                       plot_method = "plotly",
                       colorbar_len = 1,
                       # cellnote = cellnotes_base,
                       # cellnote_size = cell_note_font_size,
                       # draw_cellnote = TRUE,
                       digits = 5L,
                       # cellnote_textposition = "middle center",
                       scale = "none",
                       na.value = "grey50",
                       # limits = c(0.0, 0.005),
                       # margins = c(60,100,NA,NA),
                       # grid_color = "white",
                       # grid_width = 0.00002,
                       titleX = xLab,
                       titleY = yLab,
                       hide_colorbar = FALSE,
                       branches_lwd = NULL,
                       label_names = c("Outcome", "Target", "AUROC"),
                       # fontsize_row = 8,
                       # fontsize_col = 8,
                       labCol = c("Male", "Female"),
                       column_text_angle = 0,
                       grid_gap = 3,
                       labRow = ageLab,
                       showticklabels = c(TRUE, yTick),
                       key.title = "Pred. risk",
                       heatmap_layers = theme(axis.line=element_blank())) %>%layout(
             font = list(
               size = axis_label_font_size),
             xaxis = list(tickfont = list(size = tick_font_size)), 
             yaxis = list(tickfont = list(size = tick_font_size))
           )
  )
}

width <- 500
height <- 800

MI <- plotHeat(risk1, yTick = T, yLab = T)
MIIS <- plotHeat(risk3, yTick = F)
PE <- plotHeat(risk4, yTick = F, xLab =T)
DVT <- plotHeat(risk5, yTick = F)
VTE <- plotHeat(risk6, yTick = F)

plot <- subplot(MI, MIIS, PE, DVT, VTE, titleX = TRUE, titleY = TRUE) %>%
  layout(
    annotations = list(
      list(x = 0.075 , y = 1.05, text = "MI", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.26 , y = 1.05, text = "MI or IS", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.5 , y = 1.05, text = "PE", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.72 , y = 1.05, text = "DVT", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.93 , y = 1.05, text = "VTE", showarrow = F, xref='paper', yref='paper'))
  )
plot$width <-  1600
plot$height <- 800
print(plot)

export(plot, file = "./output/base_risk_pred.png")

#############
MI <- plotHeat(risk1, yTick = T, yLab = T)
MIIS <- plotHeat(risk3, yTick = T, yLab = T)
PE <- plotHeat(risk4, yTick = T, xLab =T)
DVT <- plotHeat(risk5, yTick = T, yLab = T)
VTE <- plotHeat(risk6, yTick = T, yLab = T)

MIIS$width <- 500
MIIS$height <- 800
MIIS <- MIIS  %>%
  layout(
    annotations = list(
      list(x = 0.15 , y = 1.05, text = "MI or IS", showarrow = F, xref='paper', yref='paper'))
  )
export(MIIS, file = "./output/MIIS_base_risk_pred.png")

VTE$width <- 500
VTE$height <- 800
VTE <- VTE   %>%
  layout(
    annotations = list(
      list(x = 0.50 , y = 1.05, text = "VTE", showarrow = F, xref='paper', yref='paper'))
  )
export(VTE, file = "./output/VTE_base_risk_pred.png")



