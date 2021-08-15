library(scales)
library(heatmaply)

base <- rep(NA, 72)

for (i in 1:72) {
  path <- paste0("~/Downloads/BaseOutput/Analysis_", i, "/plpResult/performanceEvaluation.rds")
  tryCatch(
    {
      base[i] <- as.numeric(data.frame(readRDS(path)$evaluationStatistics)["AUC.auc", "Value"])
    }, error=function(cond) {
    }
  )
}

data_mat_base <- matrix(base,
                   ncol = 3, byrow = TRUE)
ref_base <- read.csv("~/Downloads/BaseOutput/settings.csv")
ref_base <- ref_base[order(ref_base$analysisId), ]

outcome <- ref_base$outcomeName[seq(1, nrow(ref_base), 3)]
outcome <- paste0(outcome, " ", c(rep("30 BASE", 8),
                                  rep("60 BASE", 8),
                                  rep("90 BASE", 8)))

p <- heatmaply::heatmaply(data_mat_base,
          dendrogram = "none",
          ylab = "Outcome + settings",
          xlab = "Target", 
          main = "Baseline model",
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
          fontsize_row = 10,
          fontsize_col = 10,
          labCol = c("All ages", "Age >=65", "Age <65"),
          column_text_angle = 0,
          labRow = outcome,
          heatmap_layers = theme(axis.line=element_blank()))
print(p)
