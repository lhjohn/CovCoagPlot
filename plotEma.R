library(scales)
library(heatmaply)

ema <- rep(NA, 72)

for (i in 1:72) {
  path <- paste0("~/Downloads/EMAoutput/Analysis_", i, "/plpResult/performanceEvaluation.rds")
  tryCatch(
    {
      ema[i] <- as.numeric(data.frame(readRDS(path)$evaluationStatistics)["AUC.auc", "Value"])
    }, error=function(cond) {
    }
  )
}

data_mat_ema <- matrix(ema,
                   ncol = 3, 
                   byrow = TRUE)
ref_ema <- read.csv("~/Downloads/EMAoutput/settings.csv")
ref_ema <- ref_ema[order(ref_ema$analysisId), ]

outcome <- ref_ema$outcomeName[seq(1, nrow(ref_ema), 3)]
outcome <- paste0(outcome, " ", c(rep("30 EMA", 8),
                                  rep("60 EMA", 8),
                                  rep("90 EMA", 8)))

p <- heatmaply::heatmaply(data_mat_ema,
          dendrogram = "none",
          ylab = "Outcome and settings",
          xlab = "Target", 
          main = "EMA model",
          # cellnote = TRUE,
          draw_cellnote = TRUE,
          # limits = c(0.5, 1.0),
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
