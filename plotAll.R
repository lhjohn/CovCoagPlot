library(scales)
library(heatmaply)

base <- rep(NA, 72)

for (i in 1:72) {
  path <- paste0("~/Downloads/BaseOutput/Analysis_", i, "/plpResult/performanceEvaluation.rds")
  tryCatch(
    {
      base[i] <- as.numeric(data.frame(readRDS(path)$evaluationStatistics)["AUC.auc.1", "Value"])
    }, error=function(cond) {
    }
  )
}

data_mat_base <- matrix(base,
                        ncol = 3,
                        byrow = TRUE)
ref_base <- read.csv("~/Downloads/BaseOutput/settings.csv")
ref_base <- ref_base[order(ref_base$analysisId), ]

############################
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

#############################
# load validation IPCI
base_ipci <- rep(NA, 72)
ema_ipci <- rep(NA, 72)

for (i in 1:72) {
  path <- paste0("~/Downloads/BaseOutput/Validation/IPCI/Analysis_", i, "/validationResult.rds")
  tryCatch(
    {
      base_ipci[i] <- as.numeric(data.frame(readRDS(path)$performanceEvaluation$evaluationStatistics)["AUC.auc", "Value"])
    }, error=function(cond) {
    }
  )
}

for (i in 1:length(base_ipci)) {
  if(base_ipci[i] < 0.5 & !is.na(base_ipci[i])) {
    base_ipci[i] <- NA
  }
}

data_mat_base_ipci <- matrix(base_ipci,
                             ncol = 3, 
                             byrow = TRUE)
data_mat_ema_ipci <- matrix(ema_ipci,
                             ncol = 3, 
                             byrow = TRUE)

full_val_data <- matrix(, nrow = 48, ncol = 3)

j <- 1
k <- 1
for (i in seq(1, 48, 1)) { # 72 iterations
  if(i %% 2 == 0) {
    full_val_data[i,] <- data_mat_ema_ipci[j,]
    j = j+1
  } else if (i %% 2 == 1){
    full_val_data[i,] <- data_mat_base_ipci[k,]
    k = k+1
  }
}

#############################

data_mat_ema <- matrix(ema,
                       ncol = 3, 
                       byrow = TRUE)
ref_ema <- read.csv("~/Downloads/EMAoutput/settings.csv")
ref_ema <- ref_ema[order(ref_ema$analysisId), ]

################################
# interlace base and EMA data
full_data <- matrix(, nrow = 48, ncol = 3)

j <- 1
k <- 1
for (i in seq(1, 48, 1)) { # 72 iterations
  if(i %% 2 == 0) {
    full_data[i,] <- data_mat_ema[j,]
    j = j+1
  } else if (i %% 2 == 1){
    full_data[i,] <- data_mat_base[k,]
    k = k+1
  }
}

############
# get unique names
outcome <- ref_base$outcomeName[seq(1, nrow(ref_base), 3)]
outcome_base <- paste0(outcome, " ", c(rep("30 BAS", 8),
                                       rep("60 BAS", 8),
                                       rep("90 BAS", 8)))
# removed outcome here for visibility
# blank_vec <- rep(" ", 24)
outcome_ema <- paste0(outcome, " ", c(rep("30 EMA", 8),
                                      rep("60 EMA", 8),
                                      rep("90 EMA", 8)))

# Interlace ref
ref_data <- matrix(, nrow = 48, ncol = 1)

j <- 1
k <- 1
for (i in seq(1, 48, 1)) { # 72 iterations
  if(i %% 2 == 0) {
    ref_data[i] <- outcome_ema[j]
    j = j+1
  } else if (i %% 2 == 1){
    ref_data[i] <- outcome_base[k]
    k = k+1
  }
}

p <- heatmaply::heatmaply(full_data,
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
          labCol = c("All ages", "Age >=65", "Age <65"),
          column_text_angle = 0,
          labRow = ref_data,
          heatmap_layers = theme(axis.line=element_blank()))

s <- heatmaply::heatmaply(full_val_data,
                          dendrogram = "none",
                          ylab = "Outcome and setting",
                          xlab = "Target", 
                          main = "CPRD Aurum (internal)                             IPCI (external)",
                          # title = "test",
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
                          labCol = c("All ages", "Age >=65", "Age <65"),
                          column_text_angle = 0,
                          labRow = NULL,
                          showticklabels = c(TRUE, FALSE),
                          heatmap_layers = theme(axis.line=element_blank()))


r <- subplot(p, s, margin = .05, titleX = TRUE, titleY = FALSE)
r$width <- 900
r$height <- 1500

export(r, file = "scaled_iris.png")
print(r)
