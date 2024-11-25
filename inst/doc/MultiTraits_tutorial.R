## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)
## Introduction

## ----class.source = 'fold-show'-----------------------------------------------
# install.packages("MultiTraits")

## ----class.source = 'fold-show'-----------------------------------------------
# Install devtools if not already installed
# if (!requireNamespace("devtools", quietly = TRUE)) {
#   install.packages("devtools")
# }

# Install MultiTraits from GitHub
# devtools::install_github("biodiversity-monitoring/MultiTraits")

## ----class.source = 'fold-show'-----------------------------------------------
library(MultiTraits)
data(PFF)
data(WH)

# View the structure of the datasets
str(PFF)
str(WH)

## ----class.source = 'fold-show'-----------------------------------------------
LA <- c(369615.7, 11.8, 55.7, 36061.2, 22391.8, 30068.1, 31059.5, 29895.1)
LDMC <- c(25.2, 39.7, 13.3, 35.5, 33.2, 36.1, 35.2, 34.9)
SLA <- c(17.4, 6.6, 34.1, 14.5, 8.1, 12.1, 9.4, 10.9)
traits <- data.frame(LA, LDMC, SLA)
result <- CSR(data = traits)
CSR_plot(data=result)

## ----class.source = 'fold-show'-----------------------------------------------
data(PFF)
pff <- PFF[, c("SLA", "Height", "SeedMass")]
result <- LHS(pff)
head(result)
LHS_plot(result)
LHS_strategy_scheme()

## ----class.source = 'fold-show'-----------------------------------------------
data(PFF)
PFF[,3:20] <- log(PFF[,3:20])
PFF <- na.omit(PFF)
traits_dimension <-list(
  grow = c("SLA","Leaf_area","LDMC","SRL","Leaf_Nmass","Leaf_Pmass","Root_Nmass"),
  survive = c("Height","Leaf_Cmass","Root_Cmass","Leaf_CN","Leaf_NP","Leaf_CP","Root_CN"),
  reproductive = c("SeedMass","FltDate","FltDur"))
npt_result <- NPT(data = PFF, dimension = traits_dimension)
npt_result
dev.new() # A window that is too small will interfere with the drawing. 
          # Optionally, you can set the drawing window to pop up automatically.
NPT_plot(npt_result$result)
NPT_plot(npt_result$result, PFF$family)

## ----class.source = 'fold-show'-----------------------------------------------
data(WH)
WH <- WH[,4:23]
TN_corr(traits_matrix=WH, rThres = 0.2, pThres = 0.05)
Tn_result <- TN(traits_matrix = WH, rThres = 0.2, pThres = 0.05)
TN_metrics(Tn_result)
TN_plot(Tn_result, style = 1)

