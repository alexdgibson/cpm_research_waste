# perfect_theoretical_practical_auc_values.R
# graphing potential AUC values for perfect prediction, theoretical, and practical
# August 2024

# load libraries
library(tidyverse)
library(ggplot2)
library(pROC)


# create data frames to show graph the three auc values
# seed set as the day and month

set.seed(2208)
samples <- 10000
weight <- sort(rnorm(n=samples, mean = 100, sd = 2))

# simulate a sample of a binary variable
obese_perf <- ifelse(test=(runif(n=samples)<(rank(weight)/5)),
                yes = 1, no = 0)
obese_theo <- ifelse(test=(runif(n=samples)<(rank(weight)/5754)),
                     yes = 1, no = 0)
obese_prac <- ifelse(test=(runif(n=samples)<(rank(weight)/10357)),
                     yes = 1, no = 0)

# fit model
fit_perf <- glm(obese_perf ~ weight)
fit_theo <- glm(obese_theo ~ weight)
fit_prac <- glm(obese_prac ~ weight)

# set roc to start a 0.0 and 1.0
par(pty = "s")

# plot the ROC curves and AUC values for each of the models
plot.roc(obese_perf, fit_perf$fitted.values, col = "#1b9e77", legacy.axes = TRUE, print.auc = TRUE, print.auc.y = 0.3, print.auc.x = 0.3)
plot.roc(obese_theo, fit_theo$fitted.values, add = TRUE, col = "#d95f02", legacy.axes = TRUE, print.auc = TRUE, print.auc.y = 0.25, print.auc.x = 0.3)
plot.roc(obese_prac, fit_theo$fitted.values, add = TRUE, col = "#7570b3", legacy.axes = TRUE, print.auc = TRUE, print.auc.y = 0.2, print.auc.x = 0.3)

# save the image through the export function in R Studio
