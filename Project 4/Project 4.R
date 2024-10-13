#import libraries and data
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)

#load dataframe
hrdata_df <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 4/HRData.csv")

#select columns
selected_columns <- hrdata_df[, c("GenderID", "PerfScoreID", "Salary", "Age", "EngagementSurvey", "EmpSatisfaction", "Absences")]

#covariance matrix
cov_matrix <- cov(selected_columns)
print("Covariance Matrix:")
print(cov_matrix)

#correlation matrix
cor_matrix <- cor(selected_columns)
print("Correlation Matrix:")
print(cor_matrix)

#2. Visualize the correlations you have computed. There are a few different ways to  do this (including scatterplots, heat maps, etc.). Use the visualization method that you think best conveys the relations.
#converting matrices to long format for ggplot
cov_melt <- melt(cov_matrix)
cor_melt <- melt(cor_matrix)
#heatmap for covariance matrix
ggplot(data = cov_melt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "purple", high = "orange", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = "Covariance Matrix Heatmap", x = "", y = "")

# heatmap for correlation matrix
ggplot(data = cor_melt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "purple", high = "orange", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = "Correlation Matrix Heatmap", x = "", y = "")

