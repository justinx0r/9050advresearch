#import libraries and data
library(tidyverse)
library(dplyr)
read.csv("/Users/justinwilliams/Code/9050advresearch/Project 4/HRData.csv")

# 1.Report the full covariance and correlation matrix for the following variables: GenderID, PerfScoreID, Salary, Age, EngagementSurvey, EmpSatisfaction, and Absences.  

#load data
hrdata_df <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 4/HRData.csv")

#select columns
selected_columns <- hrdata_df[, c("GenderID", "PerfScoreID", "Salary", "Age", "EngagementSurvey", "EmpSatisfaction", "Absences")]

#covariance matrix
cov_matrix <- cov(selected_columns, use = "complete.obs")
print("Covariance Matrix:")
print(cov_matrix)

#correlation matrix
cor_matrix <- cor(selected_columns, use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)