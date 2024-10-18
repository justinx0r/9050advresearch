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

# 3. Provide a summary of what you see in these results. Explain the nature of the bivariate relations (or lack thereof) in these data. Write this summary like you  might see reported in a journal article

# 4. Run correlations separately for each of these two subsamples. Report the results for each and provide an interpretation of what you see as you did in Questions 2 and 3. Are the correlations the same across the subsamples and/or from the entire sample? I’m not asking for a test of statistical significance, but your own  interpretation based on the values you compute.

# Select columns
selected_columns <- hrdata_df %>%
  select(GenderID, PerfScoreID, Salary, Age, EngagementSurvey, EmpSatisfaction, Absences

# Filter data for male employees - GenderID == 0 for males


# Calculate correlation matrices for each subsample


# Print correlation matrices
print("Correlation Matrix for Males:")
print(cor_matrix_male)

print("Correlation Matrix for Females:")
print(cor_matrix_female)

# Convert matrices to long format for ggplot
cor_melt_male <- melt(cor_matrix_male)
cor_melt_female <- melt(cor_matrix_female)

# Heatmap for male correlation matrix
ggplot(data = cor_melt_male, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "purple", high = "orange", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = "Correlation Matrix Heatmap for Males", x = "", y = "")

# Heatmap for female correlation matrix
ggplot(data = cor_melt_female, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "purple", high = "orange", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = "Correlation Matrix Heatmap for Females", x = "", y = "")

# Interpretation of results ****TO EXPAND ON*****
# Based on the correlation matrices and heatmaps for both males and females, we can observe the following:
# The strength and direction of correlations between variables may differ between males and females.
# For example, the correlation between Salary and Age might be stronger in one gender compared to the other.
# Similarly, the relationship between EngagementSurvey and EmpSatisfaction could vary across genders.
# These differences suggest that gender may play a role in how these variables are related to each other.
# Overall, the correlations in the subsamples provide a more nuanced understanding of the data compared to the entire sample.