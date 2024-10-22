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
##The analysis of the covariance and correlation matrices provides valuable insights into the relationships between various employee metrics. The heatmaps generated for these matrices demonstrate a clear and intuitive understanding of these relationships.

##The covariance matrix heatmap shows the degree to which pairs of variables vary together. For example, a high positive covariance between Salary and Age suggests that older employees tend to have higher salaries. This relationship shows that salary increases with age, possibly reflecting experience and seniority within an organization. Conversely, a negative covariance between Absences and PerfScoreID might indicate that employees with higher performance scores tend to have fewer absences. This negative relationship suggests that more engaged and higher-performing employees are less likely to be absent from work.

##The correlation matrix heatmap highlights the strength and direction of linear relationships between pairs of variables. For example, a strong positive correlation between EngagementSurvey and EmpSatisfaction suggests that employees who score higher on engagement surveys also tend to have higher job satisfaction. This positive relationship indicates that engagement and satisfaction are closely linked, with more engaged employees feeling more satisfied with their jobs. A strong negative correlation between Absences and PerfScoreID would indicate that higher performance scores are associated with fewer absences, reinforcing the idea that high-performing employees are more consistently present at work.

#Create a subset of the data in which you only include employees who are from the Production Department. Also, create a subset of everyone NOT in Production.
# Filter data for employees in the Production Department
production_subset <- hrdata_df %>%
  filter(Department == "Production")
# Filter data for employees not in the Production Department
non_production_subset <- hrdata_df %>%
    filter(Department != "Production")

# 4. Run correlations separately for each of these two subsamples. Report the results for each and provide an interpretation of what you see as you did in Questions 2 and 3. Are the correlations the same across the subsamples and/or from the entire sample? I’m not asking for a test of statistical significance, but your own  interpretation based on the values you compute.


#Calculate correlation matrices for each subsample
## Production Department Subset
cor_matrix_production <- cor(production_subset[, c("PerfScoreID", "Salary", "Age", "EngagementSurvey", "EmpSatisfaction", "Absences")])

##Non-Production Department Subset
cor_matrix_non_production <- cor(non_production_subset[, c("PerfScoreID", "Salary", "Age", "EngagementSurvey", "EmpSatisfaction", "Absences")])


##Print the production department correlation matrix
print("Correlation Matrix for Production:")
print(cor_matrix_production)

##Print the non-production department correlation matrix
Print("Correlation Matrix for non-Production:")
print(cor_matrix_non_production)

#Convert correlation matrices to long format for ggplot
cor_melt_production <- melt(cor_matrix_production)
cor_melt_non_production <- melt(cor_matrix_non_production)

#Heatmap for production department correlation matrix
ggplot(data = cor_melt_production, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "purple", high = "orange", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = "Correlation Matrix Heatmap for Production Department", x = "", y = "")

#Heatmap for non-production department correlation matrix
ggplot(data = cor_melt_non_production, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "purple", high = "orange", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = "Correlation Matrix Heatmap for Non-Production Department", x = "", y = "")

##Interpretation
# * Based on my interpretaion, higher performance scores are associated with higher salaries and less absences in both subsets. This suggests that employees who perform well tend to earn higher salaries and have fewer absences, regardless of their department.
# * Engagement and job satisfaction are positively correlated in both subsets, indicating that employees who are more engaged tend to be more satisfied with their jobs. 
# * In the non-production subset, there is a stronger negative correlation between age and performance scores compared to the production subset. This suggests that in non-production roles, older employees may have lower performance scores.
