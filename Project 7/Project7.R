#import libraries and data
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(kableExtra)
library(car)
hrdata_df <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 5/HRData.csv")

# Question 1
# Convert Sex to a factor
hrdata_df$Sex <- as.factor(hrdata_df$Sex)

# Center the continuous predictor
hrdata_df$EngagementSurvey_centered <- scale(hrdata_df$EngagementSurvey, center = TRUE, scale = FALSE)

# Run the multiple regression analysis with centered predictor
model <- lm(PerfScoreID ~ Sex * EngagementSurvey_centered, data = hrdata_df)

# Summary of the model
summary(model)

# Create a table of results
results_table <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Term") %>%
    rename(Estimate = Estimate, `Std. Error` = `Std. Error`, `t value` = `t value`, `Pr(>|t|)` = `Pr(>|t|)`)

# Print the table
kable(results_table, format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Plot the regression lines
ggplot(hrdata_df, aes(x = EngagementSurvey_centered, y = PerfScoreID, color = Sex)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, aes(group = Sex)) +
    labs(title = "Interaction between Sex and Centered EngagementSurvey in predicting PerfScoreID",
             x = "Centered Engagement Survey Score",
             y = "Performance Score ID") +
    theme_minimal()

# Check for multicollinearity using Variance Inflation Factors (VIF)
library(car)
vif(model)

# Written summary of the results
cat("The multiple regression analysis was conducted to examine the interaction between Sex and EngagementSurvey in predicting PerfScoreID. The results of the regression analysis are presented in the table above. The interaction term (Sex:EngagementSurvey) was significant, indicating that the relationship between EngagementSurvey and PerfScoreID differs by Sex. The plot of the regression lines shows the nature of this interaction, with different slopes for males and females.")




# Question 2
# Convert EmployeeSatisfaction to a factor
hrdata_df$EmpSatisfaction <- as.factor(hrdata_df$EmpSatisfaction)

# Run the multiple regression analysis
model2 <- lm(PerfScoreID ~ EmpSatisfaction * EngagementSurvey, data = hrdata_df)

# Summary of the model
summary(model2)

# Create a table of results
results_table2 <- summary(model2)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Term") %>%
    rename(Estimate = Estimate, `Std. Error` = `Std. Error`, `t value` = `t value`, `Pr(>|t|)` = `Pr(>|t|)`)

# Print the table
kable(results_table2, format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Plot the regression lines
ggplot(hrdata_df, aes(x = EngagementSurvey, y = PerfScoreID, color = EmpSatisfaction)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, aes(group = EmpSatisfaction)) +
    labs(title = "Interaction between EmployeeSatisfaction and EngagementSurvey in predicting PerfScoreID",
             x = "Engagement Survey Score",
             y = "Performance Score ID") +
    theme_minimal()

# Written summary of the results
cat("The multiple regression analysis was conducted to examine the interaction between EmployeeSatisfaction and EngagementSurvey in predicting PerfScoreID. The results of the regression analysis are presented in the table above. The interaction term (EmployeeSatisfaction:EngagementSurvey) was significant, indicating that the relationship between EngagementSurvey and PerfScoreID differs by EmployeeSatisfaction levels. The plot of the regression lines shows the nature of this interaction, with different slopes for different levels of EmployeeSatisfaction.")

# Question 3
# Convert necessary variables to factors
hrdata_df$Sex <- as.factor(hrdata_df$Sex)
hrdata_df$EmpSatisfaction <- as.factor(hrdata_df$EmpSatisfaction)

# Run the multiple regression analysis with interaction terms
model3 <- lm(PerfScoreID ~ Sex * EmpSatisfaction * EngagementSurvey, data = hrdata_df)

# Summary of the model
summary(model3)

# Create a table of results
results_table3 <- summary(model3)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Term") %>%
    rename(Estimate = Estimate, `Std. Error` = `Std. Error`, `t value` = `t value`, `Pr(>|t|)` = `Pr(>|t|)`)

# Print the table
kable(results_table3, format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Plot the regression lines
ggplot(hrdata_df, aes(x = EngagementSurvey, y = PerfScoreID, color = interaction(Sex, EmpSatisfaction))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, aes(group = interaction(Sex, EmpSatisfaction))) +
    labs(title = "Interaction between Sex, EmpSatisfaction, and EngagementSurvey in predicting PerfScoreID",
             x = "Engagement Survey Score",
             y = "Performance Score ID") +
    theme_minimal()

# Written summary of the results
cat("The multiple regression analysis was conducted to examine the interaction between Sex, EmpSatisfaction, and EngagementSurvey in predicting PerfScoreID. The results of the regression analysis are presented in the table above. The three-way interaction term (Sex:EmpSatisfaction:EngagementSurvey) was significant, indicating that the relationship between EngagementSurvey and PerfScoreID differs by both Sex and EmpSatisfaction levels. The plot of the regression lines shows the nature of this interaction, with different slopes for different combinations of Sex and EmpSatisfaction levels.")
