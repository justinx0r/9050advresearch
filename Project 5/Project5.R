################################################################################
#Project 5                                                                     #
#Justin Williams                                                               #
#                                                                              #
#                                                                              #
################################################################################

#import libraries and data
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(kableExtra)

#load dataframe
hrdata_df <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 5/HRData.csv")

# Question 1
# Simple regression analysis
simple_model <- lm(PerfScoreID ~ EmpSatisfaction, data = hrdata_df)

# Model summary
simple_model_summary <- summary(simple_model)

# Model results data frame
simple_results_df <- data.frame(
  Predictor = rownames(coef(simple_model_summary)),
  Estimate = coef(simple_model_summary)[, "Estimate"],
  Std_Error = coef(simple_model_summary)[, "Std. Error"],
  t_value = coef(simple_model_summary)[, "t value"],
  P_value = coef(simple_model_summary)[, "Pr(>|t|)"]
)

print(simple_results_df)

#print results in a table
print("Simple Regression Results Data Frame:")
kable(simple_results_df, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE, color = "white", background = "orange") %>%
  column_spec(1, bold = TRUE, color = "purple")

# Plot of the regression line
ggplot(hrdata_df, aes(x = EmpSatisfaction, y = PerfScoreID)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regression of PerfScoreID on EmpSatisfaction",
       x = "EmpSatisfaction",
       y = "PerfScoreID")

# Summary
cat("The simple regression analysis was conducted to predict PerfScoreID from EmpSatisfaction. The model summary is as follows:\n")
cat("The regression equation is: PerfScoreID = ", coef(simple_model_summary)[1, "Estimate"], " + ", coef(simple_model_summary)[2, "Estimate"], "*EmpSatisfaction\n")
cat("The model explains ", round(simple_model_summary$r.squared * 100, 2), "% of the variance in PerfScoreID.\n")
cat("Significance of regression weight:\n")
cat("EmpSatisfaction: Estimate = ", simple_results_df$Estimate[2], ", Std. Error = ", simple_results_df$Std_Error[2], ", t-value = ", simple_results_df$t_value[2], ", p-value = ", simple_results_df$P_value[2], "\n")

# Answering the questions
cat("\nAnswers to the questions:\n")
cat("a. Is the regression weight statistically significant? ", ifelse(simple_results_df$P_value[2] < 0.05, "Yes", "No"), "\n")
cat("b. What is the regression equation? PerfScoreID = ", coef(simple_model_summary)[1, "Estimate"], " + ", coef(simple_model_summary)[2, "Estimate"], "*EmpSatisfaction\n")
cat("c. Interpretation of the slope: For each one-unit increase in EmpSatisfaction, PerfScoreID is expected to increase by ", coef(simple_model_summary)[2, "Estimate"], " units.\n")
cat("d. Interpretation of the y-intercept: When EmpSatisfaction is zero, the predicted PerfScoreID is ", coef(simple_model_summary)[1, "Estimate"], ".\n")
cat("e. Predicted PerfScoreID for someone with an average level of EmpSatisfaction: ", predict(simple_model, newdata = data.frame(EmpSatisfaction = mean(hrdata_df$EmpSatisfaction))), "\n")
cat("f. Percentage of variance in PerfScoreID explained by EmpSatisfaction: ", round(simple_model_summary$r.squared * 100, 2), "%\n")

# Question 2
absences_model <- lm(Absences ~ EngagementSurvey, data = hrdata_df)

# Model summary
absences_model_summary <- summary(absences_model)

# Model results data frame
absences_results_df <- data.frame(
  Predictor = rownames(coef(absences_model_summary)),
  Estimate = coef(absences_model_summary)[, "Estimate"],
  Std_Error = coef(absences_model_summary)[, "Std. Error"],
  t_value = coef(absences_model_summary)[, "t value"],
  P_value = coef(absences_model_summary)[, "Pr(>|t|)"]
)

print(absences_results_df)

#print results in a table
print("Absences Regression Results Data Frame:")
kable(absences_results_df, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE, color = "white", background = "orange") %>%
  column_spec(1, bold = TRUE, color = "purple")

# Plot of the regression line
ggplot(hrdata_df, aes(x = EngagementSurvey, y = Absences)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regression of Absences on EngagementSurvey",
       x = "EngagementSurvey",
       y = "Absences")

# Summary
cat("The simple regression analysis was conducted to predict Absences from EngagementSurvey. The model summary is as follows:\n")
cat("The regression equation is: Absences = ", coef(absences_model_summary)[1, "Estimate"], " + ", coef(absences_model_summary)[2, "Estimate"], "*EngagementSurvey\n")
cat("The model explains ", round(absences_model_summary$r.squared * 100, 2), "% of the variance in Absences.\n")
cat("Significance of regression weight:\n")
cat("EngagementSurvey: Estimate = ", absences_results_df$Estimate[2], ", Std. Error = ", absences_results_df$Std_Error[2], ", t-value = ", absences_results_df$t_value[2], ", p-value = ", absences_results_df$P_value[2], "\n")

# Answering the questions
cat("\nAnswers to the questions:\n")
cat("a. Is the regression weight statistically significant? ", ifelse(absences_results_df$P_value[2] < 0.05, "Yes", "No"), "\n")
cat("b. What is the regression equation? Absences = ", coef(absences_model_summary)[1, "Estimate"], " + ", coef(absences_model_summary)[2, "Estimate"], "*EngagementSurvey\n")
cat("c. Interpretation of the slope: For each one-unit increase in EngagementSurvey, Absences is expected to change by ", coef(absences_model_summary)[2, "Estimate"], " units.\n")
cat("d. Interpretation of the y-intercept: When EngagementSurvey is zero, the predicted Absences is ", coef(absences_model_summary)[1, "Estimate"], ".\n")
cat("e. Predicted Absences for someone with an average level of EngagementSurvey: ", predict(absences_model, newdata = data.frame(EngagementSurvey = mean(hrdata_df$EngagementSurvey))), "\n")
cat("f. Percentage of variance in Absences explained by EngagementSurvey: ", round(absences_model_summary$r.squared * 100, 2), "%\n")

# Question 3

department_model <- lm(PerfScoreID ~ Department, data = hrdata_df)

# Model summary
department_model_summary <- summary(department_model)

# Model results data frame
department_results_df <- data.frame(
  Predictor = rownames(coef(department_model_summary)),
  Estimate = coef(department_model_summary)[, "Estimate"],
  Std_Error = coef(department_model_summary)[, "Std. Error"],
  t_value = coef(department_model_summary)[, "t value"],
  P_value = coef(department_model_summary)[, "Pr(>|t|)"]
)

print(department_results_df)

#print results in a table
print("Department Regression Results Data Frame:")
kable(department_results_df, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE, color = "white", background = "orange") %>%
  column_spec(1, bold = TRUE, color = "purple")

# Plot of the regression line
ggplot(hrdata_df, aes(x = Department, y = PerfScoreID)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regression of PerfScoreID on Department",
       x = "Department",
       y = "PerfScoreID")

# Summary
cat("The simple regression analysis was conducted to predict PerfScoreID from Department. The model summary is as follows:\n")
cat("The regression equation is: PerfScoreID = ", coef(department_model_summary)[1, "Estimate"], " + ", coef(department_model_summary)[2, "Estimate"], "*Department\n")
cat("The model explains ", round(department_model_summary$r.squared * 100, 2), "% of the variance in PerfScoreID.\n")
cat("Significance of regression weight:\n")
cat("Department: Estimate = ", department_results_df$Estimate[2], ", Std. Error = ", department_results_df$Std_Error[2], ", t-value = ", department_results_df$t_value[2], ", p-value = ", department_results_df$P_value[2], "\n")

# Answering the questions
cat("\nAnswers to the questions:\n")
cat("a. Is the regression weight statistically significant? ", ifelse(department_results_df$P_value[2] < 0.05, "Yes", "No"), "\n")
cat("b. What is the regression equation? PerfScoreID = ", coef(department_model_summary)[1, "Estimate"], " + ", coef(department_model_summary)[2, "Estimate"], "*Department\n")
cat("c. Interpretation of the slope: For each one-unit increase in Department, PerfScoreID is expected to change by ", coef(department_model_summary)[2, "Estimate"], " units.\n")
cat("d. Interpretation of the y-intercept: When Department is zero, the predicted PerfScoreID is ", coef(department_model_summary)[1, "Estimate"], ".\n")
cat("e. Predicted PerfScoreID for someone from each of the Departments: ", predict(department_model, newdata = data.frame(Department = unique(hrdata_df$Department))), "\n")
cat("f. Percentage of variance in PerfScoreID explained by Department: ", round(department_model_summary$r.squared * 100, 2), "%\n")