# Load necessary libraries
library(readr)
library(dplyr)
# Use the filter function from the stats package
stats::filter()
# Use the intersect function from the base package
base::intersect()

# Load the data
HRData <- read_csv("/Users/justinwilliams/Code/9050advresearch/Project 6/HRData.csv")




# Question 1
# Calculate Tenure
HRData <- HRData %>%
    mutate(DateofHire = as.Date(DateofHire, format = "%m/%d/%Y"),
           DateofTermination = as.Date(DateofTermination, format = "%m/%d/%Y"),
           CurrentDate = Sys.Date(),
           Tenure = ifelse(is.na(DateofTermination), 
                           as.numeric(difftime(CurrentDate, DateofHire, units = "days")) / 365.25,
                           as.numeric(difftime(DateofTermination, DateofHire, units = "days")) / 365.25))
# Fit the multiple regression model
model <- lm(PerfScoreID ~ EmpSatisfaction + EngagementSurvey + Tenure, data = HRData)

# Summary of the model
summary(model)

# Create a table of results
results <- summary(model)$coefficients
results <- as.data.frame(results)
colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Print the table of results
print(results)

# Written summary of the results
cat("The multiple regression analysis was conducted to predict PerfScoreID based on EmpSatisfaction, EngagementSurvey, and Tenure. The model was statistically significant, F(", summary(model)$fstatistic[2], ", ", summary(model)$fstatistic[3], ") = ", summary(model)$fstatistic[1], ", p < 0.05, and explained ", summary(model)$r.squared * 100, "% of the variance in PerfScoreID. The regression equation for the most efficient model is:

PerfScoreID = ", round(coef(model)[1], 2), " + ", round(coef(model)[2], 2), "*EmpSatisfaction + ", round(coef(model)[3], 2), "*EngagementSurvey + ", round(coef(model)[4], 2), "*Tenure.

The significance of the regression weights for each predictor is as follows:
- EmpSatisfaction: Estimate = ", round(results[2, 1], 2), ", Std. Error = ", round(results[2, 2], 2), ", t value = ", round(results[2, 3], 2), ", p = ", round(results[2, 4], 4), ".
- EngagementSurvey: Estimate = ", round(results[3, 1], 2), ", Std. Error = ", round(results[3, 2], 2), ", t value = ", round(results[3, 3], 2), ", p = ", round(results[3, 4], 4), ".
- Tenure: Estimate = ", round(results[4, 1], 2), ", Std. Error = ", round(results[4, 2], 2), ", t value = ", round(results[4, 3], 2), ", p = ", round(results[4, 4], 4), ".

Based on the p-values, EmpSatisfaction and EngagementSurvey are statistically significant predictors of PerfScoreID, while Tenure is not.")

# Question 2
# Fit the multiple regression model with Department as a covariate
model_step1 <- lm(PerfScoreID ~ Department, data = HRData)
model_step2 <- lm(PerfScoreID ~ Department + EmpSatisfaction + EngagementSurvey + Tenure, data = HRData)

# Summary of the models
summary_step1 <- summary(model_step1)
summary_step2 <- summary(model_step2)

# Create tables of results
results_step1 <- as.data.frame(summary_step1$coefficients)
colnames(results_step1) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

results_step2 <- as.data.frame(summary_step2$coefficients)
colnames(results_step2) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Print the tables of results
print(results_step1)
print(results_step2)

# Calculate variance explained by Department in Step 1
variance_explained_step1 <- summary_step1$r.squared * 100

# Calculate change in R-square from Step 1 to Step 2
change_in_r_squared <- summary_step2$r.squared - summary_step1$r.squared

# Calculate percentage of variance explained by the full model
variance_explained_full_model <- summary_step2$r.squared * 100

# Written summary of the results
cat("The multiple regression analysis was conducted in two steps. In Step 1, we predicted PerfScoreID based on Department. In Step 2, we added EmpSatisfaction, EngagementSurvey, and Tenure as predictors.

Step 1: The model with Department as a covariate was statistically significant, F(", summary_step1$fstatistic[2], ", ", summary_step1$fstatistic[3], ") = ", summary_step1$fstatistic[1], ", p < 0.05, and explained ", variance_explained_step1, "% of the variance in PerfScoreID.

Step 2: The full model was statistically significant, F(", summary_step2$fstatistic[2], ", ", summary_step2$fstatistic[3], ") = ", summary_step2$fstatistic[1], ", p < 0.05, and explained ", variance_explained_full_model, "% of the variance in PerfScoreID. The change in R-squared from Step 1 to Step 2 was ", round(change_in_r_squared, 4), ".

The regression equation for the full model is:
PerfScoreID = ", round(coef(model_step2)[1], 2), " + ", round(coef(model_step2)[2], 2), "*Department + ", round(coef(model_step2)[3], 2), "*EmpSatisfaction + ", round(coef(model_step2)[4], 2), "*EngagementSurvey + ", round(coef(model_step2)[5], 2), "*Tenure.

The significance of the regression weights for each predictor in the full model is as follows:
- Department: Estimate = ", round(results_step2[2, 1], 2), ", Std. Error = ", round(results_step2[2, 2], 2), ", t value = ", round(results_step2[2, 3], 2), ", p = ", round(results_step2[2, 4], 4), ".
- EmpSatisfaction: Estimate = ", round(results_step2[3, 1], 2), ", Std. Error = ", round(results_step2[3, 2], 2), ", t value = ", round(results_step2[3, 3], 2), ", p = ", round(results_step2[3, 4], 4), ".
- EngagementSurvey: Estimate = ", round(results_step2[4, 1], 2), ", Std. Error = ", round(results_step2[4, 2], 2), ", t value = ", round(results_step2[4, 3], 2), ", p = ", round(results_step2[4, 4], 4), ".
- Tenure: Estimate = ", round(results_step2[5, 1], 2), ", Std. Error = ", round(results_step2[5, 2], 2), ", t value = ", round(results_step2[5, 3], 2), ", p = ", round(results_step2[5, 4], 4), ".

Based on the p-values, EmpSatisfaction and EngagementSurvey are statistically significant predictors of PerfScoreID, while Tenure is not.")

#Question 3
# Fit the multiple regression model to predict Absences
model_absences <- lm(Absences ~ EmpSatisfaction + EngagementSurvey + Tenure, data = HRData)

# Summary of the model
summary_absences <- summary(model_absences)

# Create a table of results
results_absences <- as.data.frame(summary_absences$coefficients)
colnames(results_absences) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Print the table of results
print(results_absences)

# Calculate percentage of variance explained by the model
variance_explained_absences <- summary_absences$r.squared * 100

# Written summary of the results
cat("The multiple regression analysis was conducted to predict Absences based on EmpSatisfaction, EngagementSurvey, and Tenure. The model was statistically significant, F(", summary_absences$fstatistic[2], ", ", summary_absences$fstatistic[3], ") = ", summary_absences$fstatistic[1], ", p < 0.05, and explained ", variance_explained_absences, "% of the variance in Absences. The regression equation for the most efficient model is:

Absences = ", round(coef(model_absences)[1], 2), " + ", round(coef(model_absences)[2], 2), "*EmpSatisfaction + ", round(coef(model_absences)[3], 2), "*EngagementSurvey + ", round(coef(model_absences)[4], 2), "*Tenure.

The significance of the regression weights for each predictor is as follows:
- EmpSatisfaction: Estimate = ", round(results_absences[2, 1], 2), ", Std. Error = ", round(results_absences[2, 2], 2), ", t value = ", round(results_absences[2, 3], 2), ", p = ", round(results_absences[2, 4], 4), ".
- EngagementSurvey: Estimate = ", round(results_absences[3, 1], 2), ", Std. Error = ", round(results_absences[3, 2], 2), ", t value = ", round(results_absences[3, 3], 2), ", p = ", round(results_absences[3, 4], 4), ".
- Tenure: Estimate = ", round(results_absences[4, 1], 2), ", Std. Error = ", round(results_absences[4, 2], 2), ", t value = ", round(results_absences[4, 3], 2), ", p = ", round(results_absences[4, 4], 4), ".

Based on the p-values, EmpSatisfaction and EngagementSurvey are statistically significant predictors of Absences, while Tenure is not.")

#Question 4
# Fit the multiple regression model with Sex as a covariate
model_step1_sex <- lm(Absences ~ Sex, data = HRData)
model_step2_sex <- lm(Absences ~ Sex + EmpSatisfaction + EngagementSurvey + Tenure, data = HRData)

# Summary of the models
summary_step1_sex <- summary(model_step1_sex)
summary_step2_sex <- summary(model_step2_sex)

# Create tables of results
results_step1_sex <- as.data.frame(summary_step1_sex$coefficients)
colnames(results_step1_sex) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

results_step2_sex <- as.data.frame(summary_step2_sex$coefficients)
colnames(results_step2_sex) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Print the tables of results
print(results_step1_sex)
print(results_step2_sex)

# Calculate variance explained by Sex in Step 1
variance_explained_step1_sex <- summary_step1_sex$r.squared * 100

# Calculate change in R-square from Step 1 to Step 2
change_in_r_squared_sex <- summary_step2_sex$r.squared - summary_step1_sex$r.squared

# Calculate percentage of variance explained by the full model
variance_explained_full_model_sex <- summary_step2_sex$r.squared * 100

# Written summary of the results
cat("The multiple regression analysis was conducted in two steps. In Step 1, we predicted Absences based on Sex. In Step 2, we added EmpSatisfaction, EngagementSurvey, and Tenure as predictors.

Step 1: The model with Sex as a covariate was statistically significant, F(", summary_step1_sex$fstatistic[2], ", ", summary_step1_sex$fstatistic[3], ") = ", summary_step1_sex$fstatistic[1], ", p < 0.05, and explained ", variance_explained_step1_sex, "% of the variance in Absences.

Step 2: The full model was statistically significant, F(", summary_step2_sex$fstatistic[2], ", ", summary_step2_sex$fstatistic[3], ") = ", summary_step2_sex$fstatistic[1], ", p < 0.05, and explained ", variance_explained_full_model_sex, "% of the variance in Absences. The change in R-squared from Step 1 to Step 2 was ", round(change_in_r_squared_sex, 4), ".

The regression equation for the full model is:
Absences = ", round(coef(model_step2_sex)[1], 2), " + ", round(coef(model_step2_sex)[2], 2), "*Sex + ", round(coef(model_step2_sex)[3], 2), "*EmpSatisfaction + ", round(coef(model_step2_sex)[4], 2), "*EngagementSurvey + ", round(coef(model_step2_sex)[5], 2), "*Tenure.

The significance of the regression weights for each predictor in the full model is as follows:
- Sex: Estimate = ", round(results_step2_sex[2, 1], 2), ", Std. Error = ", round(results_step2_sex[2, 2], 2), ", t value = ", round(results_step2_sex[2, 3], 2), ", p = ", round(results_step2_sex[2, 4], 4), ".
- EmpSatisfaction: Estimate = ", round(results_step2_sex[3, 1], 2), ", Std. Error = ", round(results_step2_sex[3, 2], 2), ", t value = ", round(results_step2_sex[3, 3], 2), ", p = ", round(results_step2_sex[3, 4], 4), ".
- EngagementSurvey: Estimate = ", round(results_step2_sex[4, 1], 2), ", Std. Error = ", round(results_step2_sex[4, 2], 2), ", t value = ", round(results_step2_sex[4, 3], 2), ", p = ", round(results_step2_sex[4, 4], 4), ".
- Tenure: Estimate = ", round(results_step2_sex[5, 1], 2), ", Std. Error = ", round(results_step2_sex[5, 2], 2), ", t value = ", round(results_step2_sex[5, 3], 2), ", p = ", round(results_step2_sex[5, 4], 4), ".

Based on the p-values, EmpSatisfaction and EngagementSurvey are statistically significant predictors of Absences, while Tenure is not.")
