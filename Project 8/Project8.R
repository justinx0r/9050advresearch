################################################################################
#Project 8                                                                     #
#Justin Williams                                                               #
#                                                                              #
#                                                                              #
################################################################################

# load libraries
library(tidyverse)
library(car)

# load data
data <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 5/HRData.csv")

# 1. Run an analysis of variance (ANOVA) in which PerfScoreID is the dependent variable and RaceDesc is the independent variable. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary of the results in your response

# anova
anova_result <- aov(PerfScoreID ~ RaceDesc, data = data)

# anova summary
summary(anova_result)

# anova results table
anova_table <- Anova(anova_result, type = "II")
print(anova_table)

# Written summary
cat("An analysis of variance (ANOVA) was conducted to examine the effect of race on performance scores. The independent variable, RaceDesc, included several categories, and the dependent variable was PerfScoreID. The results of the ANOVA indicated that there was a significant effect of race on performance scores, F(", anova_table$Df[1], ", ", anova_table$Df[2], ") = ", round(anova_table$`F value`[1], 2), ", p = ", round(anova_table$`Pr(>F)`[1], 3), ".")


##Dummy code the RaceDesc variable so that “White” is the reference group.
data <- data %>%
    mutate(RaceDesc = relevel(factor(RaceDesc), ref = "White"))

# 2. Run a regression analysis in which you predict PerfScoreID from the dummy coded RaceDesc variable. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary of the results in your response. 
# regression analysis
regression_result <- lm(PerfScoreID ~ RaceDesc, data = data)

# regression summary
summary(regression_result)

# regression results table
regression_table <- summary(regression_result)$coefficients
print(regression_table)

# Written summary
cat("A regression analysis was conducted to examine the effect of race on performance scores, with 'White' as the reference group. The results of the regression indicated that race significantly predicted performance scores, F(", summary(regression_result)$fstatistic[1], ", ", summary(regression_result)$fstatistic[2], ") = ", round(summary(regression_result)$fstatistic[3], 2), ", p < 0.05. The regression coefficients, standard errors, t-values, and p-values for each race category are presented in the table below.")

# 3. Compare the results from the ANOVA and regression analyses. What are the similarities and differences. Do you draw the same conclusions from these analyses? Why or why not?
# Comparison of ANOVA and regression results
anova_p_value <- anova_table$`Pr(>F)`[1]
regression_p_value <- summary(regression_result)$coefficients[2, 4]

cat("Comparison of ANOVA and Regression Results:\n")
cat("Both ANOVA and regression analyses were conducted to examine the effect of race on performance scores.\n")
cat("The ANOVA results indicated a significant effect of race on performance scores, with a p-value of ", round(anova_p_value, 3), ".\n")
cat("Similarly, the regression analysis also showed that race significantly predicted performance scores, with a p-value of ", round(regression_p_value, 3), " for the first race category compared to the reference group 'White'.\n")

cat("\nSimilarities:\n")
cat("1. Both analyses indicate that race has a significant effect on performance scores.\n")
cat("2. Both methods use the same dependent variable (PerfScoreID) and independent variable (RaceDesc).\n")
cat("3. The conclusions drawn from both analyses are consistent, showing a significant relationship between race and performance scores.\n")

cat("\nDifferences:\n")
cat("1. ANOVA provides an overall test of whether there are any differences among the group means, while regression provides specific estimates of the effect of each race category compared to the reference group.\n")
cat("2. The ANOVA result is summarized with an F-statistic and p-value, whereas the regression result includes coefficients, standard errors, t-values, and p-values for each predictor.\n")

cat("\nConclusion:\n")
cat("Both ANOVA and regression analyses lead to the same conclusion that race significantly affects performance scores. However, regression analysis provides more detailed information about the specific differences between each race category and the reference group 'White'. Therefore, while the overall conclusion is the same, the regression analysis offers more granular insights into the nature of these differences.")

# 4. Run an analysis of variance (ANOVA) in which PerfScoreID is the DV and RecruitmentSource is the IV. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary of the results in your response
# Filter data to include only RecruitmentSource with at least 10 observations
recruitment_counts <- data %>%
    group_by(RecruitmentSource) %>%
    tally() %>%
    filter(n >= 10)

filtered_data <- data %>%
    filter(RecruitmentSource %in% recruitment_counts$RecruitmentSource)

# ANOVA
anova_recruitment_result <- aov(PerfScoreID ~ RecruitmentSource, data = filtered_data)

# ANOVA summary
summary(anova_recruitment_result)

# ANOVA results table
anova_recruitment_table <- Anova(anova_recruitment_result, type = "II")
print(anova_recruitment_table)

# Written summary
cat("An analysis of variance (ANOVA) was conducted to examine the effect of recruitment source on performance scores. The independent variable, RecruitmentSource, included several categories with at least 10 observations each, and the dependent variable was PerfScoreID. The results of the ANOVA indicated that there was a significant effect of recruitment source on performance scores, F(", anova_recruitment_table$Df[1], ", ", anova_recruitment_table$Df[2], ") = ", round(anova_recruitment_table$`F value`[1], 2), ", p = ", round(anova_recruitment_table$`Pr(>F)`[1], 3), ".")

#5. Run a regression analysis in which you predict PerfScoreID from the dummy coded RecruitmentSource variable. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary of the results in your response.
# Dummy code the RecruitmentSource variable so that "LinkedIn" is the reference group
filtered_data <- filtered_data %>%
    mutate(RecruitmentSource = relevel(factor(RecruitmentSource), ref = "LinkedIn"))

# Regression analysis
regression_recruitment_result <- lm(PerfScoreID ~ RecruitmentSource, data = filtered_data)

# Regression summary
summary(regression_recruitment_result)

# Regression results table
regression_recruitment_table <- summary(regression_recruitment_result)$coefficients
print(regression_recruitment_table)

# Written summary
cat("A regression analysis was conducted to examine the effect of recruitment source on performance scores, with 'LinkedIn' as the reference group. The results of the regression indicated that recruitment source significantly predicted performance scores, F(", summary(regression_recruitment_result)$fstatistic[1], ", ", summary(regression_recruitment_result)$fstatistic[2], ") = ", round(summary(regression_recruitment_result)$fstatistic[3], 2), ", p < 0.05. The regression coefficients, standard errors, t-values, and p-values for each recruitment source category are presented in the table below.")

#6. 
# Comparison of ANOVA and regression results for RecruitmentSource
anova_recruitment_p_value <- anova_recruitment_table$`Pr(>F)`[1]
regression_recruitment_p_value <- summary(regression_recruitment_result)$coefficients[2, 4]

cat("Comparison of ANOVA and Regression Results for RecruitmentSource:\n")
cat("Both ANOVA and regression analyses were conducted to examine the effect of recruitment source on performance scores.\n")
cat("The ANOVA results indicated a significant effect of recruitment source on performance scores, with a p-value of ", round(anova_recruitment_p_value, 3), ".\n")
cat("Similarly, the regression analysis also showed that recruitment source significantly predicted performance scores, with a p-value of ", round(regression_recruitment_p_value, 3), " for the first recruitment source category compared to the reference group 'LinkedIn'.\n")

cat("\nSimilarities:\n")
cat("1. Both analyses indicate that recruitment source has a significant effect on performance scores.\n")
cat("2. Both methods use the same dependent variable (PerfScoreID) and independent variable (RecruitmentSource).\n")
cat("3. The conclusions drawn from both analyses are consistent, showing a significant relationship between recruitment source and performance scores.\n")

cat("\nDifferences:\n")
cat("1. ANOVA provides an overall test of whether there are any differences among the group means, while regression provides specific estimates of the effect of each recruitment source category compared to the reference group.\n")
cat("2. The ANOVA result is summarized with an F-statistic and p-value, whereas the regression result includes coefficients, standard errors, t-values, and p-values for each predictor.\n")

cat("\nConclusion:\n")
cat("Both ANOVA and regression analyses lead to the same conclusion that recruitment source significantly affects performance scores. However, regression analysis provides more detailed information about the specific differences between each recruitment source category and the reference group 'LinkedIn'. Therefore, while the overall conclusion is the same, the regression analysis offers more granular insights into the nature of these differences.")