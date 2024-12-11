################################################################################
#Project 9                                                                     #
#Multiple Categorical Predictors                                               #
#Justin Williams                                                               #
#                                                                              #
#                                                                              #
################################################################################

## load libraries
library(tidyverse)
library(car)

# load data
data <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 9/hrdata.csv")

# 1. Run a 2x2 analysis of variance (ANOVA) in which PerfScoreID is the DV and RaceDesc and Sex are the IVs. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary of the results in your response.

# Convert RaceDesc and Sex to factors
data$RaceDesc <- as.factor(data$RaceDesc)
data$Sex <- as.factor(data$Sex)

# Run the 2x2 ANOVA
anova_result <- aov(PerfScoreID ~ RaceDesc * Sex, data = data)

# Summary of the ANOVA
summary(anova_result)

# Create a table of results
anova_table <- summary(anova_result)
anova_table

# Written summary
cat("A two-way ANOVA was conducted to examine the effect of RaceDesc and Sex on PerfScoreID. 
The interaction effect between RaceDesc and Sex was ", ifelse(anova_table[[1]]["RaceDesc:Sex", "Pr(>F)"] < 0.05, "significant", "not significant"), " (F(", anova_table[[1]]["RaceDesc:Sex", "Df"], ", ", anova_table[[1]]["Residuals", "Df"], ") = ", round(anova_table[[1]]["RaceDesc:Sex", "F value"], 2), ", p = ", round(anova_table[[1]]["RaceDesc:Sex", "Pr(>F)"], 3), "). 
There was a ", ifelse(anova_table[[1]]["RaceDesc", "Pr(>F)"] < 0.05, "significant", "not significant"), " main effect of RaceDesc (F(", anova_table[[1]]["RaceDesc", "Df"], ", ", anova_table[[1]]["Residuals", "Df"], ") = ", round(anova_table[[1]]["RaceDesc", "F value"], 2), ", p = ", round(anova_table[[1]]["RaceDesc", "Pr(>F)"], 3), "). 
There was a ", ifelse(anova_table[[1]]["Sex", "Pr(>F)"] < 0.05, "significant", "not significant"), " main effect of Sex (F(", anova_table[[1]]["Sex", "Df"], ", ", anova_table[[1]]["Residuals", "Df"], ") = ", round(anova_table[[1]]["Sex", "F value"], 2), ", p = ", round(anova_table[[1]]["Sex", "Pr(>F)"], 3), ").")

## Dummy code the RaceDesc variable so that “White” is the reference group.

# 2. Run a regression analysis in which you test the RaceDesc x Sex interaction in predicting PerfScoreID. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary of the results in your response. 
# Dummy code the RaceDesc variable so that “White” is the reference group
data$RaceDesc <- relevel(data$RaceDesc, ref = "White")

# Run the regression analysis
regression_model <- lm(PerfScoreID ~ RaceDesc * Sex, data = data)

# Summary of the regression analysis
summary(regression_model)

# Create a table of results
regression_table <- summary(regression_model)$coefficients
regression_table

# Written summary
cat("A multiple regression analysis was conducted to examine the interaction effect of RaceDesc and Sex on PerfScoreID. 
The interaction effect between RaceDesc and Sex was ", ifelse(regression_table["RaceDescBlack:SexMale", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " (β = ", round(regression_table["RaceDescBlack:SexMale", "Estimate"], 2), ", t(", regression_model$df.residual, ") = ", round(regression_table["RaceDescBlack:SexMale", "t value"], 2), ", p = ", round(regression_table["RaceDescBlack:SexMale", "Pr(>|t|)"], 3), "). 
There was a ", ifelse(regression_table["RaceDescBlack", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " main effect of RaceDesc (β = ", round(regression_table["RaceDescBlack", "Estimate"], 2), ", t(", regression_model$df.residual, ") = ", round(regression_table["RaceDescBlack", "t value"], 2), ", p = ", round(regression_table["RaceDescBlack", "Pr(>|t|)"], 3), "). 
There was a ", ifelse(regression_table["SexMale", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " main effect of Sex (β = ", round(regression_table["SexMale", "Estimate"], 2), ", t(", regression_model$df.residual, ") = ", round(regression_table["SexMale", "t value"], 2), ", p = ", round(regression_table["SexMale", "Pr(>|t|)"], 3), ").")

# 3. Compare the results from the ANOVA and regression analyses. What are the similarities and differences. Do you draw the same conclusions from these analyses? Why or why not? 

# Compare the results from the ANOVA and regression analyses
anova_table
regression_table

# Written comparison
cat("Comparison of ANOVA and regression analyses:
Both the ANOVA and regression analyses were conducted to examine the interaction effect of RaceDesc and Sex on PerfScoreID. 
In both analyses, the interaction effect between RaceDesc and Sex was ", ifelse(anova_table[[1]]["RaceDesc:Sex", "Pr(>F)"] < 0.05, "significant", "not significant"), " in the ANOVA (F(", anova_table[[1]]["RaceDesc:Sex", "Df"], ", ", anova_table[[1]]["Residuals", "Df"], ") = ", round(anova_table[[1]]["RaceDesc:Sex", "F value"], 2), ", p = ", round(anova_table[[1]]["RaceDesc:Sex", "Pr(>F)"], 3), ") and ", ifelse(regression_table["RaceDescBlack:SexMale", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " in the regression analysis (β = ", round(regression_table["RaceDescBlack:SexMale", "Estimate"], 2), ", t(", regression_model$df.residual, ") = ", round(regression_table["RaceDescBlack:SexMale", "t value"], 2), ", p = ", round(regression_table["RaceDescBlack:SexMale", "Pr(>|t|)"], 3), ").

The main effect of RaceDesc was ", ifelse(anova_table[[1]]["RaceDesc", "Pr(>F)"] < 0.05, "significant", "not significant"), " in the ANOVA (F(", anova_table[[1]]["RaceDesc", "Df"], ", ", anova_table[[1]]["Residuals", "Df"], ") = ", round(anova_table[[1]]["RaceDesc", "F value"], 2), ", p = ", round(anova_table[[1]]["RaceDesc", "Pr(>F)"], 3), ") and ", ifelse(regression_table["RaceDescBlack", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " in the regression analysis (β = ", round(regression_table["RaceDescBlack", "Estimate"], 2), ", t(", regression_model$df.residual, ") = ", round(regression_table["RaceDescBlack", "t value"], 2), ", p = ", round(regression_table["RaceDescBlack", "Pr(>|t|)"], 3), ").

The main effect of Sex was ", ifelse(anova_table[[1]]["Sex", "Pr(>F)"] < 0.05, "significant", "not significant"), " in the ANOVA (F(", anova_table[[1]]["Sex", "Df"], ", ", anova_table[[1]]["Residuals", "Df"], ") = ", round(anova_table[[1]]["Sex", "F value"], 2), ", p = ", round(anova_table[[1]]["Sex", "Pr(>F)"], 3), ") and ", ifelse(regression_table["SexMale", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " in the regression analysis (β = ", round(regression_table["SexMale", "Estimate"], 2), ", t(", regression_model$df.residual, ") = ", round(regression_table["SexMale", "t value"], 2), ", p = ", round(regression_table["SexMale", "Pr(>|t|)"], 3), ").

Overall, the conclusions drawn from both analyses are ", ifelse(anova_table[[1]]["RaceDesc:Sex", "Pr(>F)"] < 0.05 && regression_table["RaceDescBlack:SexMale", "Pr(>|t|)"] < 0.05, "consistent", "not consistent"), ".")

## For the RecruitmentSource variable in subsequent analyses, you should only use cases for sources that have at least 10 observations.

#4. Run a 2x2  analysis of variance (ANOVA) in which PerfScoreID is the dependent and RecruitmentSource and Sex are the independent variables. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary of the results in your response.

# Filter RecruitmentSource to include only sources with at least 10 observations
recruitment_counts <- data %>% group_by(RecruitmentSource) %>% tally()
valid_sources <- recruitment_counts %>% filter(n >= 10) %>% pull(RecruitmentSource)
filtered_data <- data %>% filter(RecruitmentSource %in% valid_sources)

# Convert RecruitmentSource to factor
filtered_data$RecruitmentSource <- as.factor(filtered_data$RecruitmentSource)

# Run the 2x2 ANOVA
anova_result_recruitment <- aov(PerfScoreID ~ RecruitmentSource * Sex, data = filtered_data)

# Summary of the ANOVA
summary(anova_result_recruitment)

# Create a table of results
anova_table_recruitment <- summary(anova_result_recruitment)
anova_table_recruitment

# Written summary
cat("A two-way ANOVA was conducted to examine the effect of RecruitmentSource and Sex on PerfScoreID. 
The interaction effect between RecruitmentSource and Sex was ", ifelse(anova_table_recruitment[[1]]["RecruitmentSource:Sex", "Pr(>F)"] < 0.05, "significant", "not significant"), " (F(", anova_table_recruitment[[1]]["RecruitmentSource:Sex", "Df"], ", ", anova_table_recruitment[[1]]["Residuals", "Df"], ") = ", round(anova_table_recruitment[[1]]["RecruitmentSource:Sex", "F value"], 2), ", p = ", round(anova_table_recruitment[[1]]["RecruitmentSource:Sex", "Pr(>F)"], 3), "). 
There was a ", ifelse(anova_table_recruitment[[1]]["RecruitmentSource", "Pr(>F)"] < 0.05, "significant", "not significant"), " main effect of RecruitmentSource (F(", anova_table_recruitment[[1]]["RecruitmentSource", "Df"], ", ", anova_table_recruitment[[1]]["Residuals", "Df"], ") = ", round(anova_table_recruitment[[1]]["RecruitmentSource", "F value"], 2), ", p = ", round(anova_table_recruitment[[1]]["RecruitmentSource", "Pr(>F)"], 3), "). 
There was a ", ifelse(anova_table_recruitment[[1]]["Sex", "Pr(>F)"] < 0.05, "significant", "not significant"), " main effect of Sex (F(", anova_table_recruitment[[1]]["Sex", "Df"], ", ", anova_table_recruitment[[1]]["Residuals", "Df"], ") = ", round(anova_table_recruitment[[1]]["Sex", "F value"], 2), ", p = ", round(anova_table_recruitment[[1]]["Sex", "Pr(>F)"], 3), ").")

## Dummy code the RecruitmentSource variable so that “LinkedIn” is the reference group

# 5. Run a regression analysis in which you test RecruitmentSource x Sex  interaction in predicting PerfScoreID. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary of the results in your response.

# Dummy code the RecruitmentSource variable so that “LinkedIn” is the reference group
filtered_data$RecruitmentSource <- relevel(filtered_data$RecruitmentSource, ref = "LinkedIn")

# Run the regression analysis
regression_model_recruitment <- lm(PerfScoreID ~ RecruitmentSource * Sex, data = filtered_data)

# Summary of the regression analysis
summary(regression_model_recruitment)

# Create a table of results
regression_table_recruitment <- summary(regression_model_recruitment)$coefficients
regression_table_recruitment

# Written summary
cat("A multiple regression analysis was conducted to examine the interaction effect of RecruitmentSource and Sex on PerfScoreID. 
The interaction effect between RecruitmentSource and Sex was ", ifelse(regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " (β = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "Estimate"], 2), ", t(", regression_model_recruitment$df.residual, ") = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "t value"], 2), ", p = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "Pr(>|t|)"], 3), "). 
There was a ", ifelse(regression_table_recruitment["RecruitmentSourceCareerBuilder", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " main effect of RecruitmentSource (β = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder", "Estimate"], 2), ", t(", regression_model_recruitment$df.residual, ") = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder", "t value"], 2), ", p = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder", "Pr(>|t|)"], 3), "). 
There was a ", ifelse(regression_table_recruitment["SexMale", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " main effect of Sex (β = ", round(regression_table_recruitment["SexMale", "Estimate"], 2), ", t(", regression_model_recruitment$df.residual, ") = ", round(regression_table_recruitment["SexMale", "t value"], 2), ", p = ", round(regression_table_recruitment["SexMale", "Pr(>|t|)"], 3), ").")

# 6. Compare the results from the ANOVA and regression analyses. What are the similarities and differences. Do you draw the same conclusions from these analyses? Why or why not? 

# Compare the results from the ANOVA and regression analyses
anova_table_recruitment
regression_table_recruitment

# Written comparison
cat("Comparison of ANOVA and regression analyses:
Both the ANOVA and regression analyses were conducted to examine the interaction effect of RecruitmentSource and Sex on PerfScoreID. 
In both analyses, the interaction effect between RecruitmentSource and Sex was ", ifelse(anova_table_recruitment[[1]]["RecruitmentSource:Sex", "Pr(>F)"] < 0.05, "significant", "not significant"), " in the ANOVA (F(", anova_table_recruitment[[1]]["RecruitmentSource:Sex", "Df"], ", ", anova_table_recruitment[[1]]["Residuals", "Df"], ") = ", round(anova_table_recruitment[[1]]["RecruitmentSource:Sex", "F value"], 2), ", p = ", round(anova_table_recruitment[[1]]["RecruitmentSource:Sex", "Pr(>F)"], 3), ") and ", ifelse(regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " in the regression analysis (β = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "Estimate"], 2), ", t(", regression_model_recruitment$df.residual, ") = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "t value"], 2), ", p = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "Pr(>|t|)"], 3), ").

The main effect of RecruitmentSource was ", ifelse(anova_table_recruitment[[1]]["RecruitmentSource", "Pr(>F)"] < 0.05, "significant", "not significant"), " in the ANOVA (F(", anova_table_recruitment[[1]]["RecruitmentSource", "Df"], ", ", anova_table_recruitment[[1]]["Residuals", "Df"], ") = ", round(anova_table_recruitment[[1]]["RecruitmentSource", "F value"], 2), ", p = ", round(anova_table_recruitment[[1]]["RecruitmentSource", "Pr(>F)"], 3), ") and ", ifelse(regression_table_recruitment["RecruitmentSourceCareerBuilder", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " in the regression analysis (β = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder", "Estimate"], 2), ", t(", regression_model_recruitment$df.residual, ") = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder", "t value"], 2), ", p = ", round(regression_table_recruitment["RecruitmentSourceCareerBuilder", "Pr(>|t|)"], 3), ").

The main effect of Sex was ", ifelse(anova_table_recruitment[[1]]["Sex", "Pr(>F)"] < 0.05, "significant", "not significant"), " in the ANOVA (F(", anova_table_recruitment[[1]]["Sex", "Df"], ", ", anova_table_recruitment[[1]]["Residuals", "Df"], ") = ", round(anova_table_recruitment[[1]]["Sex", "F value"], 2), ", p = ", round(anova_table_recruitment[[1]]["Sex", "Pr(>F)"], 3), ") and ", ifelse(regression_table_recruitment["SexMale", "Pr(>|t|)"] < 0.05, "significant", "not significant"), " in the regression analysis (β = ", round(regression_table_recruitment["SexMale", "Estimate"], 2), ", t(", regression_model_recruitment$df.residual, ") = ", round(regression_table_recruitment["SexMale", "t value"], 2), ", p = ", round(regression_table_recruitment["SexMale", "Pr(>|t|)"], 3), ").

Overall, the conclusions drawn from both analyses are ", ifelse(anova_table_recruitment[[1]]["RecruitmentSource:Sex", "Pr(>F)"] < 0.05 && regression_table_recruitment["RecruitmentSourceCareerBuilder:SexMale", "Pr(>|t|)"] < 0.05, "consistent", "not consistent"), ".")