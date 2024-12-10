################################################################################
#Project 8                                                                   #
#Justin Williams                                                               #
#                                                                              #
#                                                                              #
################################################################################

# load libraries
library(tidyverse)
library(car)

# load data
data <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 8/data.csv")

# 1. Run an analysis of variance (ANOVA) in which PerfScoreID is the DV and RaceDesc is the IV. Provide a summary of this analysis like what you would find in a journal article. Be sure to provide a table of results AND a written summary  of the results in your response

# anova
anova_result <- aov(PerfScoreID ~ RaceDesc, data = data)

# anova summary
summary(anova_result)
print(anova_result)

# anova results table
anova_table <- Anova(anova_result, type = "II")
print(anova_table)

# Written summary
cat("An analysis of variance (ANOVA) was conducted to examine the effect of race on performance scores. The independent variable, RaceDesc, included several categories, and the dependent variable was PerfScoreID. The results of the ANOVA indicated that there was a significant effect of race on performance scores, F(", anova_table$Df[1], ", ", anova_table$Df[2], ") = ", round(anova_table$`F value`[1], 2), ", p = ", round(anova_table$`Pr(>F)`[1], 3), ".")