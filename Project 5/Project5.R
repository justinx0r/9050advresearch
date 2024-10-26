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

###initial analysis

#summary statistics
summary(hrdata_df$PerfScoreID)
summary(hrdata_df$EmpSatisfaction)
mode_perfscoreid <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_perfscoreid_value <- mode_perfscoreid(hrdata_df$PerfScoreID)
print(mode_perfscoreid_value)
sd(hrdata_df$PerfScoreID)
sd(hrdata_df$EmpSatisfaction)

#mean centering - needed?

#ggplot scatterplot
ggplot(hrdata_df, aes(x=PerfScoreID, y=EmpSatisfaction)) + 
    geom_point(color="orange", size=8) + 
    labs(title="Performance Score vs Employee Satisfaction", x="Performance Score", y="Employee Satisfaction") + 
    theme(panel.background = element_rect(fill = "white"),
          axis.title.x = element_text(size=30),
          axis.title.y = element_text(size=30),
          axis.text.y = element_text(size=30),
          axis.text.x = element_text(size=30),
          plot.title = element_text(size=35))

#adding regression and line
geom_smooth(method = "lm", se = FALSE, fullrange=FALSE, level=0.95, color="purple")
ggplot(hrdata_df, aes(x=PerfScoreID, y=EmpSatisfaction)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, fullrange=FALSE, level=0.95) + 
    labs(title="Performance Score vs. Employee Satisfaction", x="Performance Score ID", y="Employee Satisfaction") + 
    theme(panel.background = element_rect(fill = "white"),
          axis.title.x = element_text(size=30),
          axis.title.y = element_text(size=30),
          axis.text.y = element_text(size=30),
          axis.text.x = element_text(size=30),
          plot.title = element_text(size=35))

#fit the linear regression model
model <- lm(PerfScoreID ~ EmpSatisfaction, data = hrdata_df)

#model summary
model_summary <- summary(model)

#model results data frame
results_df <- data.frame(
Variance = model_summary$sigma^2,
Beta = coef(model_summary)[, "Estimate"],
Std_Error = coef(model_summary)[, "Std. Error"],
R = sqrt(model_summary$r.squared),
R_Squared = model_summary$r.squared,
P_value = coef(model_summary)[, "Pr(>|t|)"],
F = model_summary$fstatistic[1],
Degrees_of_Freedom = model_summary$fstatistic[2],
Degrees_of_Freedom_Residual = model_summary$fstatistic[3]
)

print(results_df)

#print results in a table
print("Results Data Frame:")
kable(results_df, format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    row_spec(0, bold = TRUE, color = "white", background = "orange") %>%
    column_spec(1, bold = TRUE, color = "black")

#model fit measures
#moel comparisons
#model specific results
