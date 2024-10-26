#import libraries and data
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)

#load dataframe
hrdata_df <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 5/HRData.csv")

#initial analysis / plot
scatterplot <- plot(hrdata_df$PerfScoreID, hrdata_df$Empsatisfaction, main="Performance Score vs Employee Satisfaction", xlab="Performance Score", ylab="Employee Satisfaction", pch=19)
print (scatterplot)
