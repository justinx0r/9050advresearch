library(tidyverse)
#import data set
library(readxl)
Project2_JW <- read_excel("/Users/justinwilliams/Code/9050advresearch/Project 2/Project2_JW.xlsx")
View(Project2_JW)

## 1. Report the descriptive statistics along with the frequency distribution and provide a detailed interpretation of how you would characterize the salary variable.
#summary statistics
summary(Project2_JW$Salary)

#mean
mean(Project2_JW$Salary)

#median
median(Project2_JW$Salary)

#mode
mode = function(){
    return(sort(-table(Project2_JW$Salary))[1])
  }
  mode()
  
#frequency table
table <- table(Project2_JW$Salary)
print(table)
barplot(table)

#plot
plot(Project2_JW$Salary, type="o", col="red")

## 2. Which employee (ID number) has the largest z-score on Salary and what is the z-score for this person? Which employee (ID number) has the smallest z-score on Salary and what is the z-score for this person?
#z-scores
#(data - mean)/std dev
library(dplyr)
z_score <- (Project2_JW$Salary-mean(Project2_JW$Salary))/sd(Project2_JW$Salary)
z_score
Project2_JW <- Project2_JW %>% mutate(SalaryZscore = (Project2_JW$Salary-mean(Project2_JW$Salary))/sd(Project2_JW$Salary))
Project2_JW$SalaryZscore

## 3.	Compute descriptive statistics for the standardized Salary variable. Report your results and produce a frequency distribution for the standardized Salary scores. Compare this distribution to the one you produced in Question 1. Are they the same or different? Explain using both your graphical results and words.
#summary statistics
summary(Project2_JW$SalaryZscore)

#mean
mean(Project2_JW$SalaryZscore)

#median
median(Project2_JW$SalaryZscore)

#mode
mode = function(){
  return(sort(-table(Project2_JW$SalaryZscore))[1])
}
mode()

#frequency table
table <- table(Project2_JW$SalaryZscore)
print((desc
barplot(table)

#plot
plot(Project2_JW$SalaryZscore, type="o", col="red")

#plot comparison between salary and salaryzscore
