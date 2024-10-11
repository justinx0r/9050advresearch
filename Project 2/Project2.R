library(tidyverse)
library(dplyr)
library(readxl)
Project2_JW <- read_excel("/Users/justinwilliams/Code/9050advresearch/Project 2/Project2_JW.xlsx")
View(Project2_JW)

## 1. Report the descriptive statistics along with the frequency distribution and provide a detailed interpretation of how you would characterize the salary variable.
#summary statistics
summary(Project2_JW$Salary)

#mean
mean_salary <- mean(Project2_JW$Salary)
mean_salary

#median
median_salary <- median(Project2_JW$Salary)
median_salary

#mode
mode_salary <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_salary_value <- mode_salary(Project2_JW$Salary)
mode_salary_value

#frequency table
salary_table <- table(Project2_JW$Salary)
print(salary_table)
barplot(salary_table, main="Salary Distribution", xlab="Salary", ylab="Frequency", col="blue")

# Interpretation
cat("The mean salary is", mean_salary, "\n")
cat("The median salary is", median_salary, "\n")
cat("The mode salary is", mode_salary_value, "\n")
cat("The frequency distribution of salaries is shown in the bar plot above.\n")

# Detailed Interpretation
cat("The salary variable shows the following characteristics:\n")
cat("1. The mean salary is", mean_salary, "which indicates the average salary of the employees.\n")
cat("2. The median salary is", median_salary, "which is the middle value when the salaries are sorted in ascending order. This suggests that half of the employees earn less than", median_salary, "and half earn more.\n")
cat("3. The mode salary is", mode_salary_value, "which is the most frequently occurring salary in the dataset.\n")
cat("4. The frequency distribution bar plot shows the distribution of salaries across different ranges. This helps in visualizing how salaries are spread out among the employees.\n")
cat("Overall, the salary variable can be characterized by its central tendency measures (mean, median, mode) and its distribution as shown in the frequency table and bar plot.\n")
summary(Project2_JW$Salary)

#frequency table
salary_table <- table(Project2_JW$Salary)
print(salary_table)
barplot(salary_table, main="Salary Distribution", xlab="Salary", ylab="Frequency", col="blue")

# Interpretation
cat("The mean salary is", mean_salary, "\n")
cat("The median salary is", median_salary, "\n")
cat("The mode salary is", mode_salary_value, "\n")
cat("The frequency distribution of salaries is shown in the bar plot above.\n")

# Detailed Interpretation
cat("The salary variable shows the following characteristics:\n")
cat("1. The mean salary is", mean_salary, "which indicates the average salary of the employees.\n")
cat("2. The median salary is", median_salary, "which is the middle value when the salaries are sorted in ascending order. This suggests that half of the employees earn less than", median_salary, "and half earn more.\n")
cat("3. The mode salary is", mode_salary_value, "which is the most frequently occurring salary in the dataset.\n")
cat("4. The frequency distribution bar plot shows the distribution of salaries across different ranges. This helps in visualizing how salaries are spread out among the employees.\n")
cat("Overall, the salary variable can be characterized by its central tendency measures (mean, median, mode) and its distribution as shown in the frequency table and bar plot.\n")
summary(Project2_JW$Salary)
  
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
# Check for NA or Inf values
if (any(is.na(data)) || any(is.infinite(data))) {
  stop("Data contains NA or Inf values")
}

#plot
plot(Project2_JW$SalaryZscore, type="o", col="red")

#plot comparison between salary and salaryzscore
