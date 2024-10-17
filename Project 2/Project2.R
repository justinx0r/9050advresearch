library(tidyverse)
library(dplyr)

# Load the data from the CSV file into a data frame
HRData_New <- read_csv("/Users/justinwilliams/Code/9050advresearch/Project 2/HRData_New.csv")

## 1. Report the descriptive statistics along with the frequency distribution and provide a detailed interpretation of how you would characterize the salary variable.
# Summary statistics
summary(HRData_New$Salary)

# Mean
mean_salary <- mean(HRData_New$Salary, na.rm = TRUE)
print(mean_salary)

# Median
median_salary <- median(HRData_New$Salary, na.rm = TRUE)
print(median_salary)

# Mode
mode_salary <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_salary_value <- mode_salary(HRData_New$Salary)
print(mode_salary_value)

# Frequency table
salary_table <- table(HRData_New$Salary)
print(salary_table)
ggplot(HRData_New, aes(x = Salary)) +
  geom_bar(fill = "blue") +
  labs(title = "Salary Distribution", x = "Salary", y = "Frequency") +
  theme_minimal()

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
summary(HRData_New$Salary)

## 2. Which employee (ID number) has the largest z-score on Salary and what is the z-score for this person? Which employee (ID number) has the smallest z-score on Salary and what is the z-score for this person?
# Calculate z-scores for Salary
HRData_New <- HRData_New %>% mutate(SalaryZscore = (Salary - mean(Salary, na.rm = TRUE)) / sd(Salary, na.rm = TRUE))

# Find the employee with the largest z-score
max_z_score <- max(HRData_New$SalaryZscore, na.rm = TRUE)
max_z_score_employee <- HRData_New %>% filter(SalaryZscore == max_z_score) %>% select(EmpID, SalaryZscore)

# Find the employee with the smallest z-score
min_z_score <- min(HRData_New$SalaryZscore, na.rm = TRUE)
min_z_score_employee <- HRData_New %>% filter(SalaryZscore == min_z_score) %>% select(EmpID, SalaryZscore)

# Print results
cat("Employee with the largest z-score on Salary:\n")
print(max_z_score_employee)
cat("Employee with the smallest z-score on Salary:\n")
print(min_z_score_employee)

## 3. Compute descriptive statistics for the standardized Salary variable. Report your results and produce a frequency distribution for the standardized Salary scores. Compare this distribution to the one you produced in Question 1. Are they the same or different? Explain using both your graphical results and words.
# Summary statistics
summary(HRData_New$SalaryZscore)

# Mean
mean(HRData_New$SalaryZscore, na.rm = TRUE)

# Median
median(HRData_New$SalaryZscore, na.rm = TRUE)

# Mode
mode_salary_zscore <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_salary_zscore_value <- mode_salary_zscore(HRData_New$SalaryZscore)
mode_salary_zscore_value

# Frequency table
salary_zscore_table <- table(HRData_New$SalaryZscore)
print(salary_zscore_table)

# Plot
ggplot(HRData_New, aes(x = SalaryZscore)) +
  geom_bar(fill = "red") +
  labs(title = "Standardized Salary Distribution", x = "Standardized Salary", y = "Frequency") +
  theme_minimal()

# Plot comparison between salary and salaryzscore
ggplot() +
  geom_bar(data = HRData_New, aes(x = Salary), fill = "blue", alpha = 0.5) +
  geom_bar(data = HRData_New, aes(x = SalaryZscore), fill = "red", alpha = 0.5) +
  labs(title = "Comparison of Salary and Standardized Salary Distributions", x = "Value", y = "Frequency") +
  theme_minimal()
