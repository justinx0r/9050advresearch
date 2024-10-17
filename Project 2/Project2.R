library(tidyverse)
library(dplyr)

# Load the data from the CSV file into a data frame
HRData_New <- read_csv("/Users/justinwilliams/Code/9050advresearch/Project 2/HRData_New.csv")
hrdata_df <- data.frame(HRData_New)

## 1. Report the descriptive statistics along with the frequency distribution and provide a detailed interpretation of how you would characterize the salary variable.
# Summary statistics
summary(hrdata_df$Salary)

# Mean
mean_salary <- mean(hrdata_df$Salary, na.rm = TRUE)
print(mean_salary)

# Median
median_salary <- median(hrdata_df$Salary, na.rm = TRUE)
print(median_salary)

# Mode
mode_salary <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_salary_value <- mode_salary(hrdata_df$Salary)
print(mode_salary_value)

# Frequency table
salary_table <- table(hrdata_df$Salary)
print(salary_table)
ggplot(hrdata_df, aes(x = Salary)) +
  geom_bar(fill = "blue") +
  labs(title = "Salary Distribution", x = "Salary", y = "Frequency") +
  theme_minimal()

# Interpretation
print(paste("The mean salary is", mean_salary)
print(paste("The median salary is", median_salary)
print(paste("The mode salary is", mode_salary_value)
print(paste("The frequency distribution of salaries is shown in the bar plot above.")

# Detailed Interpretation
## The mean salary is $69,020.68, which indicates the average salary of the employees. The range is large as well as a large variation in salaries as the max salary is $250,00, which is a high end outlier. The mean salary is higher than the median salary, neaning that the distribution is right-skewed suggesting that most employees earn less than the mean.
## The median salary is $62,810, which is the middle value when the salaries are sorted in ascending order. The median gives us a more accurate view of what the typical employee is earning, due to the right-skewness of the distribution  which impacts the mean. This could be due to company factors such as the majority of positions in the comapny being more junior.
## The mode salary is $57,815, which is the most frequently occurring salary in the dataset. The mode being less than both the mean and median again shows the right-skewness of the distribution.
## The frequency distribution bar plot shows the distribution of salaries across different ranges. This helps in visualizing how salaries are spread out among the employees.\n")
## Overall, the salary distribution is spread across a wide range of values with the majority of employees falling below the mean. A small number of employees earn significantly higher salaries than the rest, as indicated by the maximum salary of $250,000. The median may be a better measure of central tendency in this case due to the presence of large outliers.
cat("The salary variable shows the following characteristics:\n")
cat("1. The mean salary is", mean_salary, "which indicates the average salary of the employees. The range is large as well as a large variation in salaries. The max salary is $250,00 which is a high end outlier. The mean salary is higher than the median salary, neaning that the distribution is right-skewed suggesting that most employees earn less than the mean.\n")
cat("2. The median salary is", median_salary, "which is the middle value when the salaries are sorted in ascending order. This suggests that half of the employees earn less than", median_salary, "and half earn more. \n")
cat("3. The mode salary is", mode_salary_value, "which is the most frequently occurring salary in the dataset. The mode being less than both the mean and median shows the right-skewness of the distribution. \n")
cat("4. The frequency distribution bar plot shows the distribution of salaries across different ranges. This helps in visualizing how salaries are spread out among the employees.\n")
cat("Overall, the salary variable can be characterized by its central tendency measures (mean, median, mode) and its distribution as shown in the frequency table and bar plot. \n")
summary(hrdata_df$Salary)

## 2. Which employee (ID number) has the largest z-score on Salary and what is the z-score for this person? Which employee (ID number) has the smallest z-score on Salary and what is the z-score for this person?
# Calculate z-scores for Salary
salary_zscore <- scale(hrdata_df$Salary, center = TRUE, scale = TRUE)
print(salary_zscore)

# Find the index of the employee with the largest z-score
max_z_score_index <- which.max(salary_zscore)

# Retrieve the employee's EmpID and the corresponding z-score
max_z_score_employee <- hrdata_df[max_z_score_index, c("EmpID", "Salary")]

# Print the result
print(max_z_score_employee)

# Find the index of the employee with the smallest z-score
min_z_score_index <- which.min(salary_zscore)

# Retrieve the employee's EmpID and the corresponding salary for the minimum z-score
min_z_score_employee <- hrdata_df[min_z_score_index, c("EmpID", "Salary")]

# Print the result
print(min_z_score_employee)

## 3. Compute descriptive statistics for the standardized Salary variable. Report your results and produce a frequency distribution for the standardized Salary scores. Compare this distribution to the one you produced in Question 1. Are they the same or different? Explain using both your graphical results and words.
# Summary statistics
summary(hrdata_df$SalaryZscore)

# Mean
mean(hrdata_df$SalaryZscore, na.rm = TRUE)

# Median
median(hrdata_df$SalaryZscore, na.rm = TRUE)

# Mode
mode_salary_zscore <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_salary_zscore_value <- mode_salary_zscore(hrdata_df$SalaryZscore)
mode_salary_zscore_value

# Frequency table
salary_zscore_table <- table(hrdata_df$SalaryZscore)
print(salary_zscore_table)

# Plot
ggplot(hrdata_df, aes(x = SalaryZscore)) +
  geom_bar(fill = "red") +
  labs(title = "Standardized Salary Distribution", x = "Standardized Salary", y = "Frequency") +
  theme_minimal()

# Plot comparison between salary and salaryzscore
ggplot() +
  geom_bar(data = hrdata_df, aes(x = Salary), fill = "blue", alpha = 0.5) +
  geom_bar(data = hrdata_df, aes(x = SalaryZscore), fill = "red", alpha = 0.5) +
  labs(title = "Comparison of Salary and Standardized Salary Distributions", x = "Value", y = "Frequency") +
  theme_minimal()

