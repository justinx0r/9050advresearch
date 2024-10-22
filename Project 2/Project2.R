library(tidyverse)
library(dplyr)

# Load the data from the CSV file into a data frame
HRData_New <- read_csv("/Users/justinwilliams/Code/9050advresearch/Project 2/HRData_New.csv")
hrdata_df <- data.frame(HRData_New)

# 1. Report the descriptive statistics along with the frequency distribution and provide a detailed interpretation of how you would characterize the salary variable.
# Summary statistics
summary(hrdata_df$Salary)

## Mean
mean_salary <- mean(hrdata_df$Salary, na.rm = TRUE)
print(mean_salary)

## Median
median_salary <- median(hrdata_df$Salary, na.rm = TRUE)
print(median_salary)

## Mode
mode_salary <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_salary_value <- mode_salary(hrdata_df$Salary)
print(mode_salary_value)

## Frequency table
salary_table <- table(hrdata_df$Salary)
print(salary_table)

## Plot
hrdata_df %>%
  ggplot(aes(x = Salary)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Salary Distribution", x = "Salary", y = "Frequency") +
  theme_minimal()


## Interpretation
print(paste("The mean salary is", mean_salary))
print(paste("The median salary is", median_salary))
print(paste("The mode salary is", mode_salary_value))
print(paste("The frequency distribution of salaries is shown in the bar plot above."))

## Detailed Interpretation
## The mean salary is $69,020.68, which indicates the average salary of the employees. The range is large as well as a large variation in salaries as the max salary is $250,00, which is a high end outlier. The mean salary is higher than the median salary, neaning that the distribution is right-skewed suggesting that most employees earn less than the mean.
## The median salary is $62,810, which is the middle value when the salaries are sorted in ascending order. The median gives us a more accurate view of what the typical employee is earning, due to the right-skewness of the distribution  which impacts the mean. This could be due to company factors such as the majority of positions in the comapny being more junior.
## The mode salary is $57,815, which is the most frequently occurring salary in the dataset. The mode being less than both the mean and median again shows the right-skewness of the distribution.
## The frequency distribution bar plot shows the distribution of salaries across different ranges. This helps in visualizing how salaries are spread out among the employees.\n")
## Overall, the salary distribution is spread across a wide range of values with the majority of employees falling below the mean. A small number of employees earn significantly higher salaries than the rest, as indicated by the maximum salary of $250,000 - the bulk of employees earn between the 1st quartile $55,502 and the 3rd quartile $72,036. The median may be a better measure of central tendency in this case due to the presence of large outliers.

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
# Calculate the z-scores for the Salary column
hrdata_df <- hrdata_df %>%
  mutate(salary_zscore = scale(Salary))

# Plot the salary z-score
hrdata_df %>%
  ggplot(aes(x = salary_zscore)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Standardized Salary (Z-Score) Distribution", x = "Z-Score", y = "Frequency") +
  theme_minimal()

# Plot of the salary z-score and salary
hrdata_df %>%
  pivot_longer(cols = c(Salary, salary_zscore), 
               names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Measure)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~ Measure, scales = "free_x") +   # Creates separate plots for Salary and Z-Score
  labs(title = "Comparison of Salary and Standardized Salary (Z-Score) Distributions",
       x = "Value", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("purple", "orange"))

### Intepretation
# * The shape of the standardized salary distribution will be the same as the original salary distribution.
# * The scales are different as the z-scores are standardized to have a mean of 0 and a standard deviation of 1.

#4. Compute descriptive statistics for the monthly salary variable you created at the beginning of this project. Report your results and produce a frequency distribution for these scores. Compare this distribution to the ones you produced in Questions 1 & 3. Are they the same or different? Explain using both your graphical results and words.

# Monthly salary summary statistics
summary(hrdata_df$Monthly_Salary)

# Mean
mean_monthlysalary <- mean(hrdata_df$Monthly_Salary, na.rm = TRUE)

# Median
median_Monthly_Salary <- median(hrdata_df$Monthly_Salary, na.rm = TRUE)

# Mode 
mode_monthlysalary <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mode_monthly_salary_value <- mode_monthlysalary(hrdata_df$Monthly_Salary)

print(paste("Mean monthly salary:", format(round(mean_monthlysalary, 2), nsmall = 2)))
print(paste("The median monthly salary is:", median_Monthly_Salary))
print(paste("The mode monthly salary is:", mode_monthly_salary_value))

# Interpreation
## * The monthly salary distribution retains the same overall shape as the original salary (which was reported annually in Question 1). However, the monthly salary is simply scaled down by a factor of 12, so all statistics (mean, median, quartiles) reflect this transformation.
## *The median monthly salary of $5,234 is just one-twelfth of the median annual salary of $62,810.
## *The overall distribution remains positively skewed, as was the case with the original salary distribution in Question 1.

# 5. Take the square root of the Salary variable. Compute descriptive statistics for this new variable. Report your results and produce a frequency distribution for the square root scores. Compare this distribution to the ones you produced in Questions 1, 3, & 4. Are they the same or different? Explain using both your graphical results and words.

# Square root of the Salary variable
sqrt_salary <- sqrt(hrdata_df$Salary)

# Summary statistics
summary(sqrt_salary)

# Mean
mean_sqrt_salary <- mean(sqrt_salary, na.rm = TRUE)

# Median
median_sqrt_salary <- median(sqrt_salary, na.rm = TRUE)

# Mode
mode_sqrt_salary <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_sqrt_salary_value <- mode_sqrt_salary(sqrt_salary)

print(paste("Mean square root of salary:", format(round(mean_sqrt_salary, 2), nsmall = 2)))
print(paste("Median square root of salary:", median_sqrt_salary))
print(paste("Mode square root of salary:", mode_sqrt_salary_value))

# Interpreation
## * Taking the square root of the salary reduces the range of salaries, compressing large salaries more than small ones.
## * This transformation tends to reduce the skewness, making the distribution more symmetrical compared to the original salary distribution (Questions 1 and 3).

# Compute simple descriptive statistics for the PerfSocreID variable. In addition, produce a visualization of the frequency distribution of PerfScoreID.

## 6. Report the descriptive statistics along with the frequency distribution and provide a detailed interpretation of how you would characterize this variable.
### Summary statistics
summary(hrdata_df$PerfScoreID)

### Mean
mean_perfscoreid <- mean(hrdata_df$PerfScoreID, na.rm = TRUE)
print(mean_perfscoreid)

#### Median
median_perfscoreid <- median(hrdata_df$PerfScoreID, na.rm = TRUE)
print(median_perfscoreid)

#### Mode
mode_perfscoreid <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_perfscoreid_value <- mode_perfscoreid(hrdata_df$PerfScoreID)
print(mode_perfscoreid_value)

### Frequency table
perfscoreid_table <- table(hrdata_df$PerfScoreID)
print(perfscoreid_table)

#### Plot
hrdata_df %>%
  ggplot(aes(x = PerfScoreID)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "PerfScoreID Distribution", x = "PerfScoreID", y = "Frequency") +
  theme_minimal()

### Interpretation
print(paste("The mean PerfScoreID is", mean_perfscoreid))
print(paste("The median PerfScoreID is", median_perfscoreid))
print(paste("The mode PerfScoreID is", mode_perfscoreid_value))
print(paste("The frequency distribution of salaries is shown in the bar plot above.")

# * The PerfScoreID distribution is strongly right-skewed. Most employees (243 out of 311) received a score of 3, indicating that 3 is the most common (modal) performance score.
# *The mean is very close to the median (both near 3), further highlighting that the majority of employees are clustered around a performance score of 3.


# 7. Which employee (ID number) has the largest z-score on PerfScoreID and what is the z-score for this person? Which employee (ID number) has the smallest z- score on PerfScoreID and what is the z-score for this person?

# Calculate the z-scores for the PerfScoreID column
hrdata_df <- hrdata_df %>%
  mutate(perfscoreid_zscore = scale(PerfScoreID))

# Print the z-scores
print(hrdata_df$perfscoreid_zscore)

# Find the index of the employee with the largest z-score
max_z_score_index <- which.max(hrdata_df$perfscoreid_zscore)
print(max_z_score_index)

# Retrieve the employee's EmpID and the corresponding z-score
max_z_score_employee <- hrdata_df[max_z_score_index, c("EmpID", "PerfScoreID")]

# Print the result
print(max_z_score_employee)

# Find the index of the employee with the smallest z-score
min_z_score_index <- which.min(hrdata_df$perfscoreid_zscore)
print(min_z_score_index)

# Retrieve the employee's EmpID and the corresponding PerfScoreID for the minimum z-score
min_z_score_employee <- hrdata_df[min_z_score_index, c("EmpID", "PerfScoreID")]

# Print the result
print(min_z_score_employee)

# Interpretation
# * The employee with the highest performance score (4) has a z-score of 1.7417, indicating that this employee's score is significantly above the mean (by about 1.74 standard deviations).
# *The employee with the lowest performance score (1) has a z-score of -3.3684, indicating that their score is far below the mean.

# 8. Compute descriptive statistics for the standardized PerfScoreID variable. Report your results and produce a frequency distribution for the standardized PerfScoreID scores. Compare this distribution to the one you produced in Question 6. Are they the same or different? Explain using both your graphical results and words.
# Calculate the z-scores for the PerfScoreID column
hrdata_df <- hrdata_df %>%
  mutate(perfscoreid_zscore = scale(PerfScoreID))

# Summary statistics
summary(hrdata_df$perfscoreid_zscore)

# Mean
mean_perfscoreid_zscore <- mean(hrdata_df$perfscoreid_zscore, na.rm = TRUE)
print(paste("Mean of PerfScoreID z-score:", mean_perfscoreid_zscore))

# Median
median_perfscoreid_zscore <- median(hrdata_df$perfscoreid_zscore, na.rm = TRUE)
print(paste("Median of PerfScoreID z-score:", median_perfscoreid_zscore))

# Mode
mode_perfscoreid_zscore <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_perfscoreid_zscore_value <- mode_perfscoreid_zscore(hrdata_df$perfscoreid_zscore)
print(paste("Mode of PerfScoreID z-score:", mode_perfscoreid_zscore_value))

# Frequency table
perfscoreid_zscore_table <- table(hrdata_df$perfscoreid_zscore)
print(perfscoreid_zscore_table)

# Plot the PerfScoreID z-score
hrdata_df %>%
  ggplot(aes(x = perfscoreid_zscore)) +
  geom_histogram(fill = "blue", bins = 30) +
  labs(title = "Standardized PerfScoreID (Z-Score) Distribution", x = "Z-Score", y = "Frequency") +
  theme_minimal()

# Plot of the PerfScoreID z-score and PerfScoreID
hrdata_df %>%
  pivot_longer(cols = c(PerfScoreID, perfscoreid_zscore), 
               names_to = "Measure", 
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Measure)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~ Measure, scales = "free_x") +   # Creates separate plots for PerfScoreID and Z-Score
  labs(title = "Comparison of PerfScoreID and Standardized PerfScoreID (Z-Score) Distributions",
       x = "Value", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("purple", "orange"))

# Interpretation
## * The z-score distribution shows that the bulk of employees' performance scores are close to the mean, with only a few employees having very high or very low performance scores.
## * The shape of the distribution remains the same as the original PerfScoreID distribution, but it is now centered around 0 with a standardized spread.


## 9. Square the PerfScoreID variable. Compute descriptive statistics for this new variable. Report your results and produce a frequency distribution for the squared scores. Compare this distribution to the ones you produced in Questions 1 & 3. Are they the same or different? Explain using both your graphical results and words.
# Square of the PerfScoreID variable
squared_perfscoreid <- hrdata_df$PerfScoreID^2

# Summary statistics
summary(squared_perfscoreid)

# Mean
mean_squared_perfscoreid <- mean(squared_perfscoreid, na.rm = TRUE)

# Median
median_squared_perfscoreid <- median(squared_perfscoreid, na.rm = TRUE)

# Mode
mode_squared_perfscoreid <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_squared_perfscoreid_value <- mode_squared_perfscoreid(squared_perfscoreid)

print(paste("Mean squared PerfScoreID:", format(round(mean_squared_perfscoreid, 2), nsmall = 2)))
print(paste("Median squared PerfScoreID:", median_squared_perfscoreid))
print(paste("Mode squared PerfScoreID:", mode_squared_perfscoreid_value))

# Frequency table
squared_perfscoreid_table <- table(squared_perfscoreid)
print(squared_perfscoreid_table)

# Plot the squared PerfScoreID
hrdata_df %>%
  mutate(squared_perfscoreid = PerfScoreID^2) %>%
  ggplot(aes(x = squared_perfscoreid)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Squared PerfScoreID Distribution", x = "Squared PerfScoreID", y = "Frequency") +
  theme_minimal()

# Interpretation
## * The squared transformation increases differences in scores, especially for employees with extreme performance scores.
## * The median and mode remain 9, as most employees still have a performance score of 3 (which squares to 9).
