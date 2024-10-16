#load libraries and data
library(tidyverse)
library(dplyr)

#load vector
scores <- c(12, 16, 18, 14, 13, 15, 12, 13, 11, 17, 10, 13, 14)

#load data
hrdata_df <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 1/HRData_New.csv")
class(hrdata_df)

#2. I give a 10-item test of Clemson University trivia to 13 people. Their scores are as follows: 12 16 18 14 13 15 12 13 11 17 10 13 14
##a. Calculate the mean, median, and the mode.
mean_score <- mean(scores)
median_score <- median(scores)
calculate_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
mode_score <- calculate_mode(scores)
print(paste("Mean score:", mean_score))
print(paste("Median score:", median_score))
print(paste("Mode score:", mode_score))

##b. Using the mean as your parameter estimate for your model, calculate the number of errors, the sum of the absolute errors, and the sum of the squared errors.
errors_mean <- scores - mean_score
num_errors <- sum(errors_mean != 0)
print(paste("Number of errors:", num_errors))
sum_abs_errors_mean <- sum(abs(errors_mean))
print(paste("Sum of absolute errors:", sum_abs_errors_mean))
sum_sq_errors_mean <- sum(errors_mean^2)
print (paste("Sum of the Squared Errors:", sum_sq_errors_mean))

##c. Now use the median as your parameter estimate and calculate the same three error terms.
errors_median <- scores - median_score
num_errors <- sum(errors_median != 0)
print(paste("Number of errors:", num_errors))
sum_abs_errors_median <- sum(abs(errors_median))
print(paste("Sum of absolute errors:", sum_abs_errors_median))
sum_sq_errors_median <- sum(errors_median^2)
print (paste("Sum of the Squared Errors:", sum_sq_errors_median))

##d. Use the mode as your parameter estimate and calculate the same three error terms.
errors_mode <- scores - mode_score
num_errors <- sum(errors_mode != 0)
print(paste("Number of errors:", num_errors))
sum_abs_errors_mode <- sum(abs(errors_mode))
print(paste("Sum of absolute errors:", sum_abs_errors_mode))
sum_sq_errors_mode <- sum(errors_mode^2)
print (paste("Sum of the Squared Errors:", sum_sq_errors_mode))

##e. Look across your answers to (b), (c), and (d) to see for each error term, which estimate gives the lowest error? That is, which of the mean, median, and mode minimizes the sum of squared errors and by how much? Which one minimizes the sum of absolute errors and by how much? And which one minimizes the count of errors and by how much?
###The estimate that gives the lowest error is the mean parameter as the sum of squared errors is 64.77 compared to 71 for both the median and mode parameter estimates. 
###The parameter that minimizes the sum of absolute errors are both the median and mode, which is 23 compared to 23.69 for the mean parameter. 
###The parameter that minimizes the count of errors are both median and mode at 10.

#3. “HR Data.csv” contains 311 rows and 36 columns. First, open this data file in RStudio or Jamovi. Next, obtain estimates of the mean, median, variance, and standard deviation of two variables (SALARY, ENGAGEMENTSURVEY). Report these along with a one sentence interpretation of what the values mean to you.

##salary analysis

###salary statistical analysis
hrdata_sal_mean <- mean(hrdata_df$Salary)
hrdata_sal_median <- median(hrdata_df$Salary)
hrdata_sal_var <- var(hrdata_df$Salary)
hrdata_sal_stddev <- sd(hrdata_df$Salary)
print(paste("Mean salary:", hrdata_sal_mean, na.rm = TRUE))
print(paste("Median salary:", hrdata_sal_median))
print(paste("Variance of salary:", hrdata_sal_var))
print(paste("Standard deviation of salary:", hrdata_sal_stddev))

####engagement statistical analysis
hrdata_sal_mean <- mean(hrdata_df$Engagement)
hrdata_sal_median <- median(hrdata_df$Engagement)
hrdata_sal_var <- var(hrdata_df$Engagement)
hrdata_sal_stddev <- sd(hrdata_df$Engagement)
print(paste("median Engagement:", hrdata_sal_median))
print(paste("Median Engagement:", hrdata_sal_median))
print(paste("Variance of Engagement:", hrdata_sal_var))
print(paste("Standard deviation of Engagement:", hrdata_sal_stddev))

##interpretation

###mean salary interpretation
#The mean salary shows the average salary of all employees in the data is ~$70,000.

#The variance and standard deviation of the salary data show the spread of the data from the mean salary. The variance of the salary data is ~$1,000,000,000 and the standard deviation is ~$31,000. The standard deviation shows the average distance of each data point from the mean salary. This indicates that the salary data is spread out from the mean salary of ~$70,000.

#The median salary shows the middle salary of all employees in the data is ~$60,000, which is lower than the mean of ~$70,000. This indicates that the salary data is skewed to the right, with more employees earning lower salaries than higher salaries..

#4. Compare the mean SALARY for Men and Women. Briefly describe these results, being sure to indicate whether it APPEARS (we don't know how to do the formal test yet) that it would be useful to make predictions ofb SALARY conditional on employee sex (M or F) 

hrdata_sal_mean_men <- mean(hrdata_df$Salary[hrdata_df$Gender == "M"])
hrdata_sal_mean_women <- mean(hrdata_df$Salary[hrdata_df$Gender == "F"])
print(paste("Men's mean salary:", hrdata_sal_mean_men, na.rm = TRUE))
print(paste("Women's mean salary:", hrdata_sal_mean_women))
