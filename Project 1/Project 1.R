#load libraries and data
library(tidyverse)
library(dplyr)

#load vector
scores <- c(12, 16, 18, 14, 13, 15, 12, 13, 11, 17, 10, 13, 14)

#load data
hrdata_df <- read.csv("/Users/justinwilliams/Code/9050advresearch/Project 1/HRData.csv")
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

#remove non-numeric characters like "$" and "," from the Salary column
hrdata_df$Salary <- as.numeric(gsub("[^0-9.]", "", hrdata_df$Salary))

#remove non-numeric characters from the EngagementSurvey column
hrdata_df$EngagementSurvey <- as.numeric(gsub("[^0-9.]", "", hrdata_df$EngagementSurvey))

#impute missing values with the mean
hrdata_df$Salary[is.na(hrdata_df$Salary)] <- mean(hrdata_df$Salary, na.rm = TRUE)
hrdata_df$EngagementSurvey[is.na(hrdata_df$EngagementSurvey)] <- mean(hrdata_df$EngagementSurvey, na.rm = TRUE)

#convert to numeric
hrdata_df$Salary <- as.numeric(as.character(hrdata_df$Salary))
hrdata_df$EngagementSurvey <- as.numeric(as.character(hrdata_df$EngagementSurvey))

#salary statistical analysis
hrdata_mean <- mean(hrdata_df$Salary)
hrdata_sal_median <- median(hrdata_df$Salary)
hrdata_sal_var <- var(hrdata_df$Salary)
hrdata_sal_stddev <- sd(hrdata_df$Salary)
print(paste("Mean salary:", hrdata_sal_mean, na.rm = TRUE))
print(paste("Median salary:", hrdata_sal_median))
print(paste("Variance of salary:", hrdata_sal_var))
print(paste("Standard deviation of salary:", hrdata_sal_stddev))

#median statistical analysis
