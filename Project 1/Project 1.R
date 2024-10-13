#load libraries and data
library(tidyverse)
library(dplyr)

#load vector
scores <- c(12, 16, 18, 14, 13, 15, 12, 13, 11, 17, 10, 13, 14)

#1. I give a 10-item test of Clemson University trivia to 13 people. Their scores are as follows: 12 16 18 14 13 15 12 13 11 17 10 13 14
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
##c. Now use the median as your parameter estimate and calculate the same three error terms.
##d. Use the mode as your parameter estimate and calculate the same three error terms.
##e. Look across your answers to (b), (c), and (d) to see for each error term, which estimate gives the lowest error? That is, which of the mean, median, and mode minimizes the sum of squared errors and by how much? Which one minimizes the sum of absolute errors and by how much? And which one minimizes the count of errors and by how much?
