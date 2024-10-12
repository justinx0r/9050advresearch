library(tidyverse)
library(dplyr)
# define the scores as a vector
scores <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

#dataframe
scores_df <- data.frame(Scores = scores)
print(scores_df)

#1. Produce a frequency distribution. Describe the shape.
freq_dist <- table(scores_df$Scores)
print(freq_dist)

##barplot
barplot(freq_dist, main="Frequency Distribution of Scores", xlab="Scores", ylab="Frequency", col="blue")

##the shape of the distribution is uniform because each score from 0 to 9 only appears once

#2. Generate all possible samples of size 2 with replacement
samples <- expand.grid(scores, scores)

# Print the samples
print(samples)

#3. Enter the list of the means of the samples you identified for the previous question into a new R data frame. Create a frequency distribution. Describe the shape of this distribution
## Means of the samples
sample_means <- rowMeans(samples)

## Sample Dataframe
sample_means_df <- data.frame(SampleMeans = sample_means)

## Frequency Distribution
freq_dist_means <- table(sample_means_df$SampleMeans)
print(freq_dist_means)

## Barplot
barplot(freq_dist_means, main="Frequency Distribution of Sample Means", xlab="Sample Means", ylab="Frequency", col="green")

## Shape description
### The shape of the distribution of sample means is normal, unimodal, and leptokurtic.

# 4. Calculate the mean and standard deviation of the sample means. Compare these values to the population mean and standard deviation.
##define the population scores
population_scores <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

##calculate the mean and standard deviation of the population
population_mean <- mean(population_scores)
population_sd <- sd(population_scores)

##print the population mean and standard deviation
cat("Population Mean:", population_mean, "\n")
cat("Population Standard Deviation:", population_sd, "\n")

##alculate the mean and standard deviation of the sampling distribution
sampling_mean <- mean(sample_means_df$SampleMeans)
sampling_sd <- sd(sample_means_df$SampleMeans)

##print the sampling distribution mean and standard deviation
cat("Sampling Distribution Mean:", sampling_mean, "\n")
cat("Sampling Distribution Standard Deviation:", sampling_sd, "\n")

# 5. If we randomly selected a sample, what is the probability of getting a mean within 2 points of the population value?
# Define the population scores
population_scores <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

# Calculate the population mean and standard deviation
population_mean <- mean(population_scores)
population_sd <- sd(population_scores)

# Define the sample size
sample_size <- 2

# Calculate the standard error
standard_error <- population_sd / sqrt(sample_size)

# Calculate the Z-scores for the sample mean being within 2 points of the population mean
z_lower <- (population_mean - 2 - population_mean) / standard_error
z_upper <- (population_mean + 2 - population_mean) / standard_error

# Find the cumulative probability for the Z-scores
probability <- pnorm(z_upper) - pnorm(z_lower)

# Print the probability
cat("Probability of getting a sample mean within 2 points of the population mean:", probability, "\n")