###############################################################################
# Title:        Stats I - Week 2
# Description:  Statistical inference review 
# Author:       Elena Karagianni
# R version:    R 4.4.0
###############################################################################

# -------------------------------#
# 1. Setup
# -------------------------------#

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2", "dplyr", "viridis"),  pkgTest)

# Get working directory
getwd()

# Set working directory"
setwd("/Users/elkarag/Desktop/Teaching/Applied Stats I")

# -------------------------------#
# 2. Load & Inspect Data
# -------------------------------#

df <- read_csv("fictional_data.csv")

# Quick overview
head(df)
str(df)
summary(df)

# Variables:
# - income: Monthly net income (numeric)
# - edu: University-level education in years (numeric)
# - cap: Binary variable (1 = lives in capital, 0 = otherwise)

# -------------------------------#
# 3. Descriptive Statistics
# -------------------------------#

### Measures of central tendency (mean, median) and variability (variance, sd) ###

# Income
mean_income <- mean(df$income)
median_income <- median(df$income)
var_income <- var(df$income)
sd_income <- sd(df$income)


# Education
mean_edu <- mean(df$edu)
median_edu <- median(df$edu)
var_edu <- var(df$edu)
sd_edu <- sd(df$edu)

mean_edu
median_edu
var_edu
sd_edu

# Comment:
# - Mean: average value, sensitive to outliers
# - Median: midpoint of the distribution, robust to outliers
# - Variance/SD: how spread out the values are
# - Standard Error (SE): how uncertain our estimate of the mean is


# Standard Error (SE) of the mean:
# SE = sd / sqrt(n)
# - Standard deviation (sd) measures how spread out the individual data points are.
# - Standard error (se) measures how much the sample mean itself would vary
#   if we repeatedly took new samples from the population.
#   → Larger n → smaller SE (mean estimate is more precise).
#   → Larger sd → larger SE (mean estimate is less precise).
# We use SE to build confidence intervals and perform hypothesis tests.
se_income <- sd_income / sqrt(length(df$income))
se_edu <- sd_edu / sqrt(length(df$edu))

se_income
se_edu

# -------------------------------#
# 4. Visualization with ggplot2
# -------------------------------#

# Histogram of income with mean line
ggplot(df, aes(x = income)) +
  geom_histogram(binwidth = 250, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean_income, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Monthly Net Income",
       x = "Monthly Net Income (Euro)", y = "Count") +
  theme_minimal()

# Histogram of education with mean line
ggplot(df, aes(x = edu)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  geom_vline(xintercept = mean_edu, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Education (Years)",
       x = "Years of Education", y = "Count") +
  theme_minimal()

# Scatter plot: Income vs Education, colored by Capital
ggplot(df, aes(x = income, y = edu, color = factor(cap))) +
  geom_point() +
  scale_color_manual(values = c("black", "red"), labels = c("Non-capital", "Capital")) +
  labs(title = "Relationship between Education and Income",
       x = "Monthly Net Income (Euro)",
       y = "Years of University Education",
       color = "Residence") +
  theme_minimal()

# -------------------------------#
# 5. Confidence Intervals
# -------------------------------#

# 95% CI for income mean
ci_95_lower <- mean_income - 1.96 * se_income
ci_95_upper <- mean_income + 1.96 * se_income

ci_95_lower
mean_income
ci_95_upper

# 99% CI for income mean
ci_99_lower <- mean_income - 2.576 * se_income
ci_99_upper <- mean_income + 2.576 * se_income

ci_99_lower
mean_income
ci_99_upper

# Let's visualise this:
ggplot(df, aes(x = income)) +
  geom_histogram(binwidth = 250, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean_income, color = "black", size = 1) +
  geom_vline(xintercept = ci_95_lower, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ci_95_upper, color = "black", linetype = "dashed") +
  labs(title = "Income Distribution with 95% Confidence Interval",
       x = "Monthly Net Income (Euro)", y = "Count") +
  theme_minimal()

# --------------------------------------#
# 6. Central Limit Theorem (CLT) Example
# --------------------------------------#

# The CLT tells us: the sampling distribution of the mean approaches normality
# as sample size increases, regardless of the original population distribution.

set.seed(123)  # reproducibility
sample_means_n5 <- replicate(1000, mean(sample(df$income, size = 5, replace = TRUE)))
sample_means_n30 <- replicate(1000, mean(sample(df$income, size = 30, replace = TRUE)))
sample_means_n500 <- replicate(1000, mean(sample(df$income, size = 500, replace = TRUE)))

# Compare sampling distributions
df_clt <- data.frame(
  mean = c(sample_means_n5, sample_means_n30, sample_means_n500),
  n = factor(rep(c(5, 30, 500), each = 1000))
)

ggplot(df_clt, aes(x = mean, fill = n)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ n, scales = "free") +
  geom_vline(xintercept = mean_income, color = "red", linetype = "dashed") +
  labs(title = "Central Limit Theorem: Sampling Distributions of the Mean",
       x = "Sample Mean of Income", y = "Count") +
  theme_minimal()

# Interpretation:
# - With small n (5), distribution of sample means is wide and not perfectly normal.
# - With larger n (30, 500), distribution of sample means becomes narrower
#   and closer to normal, centered on the population mean.


# We learned that the sampling distribution of the mean always 
# approaches a normal distribution, regardless of the shape of the original
# distribution!

# Income, for example, is usually not really normally distributed.
# First, we generate some hypothetical income data.

income <- rgamma(1000, shape = 1.1, scale = 2000) 
summary(income)
var(income)

# Let's have a look.
par(mfrow = c(1, 2)) # two plots side-by-side
hist(income,
     bty = "n",
     las = 1,
     border = "white",
     col = viridis(2)[1]
)

plot(density(income),
     bty = "n",
     las = 1,
     col = viridis(2)[2],
     lwd = 2,
     main = "Density of income"
)


# This is clearly a non-normal distribution. Now we want to get our sampling 
# distribution of the mean again.

trial1 <- rep(NA, 100)

for (i in 1:100){
  trial1[i] <- mean(sample(income, 100))
}

hist(trial1 ,
     bty = "n",
     las = 1,
     border = "white",
     col = viridis(1),
     xlim = c(1500, 3000),
     main = "Distribution of sample means\n(sample size: n = 100)"
)

plot(density(trial1),
     bty = "n",
     las = 1,
     lwd = 2,
     col = viridis(1),
     xlim = c(1500, 3000),
     main = "Density of sample means\n(sample size: n = 100)"
)

mean(trial1)
var(trial1)


trial2 <- rep(NA, 100)

for (i in 1:100){
  trial2[i] <- mean(sample(income, 400)) #this time, we sample 400 people
}

hist(trial2,
     bty = "n",
     las = 1,
     border = "white",
     col = viridis(1),
     xlim = c(1500, 3000),
     main = "Distribution of sample means\n(sample size: n = 400)"
)

plot(density(trial2),
     bty = "n", 
     las = 1,
     lwd = 2,
     col = viridis(1),
     xlim = c(1500, 3000),
     main = "Density of sample means\n(sample size: n = 400)"
)

mean(trial2)
var(trial2)
