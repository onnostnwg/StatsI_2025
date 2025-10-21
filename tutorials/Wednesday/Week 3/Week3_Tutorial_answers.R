###############################################################################
# Title:        Stats I - Week 2 (with answers)
# Description:  Hypothesis testing, experiments, difference in means
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
lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign", "haven"),  pkgTest)

# Get working directory
getwd()

# Set working directory"
setwd("/Users/elkarag/Desktop/Teaching/Applied Stats I/Week 3")


# Agenda
# (a.) Descriptive analysis
# (b.) Confidence intervals
# (c.) Significance test for a mean
# (d.) Significance test for a difference in means

### Research Question -----------
# Is there a relationship between education and income?

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

# Quick recap: 
mean(df$income) 
var(df$income) 
sd(df$income)
se_income <- sd(df$income)/sqrt(length(df$income)) # standard error


# -------------------------------#
# 3. Visualizing the Distribution
# -------------------------------#

# Histogram of income
hist(df$income,
     #breaks = 20,
     main = "Monthly net income",
     xlab = "Euro")

# Density plot of income
plot(density(df$income),
     main = "Monthly net income",
     xlab = "Euro")

# -----------------------------------------#
# 4. Sampling Distribution & Standard Error
# -----------------------------------------#
# Which kind of inferences can we make with regards to the population,
# based on the sample data?

# Sample mean is our estimate of the population mean
mean(df$income)

# Standard error estimates the SD of the sampling distribution
se_income

# Why do we need the standard error?
# To calculate measures of uncertainty for our point estimate
# (e.g., confidence intervals and p-values)

# -------------------------------#
# 5. Confidence Intervals
# -------------------------------#
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# Point estimate
mean_income <- mean(df$income)

# Standard error
se_income <- sd(df$income) / sqrt(length(df$income))

# How do we find the multiple?

# ---- 95% Confidence Interval (Approximate) ----
# Looking at the normal distribution, we see that
# 95% of observations lie within ±1.96 (approx. 2)
# standard errors of the point estimate

lower_95 <- mean_income - 1.96 * se_income
upper_95 <- mean_income + 1.96 * se_income

lower_95
mean_income
upper_95

# ---- 95% Confidence Interval  (Precise) ----
lower_95_n <- qnorm(0.025,
                    mean = mean(df$income),
                    sd   = se_income)

upper_95_n <- qnorm(0.975,
                    mean = mean(df$income),
                    sd   = se_income)

lower_95_n
mean_income
upper_95_n

# Let's talk about qnorm()
?qnorm
qnorm(0.025) # value for first 2.5%
qnorm(0.975) # value last 2.5%
qnorm(0.025, mean=2, sd=0.4) # Change mean and standard error

# ---- 99% Confidence Interval (t Distribution) ----
# t distribution is used when the sample size is small
t_score <- qt(0.995, df = length(df$income) - 1)

lower_99_t <- mean_income - t_score * se_income
upper_99_t <- mean_income + t_score * se_income

# The same but full formula
lower_99_t <- mean_income-(t_score)*(sd(df$income)/sqrt(length(df$income)))
upper_99_t <- mean_income+(t_score)*(sd(df$income)/sqrt(length(df$income)))

lower_99_t
mean_income
upper_99_t

# ---- Visualize 95% CI on Histogram ----
hist(df$income,
     main = "Monthly net income",
     xlab = "Euro")
abline(v = mean_income, col = "black")
abline(v = lower_95, col = "black", lty = "dashed")
abline(v = upper_95, col = "black", lty = "dashed")

# Let's talk about qt()
?qt
qt(0.005, df=length(df$income)-1) # critical value for first 0.5%
qt(0.995, df=length(df$income)-1) # last 0.5%
qt(0.005, df=length(df$income)-1, lower.tail=FALSE) # last 0.5%


# -------------------------------#
# Visualize All CIs on Histogram
# -------------------------------#

# --- Z critical values for 95% & 99% ---
z95 <- 1.96
z99 <- 2.576

# Z-based CIs (95% & 99%)
z95_lower <- mean_income - z95 * se_income
z95_upper <- mean_income + z95 * se_income
z99_lower <- mean_income - z99 * se_income
z99_upper <- mean_income + z99 * se_income

# t-based CIs already computed (95% & 99%)
t95 <- qt(0.975, df = length(df$income)-1)
t95_lower <- mean_income - t95 * se_income
t95_upper <- mean_income + t95 * se_income
# (99% t CI = lower_99_t / upper_99_t)

# --- Plot ---
hist(df$income,
     main = "Monthly Net Income with 95% & 99% Confidence Intervals",
     xlab = "Income (Euro)",
     col = "gray90", border = "gray50")

# Mean
abline(v = mean_income, col = "black", lwd = 2)

# Z-score CIs (red dashed)
abline(v = c(z95_lower, z95_upper), col = "red", lty = 2, lwd = 2) # 95%
abline(v = c(z99_lower, z99_upper), col = "red", lty = 2, lwd = 1) # 99%

# t-score CIs (blue solid)
abline(v = c(t95_lower, t95_upper), col = "blue", lty = 1, lwd = 2) # 95%
abline(v = c(lower_99_t, upper_99_t), col = "blue", lty = 1, lwd = 1) # 99%

legend("topright",
       legend = c("Mean",
                  "Z 95% CI", "Z 99% CI",
                  "t 95% CI", "t 99% CI"),
       col    = c("black","red","red","blue","blue"),
       lty    = c(1,2,2,1,1),
       lwd    = c(2,2,1,2,1),
       bty    = "n")

# -------------------------------#
# 6. Correlation
# -------------------------------#

# RQ: Is there a relationship between education and income?

# Scatter plot 
plot(df$income,df$edu)
plot(df$income,df$edu,
     col=df$cap+1) # Color over third variable (+1, because first color in R is white)

# Improve visualization and save
png(file="scatter_plot.png")
plot(df$income,
     df$edu,
     col=df$cap+1,
     xlab="Monthly net income (in Euro)",
     ylab="University level education (in years)",
     main="The relationship between education and income")
# Add legend
legend(1000, 8, # x and y position of legend
       legend=c("Non capital", "Capital"),
       col=c("black","red"),
       pch=1) # Marker type (1 is default)
dev.off()

# Boxplot
boxplot(df$income ~ df$cap, 
        main="Boxplot of Income by place of residence",
        ylab="Euro",
        xlab="Place of residence",
        names=c("Non capital","Capital"))

# And with ggplot2:
# Scatterplot 
ggplot(df, aes(x = income, y = edu, color = factor(cap))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("black", "red"),
                     labels = c("Non capital", "Capital"),
                     name = "Residence") +
  labs(x = "Monthly net income (in Euro)",
       y = "University level education (in years)",
       title = "The relationship between Education and Income") +
  theme_minimal() 


# Boxplot
ggplot(df, aes(x = factor(cap), y = income, fill = factor(cap))) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray70", "red"),
                    labels = c("Non capital", "Capital"),
                    name = "Residence") +
  labs(x = "Place of residence",
       y = "Monthly net income (Euro)",
       title = "Boxplot of Income by Place of Residence") +
  scale_x_discrete(labels = c("Non capital", "Capital")) +
  theme_minimal() +
  theme(legend.position = "none") 

# -------------------------------#
# 7. Significance Tests
# -------------------------------#

# In statistics, a **significance test** checks whether an observed sample
# could plausibly have come from a population with a hypothesized parameter value.
# Here we focus on:
#   (a) Testing a single population mean
#   (b) Testing the difference between two group means


# ---------------------------------------------#
# Question:
# Is the average monthly income in our sample
# different from the population mean in Ireland (from Google: 3034)?

# Hypotheses: one or two-sided? 
# Answer: two-sided
#   H0: Average monthly income is 3034 (mu is not equal to 3034)
#   H1: Average monthly income is not 3034 (mu = 3034)

# The t-test compares the sample mean to the hypothesized value mu0,
# accounting for sample size and variability.

# Two-sided test: is the mean different (higher OR lower)?
t.test(df$income, mu = 3034)

# Help page (shows arguments, e.g. alternative = "less"/"greater")
?t.test

# One-sided test: is the mean LESS than 3034?
t.test(df$income, mu = 3034, alternative = "less")

# NOTE:
# - p-value < 0.05 : reject H0 (mean likely differs from 3034)
# - p-value ≥ 0.05 : do not reject H0 (sample mean compatible with 3034)

# The t.test() output also provides a confidence interval by default.
# We can change the confidence level easily:
t.test(df$income, mu = 3034, conf.level = 0.99)

# Double-check: our manually calculated 99% t-interval should match
lower_99_t
mean_income
upper_99_t

# So, we also found a much easier way to calculate the confidence intervals!
t.test(df$income, conf.level = 0.99, alternative = "two.sided")


# ---------------------------------------------#
# Question:
#   Do people living in the capital earn different
#   incomes than those living elsewhere?
#
# Hypotheses: one or two-sided?
#   H0: People living in a capital do not earn a different income than the rest. 
#   H1: People living in a capital earn a different income than the rest. 

# The two-sample t-test compares the means of two independent groups.
# By default, t.test() uses Welch’s t-test, which does NOT assume equal variances.


# Quick descriptive check: group means
mean(df[df$cap == 0, ]$income)  # Non-capital
mean(df[df$cap == 1, ]$income)  # Capital

# Subsetting step-by-step: 
df$cap                   # see the variable
df$cap == 0              # logical test: TRUE/FALSE
df[df$cap == 0, ]        # keep only non-capital rows
df[df$cap == 0, ]$income # select income column
mean(df[df$cap == 0, ]$income)

# Two-sample t-test (Welch)
t.test(df$income ~ df$cap, alternative = "two.sided")

# On average, do people earn more in the capital
# compared to people who do not reside in the capital?
# One-sided test: 
t.test(df$income ~ df$cap, alternative = "less")

# Interpretation:
# - If p-value < 0.05 : reject H0 (means differ significantly)
# - If alternative = "less" and p < 0.05 : non-capital income is significantly lower
#   than capital income.

# On average, do people earn more in the capital
# compared to people who do not reside in the capital?

# -----------------------------------------------------------#
### Extra activity with real-world data (difference in means): 
# -----------------------------------------------------------#

# Goal: Test whether mean Polity scores differ between
#       Eastern Europe vs Western Europe & North America.
# Data: polity.dta — Polity score (0–10), higher = more democratic.

# Why not load("polity.dta")?
# - load() is for .RData/.rda (R’s serialized objects), not Stata files.
# - Use haven::read_dta() for .dta files.
data <- read.dta("polity.dta")

# Quick look
head(data)
glimpse(data)
table(data$region)

# Variable of interest: fh_polity2 - numeric Polity score (0-10)

# Subset the two regions of interest:
west <- data$fh_polity2[data$region == "Western Europe and North America"]
east <- data$fh_polity2[data$region == "Eastern Europe"]
  
# Quick descriptive statistics
mean_west <- mean(west, na.rm = TRUE)
mean_east <- mean(east, na.rm = TRUE)
n_west    <- sum(!is.na(west))
n_east    <- sum(!is.na(east))
sd_west   <- sd(west, na.rm = TRUE)
sd_east   <- sd(east, na.rm = TRUE)

mean_west; mean_east
n_west; n_east
sd_west; sd_east

# Calculate the SEs
# SE = sample SD / sqrt(n)
se_west <- sd_west / sqrt(n_west)
se_east <- sd_east / sqrt(n_east)

se_west; se_east

# -------------------------------------#
#  Analytical CI (Normal Approximation)
# -------------------------------------#

# By hand using:
#   Diff = mean_west - mean_east
#   SE_diff = sqrt(Var_west/n_west + Var_east/n_east)
#   95% CI = Diff ± 1.96 * SE_diff

se_diff  <- sqrt((sd_west^2 / n_west) + (sd_east^2 / n_east))
diff_hat <- mean_west - mean_east

ci_low_analytic <- diff_hat - 1.96 * se_diff
ci_up_analytic  <- diff_hat + 1.96 * se_diff
ci_analytic     <- c(ci_low_analytic, ci_up_analytic)

diff_hat
se_diff
ci_analytic

# ------------------------#
#  Welch Two-Sample t-test
# ------------------------#

t_test_res <- t.test(west, east)  # two-sided Welch test
t_test_res

# Extract the CI the tidy way (matches the test above)
ci_t <- t_test_res$conf.int[1:2]
ci_t

# Conclusion?

# Example answer:
# Our analysis shows that countries in Western Europe & North America
# have a much higher average Polity score (mean = 9.97) than countries
# in Eastern Europe (mean = 6.59). The estimated difference in means is
# about 3.38 points on the 0–10 democracy scale.

# The 95% confidence interval for this difference (Welch’s t-test) is
# [2.10 , 4.66], which does not include 0. This means we can reject the
# null hypothesis of no difference between the two regions at the 5%
# significance level (p = 0.000012).

# Substantively, Western Europe & North America seem to
# score on average between about 2 and 5 points higher on the Polity
# democracy index compared to Eastern Europe.
