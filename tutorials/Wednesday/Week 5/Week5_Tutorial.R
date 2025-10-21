###############################################################################
# Title:        Stats I - Week 5
# Description:  Bivariate regression, inference, and prediction
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
lapply(c("readr", "ggplot2"),  pkgTest)

# Get working directory
getwd()

# Set working directory
setwd("/Users/elkarag/Desktop/Teaching/Applied Stats I/Week 5")

# Agenda
# (a.) Revise chi-square test
# (b.) Correlation
# (c.) Bivariate regression 

# Research questions: Is there a relationship between
# movie genre and rating?

# Load data
df <- readRDS("datasets/movies.rds")
View(df)

# Dataframe subsetting: df[rows, columns]
df_s <- df[df$genre=="Comedy" |
             df$genre=="Drama" |
             df$genre=="Documentary", ]
df_s$genre <- droplevels(df_s$genre)
View(df_s)

# Run Chi squared test
chisq.test(df_s$genre, 
           df_s$critics_rating)

# Check p-value
sprintf("%.20f",1.097e-12)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

### Look at standardized residuals ###

# Save chi-square test in object
chi_test <- chisq.test(df_s$genre, df_s$critics_rating)

# List objects inside chi_test
ls(chi_test)
chi_test$observed # f_o (observed frequencies)
chi_test$expected # f_e (expected frequencies under the assumption of H0,
# under the assumption that two variables are independent)

# Pearson residuals, 
# (observed - expected) / sqrt(expected)
chi_test$residuals 

# **Standardized** residuals,
# (observed - expected) / sqrt(V), where V is the residual cell variance
chi_test$stdres  

# How can we interpret the standardized residuals? 

# -------------------------------#
# b. Correlation
# -------------------------------#

df <- read.csv("datasets/fictional_data.csv")
View(df)

# Scatter plot 
plot(df$edu,df$income)

# Calculate correlation
cor(df$edu,df$income)

# Add to scatter plot
text(1.5, 2600, sprintf("Correlation=%s", round(cor(df$edu,df$income),4)))

# Improve visualization and save
png(file="scatter_plot.png")
plot(df$edu,
     df$income,
     ylab="Monthly net income (in Euro)",
     xlab="University level education (in years)",
     main="The Relationship between education and income") 
text(1.5, 2600, sprintf("Correlation=%s", round(cor(df$income,df$edu),4)))
dev.off()

# t-test for the correlation coefficient
cor.test(df$edu, df$income)

# Check p-value
sprintf("%.20f",7.52e-07)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# -------------------------------#
# c. Bivariate regression
# -------------------------------#

# Question: Is there a relationship between education and income?
# Model: income = b0 + b1 * education + e
summary(lm(df$income ~ df$edu))

# or 

summary(lm(income ~ edu, data = df))

# Output interpretation:
#  - Intercept (b0): expected income when education = 0
#  - Slope (b1): average change in income per additional year of education

# Save model as object
model <- lm(income~edu, data=df)

# t-test for the slope of a regression line
summary(model)
250.64/33.06 

# Check p-value
sprintf("%.20f",2.17e-06)

# Residual plot
model.res <- resid(model)

plot(df$edu, model.res, 
     ylab = "Residuals",
     xlab = "University Education (in years)")
abline(0,0)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Confidence intervals 
confint(model, level=0.95)
confint(model, level=0.99)

# Plot
plot(x=df$edu, y=df$income) # Scatter plot
abline(model) # Add regression line

plot(df$edu, df$income,
     xlab = "University Education (years)",
     ylab = "Income",
     main = "Income vs. Education")

abline(model, col = "red", lwd = 2) # Use 'model' for the regression line
abline(a = 976.16, b = 250.64, col = "darkgreen", lwd = 2) # Use the intercept/slope


# What is the prediction equation?
summary(model)
# \hat_income = 976.15 + 250.64 * educ 

# Make predictions for first observation in df
head(df)
976.16 +  250.64 * 1 # predicted outcome
model$fitted.values
1520 - (976.16 +  250.64 * 1) # error
model$residuals

# Make predictions for a range of x values
predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))

# Step by step
predict(model) # Predicted outcomes
model$fitted.values # Predicted outcomes
unique(df$edu) # Unique values of x
seq(min(df$edu), max(df$edu), by=1) # Specify a sequences for which
# predictions are to be returned
predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))

# Add standard errors
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)))
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), se.fit=TRUE)

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), interval="confidence", level=0.95)

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), interval="prediction", level=0.95)
# more variability in individual responses --> wider intervals

# Make predictions for x values not in data
predict(model, newdata=data.frame(edu = mean(df$edu))) # Mean education
mean(df$edu)
unique(df$edu) # Unique values of x
predict(model, newdata=data.frame(edu = 9)) # **But don't extrapolate**

# Plot predictions
plot(x=df$edu, y=df$income) # Scatter plot
points(df$edu, model$fitted.values, # Add another scatter plot on top
       col="green")

# Plot, regression line with confidence intervals
# Adopted from: https://stackoverflow.com/questions/46459620/plotting-a-95-confidence-interval-for-a-lm-object

# Save confidence intervals
ci <- predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu),by=1)), interval="confidence", level=0.95)
plot(df$edu, df$income) # Scatter plot
abline(model) # Add regression line
# Add lower bound
lines(seq(min(df$edu), max(df$edu),by=1), ci[,2], col="gray")
# Add upper bound
lines(seq(min(df$edu), max(df$edu),by=1), ci[,3], col="gray")

# Step by step
ci <- predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu),by=1)), interval="confidence", level=0.95)
ci # Save confidence intervals in object
# Dataframe subsetting: df[rows, columns]
ci[,2] # second column, lower bound, lwr
ci[,3] # third column, upper bound, upr

# Improve visualization and save
png(file="reg_plot.png")
plot(df$edu,
     df$incom,
     ylab="Monthly net income (in Euro)",
     xlab="University level education (in years)",
     main="The Relationship between education and income")
abline(model) # Add regression line
# Add confidence intervals
lines(seq(min(df$edu), max(df$edu),by=1), ci[,2], col="gray")
lines(seq(min(df$edu), max(df$edu),by=1), ci[,3], col="gray")
# Add legend
legend(0, 3000, # x and y position of legend
       legend=c("Predictions", "95% Confidence intervals"),
       col=c("black","gray"),
       pch=1) 
dev.off()

# And with ggplot2: 
# specify the values of interest: 
new_data <- data.frame(edu = seq(min(df$edu), max(df$edu), by = 1))

# get predictions and confidence intervals (95% CIs)
ci <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)

# combine predictions with new_data
pred_df <- cbind(new_data, ci)

# Plot using ggplot2
ggplot() +
  geom_point(data = df, aes(x = edu, y = income), color = "black") +
  geom_ribbon(data = pred_df, aes(x = edu, ymin = lwr, ymax = upr),
              fill = "gray80", alpha = 0.5) +
  geom_line(data = pred_df, aes(x = edu, y = fit),
            color = "blue") +
  labs(
    title = "The Relationship between Education and Income",
    x = "University-level Education (in years)",
    y = "Monthly Net Income (in Euro)"
  ) +
  theme_minimal()
