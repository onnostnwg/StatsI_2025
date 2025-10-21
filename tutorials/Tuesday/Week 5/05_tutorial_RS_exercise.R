# Applied Statistical Analysis I      
# Tutorial 4: Bivariate regression, inference & prediction                     

# Get working directory
getwd()

# Set working directory 
setwd("/Users/redmondscales/Documents/Applied Stats/GitHub")
getwd()

#############################
### RECAP Chi-square test ###
#############################

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


# Check p-value


# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

### Look at standardized residuals ###

# Save chi-square test in object


# List objects inside chi_test


# Pearson residuals, 
# (observed - expected) / sqrt(expected)


# **Standardized** residuals,
# (observed - expected) / sqrt(V), where V is the residual cell variance


# How can we interpret the standardized residuals? 

# Agenda 
# (a.) Correlation
# (b.) Bivariate regression 

# Research questions: 
# Is there a relationship between education and income?

# (a.) Correlation -----

# Load data 
df <- read.csv("datasets/fictional_data.csv")
View(df)

# Scatter plot 


# Calculate correlation


# Add to scatter plot


# Improve visualization and save


# t-test for the correlation coefficient

# Check p-value


# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# (b.) Bivariate regression  -----

# Fit linear regression model

# Save model as object


# t-test for the slope of a regression line


# Check p-value


# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Confidence intervals 


# Plot


# Step by step

# What is the prediction equation?


# income_pred = 976.16 + 250.64 * education

# Make predictions for first observation in df



# Make predictions for a range of x values



# Step by step
predict(model) # Predicted outcomes
model$fitted.values # Predicted outcomes
unique(df$edu) # Unique values of x
seq(min(df$edu), max(df$edu), by=1) # Specify a sequences for which
# predictions are to be returned


# Add standard errors


# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x


# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 


# more variability in individual responses --> wider intervals

# Make predictions for x values not in data

# Plot predictions



# Plot, regression line with confidence intervals
# Adopted from: https://stackoverflow.com/questions/46459620/plotting-a-95-confidence-interval-for-a-lm-object

# Save confidence intervals

# Add lower bound

# Add upper bound


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
     xlab="University level education (in years)",
     ylab="Monthly net income (in Euro)",
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


