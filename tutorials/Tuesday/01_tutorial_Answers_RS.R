# Applied Statistical Analysis I      
# Tutorial 1: Introduction & stats review                        

# Get working directory
getwd()

# Set working directory 
setwd("/Users/redmondscales/Documents/Applied Stats/GitHub/StatsI_2025")
getwd()

### Research Question -----------
# Is there a relationship between education and income?

# Selection of variables
# a. Education: University level education in years
# b. Income: Monthly net income
# c. Capital: Whether the person lives in capital or not

### Data collection ----------

# Load data 
df <- read.csv("datasets/fictional_data.csv")

# First step, look at data
View(df)
str(df) # Present structure of R object
?str

### Descriptive analysis ----------

### Income ###
hist(df$income) # Distribution
mean(df$income) # Central tendency, mean
var(df$income) # Variability, variance
sd(df$income) # Variability, standard deviation
sd(df$income)/sqrt(length((df$income))) # Variability, standard **error**

# Step by step
df$income
length((df$income))
sqrt(length((df$income)))
sqrt(19)
sd(df$income)/sqrt(length((df$income)))

### Education ###
hist(df$edu) # Distribution
mean(df$edu) # Central tendency, mean
var(df$edu) # Variability, variance
sd(df$edu) # Variability, standard deviation
sd(df$edu)/sqrt(length((df$edu))) # Variability, standard **error**

# Get summary statistics for entire dataset
summary(df)

# Which kind of inferences can we make with regards to the population,
# based on the sample data?
mean(df$income) # Sample mean is estimate for population mean
sd(df$income)/sqrt(length((df$income))) 
# Standard **error** (Sample standard deviation adjusted by sample size)
# is estimate for standard deviation of the sampling distribution

# Why do we need standard error again? --> to calculate measures of
# uncertainty for our point estimate (e.g., confidence intervals, and p-values)

### 95% Confidence level ###
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# What do we need?
mean(df$income) # Point estimate
sd(df$income)/sqrt(length((df$income))) # Standard error

# How to find the multiple?
# Looking at the normal distribution, we see that 
# 95% of observations lie within +/-1.96 (approximately 2)
# standard errors of point estimate 

# Lower bound
upper_95 = (mean(df$income))+(1.96)*(sd(df$income)/sqrt(length((df$income))))

# Upper bound
lower_95 = (mean(df$income))-(1.96)*(sd(df$income)/sqrt(length((df$income))))

# Print
lower_95
mean(df$income)
upper_95

# How to calculate 99% confidence intervals?

### Histogram ###
hist(df$income)
abline(v=mean(df$income),col="black")
abline(v=lower_95,col="black",lty="dashed")
abline(v=upper_95,col="black",lty="dashed")

# Is there a relationship between education and income?

### Scatter plot ###
plot(df$income,df$edu)
plot(df$income,df$edu,
     col=df$cap+1) # Color over third variable (+1, because first color in R is white)

# Improve visualization and save
png(file="tutorials/scatter_plot.png")
plot(df$income,
     df$edu,
     col=df$cap+1,
     xlab="Monthly net income (in Euro)",
     ylab="University level education (in years)",
     main="The Relationship between education and income")
# Add legend
legend(1000, 8, # x and y position of legend
       legend=c("Non capital", "Capital"),
       col=c("black","red"),
       pch=1) # Marker type (1 is default)
dev.off()
