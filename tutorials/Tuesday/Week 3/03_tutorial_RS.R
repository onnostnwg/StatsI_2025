# Applied Statistical Analysis I      
# Tutorial 2: Hypothesis testing, experiments, difference in means                       

# Get working directory
getwd()

# Set working directory 
setwd("/Users/redmondscales/Documents/Applied Stats/GitHub/StatsI_2025")
getwd()

# Agenda
# (a.) Descriptive analysis
# (b.) Confidence intervals
# (c.) Significance test for a mean
# (d.) Significance test for a difference in means

### Research Question -----------
# Is there a relationship between education and income?

# Load data 


# Selection of variables
# Education: University level education in years
# Income: Monthly net income
# Capital: Whether the person lives in capital or not

### (a.) Descriptive analysis ----------

# First step, look at data



# Step by step

# Get summary statistics for entire dataset


# Some quick visualizations, to look at distribution




# Which kind of inferences can we make with regards to the population,
# based on the sample data?


# Standard **error** (Sample standard deviation adjusted by sample size)
# is estimate for standard deviation of the sampling distribution

# Why do we need standard error? --> to calculate measures of
# uncertainty for our point estimate (e.g., confidence intervals, and p-values)

# (b.) Confidence intervals --------
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# What do we need?


# How to find the multiple?
# Looking at the normal distribution, we see that 
# 95% of observations lie within +/-1.96 (approximately 2)
# standard errors of point estimate 

# The **approximate** solution 
# Lower bound, 95 confidence level


# Print


# The **precise** solution, using normal distribution
# Lower bound, 95 confidence level


# Upper bound, 95 confidence level

# Step by step


# Print


# How to calculate 99% confidence intervals?
# When to use normal distribution and when to use t distribution?

# The **precise** solution, using t distribution


# Step by step 


# Print


# Update Histogram 


# Is there a relationship between education and income?

# Scatter plot 


# Improve visualization and save


# Boxplot


# (c.) Significance test for a mean ------

# What is the average monthly income in Ireland
# According to a quick Google search, it is 3034
# How does our sample compare to the population, 
# being the working population in Ireland?


# We also found a much easier way to calculate the confidence intervals (!)


# Let's double check


# (d.) Significance test for a difference in means ------

# On average, do people earn differently in the capital
# compared to people who do not reside in the capital?

# Calculate means for subgroups


# Step by step


# t-test


# On average, do people earn more in the capital
# compared to people who do not reside in the capital?

# t-test


