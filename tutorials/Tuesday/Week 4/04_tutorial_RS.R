# Applied Statistical Analysis I      
# Tutorial 3: Contingency tables, correlation & bivariate regression                      

# Get working directory
getwd()

# Set working directory 
setwd("/Users/redmondscales/Documents/Applied Stats/GitHub")
getwd()

# Agenda
# (a.) Contingency tables
# (b.) Chi-square test
# (c.) Correlation
# (d.) Bivariate regression 

# Load data 
df_not_tidy <- read.csv("datasets/movies.csv")

# First step, look at data
View(df_not_tidy)
str(df_not_tidy)
head(df_not_tidy)
summary(df_not_tidy)

# Research questions: 
# Do different genres receive varying critical appreciation?

# (a.) Contingency tables -------

# Load tidy version of data
# The data is prepared using the data_wraning.R script.

# First step, look at data


# Contingency table 


# Subset data, only consider Comedy, Documentary, Drama

# Option 1: 
# Dataframe subsetting: df[rows, columns]

# Step by step


# Option 2: Tidyverse subset

# Install and load tidyverse
# Adopted from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them



# Step by step

# Contingency table 

# Problem: Although we filtered our data 
# the underlying levels still exist. Getting rid of
# these, we use the droplevels-function.


# Contingency table 


# Add marginal distributions


# Joint probability 


# Interpret as estimated probabilities of two specific
# values of each of the variables co-occurring together

# What is the probability of a Comedy and "Rotten"?

# (A) Conditional probability 
# What is the probability of "Rotten", 
# conditional on Comedy?
?prop.table()

# Over rows --> Rating conditional on genre


# Add marginal distributions
# Over rows --> Rating conditional on genre

# Round

# Step by step 
 # Round to two decimals

# (B) Conditional probability 
# What is the probability of Comedy, 
# conditional on "Rotten?

# Over columns --> Genre conditional on rating


# Bar plot


# (b) Chi (kai) square test ------------

# Run Chi square test


# Check p-value


# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# A little side note, look at residuals

# Returns the Pearson residuals, (observed - expected) / sqrt(expected)


# (c.) Correlation -----

# Is there an association between education and income?

# Load data 
df <- read.csv("datasets/fictional_data.csv")

# Scatter plot 

# Improve visualization and save


# Calculate correlation


# Add to scatter plot


# (d.) Bivariate regression  -----

# Is there a relationship between education and income?



 # Color over third variable (+1, because first color in R is white)
