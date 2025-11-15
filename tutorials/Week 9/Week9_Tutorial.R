###############################################################################
# Title:        Stats I - Week 9
# Description:  Multiple regression
# Author:       Elena Karagianni
# R version:    R 4.5.2
###############################################################################


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
lapply(c("stargazer", "vioplot", "arm"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Research questions: 
# What is the relationship between education and Euroscepticism?

# Subsetting data

# Look at the codebook: 
# -----------------------------------------------------------
# Dependent Variable (DV)
# -----------------------------------------------------------
# euftf: European unification — should it go further or has it gone too far?
# Scale: 0 = Unification has gone too far | 10 = Unification should go further

# -----------------------------------------------------------
# Independent Variables (IV)
# -----------------------------------------------------------
# edlvdie: Highest level of education (Ireland-specific)
# eduyrs: Years of full-time education completed

# -----------------------------------------------------------
# Control Variables
# -----------------------------------------------------------
# Z1 - hinctnta: Household total net income (all sources)
#       Unit: Deciles (1 = lowest income, 10 = highest income)
#
# Z2 - trstplt: Trust in politicians
#       Scale: 0 = No trust at all | 10 = Complete trust
#
# Z3 - imwbcnt: Immigrants make the country a worse or better place to live
#       Scale: 0 = Worse place | 10 = Better place

# -----------------------------------------------------------
# Socio-Demographic Controls
# -----------------------------------------------------------
# gndr: Gender (1 = Male | 2 = Female)
# agea: Age of respondent (in years)
# brncntr: Born in country (1 = Yes | 2 = No)

# Only include Ireland and relevant variables. 
df <- read.csv("../../datasets/ESS10.csv")
df_s <- df[df$cntry=="IE", c("euftf","edlvdie","eduyrs","hinctnta","trstplt",
                             "imwbcnt","gndr","agea","brncntr")]
View(df_s)

# Reverse euftf, to measure euroscepticism more intuitively
df_s["euftf_re"] = 10 - df_s[ ,c("euftf")]

# -----------------------------------------------------------
# Categorize the education level
# -----------------------------------------------------------
# Create an empty variable
df_s["edu_cat"] <- NA

# Junior Cycle: values 1-4
df_s[(df_s$edlvdie==1) | (df_s$edlvdie==2) | (df_s$edlvdie==3) | (df_s$edlvdie==4), c("edu_cat")] <- 1 

# Leaving Certificate: values 5-9
df_s[(df_s$edlvdie==5) | (df_s$edlvdie==6) | (df_s$edlvdie==7) | (df_s$edlvdie==8) | (df_s$edlvdie==9), c("edu_cat")] <- 2 

# Advanced Certificate: values 10-12
df_s[(df_s$edlvdie==10) | (df_s$edlvdie==11) | (df_s$edlvdie==12), c("edu_cat")] <- 3 

# Bachelor Degree: values 13-15
df_s[(df_s$edlvdie==13) | (df_s$edlvdie==14) | (df_s$edlvdie==15), c("edu_cat")] <- 4 

# Postgraduate Degree: values 16-18
df_s[(df_s$edlvdie==16) | (df_s$edlvdie==17) | (df_s$edlvdie==18), c("edu_cat")] <- 5 

# Convert into factor variable
df_s$edu_cat <- factor(df_s$edu_cat, 
                       levels = c(1,2,3,4,5),
                       labels = c("Junior Cycle",
                                  "Leaving Certificate",
                                  "Advanced Certificate",
                                  "Bachelor Degree",
                                  "Postgraduate Degree"))
levels(df_s$edu_cat)
typeof(df_s$edu_cat)

# Record missing values
df_s[(df_s == -67) | (df_s == -78) | (df_s == -89) | (df_s == 77) | (df_s == 88) | 
       (df_s == 99) | (df_s == 5555) | (df_s == 7777) | (df_s == 8888) | (df_s == 9999)] <- NA

# -----------------------------------------------------------
# Descriptive Plots
# -----------------------------------------------------------
# Violin plot: European unification attitudes by education category
vioplot(df_s$euftf_re ~ df_s$edu_cat)

# Scatter plot: Relationship between education level and EU attitude
plot(df_s$edlvdie, df_s$euftf_re)

# Jittered scatter plot: Adds noise to reduce overlap of points
plot(jitter(df_s$edlvdie, 2), jitter(df_s$euftf_re, 2))

# -----------------------------------------------------------
# Simple model: socio-demographic variables only
# -----------------------------------------------------------
model_base <- # your answer here
summary(model_base)

# -----------------------------------------------------------
# (1) Hypothesis 1: Education and Euroscepticism
# Expectation: Higher education --> Lower Euroscepticism
# -----------------------------------------------------------

# Continuous measure of education
model1_cont <- # your answer here
summary(model1_cont)

# Categorical measure of education
model1_cat <- # your answer here
summary(model1_cat)

# Change reference category and re-estimate
plot(df_s$edu_cat)
df_s$edu_cat <- relevel(df_s$edu_cat, ref = 2)

model1_cat <- # your answer here
summary(model1_cat)

# -----------------------------------------------------------
# (2) Hypothesis 2: Income and Euroscepticism
# Expectation: Higher income --> Lower Euroscepticism
# -----------------------------------------------------------
model2 <- # your answer here
summary(model2)

# -----------------------------------------------------------
# (3) Hypothesis 3: Political Trust and Euroscepticism
# Expectation: Higher trust --> Lower Euroscepticism
# -----------------------------------------------------------
model3 <- # your answer here
summary(model3)

# -----------------------------------------------------------
# (4) Hypothesis 4: Immigration Attitudes and Euroscepticism
# Expectation: More positive view of immigrants --> Lower Euroscepticism
# -----------------------------------------------------------
model4 <- # your answer here
summary(model4)

# -----------------------------------------------------------
# (5) Full Model: Stepwise inclusion of dimensions
# -----------------------------------------------------------

# Education only
model1 <- # your answer here
summary(model1)

# Add economic dimension
model_eco <- # your answer here
summary(model_eco)

# Add political dimension
model_pol <- # your answer here
summary(model_pol)

# Add cultural dimension
model_cul <- # your answer here
summary(model_cul)

# Add socio-demographic controls
model_final <- # your answer here
summary(model_final)

# Get Latex table
stargazer(model1,model_eco,model_pol,model_cul,model_final)

# How to visualize results?
coefplot(model_final)
coefplot(model1, add=TRUE, col.pts="gray")

# And using the coefplot library
coefplot::multiplot(
  list("Model 1: Education Only" = model1,
       "Model 5: Final Model" = model_final),
  intercept = FALSE
)