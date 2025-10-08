### Problemset 1, Qualitative Mthods I; Trinity College Dublin 25/26; Onno Steenweg ###

## Clear working environment
rm(list=ls())

## Set working directory
setwd("C:/Users/onnos/Documents/GitHub/StatsI_2025/problemSets/PS01")

## Load data; I used iq instead of y; it'S easier to operate with
iq <- c (105 , 69 , 86 , 100 , 82 , 111 , 104 , 110 , 87 , 108 , 87 , 90 , 94 , 113 , 112 , 98 ,
        80 , 97 , 95 , 111 , 114 , 89 , 95 , 126 , 98)

## Construct length indicator
n <- length(iq)

# Sample mean; (there are no NAs, so no na.rm = T necessary for all analyses)
sample_mean <- mean(iq)

# Sample standard deviation
sample_sd <- sd(iq)

# t critical value for 90% confidence interval; use t because n<=30
t90 <- qt((1 + 0.90)/2, df = n - 1)

# Lower and upper bounds
lower_90 <- sample_mean - (t90 * (sample_sd / sqrt(n)))
upper_90 <- sample_mean + (t90 * (sample_sd / sqrt(n)))

# Combine into a confidence interval
confint90 <- c(lower_90, upper_90)

# result
confint90 
## the 90% CI is 93.96 and 102.92

