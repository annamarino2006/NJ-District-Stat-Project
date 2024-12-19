##################################################################################

## Project:  STA 215, Fall 2024, Final Project
# Located:   Posit Cloud
# File Name: lorde songs
# Date:      2024_12_18
# Who:       Anna Marino



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
raw_data <- read_delim("raw_data.csv")

data <- raw_data %>%
  filter(complete.cases(.))

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################

table(raw_data$absent_problem)

summary(raw_data$reading_score)
sd(data$reading_score)

table(raw_data$grades_offered)

summary(raw_data$expenses_per_student)
sd(data$expenses_per_student)



##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
boxplot(raw_data$reading_score ~raw_data$absent_problem)
anova <- aov(raw_data$reading_score ~ raw_data$absent_problem)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(raw_data$reading_score, raw_data$spending_per_student)
print(linear_plot)

# add x line and y line for means
meany <- mean(raw_data$reading_score)
meanx <- mean(raw_data$funding_per_student)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(raw_data$reading_score ~ raw_data$spending_per_student, raw_data = raw_data)
summary(linear_relationship)


# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(raw_data$spending_per_student)
plot(raw_data$reading_score)
abline(h = 0, col = "red")
# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(raw_data$reading_score, raw_data$spending_per_student)
chisq.test(raw_data$school_district_demographics, raw_data$absent_problem) data = data
# Residuals vs. Predictor Variable
plot(raw_data$spending_per_student, residuals,
     xlab = "Expenses per Student",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0

model <- lm(reading_score ~ spending_per_student, data = data)

residuals <- residunding_per_student residuals
     xlab = "Spending per Student"
     ylab = "Residuals"
     main = "Residual Plot: Reading Score vs. Spending per Student"
abline(h = 0, col = "red", lwd = 2)

plot(raw_data$spending_per_student)
     xlab = "Spending per Student"
     ylab = "Residuals"
     main = "Residual Plot: Reading Score vs. Spending per Student"
abline(h = 0, col = "red", lwd = 2)

plot(raw_data$funding_per_student, residuals(linear_model), 
     main = "Residual Plot", 
     xlab = "Funding Per Student", 
     ylab = "Residuals")

plot(data$funding_per_student, residuals(linear_model), 
     main = "Residual Plot", 
     xlab = "Funding Per Student", 
     ylab = "Residuals")

plot(raw_data$spending_per_student, residuals(linear_model), 
     main = "Residual Plot", 
     xlab = "Funding Per Student", 
     ylab = "Residuals")


####################  Table 2: contingency table                ####################   
##################################################################################
table(data$grades_offered, data$absent_problem)
chisq.test(data$grades_offered, data$absent_problem)
