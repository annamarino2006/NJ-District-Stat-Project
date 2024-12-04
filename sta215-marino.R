## Project:  STA 215, Spring 2024, Final Project
# Located:   Posit Cloud
# File Name: lorde songs
# Date:      2024_2_29
# Who:       Kevin Janas



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

table(data$absent_problem)

summary(data$reading_score)
sd(data$reading_score)

summary(data$students_per_district)
sd(data$students_per_district)

summary(data$student_teacher_ratio)
sd(data$student_teacher_ratio)



##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
boxplot(data$reading_score ~data$absent_problem)
anova <- aov(data$reading_score ~ data$absent_problem)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$students_per_district, data$student_teacher_ratio)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$student_teacher_ratio)
meanx <- mean(data$students_per_district)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(data$student_teacher_ratio ~ students_per_district, data = data)
summary(linear_relationship)


# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(data$students_per_district, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$grades_offered, data$absent_problem)
chisq.test(data$grades_offered, data$absent_problem)