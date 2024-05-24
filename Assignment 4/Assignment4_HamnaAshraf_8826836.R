##################################################
### PROG8430                                    ##
### Assignment 4                                ## 
##################################################
#                                               ##
##################################################
# Written by Hamna Ashraf
# ID: 8826836
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory to an appropriate location
setwd("E:\\Conestoga\\PROG8431 Data Analysis Math\\Assignment 4")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################


if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

if(!require(caret)){install.packages("caret")}
library(caret)

if(!require(fastDummies)){install.packages("fastDummies")}
library(fastDummies)

if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)

if(!require(corrgram)){install.packages("corrgram")}
library(corrgram)


##################################################
### Read in Data                                ##
##################################################

#Read the Data

#R dataset
load("A4_data_political.Rdata")
data_pol_HA <- A4_data

#Q1.1
missing <- colSums(is.na(data_pol_HA))
missing_percent_HA <-  (missing / nrow(data_pol_HA)) * 100

#let's remove missing values greater than 50% from our data
data_pol_HA <- data_pol_HA[, missing_percent_HA < 50]

#Q1.2
#our data has non-numeric data for columns e.g m.status (divorced, married, single), gender, nation etc
numeric_data<- data_pol_HA[sapply(data_pol_HA, is.numeric)]

#get the variance of data
variances <- sapply(numeric_data, var)

#We want to keep columns with variances greater than 0.3
#Columns that have variance < 0.3 are ones that have values very close to one another.

numeric_high_var <- numeric_data[, names(variances[variances > 0.3])]

#columns with non-numeric data 
data_non_numeric <- data_pol_HA[, !sapply(data_pol_HA, is.numeric)]

#combining columns with variance > 0.3 and the non-numeric columns 
data_pol_HA <- cbind(numeric_high_var, data_non_numeric)

#Q1.3

# Get numeric data from the updated dataset
numeric_data <- data_pol_HA[sapply(data_pol_HA, is.numeric)]

# Find the correlation matrix first
corr_matrix <- cor(numeric_data, use = 'all.obs') 

# Check correlation that is greater than a certain threshold
high_correlation <- findCorrelation(corr_matrix, cutoff = 0.8)

# All columns with low correlation, i.e., correlation < 0.8
# If high_correlation is empty then consider the numeric data without considering the empty high_correlation value

if (length(high_correlation) > 0) {
  numeric_low_corr <- numeric_data[, -high_correlation, drop = FALSE]
} else {
  numeric_low_corr <- numeric_data
}

# Get non-numeric data
data_non_numeric <- data_pol_HA[, !sapply(data_pol_HA, is.numeric), drop = FALSE]

# Combine the non-numeric data with the filtered numeric data
data_pol_HA <- cbind(numeric_low_corr, data_non_numeric)


#Q2.1 
#this will create column for each category in all the categorical data
data_pol_HA <- dummy_cols(data_pol_HA, remove_selected_columns = TRUE)


#Q3.1
numeric_data <- data_pol_HA[sapply(data_pol_HA, is.numeric)]

#get non-binary data which 
non_binary_numeric <- numeric_data[, sapply(numeric_data, function(x) length(unique(x)) > 2)]

par(mfrow = c(1, 7))

for (i in names(non_binary_numeric)) {
  boxplot(non_binary_numeric[[i]], main = i)
}

#Q3.2
#Boxplot stats for number of children
bp_stats <- boxplot(data_pol_HA$n.child, plot = FALSE) 

# If you want to print the stats with more context:
cat("Min: ", bp_stats$stats[1, ], "\n",
    "1st Quartile: ", bp_stats$stats[2, ], "\n",
    "Median: ", bp_stats$stats[3, ], "\n",
    "3rd Quartile: ", bp_stats$stats[4, ], "\n",
    "Max: ", bp_stats$stats[5, ], "\n")

median_child<- bp_stats$stats[3, ]

# Let's replace the outlier value with the median
data_pol_HA$n.child[data_pol_HA$n.child > 50] <- median_child

#let's see the boxplot after the outlier has been removed
numeric_data <- data_pol_HA[sapply(data_pol_HA, is.numeric)]
non_binary_numeric <- numeric_data[, sapply(numeric_data, function(x) length(unique(x)) > 2)]

par(mfrow = c(1, 7))

for (i in names(non_binary_numeric)) {
  boxplot(non_binary_numeric[[i]], main = i)
}

#Q4.1
cor_matrix <- cor(numeric_data)
print(cor_matrix)


# Create a graphical correlation matrix
corrplot(cor_matrix, method = "circle", tl.cex = 0.6) 

#check the values of correlation
corrplot(cor_matrix, method = "circle", order = "hclust",
         tl.cex = 0.6, tl.col = "black", tl.srt = 45,
         number.cex = 0.5, addCoef.col = "black")

#Q5.1
lm_pol_score <- lm(Pol ~ score, data = data_pol_HA)
summary(lm_pol_score)

#scatter plot
plot(data_pol_HA$score, data_pol_HA$Pol, 
     main = "Scatter plot 'Score' vs 'Plot' with regression line",
     xlab = "Score", 
     ylab = "Pol", 
     col = "orange",
     pch = 20)
#regression line
abline(lm_pol_score, 
       col = "red",#color 
       lwd = 2)#line width

#5.2
lm_pol_scr <- lm(Pol ~ scr, data = data_pol_HA)

#scatter plot
plot(data_pol_HA$scr, data_pol_HA$Pol, 
     main = "Scatter plot 'Scr' vs 'Plot' with regression line",
     xlab = "Scr", 
     ylab = "Pol", 
     col = "pink",
     pch = 20)
#regression line
abline(lm_pol_scr, 
       col = "purple",#color 
       lwd = 2)#line width

summary(lm_pol_scr)

#Q6.1
pol_baslineline_lm <- lm(Pol ~ ., data = data_pol_HA, na.action=na.omit)
#Pol~. takes all the columns except Pol for predictors
summary(pol_baslineline_lm)

backward_model <- step(pol_baslineline_lm, direction = "backward")
summary(backward_model)

#6.3
baselineFit <- predict(pol_baslineline_lm)
baselineRes <- residuals(pol_baslineline_lm)

backwardFit <- predict(backward_model)
backwardRes <- residuals(backward_model)


shapiro.test(baselineRes)
shapiro.test(backwardRes)



#Graphically

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(pol_baslineline_lm)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(backward_model)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


