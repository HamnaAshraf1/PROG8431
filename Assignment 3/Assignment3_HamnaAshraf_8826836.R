##################################################
### PROG8430                                    ##
### Assignment 3                                ## 
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
setwd("E:\\Conestoga\\PROG8431 Data Analysis Math\\Assignment3")

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

if(!require(cluster)){install.packages("cluster")}
library(cluster)

if(!require(factoextra)){install.packages("factoextra")}
library(factoextra)

##################################################
### Read in Data                                ##
##################################################

#Read the Data

#R dataset
load("A3_data_expense.Rdata")

for(variable in 1:ncol(PROG8430_Clst_21F)) {
  hist(PROG8430_Clst_21F[[variable]], 
  main = paste("Histogram of", names(PROG8430_Clst_21F[variable])),
  xlab=names(PROG8430_Clst_21F[variable]))
}

summary(PROG8430_Clst_21F$Entr)
summary(PROG8430_Clst_21F$Food)
summary(PROG8430_Clst_21F$Educ)
summary(PROG8430_Clst_21F$Tran)
summary(PROG8430_Clst_21F$Work)
summary(PROG8430_Clst_21F$Hous)
summary(PROG8430_Clst_21F$Othr)

stat.desc(PROG8430_Clst_21F)


#Q1.1)
norm_HA <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Standardize data
standardized_data_HA <-PROG8430_Clst_21F

for (variable in 1:ncol(PROG8430_Clst_21F)) {
  standardized_data_HA[[variable]] <- norm_HA(PROG8430_Clst_21F[[variable]])
}


stat.desc(standardized_data_HA)


#Q2.1)
par(mfrow = c(3, 3))

for(variable in 1:ncol(standardized_data_HA)) {
  hist(standardized_data_HA[[variable]], 
       main = paste("Histogram of", names(standardized_data_HA[variable])),
       xlab=names(standardized_data_HA[variable]))
}

#Q3.1)
#clustering done on the normalized data
selected_expense_HA<- standardized_data_HA[c("Food", "Work")]

cluster3 <- kmeans(selected_expense_HA, iter.max = 10, centers = 3, nstart = 10)
standardized_data_HA$c3 <- factor(cluster3$cluster) 
cluster3_centers <- data.frame(cluster=factor(1:3), cluster3$centers)

cluster4 <- kmeans(selected_expense_HA, iter.max = 10, centers = 4, nstart = 10)
standardized_data_HA$c4 <- factor(cluster4$cluster)
cluster4_centers<- data.frame(cluster=factor(1:4), cluster4$centers)


cluster5 <- kmeans(selected_expense_HA, iter.max = 10, centers = 5, nstart = 10)
standardized_data_HA$c5 <- factor(cluster5$cluster)
cluster5_centers<- data.frame(cluster=factor(1:5), cluster5$centers)


cluster6 <- kmeans(selected_expense_HA, iter.max = 10, centers = 6, nstart = 10)
standardized_data_HA$c6 <- factor(cluster6$cluster)
cluster6_centers<- data.frame(cluster=factor(1:6), cluster6$centers)


cluster7 <- kmeans(selected_expense_HA, iter.max = 10, centers = 7, nstart = 10)
standardized_data_HA$c7 <- factor(cluster7$cluster)
cluster7_centers<- data.frame(cluster=factor(1:7), cluster7$centers)


#Q3.2)
#Used to find the optimal number of clusters
fviz_nbclust(selected_expense_HA, kmeans, method = "wss")

#Q4.1)
#Based on the wss plot, the optimal value for cluster is choosen as 3
cluster2 <- kmeans(selected_expense_HA, iter.max = 10, centers = 2, nstart = 10)
standardized_data_HA$c2 <- factor(cluster2$cluster)
cluster2_centers<- data.frame(cluster=factor(1:2), cluster2$centers)

#Elbow at k = 3


#k-1 Scatter Plot
ggplot(standardized_data_HA, aes(x=Food, y=Work, color=c2, shape = c2)) + 
  geom_point(alpha=.8) +
  geom_point(data=cluster2_centers, aes(x=Food, y=Work, color= cluster, shape = cluster), size=3, stroke=3) +
  ggtitle("Cluster k = 2 Scatter Plot")

#k Scatter plot
ggplot(standardized_data_HA, aes(x=Food, y=Work, color=c3, shape = c3)) + 
  geom_point(alpha=.8) +
  geom_point(data=cluster3_centers, aes(x=Food, y=Work, color= cluster, shape = cluster), size=3, stroke=3) +
  ggtitle("Cluster k = 3 Scatter Plot")

#k+1 Scatter Plot
ggplot(standardized_data_HA, aes(x=Food, y=Work, color=c4, shape = c4)) + 
  geom_point(alpha=.8) +
  geom_point(data=cluster4_centers, aes(x=Food, y=Work, color= cluster, shape = cluster), size=3, stroke=3) +
  ggtitle("Cluster k = 4 Scatter Plot")



#Q4.3)
 
#clusters = 4
k4_ExpenseSUM_HA <- standardized_data_HA %>% 
  group_by(c4) %>% 
  summarise(count=n(), mean_food = mean(Food), mean_work = mean(Work),
            median_food = median(Food), median_work = median(Work), std_food = sd(Food),
            std_work = sd(Work))


#Q4.4)
cluster_names <- c("Moderate Spender", "Most Work Spenders", "Most Food Spenders", "High Spenders")

k4_ExpenseSUM_HA$c4 <- cluster_names[k4_ExpenseSUM_HA$c4]

print(k4_ExpenseSUM_HA)



