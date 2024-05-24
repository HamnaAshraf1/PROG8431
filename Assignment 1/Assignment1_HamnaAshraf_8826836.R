##################################################
### PROG8430                                    ##
### Assignment 1                                ## 
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
setwd("E:\\Conestoga\\PROG8431 Data Analysis Math\\Assignment1")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

##################################################
### Read in Data                                ##
##################################################

#Read the Data

#R dataset

load("A1_data.Rdata")

#Q1 
#1.1 a)
df_HA<-data.frame(
  Income = PROG8431_Assign1_Explore$income,
  Marital_Status = PROG8431_Assign1_Explore$m.status
)

tot_sum_HA <- aggregate(Income ~ Marital_Status, data = df_HA, FUN = sum)
print(tot_sum_HA)

#1.1 b)

max_income_HA <-tot_sum_HA[which.max(tot_sum_HA$Income),]
print(max_income_HA)

#Q1.2 a)

respondants_HA <- data.frame(
  Age = PROG8431_Assign1_Explore$age,
  Nation = PROG8431_Assign1_Explore$nation,
  Children = PROG8431_Assign1_Explore$n.child
)

mean_age_HA<- aggregate(Age ~ Nation, data = subset(respondants_HA, Nation=="Asia"), FUN = mean)
mean_age_HA$Age <- round(mean_age_HA$Age, digits = 2)
print(mean_age_HA)

#Q1.2 b) Mean weighted by number of children
a_respondents <- subset(respondants_HA, Nation == "Asia")
weighted_mean_age_HA <- weighted.mean(a_respondents$Age, w = a_respondents$Children)
cat("The weighted average is:",weighted_mean_age_HA)

#Q1.3 a)

df_political_HA<-data.frame(
  Political = PROG8431_Assign1_Explore$political,
  Score = PROG8431_Assign1_Explore$score,
  Gender= PROG8431_Assign1_Explore$gender
)

political_score_HA <- aggregate(Score ~ Gender, data = df_political_HA , FUN = mean)
print(political_score_HA)

#Q1.3 b)
gender_score_HA <- ifelse(political_score_HA$Score[1] > political_score_HA$Score[2], "Female", "Male")
cat("Gender with the higher mean score:", gender_score_HA)


#Q1.4)
time1_q_HA = quantile(PROG8431_Assign1_Explore$time1, c(.34, .63)) 
cat("34th and 63rd percentile:",time1_q_HA)


#Q2:
#2.1a)
resp_political_HA <- table(PROG8431_Assign1_Explore$political)

pie(resp_political_HA, main = "Respondants with Political Affiliation",
    labels = paste(names(resp_political_HA), "= ", resp_political_HA), cex = 0.7,
    col = heat.colors(length(resp_political_HA)))
#2.1b)
max_res_political_HA<- names(resp_political_HA)[which.max(resp_political_HA)]
cat("Max number of respondants are from:", max_res_political_HA)

#2.1c)
min_res_political_HA<- names(resp_political_HA)[which.min(resp_political_HA)]
cat("Min number of respondants are from:", min_res_political_HA)

#2.2a)
df_treatment_HA<- data.frame(
  Group = PROG8431_Assign1_Explore$group,
  Nation = PROG8431_Assign1_Explore$nation
)

treatment_resp_HA <- aggregate(Group ~ Nation, data = subset(df_treatment_HA, Group== "treat"), FUN= length)
treatment_length<-sum(treatment_resp_HA$Group)

treatment_resp_HA$Percentage <- (treatment_resp_HA$Group / treatment_length) * 100
print(treatment_resp_HA)
#2.2b)
max_treatment_HA <- max(treatment_resp_HA$Percentage)

max_treatment_HA <-treatment_resp_HA[which.max(treatment_resp_HA$Percentage),]
print(max_treatment_HA[c("Nation", "Percentage")])

#2.2c)
min_treatment_HA <- min(treatment_resp_HA$Percentage)

min_treatment_HA <-treatment_resp_HA[which.min(treatment_resp_HA$Percentage),]
print(min_treatment_HA[c("Nation", "Percentage")])

#2.3a)
df_scr_by_region_HA <- data.frame(
  Region = PROG8431_Assign1_Explore$nation,
  Scr = PROG8431_Assign1_Explore$scr,
  Score = PROG8431_Assign1_Explore$score
  
  
)

mean_scr_by_region_HA <- aggregate(Scr ~ Region, data = df_scr_by_region_HA , FUN = mean)


barplot(mean_scr_by_region_HA$Scr, 
        names.arg = mean_scr_by_region_HA$Region,
        main = "Mean Standardized Score by Region",
        xlab = "Region",
        ylab = "Mean Score",
        cex.names = 0.7
)

#2.3b)
#Lowest Mean Score
lowest_scr_HA <- mean_scr_by_region_HA[which.min(mean_scr_by_region_HA$Scr),]
print(lowest_scr_HA)

#2.3c)
#Highest Mean Score
highest_scr_HA <- mean_scr_by_region_HA[which.max(mean_scr_by_region_HA$Scr),]
print(highest_scr_HA)

#2.4a)
income_food_per_HA <- (PROG8431_Assign1_Explore$food) *100

food_hist<- hist(income_food_per_HA, breaks=5, plot = FALSE)

hist(income_food_per_HA, breaks = 5, 
     main="Number of people spending percentage of income on food",
     xlab="Percentage of Income going to food",
     ylab="Number of people", 
     col="lightgreen")

#2.4b)
highest_freq <- which.max(food_hist$count)
income_range_per_HA <- c(food_hist$breaks[highest_freq], food_hist$breaks[highest_freq + 1])

cat("Most people spend", income_range_per_HA[1], "to", income_range_per_HA[2], "percent of their income on food.")

#2.5a)


boxplot(income ~ m.status, data = PROG8431_Assign1_Explore, 
        main = "Distribution of Income by Marital Status", 
        xlab = "Marital Status", 
        ylab = "Income")


mean_incomes <- aggregate(income ~ m.status, data=PROG8431_Assign1_Explore, FUN=mean)

#2.5b)
highest_average_income_HA <- mean_incomes[which.max(mean_incomes$income), ]
cat("Highest Average Income:\n")
print(highest_average_income_HA)

#2.5c)
lowest_average_income_HA <- mean_incomes[which.min(mean_incomes$income), ]
cat("Lowest Average Income:\n")
print(lowest_average_income_HA)

#2.5d)
variability<- aggregate(income ~ m.status, data=PROG8431_Assign1_Explore, FUN=sd)
greatest_variability_HA <- variability[which.max(variability$income), ]

cat("Greatest income variability:\n")
print(greatest_variability_HA)


#2.6a)
hist(PROG8431_Assign1_Explore$income, main="Histogram for Income", 
     xlab="Income", 
     col="lightgreen"     
     )

#2.6b)
hist(PROG8431_Assign1_Explore$scr, main="Histogram for Standardized Score", 
     xlab="Standardized Score", 
     col="orange"     
)

#2.6c)
plot(PROG8431_Assign1_Explore$income, PROG8431_Assign1_Explore$scr, 
     main="Income vs. Standardized Score", 
     xlab="Income", 
     ylab="Standardized Score",
     col = "blue")

#2.6d) Answer in word document

#2.6e)
cor_income_sts_HA <- cor(PROG8431_Assign1_Explore$income, PROG8431_Assign1_Explore$scr)
cat("The correlation between Income and Standardized Score is:", cor_income_sts_HA)