##################################################
### PROG8430                                    ##
### Assignment 2                                ## 
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
setwd("E:\\Conestoga\\PROG8431 Data Analysis Math\\Assignment 2")

options(scipen=9)

##################################################
###  Packages Installed                         ##
##################################################
install.packages("forecast")
install.packages(tseries)
##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it
library(tseries)
library("smooth")


##################################################
### Read in Data                                ##
##################################################

#Read the Data

#R dataset

load("A2_data_temphist.Rdata")

#Checking the first 10 lines of data
head(Ayr_temphist,10)

#Checking structure of data:
str(Ayr_temphist)

temp<- Ayr_temphist[, "Temp"]

print(Ayr_temphist)

#It shows in the print statement that Start is 1968, End 2003, freq =1

#Q1.1:
#Creating Appropriate Time Series Datatype
data_temp = ts(temp, start = 1968, end = 2003, frequency = 1)
print(data_temp)

#Q2: 
#2.1)
summary(data_temp)

#2.2)
plot(data_temp, 
     main = "Change in Temperature over the Years (1968 - 2003)",
     xlab = "Year",
     ylab = "Temperature",
     col = "red")

#2.3)

#Decompose and check monthly trends for years 1988-1990:

monthly_data <- ts(temp, start = c(1988, 1), frequency = 12)
monthly_decompose <- decompose(monthly_data)
plot(monthly_decompose)


#2.4)

deseasonalize_monthly_data <- monthly_data - monthly_decompose$seasonal
plot(deseasonalize_monthly_data,
     main='Deseasonalized Data 1988-1990',
     ylab = 'Deseasonalized')



#2.5) Explanation in doc

#Q3
#3.1)

mov_avg_4 <- stats::filter(data_temp, rep(1/4, 4), sides=2) 
mov_avg_8 <- stats::filter(data_temp, rep(1/8, 8), sides=2)
mov_avg_12 <- stats::filter(data_temp, rep(1/12, 12), sides=2)

plot(data_temp,
     main="Time Series with different moving averages",
     xlab="Years",
     ylab="Temperature")

lines(mov_avg_4, col="darkorange")
lines(mov_avg_8, col="darkgreen")
lines(mov_avg_12, col="red")
legend(legend = c("Original", "Move Avg - 4", "Move Avg - 8", "Move Avg - 12"), 
       cex=0.5,
       col=c("black", "darkorange", "darkgreen", "red"), 
       lty=1, 
       "topright")

#3.2)

adf_test<- adf.test(data_temp)
print(adf_test)

#3.3)
acf(data_temp, 
    main="Auto-Correlation Function (ACF) of Temperature")


#Q4
#4.1)

#Single Moving Average
single_mov_avg <- sma(data_temp)

#Forcast
mov_avg_forecast <- forecast::forecast(single_mov_avg, h=5, level=0.75)

plot(mov_avg_forecast,
     main = "Forcast for 5 years (75% prediction) with Moving Average" , 
     xlab = "Years", 
     ylab = "Temperature")


#4.2)
#Exponential Smoothing
exponential_smoothing <- es(data_temp)
#Forcast
expo_avg_forecast <- forecast::forecast(exponential_smoothing, h=5,level=0.75)
plot(expo_avg_forecast,
     main = "Forecast for 5 years (75% prediction) with Exponential Smoothing" , 
     xlab = "Years", 
     ylab = "Temperature")

#4.3) #Explained in Word doc
print(mov_avg_forecast)


print(expo_avg_forecast)




