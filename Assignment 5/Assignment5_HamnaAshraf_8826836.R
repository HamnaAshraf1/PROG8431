##################################################
### PROG8430                                    ##
### Assignment 5                                ## 
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
setwd("E:\\Conestoga\\PROG8431 Data Analysis Math\\Assignment 5")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)
        
if(!requireNamespace("pROC", quietly = TRUE)) {install.packages("pROC")}
library(pROC)

if(!requireNamespace("e1071", quietly = TRUE)) {install.packages("e1071")}
library(e1071)

if(!requireNamespace("MASS", quietly = TRUE)) {install.packages("MASS")}
library(MASS)

##################################################
### Read in Data                                ##
##################################################

#Read the Data

#R dataset
load("A5_tumor_data.Rdata")
data_tumor_HA <- Tumor_data

#Q1.1:
missing <- colSums(is.na(data_tumor_HA))
missing_percent_HA <-  (missing / nrow(data_tumor_HA)) * 100

print(missing)
print(missing_percent_HA)

#let's remove missing values greater than 50% from our data
data_tumor_HA <- data_tumor_HA[, missing_percent_HA < 50]

#Q2.1
cor_matrix <- cor(data_tumor_HA, use = "complete.obs")

#check the values of correlation
corrplot(cor_matrix, method = "circle", order = "hclust",
         tl.cex = 0.6, tl.col = "black", tl.srt = 45,
         number.cex = 0.5, addCoef.col = "black")

print(cor_matrix)

ct_brain <- table(data_tumor_HA$Brain, data_tumor_HA$Out)
ct_marrow <- table(data_tumor_HA$Marrow, data_tumor_HA$Out)

# Perform Chi-squared tests
chi_squared_brain <- chisq.test(ct_brain)
chi_squared_marrow <- chisq.test(ct_marrow)

# Output the results of the Chi-squared tests
print(chi_squared_brain)
print(chi_squared_marrow)

#Q3.1:
model <- glm(Out ~ ., data = data_tumor_HA, family = binomial)

#stepwise model selection:
#this model will give us the formula for the model that has best AIC
stepwise <- step(model, direction = "both", trace = FALSE)

#Q3.2:
pred <- predict(stepwise, type = "response")
pred_class <- numeric(length(pred))  

for (i in 1:length(pred)) {
  #if probability is greater than threshold 0.5 then assign 1 otherwise 0
  if (pred[i] > 0.5) {
    pred_class[i] <- 1
  } else {
    pred_class[i] <- 0
  }
}

#confusion matrix
cm <- table(Predicted = pred_class, Actual = data_tumor_HA$Out)
print(cm)

#Q3.3 
#a. Accuracy 

#(TP + TN) / (TP + TN + FP + FN)
accuracy <- sum(diag(cm)) / sum(cm) 

#b. Specificity 

#TN / (TN + FP)
specificity <- cm[1,1] / (cm[1,1] + cm[1,2])

#c. Sensitivity 

#TP / (TP + FN)
sensitivity <- cm[2,2] / (cm[2,2] + cm[2,1])

#d. Precision

#TP / (TP + FP)
precision <- cm[2,2] / (cm[2,2] + cm[1,2])

print(paste("Accuracy:", accuracy))
print(paste("Specificity:", specificity))
print(paste("Sensitivity:", sensitivity))
print(paste("Precision:", precision))


#Q3.4:
curve_roc <- roc(response = data_tumor_HA$Out, predictor = pred)

#ROC curve
plot(curve_roc, main="ROC Curve", col="red", lwd=2)

# Calculate the AUC
auc_value <- auc(curve_roc)
cat("AUC:", auc_value, "\n")

#3.5
start <- Sys.time()
stepwise_temp <- step(model, direction = "both", trace = FALSE)
train_time <- Sys.time() - start
cat("Training time:", train_time)

start <- Sys.time()
predicted_prob <- predict(stepwise_temp, type = "response")
pred_time <- Sys.time() - start
cat("Prediction time:", pred_time)

#Q4.1:
data_tumor_HA$Age <- as.factor(data_tumor_HA$Age)
data_tumor_HA$Sex <- as.factor(data_tumor_HA$Sex)
data_tumor_HA$Bone <- as.factor(data_tumor_HA$Bone)
data_tumor_HA$Marrow <- as.factor(data_tumor_HA$Marrow)
data_tumor_HA$Lung <- as.factor(data_tumor_HA$Lung)
data_tumor_HA$Pleura <- as.factor(data_tumor_HA$Pleura)
data_tumor_HA$Liver <- as.factor(data_tumor_HA$Liver)
data_tumor_HA$Brain <- as.factor(data_tumor_HA$Brain)
data_tumor_HA$Skin <- as.factor(data_tumor_HA$Skin)
data_tumor_HA$Neck <- as.factor(data_tumor_HA$Neck)
data_tumor_HA$Supra <- as.factor(data_tumor_HA$Supra)
data_tumor_HA$Axil <- as.factor(data_tumor_HA$Axil)
data_tumor_HA$Media <- as.factor(data_tumor_HA$Media)
data_tumor_HA$Out <- as.factor(data_tumor_HA$Out)

#4.2:
start <- Sys.time()
naive_bayes_model <- naiveBayes(Out ~ ., data = data_tumor_HA)
train_time_naive <- Sys.time() - start

start <- Sys.time()
pred_class_naive <- predict(naive_bayes_model, data_tumor_HA)
pred_time_naive <- Sys.time() - start


#4.3
cm_naive <- table(Predicted = pred_class_naive, Actual = data_tumor_HA$Out)
print(cm_naive)

#Q4.4 
#a. Accuracy 

#(TP + TN) / (TP + TN + FP + FN)
accuracy <- sum(diag(cm_naive)) / sum(cm_naive) 

#b. Specificity 

#TN / (TN + FP)
specificity <- cm_naive[1,1] / (cm_naive[1,1] + cm_naive[1,2])

#c. Sensitivity 

#TP / (TP + FN)
sensitivity <- cm_naive[2,2] / (cm_naive[2,2] + cm_naive[2,1])

#d. Precision

#TP / (TP + FP)
precision <- cm_naive[2,2] / (cm_naive[2,2] + cm_naive[1,2])

print(paste("Accuracy:", accuracy))
print(paste("Specificity:", specificity))
print(paste("Sensitivity:", sensitivity))
print(paste("Precision:", precision))


#4.5:
cat("Training time: ", train_time_naive)
cat("Prediction Time: ", pred_time_naive)

#5.2:
start <- Sys.time()
lda_model <- lda(Out ~ ., data = data_tumor_HA)
train_time_lda <- Sys.time() - start

start <- Sys.time()
pred_class_lda <- predict(lda_model, data_tumor_HA)$class  
pred_time_lda <- Sys.time() - start  


#5.3:
cm_lda <- table(Predicted = pred_class_lda, Actual = data_tumor_HA$Out)
print(cm_lda)

#5.4:

#a. Accuracy 

#(TP + TN) / (TP + TN + FP + FN)
accuracy <- sum(diag(cm_lda)) / sum(cm_lda) 

#b. Specificity 

#TN / (TN + FP)
specificity <- cm_lda[1,1] / (cm_lda[1,1] + cm_lda[1,2])

#c. Sensitivity 

#TP / (TP + FN)
sensitivity <- cm_lda[2,2] / (cm_lda[2,2] + cm_lda[2,1])

#d. Precision

#TP / (TP + FP)
precision <- cm_lda[2,2] / (cm_lda[2,2] + cm_lda[1,2])

print(paste("Accuracy:", accuracy))
print(paste("Specificity:", specificity))
print(paste("Sensitivity:", sensitivity))
print(paste("Precision:", precision))

#5.5:
cat("Training time: ", train_time_lda)
cat("Prediction Time: ", pred_time_lda)

