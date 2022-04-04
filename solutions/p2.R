setwd("C:/Data/BujuBanton/msc/comp6115kdda/worksheets/Project1")
getwd()
rm(list = ls())
#options(scipen = 99999)

#Authors BH&OTB
library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)
library(rpart)
library(pROC)

### Step 1 - Load data and get summaries 
dataset <- read.csv("framingham.csv") %>% # read in the data
   mutate(TenYearCHD = factor(TenYearCHD)) # target variable dependent variable to factor
         

summary(dataset)
str(dataset)

### Step 2 - Split data into training and testing data 
set.seed(1)
DTDataset <-sample.split(Y=dataset$TenYearCHD, SplitRatio = 0.7)
DTtrainData <- dataset[DTDataset,]
dim(DTtrainData)
DTtestData <- dataset[!DTDataset,]
dim(DTtestData)

### Step 3 - Fit a Decision Tree using training data
#DTmodel <- rpart(TenYearCHD ~ .,method="class", data=DTtrainData, parms = list (split ="information gain"), control = rpart.control(minsplit = 10, maxdepth = 5))
#DTmodel <- rpart(TenYearCHD ~ .,method="class", data=DTtrainData, parms = list (split ="gini"), control = rpart.control(minsplit = 15, maxdepth = 5))  
DTmodel <- rpart(TenYearCHD ~ .,method="class", data=DTtrainData)  

# Fitting the model
#rpart.plot(DTmodel, type=3, extra = 101, fallen.leaves = F, cex = 0.8) ##try extra with 2,8,4, 101
rpart.plot(DTmodel) ##try extra with 2,8,4, 101

summary(DTmodel) # detailed summary of splits
DTmodel #prints the rules

###Step 4 - Use the fitted model to do predictions for the test data

DTpredTest <- predict(DTmodel, DTtestData, type="class")
DTprobTest <- predict(DTmodel, DTtestData, type="prob")

DTactualTest <- DTtestData$TenYearCHD

### Step 5 - Create Confusion Matrix and compute the misclassification error
DTt <- table(predictions= DTpredTest, actual = DTactualTest)
DTt # Confusion matrix
DTaccuracy <- sum(diag(DTt))/sum(DTt)
DTaccuracy 

## Visualization of probabilities
hist(DTprobTest[,2], breaks = 100)

### ROC and Area Under the Curve
DTROC <- roc(DTactualTest, DTprobTest[,2])
plot(DTROC, col="blue")
DTAUC <- auc(DTROC)
DTAUC

#A new dataframe with Predicted Prob, Actual Value and Predicted Value
DTpredicted_data <- data.frame(Probs = DTprobTest, Actual_Value= DTactualTest ,Predicted_Value = DTpredTest )  #Create data frame with prob and predictions
DTpredicted_data <- DTpredicted_data[order(DTpredicted_data$Probs.1, decreasing=TRUE),] # Sort on Probabilities
DTpredicted_data$Rank <- 1:nrow(DTpredicted_data) # Add a new variable rank

library(ggplot2)

ggplot(data=DTpredicted_data, aes(x=Rank, y=Probs.1)) + 
  geom_point(aes(color = DTpredicted_data$Actual_Value)) + xlab("Index") + ylab("Predicted Probability of getting Cardiovascular Diseases")

### Step 6 - Use model to make predictions on newdata. Note we can specify the newData as data.frame with one or many records
DTnewData <- data.frame(male = 0, age = 50, education = 0, currentSmoker = 0, cigsPerDay = 9, BPMeds = 0, prevalentStroke = 0, prevalentHyp = 0, diabetes = 0, totChol = 236, sysBP = 102.0, diaBP = 71, BMI = 100, heartRate = 100, glucose = 200 )

 
#2) age< 46.5 1199  80 0 (0.93327773 0.06672227) *
#12) glucose< 161.5 1041 150 0 (0.85590778 0.14409222) *
#  13) glucose>=161.5 9   1 1 (0.11111111 0.88888889) *
#56) diaBP>=70.5 624 154 0 (0.75320513 0.24679487) *
#  57) diaBP< 70.5 12   3 1 (0.25000000 0.75000000) *
#58) BMI>=26.6 29  11 0 (0.62068966 0.37931034) *
#  59) BMI< 26.6 17   4 1 (0.23529412 0.76470588) *
#  15) glucose>=116.5 36  10 1 (0.27777778 0.72222222) *
#
##Convert to factors DTnewData
#
#DTnewData$male <- as.factor(DTnewData$male)
#DTnewData$currentSmoker <- as.factor(DTnewData$currentSmoker)
#DTnewData$BPMeds <- as.factor(DTnewData$BPMeds)
#DTnewData$prevalentStroke <- as.factor(DTnewData$prevalentStroke)
#DTnewData$prevalentHyp <- as.factor(DTnewData$prevalentHyp)
#DTnewData$diabetes <- as.factor(DTnewData$diabetes)
##
#
DTpredProbability <-predict(DTmodel, DTnewData, type='prob')
DTpredProbability
#
## Performance measures - 
#
#set.seed(1), gini
# Simplicity = 15 leaves
# Accuracy = 0.8394965 0r 0.84
# AUC = 0.6621 0r 0.66
#
#set.seed(1), information
# Simplicity = 10 leaves
# Accuracy = 0.8394965 0r 0.84
# AUC = 0.6621 0r 0.66
#
#set.seed(1), blank
# Simplicity = 10 leaves
# Accuracy = 0.8371361 0r 0.84
# AUC = 0.6627 0r 0.66
#
#
#* denotes terminal node
#
#1) root 2967 451 0 (0.84799461 0.15200539)  
#2) age< 46.5 1199  80 0 (0.93327773 0.06672227) *
#12) glucose< 161.5 1041 150 0 (0.85590778 0.14409222) *
#  13) glucose>=161.5 9   1 1 (0.11111111 0.88888889) *
#56) diaBP>=70.5 624 154 0 (0.75320513 0.24679487) *
#  57) diaBP< 70.5 12   3 1 (0.25000000 0.75000000) *
#58) BMI>=26.6 29  11 0 (0.62068966 0.37931034) *
#  59) BMI< 26.6 17   4 1 (0.23529412 0.76470588) *
#  15) glucose>=116.5 36  10 1 (0.27777778 0.72222222) *
#
### Step 7 - EXAMINING STABILITY - Creating Decile Plots for Class 1 or 0 Sort 
#
#-----Create empty df-------
DTdecile<- data.frame(matrix(ncol=4,nrow = 0))
colnames(DTdecile)<- c("Decile","per_correct_preds","No_correct_Preds","cum_preds")
#-----Initialize variables
DTnum_of_deciles = 10
DTObs_per_decile <- nrow(DTpredicted_data)/DTnum_of_deciles
DTdecile_count = 1
DTstart = 1
DTstop = (DTstart-1) + DTObs_per_decile
DTprev_cum_pred <- 0
DTx = 0

#-----Loop through DF and create deciles
while (DTx < nrow(DTpredicted_data)) {
  DTsubset <- DTpredicted_data[c(DTstart:DTstop),]
  DTcorrect_count <- ifelse(DTsubset$Actual_Value == DTsubset$Predicted_Value, 1, 0)
  DTno_correct_Preds <- sum(DTcorrect_count, na.rm = TRUE)
  DTper_correct_Preds <- (DTno_correct_Preds / DTObs_per_decile) * 100
  DTcum_preds <- DTno_correct_Preds+DTprev_cum_pred
  DTaddRow <- data.frame("Decile" = DTdecile_count, "per_correct_preds" = DTper_correct_Preds, "No_correct_Preds" = DTno_correct_Preds, "cum_preds" = DTcum_preds)
  DTdecile <- rbind(DTdecile,DTaddRow)
  DTprev_cum_pred <- DTprev_cum_pred+DTno_correct_Preds
  DTstart <- DTstop + 1
  DTstop = (DTstart - 1) + DTObs_per_decile
  DTx <- DTx+DTObs_per_decile
  DTdecile_count <- DTdecile_count + 1
}
#------Stability plot (correct preds per decile)
plot(DTdecile$Decile,DTdecile$per_correct_preds,type = "l",xlab = "Decile",ylab = "Percentage of correct predictions",main="Stability Plot for Class 1")

write.csv(dataset,file = "whaterthenameis.csv",row.names = FALSE)
dataset <- read.csv("whaterthenameis.csv",header = TRUE)
