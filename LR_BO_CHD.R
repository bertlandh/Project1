setwd("C:/Data/BujuBanton/msc/comp6115kdda/worksheets/Project1")
getwd()
rm(list = ls())
options(scipen = 99999)

#install needed libraries
#installus <- c("caTools")
#install.packages(installus)

library(caTools)

### Step 1 - Load data and get summaries 
dataset <- read.csv("./data/framingham.csv")
summary(dataset)
str(dataset)

#how many missing value in each ro/column
apply(dataset, 2,function(x) sum(is.na(x))) #number of NA per column

## Replacing null values with the mean value
dataset[is.na(dataset$education),"education"] <- mean(dataset$education,na.rm = T)
dataset[is.na(dataset$cigsPerDay),"cigsPerDay"] <- mean(dataset$cigsPerDay, na.rm = T)
dataset[is.na(dataset$BPMeds),"BPMeds"] <- mean(dataset$BPMeds,na.rm = T)
dataset[is.na(dataset$totChol),"totChol"] <- mean(dataset$totChol,na.rm = T)
dataset[is.na(dataset$BMI),"BMI"] <- mean(dataset$BMI,na.rm = T)
dataset[is.na(dataset$heartRate),"heartRate"] <- mean(dataset$heartRate,na.rm = T)
dataset[is.na(dataset$glucose),"glucose"] <- mean(dataset$glucose,na.rm = T)

# target variable dependent variable to factor
dataset$TenYearCHD <- as.factor(dataset$TenYearCHD)

## Rechecking for null values
apply(dataset, 2,function(x) sum(is.na(x))) #number of NA per column

dim(dataset)
head(dataset, 10)

barplot(table(dataset$TenYearCHD), ylab ="Frequency", main = "Distribution of Target Class", col="lightblue")
hist(dataset$age)

## CHD value counts
table(dataset$TenYearCHD)

### Step 2 - Split data into training and testing data 
set.seed(20)
LRDataset <-sample.split(Y=dataset$TenYearCHD, SplitRatio = 0.7)
LRtrainData <- dataset[LRDataset,]
dim(LRtrainData)
LRtestData <- dataset[!LRDataset,]
dim(LRtestData)

### Step 3 - Fit a Logistic Model using training data
#Target Variable = TenYearCHD, Input Vaiables = All, family = binomial (binary target variable) - Logistic regression using logit 
LRModel <- glm(TenYearCHD ~ ., data=LRtrainData, family=binomial(link="logit"))
summary(LRModel)

# log likelihood
logLik(LRModel)

# R-squared
LRll.null <- LRModel$null.deviance/-2
LRll.proposed <- LRModel$deviance/-2
LRr_sq <- (LRll.null - LRll.proposed)/LRll.null
LRr_sq

#P value
LRp_value <- 1 - pchisq(2*(LRll.proposed - LRll.null), df = (length(LRModel$coefficients)-1))
LRp_value

###Step 4 - Use the fitted model to do predictions for the test data
#LRprobTest- Probability for Test data, LRpredTest - Predictions for Test data, actual - Actual Value in test data
LRprobTest=predict(LRModel, LRtestData, type = "response")  # Predict probabilities
LRprobTest

#Recode probability to classification
LRpredVal <- ifelse(LRprobTest >= 0.5, 1, 0)
LRpredTest <- factor(LRpredVal, levels = c(0,1))

#LRprobTest [0:45]
#LRpredTest[0:45]

LRprobTest
LRpredTest

LRactualTest <-LRtestData$TenYearCHD
#LRactualTest[0:5]
LRactualTest

### Step 5 - Create Confusion Matrix and compute the misclassification error
LRt <- table(predictions=LRpredTest, actual = LRactualTest)
LRt # Confusion matrix
hist(LRt)

LRaccuracy <- sum(diag(LRt))/sum(LRt)
LRaccuracy

### ROC and Area Under the Curve
LRROC1 <- roc(LRactualTest, LRprobTest)
plot(LRROC1, col="blue")
LRAUC1 <- auc(LRROC1)
LRAUC1

#A new dataframe with Predicted Prob, Actual Value and Predicted Value
LRpredicted_data <- data.frame(Probs = LRprobTest, Actual_Value=LRactualTest,Predicted_Value = LRpredTest )  #Create data frame with prob and predictions
LRpredicted_data <- LRpredicted_data[order(LRpredicted_data$Probs, decreasing=TRUE),] # Sort on Probabilities
LRpredicted_data$Rank <- 1:nrow(LRpredicted_data) # Add a new variable rank

library(ggplot2)

ggplot(data=LRpredicted_data, aes(x=Rank, y=Probs)) + 
  geom_point(aes(color = LRpredicted_data$Actual_Value)) + xlab("Index") + ylab("Heart Disease Prediction")

### Step 6 - Use model to make predictions on newdata. Note we can specify the newData as data.frame with one or many records
LRnewData <- data.frame(male = 0, age = 50, education = 0, currentSmoker = 0, cigsPerDay = 9, BPMeds = 0, prevalentStroke = 0, prevalentHyp = 0, diabetes = 0, totChol = 236, sysBP = 102.0, diaBP = 71, BMI = 100, heartRate = 100, glucose = 200 )

# terminal nodes
# age
# glucose
# diaBP
# BMI
predProbability <-predict(LRModel, LRnewData, type='response')
predProbability

## Performance measures
# Simplicity =  coefficients
# Accuracy = 0.8552321 0r 0.86
# AUC = 0.7011 0r 0.7

write.csv(dataset,file = "./output/LR_BO_CHD.csv",row.names = FALSE)
dataset <- read.csv("./output/LR_BO_CHD.csv",header = TRUE)
