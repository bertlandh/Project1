setwd("C:/Data/BujuBanton/msc/comp6115kdda/worksheets/Project1")
getwd()
rm(list = ls())
options(scipen = 99999)

#install needed libraries
#installus <- c("caTools","pROC","keras")
#install.packages(installus)

library(caTools)
library(pROC)
library(keras)

### Step 1 - Load data and get summaries 
dataset <- read.csv("./data/framingham.csv")
summary(dataset)
str(dataset)

#how many missing value in each ro/column
apply(dataset, 2,function(x) sum(is.na(x))) #number of NA per column

## Replacing null values with the mean value
dataset[is.na(dataset$education),"education"] <- as.integer(mean(dataset$education,na.rm = T))
dataset[is.na(dataset$cigsPerDay),"cigsPerDay"] <- as.integer(mean(dataset$cigsPerDay, na.rm = T))
dataset[is.na(dataset$BPMeds),"BPMeds"] <- as.integer(mean(dataset$BPMeds,na.rm = T))
dataset[is.na(dataset$totChol),"totChol"] <- as.integer(mean(dataset$totChol,na.rm = T))
dataset[is.na(dataset$BMI),"BMI"] <- as.integer(mean(dataset$BMI,na.rm = T))
dataset[is.na(dataset$heartRate),"heartRate"] <- as.integer(mean(dataset$heartRate,na.rm = T))
dataset[is.na(dataset$glucose),"glucose"] <- as.integer(mean(dataset$glucose,na.rm = T))

## Remove null values
dataset <- dataset[!is.na(dataset$education),]
dataset <- dataset[!is.na(dataset$cigsPerDay),]
dataset <- dataset[!is.na(dataset$BPMeds),]
dataset <- dataset[!is.na(dataset$totChol),]
dataset <- dataset[!is.na(dataset$BMI),]
dataset <- dataset[!is.na(dataset$heartRate),]
dataset <- dataset[!is.na(dataset$glucose),]

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
set.seed(7)
NNnewDataset <-sample.split(Y=dataset$TenYearCHD, SplitRatio = 0.7)
NNtrainData <- dataset[NNnewDataset,]
NNtestData <- dataset[!NNnewDataset,]

## NNtrainX contains first 15 columns of NNtrainData
NNtrainX <- NNtrainData[, 1:15]
summary(NNtrainX)

## NNtrainY contains last column of NNtrainData
NNtrainY <- as.data.frame(NNtrainData$TenYearCHD)
colnames(NNtrainY) <- c("Class") #Modify variable name to Class

## testX contains first 15 columns of NNtestData
NNtestX <- NNtestData[, 1:15]
summary(NNtestX)

## NNtestY contains last column of NNtestData
NNtestY <- as.data.frame(NNtestData$TenYearCHD)
colnames(NNtestY) <- c("Class") #Modify variable name to Class

## NNnewTrain -> Change training (X & Y) data into correct format to build model - convert X and Y to vectors and matrices
NNnewTrainY <- as.vector(NNtrainY)
NNnewTrainY <- as.matrix(NNnewTrainY)

write.csv(dataset,file = "./output/NN_BO_CHD.csv",row.names = FALSE)
dataset <- read.csv("./output/NN_BO_CHD.csv",header = TRUE)
