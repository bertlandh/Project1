#Author BH

setwd("//jr1/c$/Data/BujuBanton/msc/comp6115kdda/worksheets/Project1/")
getwd()
rm(list = ls())
options(scipen = 99999)

#install needed libraries
#installus <- c("rpart","raprt.plot","pROC","caTools","keras","readr","dplyr","party","partykit")
#install.packages(installus)

#loading libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(caret)
library(elasticnet)
library(knitr)
library(matrixStats)
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
dataset <- read.csv("./data/ENB2012_data.csv") #read in the data
  
head(dataset)
summary(dataset)
class(dataset)
str(dataset)
names(dataset)

colnames(dataset) <- c('Relative_Compactness',
                      'Surface_Area',
                      'Wall_Area',
                      'Roof_Area',
                      'Overall_Height',
                      'Orientation',
                      'Glazing_Area',
                      'Glazing_Area_Distribution',
                      'Heating_Load',
                      'Cooling_Load')

#cleaning NAs
colSums(is.na(dataset))

#cleaning blank observations
colSums(dataset == "")

boxplot(dataset)

#scaling the data set
dataset[,1:8] <- scale(dataset[,1:8])

boxplot(dataset)

#check the mean of each feature to make sure that the data set is scaled. Means should be 0
options(digits = 3)
format(colMeans(dataset[,1:8]), scientific = FALSE)

#check the standard deviation. Should be 1
dataset %>% select(-Heating_Load,-Cooling_Load) %>% summarise_if(is.numeric,sd)

#Data Visualization

#the density of heating load
dataset %>% ggplot(aes(Heating_Load)) +
  geom_density(aes(fill = "red", color = "red")) +
  xlab("heating lab") +
  ggtitle("Density of Heating Load") +
  theme_economist() +
  theme(legend.position = "none")

#the density of Cooling load
dataset %>% ggplot(aes(Cooling_Load)) +
  geom_density(aes(fill = "blue", color = "blue")) +
  xlab("cooling lab") +
  ggtitle("Density of Cooling Load") +
  theme_economist() +
  theme(legend.position = "none")

#Both heating and cooling density look similar. scatter plot of surface area and heating load
dataset %>% ggplot(aes(Surface_Area,Heating_Load)) +
  geom_point(aes(color = "red")) +
  xlab("surface area") +
  ylab("heating load")+
  ggtitle("Surface area and heat") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of roof area and heating load
dataset %>% ggplot(aes(Roof_Area,Heating_Load)) +
  geom_point(aes(color = "red")) +
  xlab("roof area") +
  ylab("heating load")+
  ggtitle("Roof area and heat") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of compactness and heating load
dataset %>% ggplot(aes(Relative_Compactness,Heating_Load)) +
  geom_point(aes(color = "red")) +
  xlab("relative compactness") +
  ylab("heating load") +
  ggtitle("Relative Compactness and Heating Load") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of surface area and cooling load
dataset %>% ggplot(aes(Surface_Area,Cooling_Load)) +
  geom_point(aes(color = "blue")) +
  xlab("surface area") +
  ylab("cooling load")+
  ggtitle("Surface area and cooling") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of roof area and cooling load
dataset %>% ggplot(aes(Roof_Area,Cooling_Load)) +
  geom_point(aes(color = "blue")) +
  xlab("roof area") +
  ylab("cooling load")+
  ggtitle("Roof area and cooling") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of compactness and cooling load
dataset %>% ggplot(aes(Relative_Compactness,Cooling_Load)) +
  geom_point(aes(color = "blue")) +
  xlab("relative compactness") +
  ylab("cooling load") +
  ggtitle("Relative Compactness and Cooling Load") +
  theme_economist() +
  theme(legend.position = "none")

### Step 2 - Split data into training and testing data 
set.seed(1)
DTDataset <-sample.split(Y=dataset$Heating_Load, SplitRatio = 0.7)
DTtrainData <- dataset[DTDataset,]
dim(DTtrainData)
hist(DTtrainData$Heating_Load)
DTtestData <- dataset[!DTDataset,]
dim(DTtestData)

### Step 3 - Fit a Decision Tree using training data
#DTmodel <- rpart(Heating_Load ~ .,method="class", data=DTtrainData, parms = list (split ="information gain"), control = rpart.control(minsplit = 10, maxdepth = 5))
#DTmodel <- rpart(Heating_Load ~ .,method="class", data=DTtrainData, parms = list (split ="gini"), control = rpart.control(minsplit = 15, maxdepth = 5))  
#DTmodel <- rpart(Heating_Load ~ .,method="class", data=DTtrainData)
DTmodel <- rpart(Heating_Load ~ ., data=DTtrainData)  

# Fitting the model
#rpart.plot(DTmodel, type=3, extra = 101, fallen.leaves = F, cex = 0.8) ##try extra with 2,8,4, 101
rpart.plot(DTmodel,box.palette="blue")

summary(DTmodel) # detailed summary of splits
DTmodel #prints the rules

###Step 4 - Use the fitted model to do predictions for the test data

DTpredTest <- predict(DTmodel, DTtestData, type="class")
DTprobTest <- predict(DTmodel, DTtestData, type="prob")

DTactualTest <- DTtestData$Heating_Load

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

# terminal nodes
# age
# glucose
# diaBP
# BMI

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
# Accuracy = 0.8363493 0r 0.84
# AUC = 0.6627 0r 0.66
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

write.csv(dataset,file = "./output/DT_B.csv",row.names = FALSE)
dataset <- read.csv("./output/DT_B.csv",header = TRUE)
