library(caTools)
library(pROC)

### Step 1 - Load data and get summaries
#file enerdata_clean_transformed.csv
EEDataset <- read.csv(file.choose())
summary(EEDataset)
str(EEDataset)
#EEDataset$Y1_ctype <- as.factor(EEDataset$Y1_ctype)
str(EEDataset)
dim(EEDataset)
head(EEDataset, 10)  #tail prints from back
is.na(EEDataset)
cor(EEDataset$X1, EEDataset$X2)  #[,-9] to exclude the 9th Class variable
pairs(EEDataset[,-9])
table(EEDataset$Y1_ctype)
barplot(table(EEDataset$Y1_ctype), ylab ="Frequency", main = "Distribution of Target Class", col="lightblue")
hist(EEDataset$X1)

### Step 2 - Split data into training and testing data
EEDataset$is_high <- EEDataset$Y1_ctype
EEDataset$is_high[EEDataset$Y1_ctype == "High"] <- 1
EEDataset$is_high[EEDataset$Y1_ctype == "Low"] <- 0
EEDataset$Y1_ctype <- NULL
EEDataset
EEDataset$is_high <- as.numeric(EEDataset$is_high)

set.seed(20)
newDataset <-sample.split(Y=EEDataset$is_high, SplitRatio = 0.7)
trainData <- EEDataset[newDataset,]
dim(trainData)
testData <- EEDataset[!newDataset,]
dim(testData)


### Step 3 - Fit a Logistic Model using training data
rModel <- glm(is_high ~ X1+X2+X3+X4, data=trainData, family=binomial(link="logit"))
par(mfrow=c(2,2))
plot(rModel)
par(mfrow=c(1,1))

#Target Variable = Class, Input Vaiables = All, family = binomial (binary target variable) - Logistic regression using logit
summary(rModel)


# log likelihood
logLik(rModel)

# R-squared
ll.null <- rModel$null.deviance/-2
ll.proposed <- rModel$deviance/-2
r_sq <- (ll.null - ll.proposed)/ll.null
r_sq

#P value
p_value <- 1 - pchisq(2*(ll.proposed - ll.null), df = (length(rModel$coefficients)-1))
p_value

###Step 4 - Use the fitted model to do predictions for the test data
#probTest- Probability for Test data, predTest - Predictions for Test data, actual - Actual Value in test data
testData
probTest=predict(rModel, testData, type = "response")  # Predict probabilities


#Recode probability to classification
predVal <- ifelse(probTest >= 0.5, 1, 0)
predTest <- factor(predVal, levels = c(0,1))

probTest [0:5]
predTest[0:5]

actualTest <-testData$is_high
actualTest[0:5]

### Step 5 - Create Confusion Matrix and compute the misclassification error

t <- table(predictions=predTest, actual = actualTest)
t # Confusion matrix
accuracy <- sum(diag(t))/sum(t)
accuracy

### ROC and Area Under the Curve

ROC1 <- roc(actualTest, probTest)
plot(ROC1, col="blue")
AUC1 <- auc(ROC1)
AUC1


## Performnce measures - 
# Simplicity = 7 coefficients
# R2 = 0.304
# Accuracy = 0.9043478 or 0.9
# AUC = 0.9431 0r 0.94


#--------------------
#A new dataframe with Predicted Prob, Actual Value and Predicted Value
predicted_data <- data.frame(Probs = probTest, Actual_Value= actualTest ,Predicted_Value = predTest )  #Create data frame with prob and predictions
predicted_data <- predicted_data[order(predicted_data$Probs, decreasing=TRUE),] # Sort on Probabilities
predicted_data$Rank <- 1:nrow(predicted_data) # Add a new variable rank



### EXAMINING STABILITY - Creating Decile Plots

#-----Create empty df-------
decileDF<- data.frame(matrix(ncol=4,nrow = 0))
colnames(decileDF)<- c("Decile","per_correct_preds","No_correct_Preds","cum_preds")
#-----Initialize varables
num_of_deciles=10
Obs_per_decile<-nrow(predicted_data)/num_of_deciles
decile_count=1
start=1
stop=(start-1) + Obs_per_decile
prev_cum_pred<-0
x=0
#-----Loop through DF and create deciles
while (x < nrow(predicted_data)) {
  subset<-predicted_data[c(start:stop),]
  correct_count<- ifelse(subset$Actual_Value==subset$Predicted_Value,1,0)
  no_correct_Preds<-sum(correct_count,na.rm = TRUE)
  per_correct_Preds<-(no_correct_Preds/Obs_per_decile)*100
  cum_preds<-no_correct_Preds+prev_cum_pred
  addRow<-data.frame("Decile"=decile_count,"per_correct_preds"=per_correct_Preds,"No_correct_Preds"=no_correct_Preds,"cum_preds"=cum_preds)
  decileDF<-rbind(decileDF,addRow)
  prev_cum_pred<-prev_cum_pred+no_correct_Preds
  start<-stop+1
  stop=(start-1) + Obs_per_decile
  x<-x+Obs_per_decile
  decile_count<-decile_count+1
}
#------Stability plot (correct preds per decile)
plot(decileDF$Decile,decileDF$per_correct_preds,type = "l",xlab = "Decile",ylab = "Percentage of correct predictions",main="Stability Plot for LR2")
