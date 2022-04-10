# Building a neural network to classify heating loads based on the 8 criteria

library(caTools)
library(pROC)
library(keras)
library(caret)

#-------------------------------------------------------------
### Step 1 - Load data and get summaries 
#file enerdata_clean_transformed.csv
## Choose file
dataset <- read.csv(file.choose())
summary(dataset)
head(dataset)
#diab <- dataset
diab

dataset$Y1 <- NULL
dataset$Y1_Type <- NULL

summary(dataset)
# is High then 1, low then zero

dataset$is_high <- dataset$Y1_ctype
dataset$is_high[dataset$Y1_ctype == "High"] <- 1
dataset$is_high[dataset$Y1_ctype == "Low"] <- 0
dataset$Y1_ctype <- NULL 
dataset$is_high <- as.numeric(dataset$is_high)
head(dataset)
str(dataset)

#--------------------------------------------------------------------------
### Step 2 - Split data into training and testing data 
set.seed(7)
newDataset <-sample.split(Y=dataset$is_high, SplitRatio = 0.7)
trainData <- dataset[newDataset,]
testData <- dataset[!newDataset,]

## trainX contains first 8 columns of trainData
trainX <- trainData[, 1:7]
summary(trainX)

## trainY contains last column of trainData
trainY <- as.data.frame(trainData$is_high)
colnames(trainY) <- c("High") #Modify variable name to Class

## testX contains first 8 columns of testData
testX <- testData[, 1:7]
summary(testX)

## testY contains last column of testData
testY <- as.data.frame(testData$is_high)
colnames(testY) <- c("High") #Modify variable name to Class

## newTrain -> Change training (X & Y) data into correct format to build model - convert X and Y to vectors and matrices
newTrainY <- as.vector(trainY)
newTrainY <- as.matrix(newTrainY)
#newTrainY <- as.numeric(newTrainY)
#combine all individuals vectors
newTrainX <- cbind(as.vector(trainX$X1), as.vector(trainX$X2),
                   as.vector(trainX$X3), as.vector(trainX$X4),
                   as.vector(trainX$X5), as.vector(trainX$X6), 
                   as.vector(trainX$X8))   #combining vectors

#newTrainX  <- normalize(newTrainX) #Normalize data
#summary(newTrainX)

## newTest -> Change test (X & Y) data into correct format to build model - convert X and Y to vectors and matrices
newTestY <- as.vector(testY)
newTestY <- as.matrix(newTestY)
#newTestY <- as.numeric(newTestY)

#combine all individuals vectors
newTestX <- cbind(as.vector(testX$X1), as.vector(testX$X2),
                  as.vector(testX$X3), as.vector(testX$X4),
                  as.vector(testX$X5), as.vector(testX$X6), 
                  as.vector(testX$X8))

#newTestX  <- normalize(newTestX)  #Normailze data
#summary(newTestX)

#-----------------------------------------------------------------------------------------

### Step 3 - Using Keras - create, complie, Fit a model using training data

## Creating model
model <- keras_model_sequential()

model %>% 
  layer_dense(units = 4, input_shape = c(7)) %>% layer_activation("sigmoid") %>% 
  layer_dense(units = 8) %>% layer_activation("sigmoid") %>%
  layer_dense(units = 1) %>%
  layer_activation("sigmoid") 

summary(model)

## Compile model
model %>% compile(
  optimizer = optimizer_adam(),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

## Fit model 
nnModel <- model %>% fit(
  newTrainX, newTrainY, 
  epochs = 120, batch_size = 10, 
)

## Visual 
plot(nnModel)

#------------------------------------------------------
###Step 4 - Use the fitted model to do predictions on the test data
#prob- Probability, pred - Prediction

model %>% evaluate(newTestX, newTestY)

probTest <- model %>% predict(newTestX)

#Recode probability to classification
predVal <- ifelse(probTest >= 0.5, 1, 0)
predTest <- factor(predVal, levels = c(0,1))
predTest[0:5]

actualTest <-testY$High
actualTest[0:5]

#------------------------------------------------------

### Step 5 - Create Confusion Matrix and compute the misclassification error

## Evaluate model
model %>% evaluate(newTestX, newTestY)

##Confusion Matrix
probTest <- model %>% predict(newTestX)
t <- table(predictions=predTest, actual = actualTest)
t 
accuracy <- sum(diag(t))/sum(t)
accuracy

### ROC and Area Under the Curve
ROC <- roc(actualTest, probTest)
plot(ROC, col="blue")
AUC <- auc(ROC)
AUC

#A new dataframe with Predicted Prob, Actual Value and Predicted Value
predicted_data <- data.frame(Probs = probTest, Actual_Value=actualTest,Predicted_Value = predTest )  #Create data frame with prob and predictions
predicted_data <- predicted_data[order(predicted_data$Probs, decreasing=TRUE),] # Sort on Probabilities
predicted_data$Rank <- 1:nrow(predicted_data) # Add a new variable rank

library(ggplot2)

ggplot(data=predicted_data, aes(x=Rank, y=Probs)) + 
  geom_point(aes(color = predicted_data$Actual_Value)) + xlab("Index") + ylab("Predicted Probability of getting High Heating Load")

### Step 6 - EXAMINING STABILITY - Creating Decile Plots

#-----Create empty df-------
decileDF<- data.frame(matrix(ncol=3,nrow = 0))
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
plot(decileDF$Decile,decileDF$per_correct_preds,type = "l",xlab = "Decile",ylab = "Percentage of correct predictions",main="Stability Plot for NN")


