# Title:  Decision Trees
# File:   DecisionTrees.R
# Course: Knowledge Discovery and Data Analytics I - COMP 6115

# INSTALL AND LOAD PACKAGES ################################

# Install pacman if you don't have it (uncomment next line)
# install.packages("pacman")

# Install and/or load packages with pacman
pacman::p_load(  # Use p_load function from pacman
  caret,         # Train/test functions
  e1071,         # Machine learning functions
  magrittr,      # Pipes
  pacman,        # Load/unload packages
  rattle,        # Pretty plot for decision trees
  rio,           # Import/export data
  tidyverse,     # So many reasons
  rpart,         # Fit a rpart model
  rpart.plot,    # Plot an rpart model
  pROC           # Tools for visualizing
)

# Set random seed to reproduce the results
set.seed(1)

# LOAD AND PREPARE DATA ####################################

# Use the `ENB2012_data` datasets that were created previously 
# in "ENB2012_data.R"

# Import training data `trn`
trn <- import("ENB2012_trn.rds")

# Import testing data `tst`
tst <- import("ENB2012_tst.rds")

# DECISON TREES  ####################################
## Fit a Decision Tree using training data  ####################################
DTmodel1 <- rpart(y ~ .,method="class", data=trn, parms = list (split ="information gain"), control = rpart.control(minsplit = 10, maxdepth = 5))
#DTmodel1 <- rpart(y ~ .,method="class", data=trn, parms = list (split ="gini"), control = rpart.control(minsplit = 15, maxdepth = 5))  

## Fitting the model   ####################################
rpart.plot(DTmodel1, type=3, extra = 101, fallen.leaves = F, cex = 0.8)
rpart.plot(DTmodel1)

summary(DTmodel1) # detailed summary of splits
DTmodel1 #prints the rules

## Use the fitted model to do predictions for the test data  ####################################
DTHLpredTest <- predict(DTmodel1, tst, type="class")
DTHLprobTest <- predict(DTmodel1, tst, type="prob")

DTHLactualTest <- tst$y

## Create Confusion Matrix and compute the misclassification error  ####################################
DTHLt <- table(predictions= DTHLpredTest, actual = DTHLactualTest)
DTHLt # Confusion matrix
DTHLaccuracy <- sum(diag(DTHLt))/sum(DTHLt)
DTHLaccuracy 

## Visualization of probabilities
hist(DTHLprobTest[,2], breaks = 100)

## ROC and Area Under the Curve
DTHLROC <- roc(DTHLactualTest, DTHLprobTest[,2])
plot(DTHLROC, col="blue")
DTHLAUC <- auc(DTHLROC)
DTHLAUC

# A new dataframe with Predicted Prob, Actual Value and Predicted Value
DTHLpredicted_data <- data.frame(Probs = DTHLprobTest, Actual_Value= DTHLactualTest ,Predicted_Value = DTHLpredTest )  #Create data frame with prob and predictions
DTHLpredicted_data <- DTHLpredicted_data[order(DTHLpredicted_data$Probs.High, decreasing=TRUE),] # Sort on Probabilities
DTHLpredicted_data$Rank <- 1:nrow(DTHLpredicted_data) # Add a new variable rank

ggplot(data=DTHLpredicted_data, aes(x=Rank, y=Probs.High)) + 
  geom_point(aes(color = DTHLpredicted_data$Actual_Value)) + xlab("Index") + ylab("Predicted Probability of Heating Load")

## Use model to make predictions on newdata. Note we can specify the newData as data.frame with one or many records  ####################################
#DTHLnewData <- data.frame(X1=8,X2=2.5,X3=4.857143,X4=2,X5=7,X6=4,X8=4) # High
DTHLnewData <- data.frame(X1=2.75,X2=7.75,X3=3.571429,X4=10,X5=3.5,X6=2,X8=2) # Low
DTHLnewData$X5 <- as.factor(DTHLnewData$X5)
DTHLnewData$X6 <- as.factor(DTHLnewData$X6)
DTHLnewData$X8 <- as.factor(DTHLnewData$X8)

DTHLpredProbability <-predict(DTmodel1, DTHLnewData, type='prob')
DTHLpredProbability

# EXAMINING STABILITY - Creating Decile Plots for Class High or Low Sort  ####################################
#
#-----Create empty df
DTdecile<- data.frame(matrix(ncol=4,nrow = 0))
colnames(DTdecile)<- c("Decile","per_correct_preds","No_correct_Preds","cum_preds")
#-----Initialize variables
DTnum_of_deciles = 10
DTObs_per_decile <- nrow(DTHLpredicted_data)/DTnum_of_deciles
DTdecile_count = 1
DTstart = 1
DTstop = (DTstart-1) + DTObs_per_decile
DTprev_cum_pred <- 0
DTx = 0

#-----Loop through DF and create deciles
while (DTx < nrow(DTHLpredicted_data)) {
  DTsubset <- DTHLpredicted_data[c(DTstart:DTstop),]
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
plot(DTdecile$Decile,DTdecile$per_correct_preds,type = "l",xlab = "Decile",ylab = "Percentage of correct predictions",main="Stability Plot for DT")

# PERFORMANCE MEASURES ##################################################

# Decision Tree Model
#
#set.seed(1), gini
# Simplicity = 15 leaves
# Accuracy = 0.9090909 0r 0.91
# AUC = 0.957 0r 0.96
#
#set.seed(1), information
# Simplicity = 10 leaves
# Accuracy = 0.9220779 0r 0.92
# AUC = 0.9733 0r 0.97
#

# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from the environment

# Clear packages
p_unload(all)    # Remove all contributed packages

# Clear plots
graphics.off()   # Clears plots, closes all graphics devices

# Clear console
cat("\014")      # Mimics ctrl+L

# Clear R
#   You may want to use Session > Restart R, as well, which 
#   resets changed options, relative paths, dependencies, 
#   and so on to let you start with a clean slate

# Clear mind :)
