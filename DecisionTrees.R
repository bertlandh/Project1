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
  tidyverse      # So many reasons
)

# Set random seed to reproduce the results
set.seed(1)

# LOAD AND PREPARE DATA ####################################

# Use the `ENB2012_data` datasets that were created previously 
# in "ENB2012_data.R"

# Import training data `trn`
trn <- import("data/ENB2012_trn.rds")

# Import testing data `tst`
tst <- import("data/ENB2012_tst.rds")

# MODEL TRAINING DATA ######################################

# set training control parameters
ctrlparam <- trainControl(
  method  = "repeatedcv",   # method
  number  = 5,              # 5 fold
  repeats = 3               # 3 repeats
)

# Train decision tree on training data (takes a moment).
# First method tunes the complexity parameter.
dt1 <- train(
  y ~ .,                  # Use all vars to predict spam
  data = trn,             # Use training data
  method = "rpart",       # Tune the complexity parameter
  trControl = ctrlparam,  # Control parameters
  tuneLength = 10         # Try ten parameters
)

# Show processing summary
dt1

# Plot accuracy by complexity parameter values
dt1 %>% plot()
dt1 %>% plot(ylim = c(0, 1))  # Plot with 0-100% range

# Second method tunes the maximum tree depth
dt2 <- train(
  y ~ .,                  # Use all vars to predict spam
  data = trn,             # Use training data
  method = "rpart2",      # Tune the maximum tree depth
  trControl = ctrlparam,  # Control parameters
  tuneLength = 10         # Try ten parameters
)

# Show processing summary
dt2

# Plot the accuracy for different parameter values
dt2 %>% plot()
dt2 %>% plot(ylim = c(0, 1))  # Plot with 0-100% range

# Select the final model depending upon final accuracy
finaldt <- if (max(dt1$results$Accuracy) > 
               max(dt2$results$Accuracy)) {
  dt1
} else {
  dt2
}

# Description of final training model
finaldt$finalModel

# Plot the final decision tree model
finaldt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Heating_Load",
    sub  = "Training Data"
  )

# VALIDATE ON TEST DATA ####################################

# Predict on test set
pred <- finaldt %>%
  predict(newdata = tst)

# Accuracy of model on test data
cmtest <- pred %>%
  confusionMatrix(reference = tst$y)

# Plot the confusion matrix
cmtest$table %>% 
  fourfoldplot(color = c("red", "lightblue"))

# Print the confusion matrix
cmtest %>% print()



#############my code
### Step 3 - Fit a Decision Tree using training data
#DTmodel <- rpart(Heating_Load ~ .,method="class", data=DTtrainData, parms = list (split ="information gain"), control = rpart.control(minsplit = 10, maxdepth = 5))
DTHLmodel <- rpart(Heating_Load ~ ., data=DTHLtrainData)

# Fitting the model
rpart.plot(DTHLmodel, type=3, extra = 101, fallen.leaves = F, cex = 0.8,box.palette="blue")
rpart.plot(DTHLmodel,box.palette="blue")

summary(DTHLmodel) # detailed summary of splits
DTHLmodel #prints the rules


###Step 4 - Use the fitted model to do predictions for the test data

DTHLpredTest <- predict(DTHLmodel, DTHLtestData, type="class")
DTHLprobTest <- predict(DTHLmodel, DTHLtestData, type="prob")

DTHLactualTest <- DTHLtestData$Heating_Load

### Step 5 - Create Confusion Matrix and compute the misclassification error
DTHLt <- table(predictions= DTHLpredTest, actual = DTHLactualTest)
DTHLt # Confusion matrix
DTHLaccuracy <- sum(diag(DTHLt))/sum(DTHLt)
DTHLaccuracy 

## Visualization of probabilities
hist(DTHLprobTest[,2], breaks = 100)

### ROC and Area Under the Curve
DTHLROC <- roc(DTHLactualTest, DTHLprobTest[,2])
plot(DTHLROC, col="blue")
DTHLAUC <- auc(DTHLROC)
DTHLAUC

#A new dataframe with Predicted Prob, Actual Value and Predicted Value
DTHLpredicted_data <- data.frame(Probs = DTHLprobTest, Actual_Value= DTHLactualTest ,Predicted_Value = DTHLpredTest )  #Create data frame with prob and predictions
DTHLpredicted_data <- DTHLpredicted_data[order(DTHLpredicted_data$Probs.1, decreasing=TRUE),] # Sort on Probabilities
DTHLpredicted_data$Rank <- 1:nrow(DTHLpredicted_data) # Add a new variable rank

ggplot(data=DTpredicted_data, aes(x=Rank, y=Probs.1)) + 
  geom_point(aes(color = DTHLpredicted_data$Actual_Value)) + xlab("Index") + ylab("Predicted Probability of Heating Load")

### Step 6 - Use model to make predictions on newdata. Note we can specify the newData as data.frame with one or many records
#DTnewData <- data.frame(male = 0, age = 50, education = 0, currentSmoker = 0, cigsPerDay = 9, BPMeds = 0, prevalentStroke = 0, prevalentHyp = 0, diabetes = 0, totChol = 236, sysBP = 102.0, diaBP = 71, BMI = 100, heartRate = 100, glucose = 200 )

# terminal nodes
# age
# glucose
# diaBP
# BMI

DTHLpredProbability <-predict(DTHLmodel, DTHLnewData, type='prob')
DTHLpredProbability


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

# PERFORMANCE MEASURES ##################################################

# Decision Tree Model
# Accuracy: .9029
# Reference: Not Spam: 796 / 62
# Reference: Spam: 72 / 450
# Sensitivity = 796 / (796 + 62) = .9277
# Specificity = 450 / (450 + 72) = .8621


##  - 
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
