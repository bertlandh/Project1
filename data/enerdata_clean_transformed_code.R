## Reading a cleaning the data for processing into a neural network dataset
## First pass 


edata <-read.csv("enerdata_with_class.csv")

str(edata)
summary(edata)
plot(edata)

#---------------------------------------------------------------------------
# Data Cleaning

#X1 - Relative compactness
hist(edata$X1)
  # --> clean involve making the values between 0-10
unique(edata$X1)
R <- (edata$X1 - min(edata$X1))/(max(edata$X1) - min(edata$X1)) * (10-1) + 1
edata$X1 <- R

#X2 - Surface Area
hist(edata$X2)
  # --> clean involve making the values between 0-10
  # --> data, mean and median are similar 
R <- (edata$X2 - min(edata$X2))/(max(edata$X2) - min(edata$X2)) * (10-1) + 1
edata$X2 <- R

#X3 - Wall Area
hist(edata$X3)
  # --> relative normal distribution
  # --> cleaning involve making data between 0-10

R <- (edata$X3 - min(edata$X3))/(max(edata$X3) - min(edata$X3)) * (10-1) + 1
edata$X3 <- R

#X4 - Roof Area
hist(edata$X4)
boxplot(edata$X4)
  # --> make data between 0-10
  # --> data is skewed to the right 
boxplot(edata$X4 ~ edata$Y1_Type)
cor(edata$X4, edata$Y1)
hist((edata$X4 -mean(edata$X4))/sd(edata$X4))
lim <- 2*sd(edata$X4)
upp <- mean(edata$X4)+lim
low <- mean(edata$X4)-lim
edata[edata$X4>upp,]
edata[edata$X4<low,]
  #-->  No outliners detected when evaluating at 95% of the data at 2sd from mean

R <- (edata$X4 - min(edata$X4))/(max(edata$X4) - min(edata$X4)) * (10-1) + 1
edata$X4 <- R

#X5 - Overall Height
hist(edata$X5)
  #--> is a categorical value with 3.5, 7
edata$X5 <- as.factor(edata$X5)

#X6 - Orientation
hist(edata$X6)
  # --> also a factor variable with 2,3,4,5 uniformly distributed
unique(edata$X6)

edata$X6 <- as.factor(edata$X6)

#X7 - Glazing Area
#glazing refers to the installation of glass in windows, doors, or any other fixed opening
hist(edata$X7)
plot(edata$Y1,edata$X7)
unique(edata$X7)
  # also a factor variable in the states of 0.00, 0.10, 0.25, 0.40
  # unsure if glazing area is significant to analysis

  # this is removed since the data is represented in X8
edata$X7 <- NULL

#X8 - Glazing area distribution
hist(edata$X8)
unique(edata$X8)
  # --> this field describes the states of the X7 variable as factors 0-5; i.e. 6 states of glazing
edata$X8 <- as.factor(edata$X8)

#Limitations - based on how the data was provided we do not know what Orientation means, 
# where these houses were located, or what time of the year the data was collected for energy efficiency


#Y1 - Heating loads
#The heating load is the amount of heat energy that would need to be added to a space to maintain 
#the temperature in an acceptable range. 
hist(edata$Y1)

lim <- 2*sd(edata$Y1)
upp <- mean(edata$Y1) + lim
low <- mean(edata$Y1) - lim
edata[edata$Y1 < low, ]
edata[edata$Y1 > upp, ]
  # --> there are 7 obs that are over the 2sd of the mean, however they are not significant distance away,
  # --> so the obs will remain is we should test at 3sd from the mean.
summary(edata$Y1)
  #--> data is slightly skewed to the left but not far apart to statistically normalize 


# we introduce a new variable to indicate the high or low heating loads. This is done by combine 1 &3 as low
# and 2 as high.
edata$Y1_ctype = edata$Y1_Type
edata$Y1_ctype[edata$Y1_ctype == 1 | edata$Y1_ctype == 3] <- "Low"
edata$Y1_ctype[edata$Y1_ctype == 2] <- "High"
unique(edata$Y1_ctype)

#-----------------------------------------------------------------------------------------------------------
#Clean and Transformed data
str(edata)
summary(edata)

write.csv(edata, "enerdata_clean_transformed.csv", row.names = F)
