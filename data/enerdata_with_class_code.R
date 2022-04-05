## Read energy data from project.data folder
## file name : energydata_ENB2012_data

energy_data <- read.csv(choose.files(), header=T)
summary(energy_data)
str(energy_data)

energy_data$Y2 <- NULL

k <- kmeans(energy_data$Y1,3)
k$cluster

energy_data$Y1_Type <- k$cluster
View(energy_data)
boxplot(Y1 ~ Y1_Type, data = energy_data)

# Where 1 is low, 3 is medium and 2 is high
