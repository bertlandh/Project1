# Title:  Decision Trees
# File:   ENB2012_data.R
# Course: Knowledge Discovery and Data Analytics I - COMP 6115

# INSTALL AND LOAD PACKAGES ################################

# Install pacman if you don't have it (uncomment next line)
# install.packages("pacman")

# Install and/or load packages with pacman
pacman::p_load(  # Use p_load function from pacman
  GGally,        # Plotting option
  magrittr,      # Pipes
  pacman,        # Load/unload packages
  rio,           # Import/export data
  tidyverse      # So many reasons
)

# LOAD AND PREPARE DATA ####################################

# Import the data
df <-  import("data/ENB2012_data.csv") %>%
  as_tibble() %>%  # Save as tibble, which prints better
  select(-Y2)

# Look at the variable names
df %>% names()

head(df)
summary(df)
class(df)
str(df)


#X1 - Relative compactness
hist(df$X1)
# --> clean involve making the values between 0-10
unique(df$X1)
R <- (df$X1 - min(df$X1))/(max(df$X1) - min(df$X1)) * (10-1) + 1
df$X1 <- R

#X2 - Surface Area
hist(df$X2)
# --> clean involve making the values between 0-10
# --> data, mean and median are similar 
R <- (df$X2 - min(df$X2))/(max(df$X2) - min(df$X2)) * (10-1) + 1
df$X2 <- R

#X3 - Wall Area
hist(df$X3)
# --> relative normal distribution
# --> cleaning involve making data between 0-10

R <- (df$X3 - min(df$X3))/(max(df$X3) - min(df$X3)) * (10-1) + 1
df$X3 <- R

#X4 - Roof Area
hist(df$X4)
boxplot(df$X4)
# --> make data between 0-10
# --> data is skewed to the right 
cor(df$X4, df$Y1)
hist((df$X4 -mean(df$X4))/sd(df$X4))
lim <- 2*sd(df$X4)
upp <- mean(df$X4)+lim
low <- mean(df$X4)-lim
df[df$X4>upp,]
df[df$X4<low,]
#-->  No outliners detected when evaluating at 95% of the data at 2sd from mean

R <- (df$X4 - min(df$X4))/(max(df$X4) - min(df$X4)) * (10-1) + 1
df$X4 <- R

#X5 - Overall Height
hist(df$X5)
#--> is a categorical value with 3.5, 7
df$X5 <- as.factor(df$X5)

#X6 - Orientation
hist(df$X6)
# --> also a factor variable with 2,3,4,5 uniformly distributed
unique(df$X6)

df$X6 <- as.factor(df$X6)

#X7 - Glazing Area
#glazing refers to the installation of glass in windows, doors, or any other fixed opening
hist(df$X7)
plot(df$Y1,df$X7)
unique(df$X7)
# also a factor variable in the states of 0.00, 0.10, 0.25, 0.40
# unsure if glazing area is significant to analysis

# this is removed since the data is represented in X8
df$X7 <- NULL

#X8 - Glazing area distribution
hist(df$X8)
unique(df$X8)
# --> this field describes the states of the X7 variable as factors 0-5; i.e. 6 states of glazing
df$X8 <- as.factor(df$X8)

#Limitations - based on how the data was provided we do not know what Orientation means, 
# where these houses were located, or what time of the year the data was collected for energy efficiency


#Y1 - Heating loads
#The heating load is the amount of heat energy that would need to be added to a space to maintain 
#the temperature in an acceptable range. 
# hist(df$Y1)
# 
# lim <- 2*sd(df$Y1)
# upp <- mean(df$Y1) + lim
# low <- mean(df$Y1) - lim
# df[df$Y1 < low, ]
# df[df$Y1 > upp, ]
# --> there are 7 obs that are over the 2sd of the mean, however they are not significant distance away,
# --> so the obs will remain is we should test at 3sd from the mean.
# summary(df$Y1)
#--> data is slightly skewed to the left but not far apart to statistically normalize 


# we introduce a new variable to indicate the high or low heating loads. This is done by combine 1 &3 as low
# and 2 as high.
# df$Y1_ctype = df$Y1_Type
# df$Y1_ctype[df$Y1_ctype == 1 | df$Y1_ctype == 3] <- "Low"
# df$Y1_ctype[df$Y1_ctype == 2] <- "High"
# unique(df$Y1_ctype)
# 
# edata$Y1_ctype = edata$Y1_Type
# edata$Y1_ctype[edata$Y1_ctype == 1 | edata$Y1_ctype == 3] <- "Low"
# edata$Y1_ctype[edata$Y1_ctype == 2] <- "High"
# unique(edata$Y1_ctype)


boxplot(df$Y1)
hist(df$Y1)
# Rename the class label as y; change values 0 to "notSpam"
# and 1 to "spam"; convert to factor
df %<>% 
  rename(y = Y1) %>%   # Rename class variable as `y`
  mutate(
    y = ifelse(
      y >= 30, 
      "High", 
      "Low"
    )
  ) %>%
  mutate(y = factor(y))  # Recode class label as factor
  
  # Check the variable `y`; `forcats::fct_count` gives
  # frequencies in factor order
  df %>% 
  pull(y) %>%  # Return a vector instead of a dataframe
  fct_count()  # Count frequencies in factor order

#-----------------------------------------------------------------------------------------------------------
#Clean and Transformed data
str(df)
summary(df)


# SPLIT DATA ##############################################

# Some demonstrations will use separate testing and training
# datasets for validation.

# Set random seed for reproducibility in processes like
# splitting the data
set.seed(1)  # You can use any number here

# Split data into training (trn) and testing (tst) sets
df %<>% mutate(ID = row_number())  # Add row ID
trn <- df %>%                      # Create trn
  slice_sample(prop = .70)         # 70% in trn
tst <- df %>%                      # Create tst
  anti_join(trn, by = "ID") %>%    # Remaining data in tst
  select(-ID)                      # Remove id from tst
trn %<>% select(-ID)               # Remove id from trn
df %<>% select(-ID)                # Remove id from df

# EXPLORE TRAINING DATA ####################################

# Bar chart of `y`, which is the spam/not class variable
trn %>%
  ggplot() + 
  geom_bar(aes(x = y, fill = y)) 

# Randomly select a few variables to plot; focus on `y` in
# the first column and first row
trn %>% 
  select(y, X1, X6, X5)  %>%
  ggpairs(
    aes(color = trn$y),  # Color code is spam vs. not spam
    lower = list(
      combo = wrap(
        "facethist", 
        binwidth = 0.5
      )
    )
  )

# Stacked histograms of a few variables; note the sparse
# nature of text data
# trn %>% 
#   select(X1, X2, X3, X4, X5, X6, X8, y) %>% 
#   gather(var, val, -y) %>%  # Gather key value pairs
#   ggplot(aes(x = val, group = y, fill = y)) +
#   geom_histogram(binwidth = 1) +
#   facet_wrap(~var, ncol = 3) +
#   theme(legend.position = "bottom")

# SAVE DATA ################################################

# Use saveRDS(), which save data to native R formats
df  %>% saveRDS("data/ENB2012_data.rds")
trn %>% saveRDS("data/ENB2012_trn.rds")
tst %>% saveRDS("data/ENB2012_tst.rds")

# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from the environment

# Clear packages
p_unload(all)    # Remove all contributed packages

# Clear plots
graphics.off()   # Clears plots, closes all graphics devices

cat("\014")      # Mimics ctrl+L

# Clear R
#   You may want to use Session > Restart R, as well, which 
#   resets changed options, relative paths, dependencies, 
#   and so on to let you start with a clean slate

# Clear mind :)
