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
  as_tibble()  # Save as tibble, which prints better

# Look at the variable names
df %>% names()

head(df)
summary(df)
class(df)
str(df)

# Rename the class label as y; change values 0 to "notSpam"
# and 1 to "spam"; convert to factor
df %<>% 
  rename(y = Y1) %>%   # Rename class variable as `y`
  mutate(
    y = ifelse(
      y >= 20, 
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
  select(y, X1, X7, X5)  %>%
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
trn %>% 
  select(X1, X2, X3, X4, X5, X6, X7, X8, y) %>% 
  gather(var, val, -y) %>%  # Gather key value pairs
  ggplot(aes(x = val, group = y, fill = y)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~var, ncol = 3) +
  theme(legend.position = "bottom")

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

# Clear console
cat("\014")      # Mimics ctrl+L

# Clear R
#   You may want to use Session > Restart R, as well, which 
#   resets changed options, relative paths, dependencies, 
#   and so on to let you start with a clean slate

# Clear mind :)
