# Load necessary libraries
library(dplyr)
library(rsample) # For data splitting
library(data.table) # For single feature one-hot encoding
library(nestedcv)  # For full data frame one-hot encoding
library(scales)  # For Min-Max scaling

# Load the dataset (replace with your actual file path)
gtky <- read.csv("ds4400_clean_gtky.csv", stringsAsFactors = T)

# Preview the data
head(gtky)

# One-hot encoding the categorical features
# Creates dummy variables for the "class" column
onehot_class <- model.matrix(~ class - 1, data = gtky) %>%
  as.data.frame() %>%
  select(-starts_with("classX"))  # Remove baseline column

# Preview the encoded class features
head(onehot_class)

# One-hot encoding the full dataset
# This removes the first level as the baseline for all categorical features
onehot_gtky <- nestedcv::one_hot(as.data.table(gtky))

# Preview the encoded dataset
head(onehot_gtky)

# Convert the one-hot encoded data to a numeric matrix
onehotnp_gtky <- as.matrix(onehot_gtky)

# Standardization (data centering and scaling)
X <- matrix(c(1, 1, 1000,
              2, 2, 850,
              2, 3, 1400,
              1, 1, 800,
              4, 2, 1050), nrow = 5, byrow = TRUE)

# Standardize using scale()
X_standardized <- scale(X)

# Check the standardized data
X_standardized

# Verify mean is 0 and standard deviation is 1 for all columns
colMeans(X_standardized)  # Should be close to 0
apply(X_standardized, 2, sd)  # Should be close to 1

# Min-max scaling
X <- onehotnp_gtky
X_scaled <- apply(X, 2, function(x) rescale(x, to = c(0, 1)))

# Round to 2 decimal places for comparison
X_scaled <- round(X_scaled, 2)
head(X_scaled)

# Separating data into training and test sets for cross-validation
# Assuming last column is the target variable
Phi <- X_scaled[, -5]  # Features (everything except co_opYes)
y <- X_scaled[, 5]    # Target (co_opYes)

# Add a bias column
# If Phi is a matrix
Phi <- cbind(bias = 1, Phi)
## If operating on the original data frame
# gtky$bias <- 1

# Split the data
set.seed(42)
# An easy way to get some indices
train_indices <- sample(1:nrow(X), size = round(nrow(X)*.7), replace = F) # about 70% training data

Phi_train <- Phi[train_indices, ]
Phi_test <- Phi[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

##Another way, using tidymodels and rsample; operates on a data frame instead of a matrix; can convert to matrices/vectors after
## This does a two-way split (training-test, with prop = proportion for training)
# data_split <- initial_split(as.data.frame(onehot_gtky), prop = 0.7)
# data_split
# data_train <- training(data_split)
# data_test <- testing(data_split)
## To do a three-way slipt (training-validation-test) you can do:
# val_split <- initial_validation_split(as.data.frame(onehot_gtky), prop = c(0.7, 0.15))
# val_train <- training(val_split)
# val_val <- validation(val_split)
# val_test <- testing(val_split)


# Training data
Phi_train
y_train

# Testing data
Phi_test
y_test
