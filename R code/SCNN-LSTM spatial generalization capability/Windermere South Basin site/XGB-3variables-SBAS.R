library(xgboost)
library(mcga)

# Read and modify data
ori_data <- read.csv("SBAS_2012_2013_2014_2015.csv")
ori_data <- ori_data[-1, -1]  # Remove the first row and first column
ori_data <- ori_data[, c(1, 6, 13)]  # Keep only "WT1", "WT13", and "AT"
colnames(ori_data) <- c("WT1", "WT13", "AT")

# Handle missing values
ori_data <- na.omit(ori_data)
for(i in 1:3){
  ori_data[, i] <- as.numeric(ori_data[, i])
}

# Set parameters
lookback <- 144
step <- 6
delay <- 12
n_sample <- dim(ori_data)[1]
n_train <- round(n_sample * 0.5)
n_val <- round(n_sample * 0.25)
n_test <- n_sample - n_train - n_val - lookback - delay + 1

# Standardization
mean <- apply(ori_data[1:n_train,], 2, mean)
sd <- apply(ori_data[1:n_train,], 2, sd)
to_train <- scale(ori_data, center = mean, scale = sd)

# Build training, validation, and test sets
{
  # Training
  train <- array(0, dim = c(n_train, lookback / step * 3))
  twp5 <- array(0, dim = n_train)
  tw12 <- array(0, dim = n_train)
  twat <- array(0, dim = n_train)  # Add AT column labels
  
  for(i in 1:n_train){
    train[i,] <- c(to_train[seq(i, i + lookback - 1, length.out = lookback / step), ])
    twp5[i] <- to_train[i + lookback - 1 + delay, 1]  # WT1
    tw12[i] <- to_train[i + lookback - 1 + delay, 2]  # WT13
    twat[i] <- to_train[i + lookback - 1 + delay, 3]  # AT
  }
  
  # Validation
  val <- array(0, dim = c(n_val, lookback / step * 3))
  vwp5 <- array(0, dim = n_val)
  vw12 <- array(0, dim = n_val)
  vwat <- array(0, dim = n_val)  # Add AT column validation labels
  
  for(i in 1:n_val){
    val[i,] <- c(to_train[seq(i + n_train, i + n_train + lookback - 1, length.out = lookback / step), ])
    vwp5[i] <- to_train[i + n_train + lookback - 1 + delay, 1]  # WT1
    vw12[i] <- to_train[i + n_train + lookback - 1 + delay, 2]  # WT13
    vwat[i] <- to_train[i + n_train + lookback - 1 + delay, 3]  # AT
  }
  
  # Test
  test <- array(0, dim = c(n_test, lookback / step * 3))
  tewp5 <- array(0, dim = n_test)
  tew12 <- array(0, dim = n_test)
  tewat <- array(0, dim = n_test)  # Add AT column test labels
  
  for(i in 1:n_test){
    test[i,] <- c(to_train[seq(i + n_train + n_val, i + n_train + n_val + lookback - 1, length.out = lookback / step), ])
    tewp5[i] <- to_train[i + n_train + n_val + lookback - 1 + delay, 1]  # WT1
    tew12[i] <- to_train[i + n_train + n_val + lookback - 1 + delay, 2]  # WT13
    tewat[i] <- to_train[i + n_train + n_val + lookback - 1 + delay, 3]  # AT
  }
}

# Fitness function of the GA
ajfun <- function(x, train, twp5, val, vwp5){
  xgb <- xgboost(data = train, label = twp5, verbose = 0,
                 nrounds = ceiling(x[1]), max_depth = ceiling(x[2]))
  pre_val <- predict(xgb, val)
  return(-mean((pre_val - vwp5)^2))
}

# Optimize hyperparameters using Genetic Algorithm (GA)
GA_XGB <- mcga2(fitness = ajfun, min = c(3, 6), max = c(40, 25),
                maxiter = 30, popSize = 20, train = train, twp5 = twp5, val = val, vwp5 = vwp5)

par_xgb <- list(nrounds = GA_XGB@solution[1, 1], max_depth = GA_XGB@solution[1, 2])

# Record metrics
mark <- c("p5", "12", "AT")
indicator <- matrix(0, nrow = 3, ncol = 12)
rownames(indicator) <- paste0("D", mark)
colnames(indicator) <- c(paste0("Train", c("MAE", "MAPE", "RMSE", "NSE")),
                         paste0("Val", c("MAE", "MAPE", "RMSE", "NSE")),
                         paste0("Test", c("MAE", "MAPE", "RMSE", "NSE")))

print(paste("par_xgb[1] (nrounds):", par_xgb[1]))
print(paste("par_xgb[2] (max_depth):", par_xgb[2]))

nrounds <- round(as.numeric(par_xgb[1]))  # Round to integer
max_depth <- round(as.numeric(par_xgb[2]))  # Round to integer

# Training, validation, and testing
for(k in 1:3){
  # Select labels based on the mark
  if(k == 1){
    label_train <- twp5
    label_val <- vwp5
    label_test <- tewp5
  } else if(k == 2){
    label_train <- tw12
    label_val <- vw12
    label_test <- tew12
  } else {
    label_train <- twat
    label_val <- vwat
    label_test <- tewat
  }
  
  # Train XGBoost model
  xgb <- xgboost(data = train, label = label_train, 
                 nrounds = nrounds, max_depth = max_depth)
  
  # Denormalization
  label_train <- label_train * sd[k] + mean[k]
  label_val <- label_val * sd[k] + mean[k]
  label_test <- label_test * sd[k] + mean[k]
  
  pre_train <- predict(xgb, train)
  pre_train <- pre_train * sd[k] + mean[k]
  pre_val <- predict(xgb, val)
  pre_val <- pre_val * sd[k] + mean[k]
  pre_test <- predict(xgb, test)
  pre_test <- pre_test * sd[k] + mean[k]
  
  # Calculate evaluation metrics (MAE, MAPE, RMSE, NSE)
  train_mae <- mean(abs(label_train - pre_train))
  train_mape <- mean(abs(label_train - pre_train) / label_train)
  train_rmse <- sqrt(mean((label_train - pre_train)^2))
  train_nse <- 1 - sum((label_train - pre_train)^2) / sum((label_train - mean(label_train))^2)
  
  val_mae <- mean(abs(label_val - pre_val))
  val_mape <- mean(abs(label_val - pre_val) / label_val)
  val_rmse <- sqrt(mean((label_val - pre_val)^2))
  val_nse <- 1 - sum((label_val - pre_val)^2) / sum((label_val - mean(label_val))^2)
  
  test_mae <- mean(abs(label_test - pre_test))
  test_mape <- mean(abs(label_test - pre_test) / label_test)
  test_rmse <- sqrt(mean((label_test - pre_test)^2))
  test_nse <- 1 - sum((label_test - pre_test)^2) / sum((label_test - mean(label_test))^2)
  
  # Store results in the metrics matrix
  indicator[k,] <- c(train_mae, train_mape, train_rmse, train_nse,
                     val_mae, val_mape, val_rmse, val_nse,
                     test_mae, test_mape, test_rmse, test_nse)
  
  # Save model
  save(xgb, file = paste0("XGB", mark[k], ".RData"))
}

# Save evaluation metrics to a CSV file
write.csv(indicator, "Indicator_xgb_3variables.csv")

# Save results for each stage to CSV files
for(k in 1:3){
  # Select labels based on the mark
  if(k == 1){
    label_train <- twp5
    label_val <- vwp5
    label_test <- tewp5
  } else if(k == 2){
    label_train <- tw12
    label_val <- vw12
    label_test <- tew12
  } else {
    label_train <- twat
    label_val <- vwat
    label_test <- tewat
  }
  
  # Train XGBoost model
  xgb <- xgboost(data = train, label = label_train, 
                 nrounds = nrounds, max_depth = max_depth)
  
  # Denormalization
  label_train <- label_train * sd[k] + mean[k]
  label_val <- label_val * sd[k] + mean[k]
  label_test <- label_test * sd[k] + mean[k]
  
  pre_train <- predict(xgb, train)
  pre_train <- pre_train * sd[k] + mean[k]
  pre_val <- predict(xgb, val)
  pre_val <- pre_val * sd[k] + mean[k]
  pre_test <- predict(xgb, test)
  pre_test <- pre_test * sd[k] + mean[k]
  
  # Save results separately to CSV files
  train_results <- data.frame(label = label_train, prediction = pre_train)
  val_results <- data.frame(label = label_val, prediction = pre_val)
  test_results <- data.frame(label = label_test, prediction = pre_test)
  
  write.csv(train_results, paste0("train_results_", mark[k], ".csv"), row.names = FALSE)
  write.csv(val_results, paste0("val_results_", mark[k], ".csv"), row.names = FALSE)
  write.csv(test_results, paste0("test_results_", mark[k], ".csv"), row.names = FALSE)
}
