# Import necessary library
library(keras)

# Read the CSV file
ori_data <- read.csv("Blel_2008_2009_2010_2011.csv")
ori_data <- ori_data[-1,-1]  # Remove the first row and first column
ori_data <- ori_data[,c(1,12,13)]  # Select columns WT.5, WT12, and AT
colnames(ori_data) <- c("WT.5","WT12","AT")  # Rename columns

# Remove missing values
ori_data <- na.omit(ori_data)

# Convert data to numeric format
for(i in 1:3){
  ori_data[,i] <- as.numeric(ori_data[,i])
}

# Set parameters for sliding window
lookback <- 144
step <- 1
delay <- 12
n_sample <- dim(ori_data)[1]  # Total number of samples
n_train <- round(n_sample*0.5)  # Training set size
n_val <- round(n_sample*0.25)  # Validation set size
n_test <- n_sample-n_train-n_val-lookback-delay+1  # Test set size

# Standardize the data using training set mean and standard deviation
mean <- apply(ori_data[1:n_train,],2,mean)
sd <- apply(ori_data[1:n_train,],2,sd)
to_train <- scale(ori_data,center = mean,scale = sd)

# Divide dataset
{
  # Training set
  train <- array(0,dim = c(n_train,lookback/step,3))  # Initialize input array
  tw.5 <- array(0,dim = n_train)  # Initialize target array
  for(i in 1:n_train){
    train[i,,] <- to_train[seq(i,i+lookback-1,
                               length.out = lookback/step),]
    tw.5[i] <- to_train[i+lookback-1+delay,1]  # Target value
  }
  # Validation set
  val <- array(0,dim = c(n_val,lookback/step,3))  # Initialize input array
  vw.5 <- array(0,dim = n_val)  # Initialize target array
  for(i in 1:n_val){
    val[i,,] <- to_train[seq(i+n_train,i+n_train+lookback-1,
                             length.out = lookback/step),]
    vw.5[i] <- to_train[i+n_train+lookback-1+delay,1]  # Target value
  }
  # Test set
  test <- array(0,dim = c(n_test,lookback/step,3))  # Initialize input array
  tew.5 <- array(0,dim = n_test)  # Initialize target array
  for(i in 1:n_test){
    test[i,,] <- to_train[seq(i+n_train+n_val,i+n_train+n_val+lookback-1,
                              length.out = lookback/step),]
    tew.5[i] <- to_train[i+n_train+n_val+lookback-1+delay,1]  # Target value
  }
}

# Define the CNN-LSTM model
input <- layer_input(shape = c(lookback/step,3))  # Input layer
output <- input %>% layer_conv_1d(filters = 64,kernel_size = 5,
                                  activation = "relu") %>%  # Convolutional layer
  layer_lstm(units = 32) %>%  # LSTM layer
  layer_dense(units = 1)  # Dense layer for output
model_cnnlstm <- keras_model(input,output)
model_cnnlstm %>% compile(optimizer="nadam",  # Compile the model
                          loss="mae")
callback <- list(
  callback_model_checkpoint(monitor = "val_loss",  # Save the best model
                            save_best_only = T,
                            filepath = "watertemp.h5"),
  callback_reduce_lr_on_plateau(monitor = "val_loss",  # Reduce learning rate on plateau
                                factor = 0.2,
                                patience = 4)
)

# Train the model
model_cnnlstm %>% fit(train,tw.5,
                      validation_data=list(val,vw.5),
                      epoch=15,
                      batch_size=32,
                      callbacks=callback,
                      verbose=2)

# Load the best model
model_cnnlstm <- load_model_hdf5("watertemp.h5")
save_model_hdf5(model_cnnlstm,"CNN-LSTM-3variables.h5")

# Calculate predictions and metrics for the training set
train_pre <- model_cnnlstm %>% predict(train) * sd[1] + mean[1]
train_ori <- tw.5 * sd[1] + mean[1]
train_mae <- mean(abs(train_pre[,1] - train_ori))
train_mape <- mean(abs(train_pre[,1] - train_ori) / train_ori)
train_rmse <- sqrt(mean((train_pre[,1] - train_ori)^2))
train_nse <- 1 - sum((train_pre[,1] - train_ori)^2) / sum((train_ori - mean(train_ori))^2)

# Calculate predictions and metrics for the validation set
val_pre <- model_cnnlstm %>% predict(val) * sd[1] + mean[1]
val_ori <- vw.5 * sd[1] + mean[1]
val_mae <- mean(abs(val_pre[,1] - val_ori))
val_mape <- mean(abs(val_pre[,1] - val_ori) / val_ori)
val_rmse <- sqrt(mean((val_pre[,1] - val_ori)^2))
val_nse <- 1 - sum((val_pre[,1] - val_ori)^2) / sum((val_ori - mean(val_ori))^2)

# Calculate predictions and metrics for the test set
test_pre <- model_cnnlstm %>% predict(test) * sd[1] + mean[1]
test_ori <- tew.5 * sd[1] + mean[1]
test_mae <- mean(abs(test_pre[,1] - test_ori))
test_mape <- mean(abs(test_pre[,1] - test_ori) / test_ori)
test_rmse <- sqrt(mean((test_pre[,1] - test_ori)^2))
test_nse <- 1 - sum((test_pre[,1] - test_ori)^2) / sum((test_ori - mean(test_ori))^2)

# Create data frames for predicted and original values for each dataset
train_results <- data.frame(
  Set = rep("Train", length(train_ori)),
  Predicted = train_pre[,1],
  Original = train_ori
)

val_results <- data.frame(
  Set = rep("Validation", length(val_ori)),
  Predicted = val_pre[,1],
  Original = val_ori
)

test_results <- data.frame(
  Set = rep("Test", length(test_ori)),
  Predicted = test_pre[,1],
  Original = test_ori
)

# Combine results from all datasets
results <- rbind(train_results, val_results, test_results)

# Create a summary of evaluation metrics
metrics <- data.frame(
  Set = c("Train", "Validation", "Test"),
  MAE = c(train_mae, val_mae, test_mae),
  MAPE = c(train_mape, val_mape, test_mape),
  RMSE = c(train_rmse, val_rmse, test_rmse),
  NSE = c(train_nse, val_nse, test_nse)
)

# Save predictions and metrics to CSV files
write.csv(results, "CNN-LSTM-3variables_predictions.csv", row.names = FALSE)
write.csv(metrics, "CNN-LSTM-3variables_metrics.csv", row.names = FALSE)
