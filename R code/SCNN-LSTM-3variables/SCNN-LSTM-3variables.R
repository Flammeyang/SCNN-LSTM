library(keras)

# Load data and preprocess
ori_data <- read.csv("Blel_2008_2009_2010_2011.csv")
ori_data <- ori_data[-1,-1]
ori_data <- ori_data[,c(1,12,13)]
colnames(ori_data) <- c("WT.5","WT12","AT")

ori_data <- na.omit(ori_data)
for(i in 1:3){
  ori_data[,i] <- as.numeric(ori_data[,i])
}

# Define hyperparameters
lookback <- 144
step <- 1
delay <- 12
n_sample <- dim(ori_data)[1]
n_train <- round(n_sample*0.5) # Number of training samples
n_val <- round(n_sample*0.25)  # Number of validation samples
n_test <- n_sample-n_train-n_val-lookback-delay+1 # Number of testing samples

# Normalize the dataset
mean <- apply(ori_data[1:n_train,],2,mean)
sd <- apply(ori_data[1:n_train,],2,sd)
to_train <- scale(ori_data,center = mean,scale = sd)

# Split dataset
{
  # Training set
  train <- array(0,dim = c(n_train,lookback/step,3))
  tw.5 <- array(0,dim = n_train)
  for(i in 1:n_train){
    train[i,,] <- to_train[seq(i,i+lookback-1,
                               length.out = lookback/step),]
    tw.5[i] <- to_train[i+lookback-1+delay,1]
  }
  # Validation set
  val <- array(0,dim = c(n_val,lookback/step,3))
  vw.5 <- array(0,dim = n_val)
  for(i in 1:n_val){
    val[i,,] <- to_train[seq(i+n_train,i+n_train+lookback-1,
                             length.out = lookback/step),]
    vw.5[i] <- to_train[i+n_train+lookback-1+delay,1]
  }
  # Test set
  test <- array(0,dim = c(n_test,lookback/step,3))
  tew.5 <- array(0,dim = n_test)
  for(i in 1:n_test){
    test[i,,] <- to_train[seq(i+n_train+n_val,i+n_train+n_val+lookback-1,
                              length.out = lookback/step),]
    tew.5[i] <- to_train[i+n_train+n_val+lookback-1+delay,1]
  }
}

# Define SCNN-LSTM model structure
input <- layer_input(shape = c(lookback/step,3))
output <- input %>% layer_separable_conv_1d(filters = 64,kernel_size = 5,
                                            activation = "relu") %>% 
  layer_lstm(units = 32) %>% 
  layer_dense(units = 1)
model_scnnlstm <- keras_model(input,output)

# Compile the model
model_scnnlstm %>% compile(optimizer="nadam",
                           loss="mae")

# Define callbacks
callback <- list(
  callback_model_checkpoint(monitor = "val_loss",
                            save_best_only = T,
                            filepath = "watertemp.h5"),
  callback_reduce_lr_on_plateau(monitor = "val_loss",
                                factor = 0.2,
                                patience = 5)
)

# Train the model
model_scnnlstm %>% fit(train,tw.5,
                       validation_data=list(val,vw.5),
                       epoch=15,
                       batch_size=32,
                       callbacks=callback,
                       verbose=2)

# Load the best model
model_scnnlstm <- load_model_hdf5("watertemp.h5")
save_model_hdf5(model_scnnlstm,"SCNN-LSTM-3variables.h5")

# Calculate predictions and evaluation metrics for the training set
train_pre <- model_scnnlstm %>% predict(train) * sd[1] + mean[1]
train_ori <- tw.5 * sd[1] + mean[1]
train_mae <- mean(abs(train_pre[,1] - train_ori))  # Mean Absolute Error
train_mape <- mean(abs(train_pre[,1] - train_ori) / train_ori)  # Mean Absolute Percentage Error
train_rmse <- sqrt(mean((train_pre[,1] - train_ori)^2))  # Root Mean Square Error
train_nse <- 1 - sum((train_pre[,1] - train_ori)^2) / sum((train_ori - mean(train_ori))^2)  # Nash-Sutcliffe Efficiency

# Calculate predictions and evaluation metrics for the validation set
val_pre <- model_scnnlstm %>% predict(val) * sd[1] + mean[1]
val_ori <- vw.5 * sd[1] + mean[1]
val_mae <- mean(abs(val_pre[,1] - val_ori))
val_mape <- mean(abs(val_pre[,1] - val_ori) / val_ori)
val_rmse <- sqrt(mean((val_pre[,1] - val_ori)^2))
val_nse <- 1 - sum((val_pre[,1] - val_ori)^2) / sum((val_ori - mean(val_ori))^2)

# Calculate predictions and evaluation metrics for the test set
test_pre <- model_scnnlstm %>% predict(test) * sd[1] + mean[1]
test_ori <- tew.5 * sd[1] + mean[1]
test_mae <- mean(abs(test_pre[,1] - test_ori))
test_mape <- mean(abs(test_pre[,1] - test_ori) / test_ori)
test_rmse <- sqrt(mean((test_pre[,1] - test_ori)^2))
test_nse <- 1 - sum((test_pre[,1] - test_ori)^2) / sum((test_ori - mean(test_ori))^2)

# Create data.frames for predictions and original values for each dataset
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

# Create summary of evaluation metrics
metrics <- data.frame(
  Set = c("Train", "Validation", "Test"),
  MAE = c(train_mae, val_mae, test_mae),
  MAPE = c(train_mape, val_mape, test_mape),
  RMSE = c(train_rmse, val_rmse, test_rmse),
  NSE = c(train_nse, val_nse, test_nse)
)

# Save predictions and metrics to CSV files
write.csv(results, "SCNN-LSTM-3variables_predictions.csv", row.names = FALSE)
write.csv(metrics, "SCNN-LSTM-3variables_metrics.csv", row.names = FALSE)
