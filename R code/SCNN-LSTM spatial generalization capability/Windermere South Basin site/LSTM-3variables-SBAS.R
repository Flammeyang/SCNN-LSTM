library(keras)

ori_data <- read.csv("SBAS_2012_2013_2014_2015.csv")
ori_data <- ori_data[-1,-1]
ori_data <- ori_data[,c(1,6,13)]
colnames(ori_data) <- c("WT1","WT13","AT")

ori_data <- na.omit(ori_data)
for(i in 1:3){
  ori_data[,i] <- as.numeric(ori_data[,i])
}

lookback <- 144
step <- 1
delay <- 12
n_sample <- dim(ori_data)[1]
n_train <- round(n_sample*0.5)
n_val <- round(n_sample*0.25)
n_test <- n_sample-n_train-n_val-lookback-delay+1

mean <- apply(ori_data[1:n_train,],2,mean)
sd <- apply(ori_data[1:n_train,],2,sd)
to_train <- scale(ori_data,center = mean,scale = sd)

# Dividing dataset
{
  #training
  train <- array(0,dim = c(n_train,lookback/step,3))
  tw.5 <- array(0,dim = n_train)
  for(i in 1:n_train){
    train[i,,] <- to_train[seq(i,i+lookback-1,
                               length.out = lookback/step),]
    tw.5[i] <- to_train[i+lookback-1+delay,1]
  }
  #validation
  val <- array(0,dim = c(n_val,lookback/step,3))
  vw.5 <- array(0,dim = n_val)
  for(i in 1:n_val){
    val[i,,] <- to_train[seq(i+n_train,i+n_train+lookback-1,
                             length.out = lookback/step),]
    vw.5[i] <- to_train[i+n_train+lookback-1+delay,1]
  }
  #test
  test <- array(0,dim = c(n_test,lookback/step,3))
  tew.5 <- array(0,dim = n_test)
  for(i in 1:n_test){
    test[i,,] <- to_train[seq(i+n_train+n_val,i+n_train+n_val+lookback-1,
                              length.out = lookback/step),]
    tew.5[i] <- to_train[i+n_train+n_val+lookback-1+delay,1]
  }
}

# Build LSTM model
input <- layer_input(shape = c(lookback/step,3))
output <- input %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1)

model_lstm <- keras_model(input, output)
model_lstm %>% compile(optimizer = "nadam", loss = "mae")

# Callbacks
callback <- list(
  callback_model_checkpoint(monitor = "val_loss",
                            save_best_only = TRUE,
                            filepath = "watertemp_lstm.h5"),
  callback_reduce_lr_on_plateau(monitor = "val_loss",
                                factor = 0.2,
                                patience = 5)
)

# Training LSTM model
model_lstm %>% fit(train, tw.5,
                   validation_data = list(val, vw.5),
                   epochs = 20,
                   batch_size = 32,
                   callbacks = callback,
                   verbose = 2)

# Load best model
model_lstm <- load_model_hdf5("watertemp_lstm.h5")
save_model_hdf5(model_lstm, "LSTM-3variables.h5")

# Predictions and evaluation for training set
train_pre <- model_lstm %>% predict(train) * sd[1] + mean[1]
train_ori <- tw.5 * sd[1] + mean[1]
train_mae <- mean(abs(train_pre[,1] - train_ori))
train_mape <- mean(abs(train_pre[,1] - train_ori) / train_ori)
train_rmse <- sqrt(mean((train_pre[,1] - train_ori)^2))
train_nse <- 1 - sum((train_pre[,1] - train_ori)^2) / sum((train_ori - mean(train_ori))^2)

# Predictions and evaluation for validation set
val_pre <- model_lstm %>% predict(val) * sd[1] + mean[1]
val_ori <- vw.5 * sd[1] + mean[1]
val_mae <- mean(abs(val_pre[,1] - val_ori))
val_mape <- mean(abs(val_pre[,1] - val_ori) / val_ori)
val_rmse <- sqrt(mean((val_pre[,1] - val_ori)^2))
val_nse <- 1 - sum((val_pre[,1] - val_ori)^2) / sum((val_ori - mean(val_ori))^2)

# Predictions and evaluation for test set
test_pre <- model_lstm %>% predict(test) * sd[1] + mean[1]
test_ori <- tew.5 * sd[1] + mean[1]
test_mae <- mean(abs(test_pre[,1] - test_ori))
test_mape <- mean(abs(test_pre[,1] - test_ori) / test_ori)
test_rmse <- sqrt(mean((test_pre[,1] - test_ori)^2))
test_nse <- 1 - sum((test_pre[,1] - test_ori)^2) / sum((test_ori - mean(test_ori))^2)

# Results for predictions and evaluation metrics
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

# Combine all datasets results
results <- rbind(train_results, val_results, test_results)

# Metrics summary
metrics <- data.frame(
  Set = c("Train", "Validation", "Test"),
  MAE = c(train_mae, val_mae, test_mae),
  MAPE = c(train_mape, val_mape, test_mape),
  RMSE = c(train_rmse, val_rmse, test_rmse),
  NSE = c(train_nse, val_nse, test_nse)
)

# Save predictions and metrics to CSV
write.csv(results, "LSTM-3variables_predictions.csv", row.names = FALSE)
write.csv(metrics, "LSTM-3variables_metrics.csv", row.names = FALSE)
