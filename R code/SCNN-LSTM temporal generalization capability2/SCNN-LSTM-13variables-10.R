library(keras)
ori_data <- read.csv("Extracted_Blel2.csv")
ori_data <- ori_data[-1, -1]
ori_data <- ori_data[, -c(14, 15)]
colnames(ori_data) <- c("WT.5", "WT1", "WT2", "WT3", "WT4",
                        "wT5", "WT6", "WT7", "WT8", "WT9", "WT10",
                        "WT12", "AT")

ori_data <- na.omit(ori_data)
for (i in 1:12) {
  ori_data[, i] <- as.numeric(ori_data[, i])
}

lookback <- 12
step <- 1
delay <- 1
n_sample <- dim(ori_data)[1]
n_train <- round(n_sample * 0.5)
n_val <- round(n_sample * 0.25)
n_test <- n_sample - n_train - n_val - lookback - delay + 1

mean <- apply(ori_data[1:n_train, ], 2, mean)
sd <- apply(ori_data[1:n_train, ], 2, sd)
to_train <- scale(ori_data, center = mean, scale = sd)


{
  # training
  train <- array(0, dim = c(n_train, lookback / step, 13))
  tw.5 <- array(0, dim = n_train)
  tw12 <- array(0, dim = n_train)
  for (i in 1:n_train) {
    train[i, , ] <- to_train[seq(i, i + lookback - 1, length.out = lookback / step), ]
    tw.5[i] <- to_train[i + lookback - 1 + delay, 1]
    tw12[i] <- to_train[i + lookback - 1 + delay, 12]
  }
  for (j in 1:10) {
    m_0 <- array(0, dim = n_train)
    for (i in 1:n_train) {
      m_0[i] <- to_train[i + lookback - 1 + delay, j + 1]
    }
    assign(paste0("tw", j), m_0)
  }
  # validation
  val <- array(0, dim = c(n_val, lookback / step, 13))
  vw.5 <- array(0, dim = n_val)
  vw12 <- array(0, dim = n_val)
  for (i in 1:n_val) {
    val[i, , ] <- to_train[seq(i + n_train, i + n_train + lookback - 1, length.out = lookback / step), ]
    vw.5[i] <- to_train[i + n_train + lookback - 1 + delay, 1]
    vw12[i] <- to_train[i + n_train + lookback - 1 + delay, 12]
  }
  for (j in 1:10) {
    m_0 <- array(0, dim = n_val)
    for (i in 1:n_val) {
      m_0[i] <- to_train[i + n_train + lookback - 1 + delay, j + 1]
    }
    assign(paste0("vw", j), m_0)
  }
  # test
  test <- array(0, dim = c(n_test, lookback / step, 13))
  tew.5 <- array(0, dim = n_test)
  tew12 <- array(0, dim = n_test)
  for (i in 1:n_test) {
    test[i, , ] <- to_train[seq(i + n_train + n_val, i + n_train + n_val + lookback - 1, length.out = lookback / step), ]
    tew.5[i] <- to_train[i + n_train + n_val + lookback - 1 + delay, 1]
    tew12[i] <- to_train[i + n_train + n_val + lookback - 1 + delay, 12]
  }
  for (j in 1:10) {
    m_0 <- array(0, dim = n_test)
    for (i in 1:n_test) {
      m_0[i] <- to_train[i + n_train + n_val + lookback - 1 + delay, j + 1]
    }
    assign(paste0("tew", j), m_0)
  }
}

mark <- c(".5", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12)
all_data <- list()  

for (kk in 9:12) {
  matr <- matrix(0, ncol = 12, nrow = 10)
  colnames(matr) <- rep(c("MAE", "RMSE", "NSE", "MAPE"), 3)
  pre_ori_list <- list()  
  
  for (i in 1:10) {
    input <- layer_input(shape = c(lookback / step, 13))
    output <- input %>% 
      layer_separable_conv_1d(filters = 64, kernel_size = 5, activation = "relu") %>% 
      layer_lstm(units = 32) %>% 
      layer_dense(units = 1)
    model_lstm <- keras_model(input, output)
    model_lstm %>% compile(optimizer = "nadam", loss = "mae")
    callback <- list(
      callback_model_checkpoint(monitor = "val_loss", save_best_only = TRUE, filepath = "watertemp.h5"),
      callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.2, patience = 5)
    )
    
    model_lstm %>% fit(
      train, get(paste0("tw", mark[kk])),
      validation_data = list(val, get(paste0("vw", mark[kk]))),
      epochs = 30, batch_size = 32, callbacks = callback, verbose = 2
    )
    
    model_lstm <- load_model_hdf5("watertemp.h5")
    save_model_hdf5(model_lstm, paste0("SCNN-LSTM_", mark[kk], "_", i, ".h5"))
    
    
    pre_test <- model_lstm %>% predict(test) * sd[kk] + mean[kk]
    ori_test <- get(paste0("tew", mark[kk])) * sd[kk] + mean[kk]
    test_mae <- mean(abs(pre_test[, 1] - ori_test))
    test_rmse <- sqrt(mean((pre_test[, 1] - ori_test)^2))
    test_nse <- 1 - sum((pre_test[, 1] - ori_test)^2) / sum((ori_test - mean(ori_test))^2)
    test_mape <- mean(abs((pre_test[, 1] - ori_test) / ori_test)) * 100
    
    
    pre_val <- model_lstm %>% predict(val) * sd[kk] + mean[kk]
    ori_val <- get(paste0("vw", mark[kk])) * sd[kk] + mean[kk]
    val_mae <- mean(abs(pre_val[, 1] - ori_val))
    val_rmse <- sqrt(mean((pre_val[, 1] - ori_val)^2))
    val_nse <- 1 - sum((pre_val[, 1] - ori_val)^2) / sum((ori_val - mean(ori_val))^2)
    val_mape <- mean(abs((pre_val[, 1] - ori_val) / ori_val)) * 100
    
    
    pre_train <- model_lstm %>% predict(train) * sd[kk] + mean[kk]
    ori_train <- get(paste0("tw", mark[kk])) * sd[kk] + mean[kk]
    train_mae <- mean(abs(pre_train[, 1] - ori_train))
    train_rmse <- sqrt(mean((pre_train[, 1] - ori_train)^2))
    train_nse <- 1 - sum((pre_train[, 1] - ori_train)^2) / sum((ori_train - mean(ori_train))^2)
    train_mape <- mean(abs((pre_train[, 1] - ori_train) / ori_train)) * 100
    
    
    pre_ori_list[[i]] <- data.frame(
      Type = rep(c("Train", "Val", "Test"), c(length(pre_train), length(pre_val), length(pre_test))),
      Pre = c(pre_train[, 1], pre_val[, 1], pre_test[, 1]),
      Ori = c(ori_train, ori_val, ori_test),
      MAE = c(train_mae, val_mae, test_mae),
      RMSE = c(train_rmse, val_rmse, test_rmse),
      NSE = c(train_nse, val_nse, test_nse),
      MAPE = c(train_mape, val_mape, test_mape)
    )
    
    matr[i, ] <- c(train_mae, train_rmse, train_nse, train_mape, 
                   val_mae, val_rmse, val_nse, val_mape, 
                   test_mae, test_rmse, test_nse, test_mape)
  }
  
  write.csv(matr, paste0("Result_SCNN_LSTM_", mark[kk], ".csv"))
  write.csv(do.call(rbind, pre_ori_list), paste0("Pre_And_Ori_SCNN_LSTM_", mark[kk], ".csv"))
  all_data[[kk]] <- pre_ori_list
}

