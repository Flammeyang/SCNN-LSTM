#A code file for optimizing the parameters of the air2water model
library(mcga)
air2_ga <- mcga2(fitness_air2, max = upper, min = lower, maxiter = 30,
                 popSize = 30)
solve_p <- air2_ga@solution[1,]

library(ggplot2)

solve_p <- c(5.74E-03,5.76E-01,2.79E-06,6.87E-08,
             -1.67E-08,1.13E+01,9.86E+00,7.83E-02)
#train
training <- air2_train(solve_p)
out_train <- data.frame(measurements = training$raw,
                        predictions = training$prediction)
write.csv(out_train,"Air2_train.csv", row.names = FALSE)
#write.csv(results, "SCNN-LSTM-3variables_predictions.csv", row.names = FALSE)
mae_train <- mean(abs((training$raw-training$prediction)))
mape_train <- mean(abs((training$raw-training$prediction))/training$raw)
rmse_train <- sqrt(mean((training$raw-training$prediction)^2))
nsc_train <- 1 - sum((training$raw-training$prediction)^2)/
  sum((training$raw-mean(training$raw))^2)
ggplot()+
  geom_point(aes(training$raw,training$prediction),size = 0.1)+
  xlab("Measurment")+
  ylab("Prediction")+
  theme_bw()

#print(solve_p)      # 查看优化的参数 solve_p

#test
testing <- air2_test(solve_p)
out_test <- data.frame(measurements = testing$raw,
                        predictions = testing$prediction)
write.csv(out_test,"Air2_test.csv", row.names = FALSE)
#write.csv(results, "SCNN-LSTM-3variables_predictions.csv", row.names = FALSE)
mae_test <- mean(abs((testing$raw-testing$prediction)))
mape_test <- mean(abs((testing$raw-testing$prediction))/testing$raw)
rmse_test <- sqrt(mean((testing$raw-testing$prediction)^2))
nsc_test <- 1 - sum((testing$raw-testing$prediction)^2)/
  sum((testing$raw-mean(testing$raw))^2)
ggplot()+
  geom_point(aes(testing$raw,testing$prediction),size = 0.1)+
  xlab("Measurment")+
  ylab("Prediction")+
  theme_bw()


# 整理结果为数据框
results <- data.frame(
  Metric = c("MAE", "MAPE", "RMSE", "NSC"),
  Train = c(mae_train, mape_train, rmse_train, nsc_train),
  Test = c(mae_test, mape_test, rmse_test, nsc_test)
)

# 将结果保存到 CSV 文件
write.csv(results, "air2water_results.csv", row.names = FALSE)


