ori_data <- read.csv("SBAS_2012_2013_2014_2015.csv")
ori_data <- ori_data[-1, -1]  # Remove the first row and the first column
ori_data <- na.omit(ori_data)  # Remove rows with missing values
ori_data <- ori_data[, c(1, 6, 13)]  # Retain columns "DX1", "DX35", and "AT"
# TW = DX1; TR = DX35; TA = AT
colnames(ori_data) <- c("TW", "TR", "TA")  # Rename columns
ori_data <- apply(ori_data, 2, as.numeric)  # Convert data to numeric
ori_data <- data.frame(ori_data)  # Convert to a data frame

# Split the dataset into four parts
Y1 <- ori_data[1:8784, ]
Y2 <- ori_data[8785:17454, ]
Y3 <- ori_data[17545:26304, ]
Y4 <- ori_data[26305:35051, ]

sum(is.na(Y4))  # Check if there are missing values in Y4

# If missing values exist, clean them using the following method:
Y4 <- na.omit(Y4)  # Remove rows with missing values

# Define the delta_1 function
delta_1 <- function(TR, TW, p6) {
  delta <- exp((TR - TW) / p6)  # Calculate delta when TW >= TR
  return(delta)
}

# Define the delta_2 function
delta_2 <- function(TR, TW, p7, p8) {
  delta <- exp((TW - TR) / p7) + exp(-TW / p8)  # Calculate delta when TW < TR
  return(delta)
}

# Define the dw_dt function for temperature rate of change
dw_dt <- function(TW, TR, TA, p_i, t) {
  if (TW >= TR) {
    delta <- delta_1(4, TW, p_i[6])  # Use delta_1 when TW >= TR
  }
  if (TW < TR) {
    delta <- delta_2(4, TW, p_i[7], p_i[8])  # Use delta_2 when TW < TR
  }
  # Calculate the rate of change of TW
  dTW <- 1 / delta * (p_i[1] * cos(2 * pi * (t / tyr - p_i[2])) + 
                        p_i[3] + p_i[4] * (TA - TW) + p_i[5] * TW)
  return(dTW)
}
