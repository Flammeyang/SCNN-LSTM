# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd
import tensorflow as tf
from tensorflow.keras import layers, models
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from tqdm import tqdm

# Read the data
ori_data = pd.read_csv("Blel_2008_2009_2010_2011.csv")
ori_data = ori_data.iloc[:, 1:]  

ori_data.columns = ["WT.5", "WT1", "WT2", "WT3", "WT4",
                    "wT5", "WT6", "WT7", "WT8", "WT9", "WT10",
                    "WT12", "AT", "Pyran", "Wind"]

# Delete missing values
ori_data = ori_data.dropna()

# Convert data types
for col in ori_data.columns:
    ori_data[col] = pd.to_numeric(ori_data[col])

# Data standardization
lookback = 144
step = 1
delay = 12
n_sample = ori_data.shape[0]
n_train = int(n_sample * 0.5)
n_val = int(n_sample * 0.25)
n_test = n_sample - n_train - n_val - lookback - delay + 1

scaler = StandardScaler()
scaler.fit(ori_data.iloc[:n_train, :])
to_train = scaler.transform(ori_data)

# Split the data
def create_dataset(data, lookback, delay, step, n_samples, n_features):
    X, y = [], []
    for i in range(n_samples - lookback - delay + 1):
        X.append(data[i:i + lookback, :])
        y.append(data[i + lookback + delay - 1, 0])  
    return np.array(X), np.array(y)

# Training set
train, tw_5 = create_dataset(to_train[:n_train], lookback, delay, step, n_train, ori_data.shape[1])

# Validation sets
val, vw_5 = create_dataset(to_train[n_train:n_train + n_val], lookback, delay, step, n_val, ori_data.shape[1])

# Testing sets
test, tew_5 = create_dataset(to_train[n_train + n_val:n_train + n_val + n_test], lookback, delay, step, n_test, ori_data.shape[1])

# Initialize a model
def create_model():
    model = models.Sequential([
        layers.Input(shape=(lookback // step, 15)),  # Input shapes
        layers.SeparableConv1D(64, 5, activation='relu'),
        layers.LSTM(32),
        layers.Dense(1)
    ])
    model.compile(optimizer='nadam', loss='mae')
    return model

# Calculate error (MAE)
def calculate_mae(predictions, actual_values):
    return np.mean(np.abs(predictions - actual_values))

# Calculate feature importance
def calculate_feature_importance(model, test_data, test_labels, feature_data, metric_func):
    feature_importance = []
    for feature_index in tqdm(range(feature_data.shape[2])): # Iterate through all features
        # Save the current feature data
        original_feature = test_data[:, :, feature_index].copy()
        
        # Shuffle the current feature
        np.random.shuffle(test_data[:, :, feature_index])
        
        # Re-predict and calculate the error
        predictions = model.predict(test_data)
        metric = metric_func(predictions[:, 0], test_labels)
        feature_importance.append(metric)
        
        # Restore the original feature data
        test_data[:, :, feature_index] = original_feature
    
    return feature_importance

# Plot feature importance ranking and save as an image
def plot_feature_importance(importance, title, filename):
    importance_df = pd.DataFrame({
        'Feature': ori_data.columns,
        'Importance': importance
    })
    importance_df = importance_df.sort_values(by='Importance', ascending=False)
    
    plt.figure(figsize=(10, 6))
    plt.barh(importance_df['Feature'], importance_df['Importance'])
    plt.xlabel('Importance')
    plt.title(title)
    plt.savefig(filename)
    plt.close()

# Save all feature importance results
feature_importance_all = []

# Loop through each target variable and train the model
for i in range(0, 12):   # Corresponds to "WT0.5" to "WT12" (range(0, 12) = [0,1,2,...,11])
    target_col = ori_data.columns[i]  # Current target column
    
    # Update the target variable for training and validation sets
    train_target = train[:, 0, i]
    val_target = val[:, 0, i]
    test_target = tew_5

    # Initialize the model and train
    model = create_model()
    model.fit(train, train_target, epochs=15, batch_size=32, validation_data=(val, val_target),
              callbacks=[tf.keras.callbacks.ModelCheckpoint(f"{target_col}_watertemp.keras", save_best_only=True),
                         tf.keras.callbacks.ReduceLROnPlateau(monitor="val_loss", factor=0.2, patience=5)])
    
    # Calculate feature importance
    importance_mae = calculate_feature_importance(model, test, test_target, train, calculate_mae)
    
    # Save feature importance to Excel
    feature_importance_df = pd.DataFrame({
        'Feature': ori_data.columns,
        f'Importance (MAE)': importance_mae,
    })
    feature_importance_all.append(feature_importance_df)

    # Plot and save the image
    plot_feature_importance(importance_mae, f'Feature Importance by MAE for {target_col}', f'feature_importance_MAE{i}.jpg')

# Combine all feature importance results and save as an Excel file
final_feature_importance_df = pd.concat(feature_importance_all, axis=1)
final_feature_importance_df.to_excel("feature_importance_df.xlsx", index=False)

print("Feature importance plots saved and Excel file created.")
