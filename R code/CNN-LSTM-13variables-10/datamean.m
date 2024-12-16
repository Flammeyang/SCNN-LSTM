% Data Preprocessing
close all;
clc;
clear;

% List of filenames
filenames = {'CCNN_.5.csv', 'CCNN_1.csv', 'CCNN_2.csv', 'CCNN_3.csv', ...
             'CCNN_4.csv', 'CCNN_5.csv', 'CCNN_6.csv', 'CCNN_7.csv', ...
             'CCNN_8.csv', 'CCNN_9.csv', 'CCNN_10.csv', 'CCNN_12.csv'};

% Create an empty array to store data from all files
allData = [];

% Loop to read data from each file in the range A2:M11
for i = 1:length(filenames)
    % Read data from the range A2:M11 using xlsread
    data = xlsread(filenames{i}, 'A2:M11');
    % Append the data to allData, aligning by columns
    allData = [allData; data];
end

% Calculate the mean of each column
columnMeans = mean(allData, 1);
