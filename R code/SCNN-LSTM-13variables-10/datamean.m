% Data preprocessing
close all;
clc;
clear;

% List of filenames
filenames = {'CSNN_.5.csv', 'CSNN_1.csv', 'CSNN_2.csv', 'CSNN_3.csv', ...
             'CSNN_4.csv', 'CSNN_5.csv', 'CSNN_6.csv', 'CSNN_7.csv', ...
             'CSNN_8.csv', 'CSNN_9.csv', 'CSNN_10.csv','CSNN_12.csv'};

% Create an empty array to store data from all files
allData = [];

% Loop through each file to read the data range A2:M11
for i = 1:length(filenames)
    % Read the data range A2:M11 using xlsread
    data = xlsread(filenames{i}, 'A2:M11');
    % Append the data to allData, arranging it column-wise
    allData = [allData; data];
end

% Calculate the mean of each column
columnMeans = mean(allData, 1);
