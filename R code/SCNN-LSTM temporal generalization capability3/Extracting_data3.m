% Data preprocessing
close all;
clc;
clear;

% Define sheet names
sheets = {'2008', '2009', '2010', '2011'};

% Read data from the current sheet
data = xlsread('Extracted_Blel1.xlsx', 'Sheet2');

% Extract data every 7 rows
extractedData = data(1:7:end, :);

% Save the extracted data into a specific sheet in a new file
xlswrite('Extracted_Blel3.xlsx', extractedData, 'Sheet1');

% Notify user of completion
disp(['Data has been successfully extracted and saved to sheet ', 'Sheet1', ' in Extracted_Blel3.xlsx.']);
