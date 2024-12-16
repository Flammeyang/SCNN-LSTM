% Data preprocessing
close all;
clc;
clear;

% Define sheet names
sheets = {'2008', '2009', '2010', '2011'};

% Loop through each sheet
for i = 1:length(sheets)
    % Read data from the current sheet
    data = xlsread('Blel_2008_2009_2010_2011.xlsx', sheets{i});
    
    % Extract data every 24 rows
    extractedData = data(1:24:end, :);
    
    % Save the extracted data into different sheets within the same file
    xlswrite('Extracted_Blel.xlsx', extractedData, sheets{i});
    
    % Notify user of completion
    disp(['Data has been successfully extracted and saved to sheet ', sheets{i}, ' in Extracted_Blel.xlsx.']);
end
