% Data Preprocessing
close all;
clc;
clear;

% Read data using csvread (plain numeric file without headers)
data = csvread('SCNN_SD_distribution.csv'); % Directly read all data
values = data(:); % Flatten data into a column vector

% Calculate the mean value
mean_value = mean(values);

% Display the mean value
disp(['The mean value of the data is: ', num2str(mean_value)]);

% Define range intervals
edges = 0:0.1:0.6; % Intervals [0.0-0.1, 0.1-0.2, ..., 0.5-0.6]

% Calculate frequency and percentage for each interval
[counts, ~] = histcounts(values, edges); % Count the frequency for each interval
percentages = counts / sum(counts) * 100; % Calculate percentages

% Output analysis results to the command window
disp('Percentage analysis for each range:');
for i = 1:length(edges)-1
    fprintf('%.1f - %.1f: %.2f%%\n', edges(i), edges(i+1), percentages(i));
end

% Plot distribution histogram
figure;
histogram(values, edges, 'FaceColor', [0.2, 0.6, 0.8], 'EdgeColor', 'black');
xlabel('Value Ranges');
ylabel('Frequency');
title('SCNN_SD_distribution Value Ranges');
grid on;

% Plot percentage bar chart
figure;
bar(edges(1:end-1) + diff(edges)/2, percentages, 'FaceColor', [0.6, 0.2, 0.8]);
set(gca, 'XTick', edges(1:end-1) + diff(edges)/2); % Set x-axis ticks
set(gca, 'XTickLabel', {'0.0-0.1', '0.1-0.2', '0.2-0.3', '0.3-0.4', '0.4-0.5', '0.5-0.6'});
xlabel('Value Ranges');
ylabel('Percentage (%)');
title('SCNN_SD_distribution Percentage Distribution');
grid on;
