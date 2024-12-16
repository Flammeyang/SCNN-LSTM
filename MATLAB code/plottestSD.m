% Data Preprocessing
close all;
clc;
clear;

% Read data
data = xlsread('SCNN_SD.csv', 'C3:N8762'); % Read temperature data
% Assume data is a 2D matrix
maxValue = max(data(:)); % Get the maximum value of data
minValue = min(data(:)); % Get the minimum value of data

% Output results
disp(['Maximum value: ', num2str(maxValue)]);
disp(['Minimum value: ', num2str(minValue)]);

y = xlsread('SCNN_SD.csv', 'A3:A8762'); % Optional additional data reading
Tem = data; % Temperature data
x = 1:8760; % Time
year = [0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12]; % Depth

% Adjust grid coordinates to fit the input requirements of pcolor
[X, Y] = meshgrid(x, year); % Generate grid
figure(1);
% Draw pseudocolor plot
h = pcolor(X, Y, Tem'); % Transpose Tem to align with the grid of X and Y
set(gcf, 'Position', [200, 50, 600, 380]);

shading interp;
colormap(jet);

% Set axis
axis([-100 8850 0.3 12.05]); % Axis range
set(gca, 'YDir', 'reverse'); % Reverse depth axis
xlabel('Day since 01 Jan 2011', 'FontSize', 16);
ylabel('Depth (m)', 'FontSize', 16);
title('(c) SCNN-LSTM prediction of temperature SD profile', 'FontSize', 16);

% Draw colorbar
h = colorbar; % Get colorbar handle
set(h, 'FontSize', 11, 'LineWidth', 1.5); % Set font size and line width for the colorbar
h.Position = [0.880, 0.13, 0.045, 0.8];   % Set colorbar position
caxis([0.000, 0.6]); % Set color range

% Adjust colorbar ticks
ticks = h.Ticks; % Get colorbar ticks
tickLabels = arrayfun(@(x) sprintf('%.3f', x), ticks, 'UniformOutput', false); % Format ticks to 3 decimal places
h.TickLabels = tickLabels;

% Remove extra white margins
set(gca, 'LooseInset', [0.02, 0.02, 0.135, 0.02]);

% Set axis tick style
ax = gca;
ax.TickLength = [0.005, 0.00];
ax.XTick = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000];
ax.YTick = [0, 2, 4, 6, 8, 10, 12];
set(gca, 'FontName', 'Arial', 'FontSize', 15, 'LineWidth', 1.5);