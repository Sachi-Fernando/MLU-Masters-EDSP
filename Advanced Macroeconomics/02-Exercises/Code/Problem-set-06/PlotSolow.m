%------------------------------------------------------%
%-------------- Plot Solow Growth Model ---------------%
%------------------------------------------------------%

disp('************************************************');
disp('Plot Solow Growth Model');

clear all;
close all;

% Some useful definitions
% -----------------------

% Save plots
DoSavePlots = 1;

% Line Properties
StdLineWidth = 2;

% Colorblind barrier-free color pallet
BfBlack        = [   0,   0,   0 ]/255;
BfOrange       = [ 230, 159,   0 ]/255;
BfSkyBlue      = [  86, 180, 233 ]/255;
BfBluishGreen  = [   0, 158, 115 ]/255;
BfYellow       = [ 240, 228,  66 ]/255;
BfBlue         = [   0, 114, 178 ]/255;
BfVermillon    = [ 213,  94,   0 ]/255;
BfRedishPurple = [ 204, 121, 167 ]/255;

% Load data from Dynare simulations
% ---------------------------------

% name of Excel file
filename = 'results.xlsx';

% different tables for scenarios
data_1 = readtable(filename, 'Sheet', 'z'); % permanent increase in TFP
data_2 = readtable(filename, 'Sheet', 's'); % permanend increase in savings rate
data_3 = readtable(filename, 'Sheet', 'n'); % permanent increase in population growth
    
% Extract variable names and scenarios
% ------------------------------------

% all variables
vars = data_1.Properties.VariableNames;

% variables not to log-transform
no_log_vars = {'r', 's', 'n'};

% variables that should always be plotted
common_vars = {'k', 'y', 'c', 'r', 'w'};

% variables that should be plotted depending on scenario
scenario_vars = {'z', 's', 'n'};

% scenarios
scenario_titles = {'Total Factor Productivity', 'Savings Rate', 'Growth Rate of Population'};

% datasets
data_all = {data_1, data_2, data_3};

% Plot time paths of variables
% ----------------------------

for Scenario = 1:3

    % name of figure
    figure('Name', scenario_titles{Scenario});
    
    % data for current scenario
    current_data = data_all{Scenario};
    
    % list of variables for current scenario
    vars_to_plot = [common_vars, scenario_vars(Scenario)];
    
    for i = 1:6

        % plot with subplots
        subplot(2, 3, i); 

        % current variable
        varname = vars_to_plot{i};
        
        % time series from table
        series = current_data.(varname);
        
        % calculate deviation from initial steady state
        if ismember(varname, no_log_vars)
            % in percentage points for rates
            irf = 100 * (series - series(1)); 
            y_label = '%-pts.';
        else
            % in percent for levels
            irf = 100 * (log(series) - log(series(1))); 
            y_label = '%-dev.';
        end
        
        % plot simulated time series
        plot(irf, 'Color', BfBlue, 'LineWidth', StdLineWidth);
        title(varname);
        xlabel('t');
        ylabel(y_label);
        grid on;
    end
    
    % title of plot
    sgtitle(['Permanent Shock on ', scenario_titles{Scenario}]);
    
    % save plot optionally
    if DoSavePlots
        saveas(gcf, fullfile(sprintf('Scenario%d.png', Scenario)));
    end
end

disp('************************************************');
