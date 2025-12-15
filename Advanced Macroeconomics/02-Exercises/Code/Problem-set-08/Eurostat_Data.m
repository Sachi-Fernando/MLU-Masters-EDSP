%%===========================================================================================%%
%%===================Advanced Macroeconomics Tutorial 8 Winter 25/26 ========================%%
%%===========================================================================================%%

disp('************************************************');
disp('Plot Eurostat National Accounts Data');

clear all;
close all;

LoadData = 1; % Data should be loaded
DefineVars = 1; % Variables should be defined
PlotOverview = 1; % Show plots
DoSavePlots = 1; % Save plots

% Check for Matlab/Octave
% -----------------------
MyEnv.Octave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
MyEnv.Matlab = ~MyEnv.Octave;

% Load packages (Octave only)
% ---------------------------
if ~MyEnv.Matlab
  pkg load io;
  pkg load dataframe;
end

% Some useful defintions
% ----------------------
fig_counter = 0;

% Colorblind barrier-free color pallet
BfBlack        = [   0,   0,   0 ]/255;
BfOrange       = [ 230, 159,   0 ]/255;
BfSkyBlue      = [  86, 180, 233 ]/255;
BfBluishGreen  = [   0, 158, 115 ]/255;
BfYellow       = [ 240, 228,  66 ]/255;
BfBlue         = [   0, 114, 178 ]/255;
BfVermillon    = [ 213,  94,   0 ]/255;
BfRedishPurple = [ 204, 121, 167 ]/255;

% Line Properties
StdLineWidth = 2;

% Load NA Data from EUROSTAT                 
% --------------------------
if LoadData 
    if MyEnv.Matlab
        Data = readtable('Eurostat_Data.csv');
    else
        Data = dataframe('Eurostat_Data.csv');
    end
end

% Define Variables
% -----------------------------------
if DefineVars
    % Define variables for raw data
    GDP_nom = Data.GDP_nom;
    GDP_defl = Data.GDP_defl;
    GOV_nom = Data.GOV_nom;
    GOV_defl = Data.GOV_defl;

    % Time variable
    TIME = Data.TIME;

    % Variables in real terms
    GDP_real = GDP_nom./GDP_defl.*100; 
    GOV_real = GOV_nom./GOV_defl.*100;

    % Government spending to GDP ratio (%)
    GOV_to_GDP = 100.*GOV_real./GDP_real;

    % index 2015 = 100
    GDP_ind = 100.*GDP_real./GDP_real(21);
    GOV_ind = 100.*GOV_real./GOV_real(21);

    % cyclical real GDP and GOV
    GDP_cycle = hpfilter(log(GDP_real), 1600);
    GOV_cycle = hpfilter(log(GOV_real), 1600);

    disp('-------------------------------------------');
    disp('---------- Average GOV/GDP (%) ------------');    
    disp(' ');
    GOV_to_GDP_av = mean(GOV_to_GDP);
    disp(['GOV_to_GDP_av = ', num2str(GOV_to_GDP_av)]);
    disp(' ');
    disp('-------------------------------------------');
    
    disp('-------------------------------------------');
    disp('--------- Correlation GOV and GDP ---------');
    disp(' ');
    GOV_GDP_corr = corr(GOV_cycle, GDP_cycle);
    disp(['GOV_GDP_corr = ', num2str(GOV_GDP_corr)]);
    disp(' ');
    disp('-------------------------------------------');
end

if PlotOverview

    % Number of rows and columns
    rows = 2;
    cols = 1;

    % Plot real GDp as index 2015 = 100
    subplot(rows,cols,1);
    hold on;
    set (gca, 'ygrid', 'on');
    set (gca, 'xgrid', 'on');
    plot(TIME, GDP_ind, 'color', BfBlue, 'linewidth', StdLineWidth);
    title('Real GDP');
    ylabel('index 2010 = 100');
    xlim([TIME(1), TIME(end)]);
    xbounds = xlim();
    set(gca, 'xtick', xbounds(1):1:xbounds(2));
    hold off;
    
    % Plot real government spending as index 2015 = 100
    subplot(rows,cols,2);
    hold on;
    set (gca, 'ygrid', 'on');
    set (gca, 'xgrid', 'on');
    plot(TIME, GOV_ind, 'color', BfBlue, 'linewidth', StdLineWidth);
    title('Real Government Spending');
    ylabel('index 2010 = 100');
    xlim([TIME(1), TIME(end)]);
    xbounds = xlim();   
    set(gca, 'xtick', xbounds(1):1:xbounds(2));
    hold off;
    
    % Save plot as png
    if DoSavePlots
       print('EUROSTAT_NA_ind.png', '-dpng');
    end

    % New object for next plot
    figure();

    % Plot government spending share on GDP
    hold on;
    set (gca, 'ygrid', 'on');
    set (gca, 'xgrid', 'on');
    plot(TIME, GOV_to_GDP_av.*ones(1,length(TIME)), 'linestyle', ':', 'color', BfOrange, 'LineWidth', StdLineWidth);
    plot(TIME, GOV_to_GDP, 'color', BfBlue, 'linewidth', StdLineWidth);
    legend({'average'}, 'location', 'southoutside', 'orientation', 'horizontal');
    legend boxoff;
    title('Government Spending Share on GDP');
    ylabel('percent');
    xlim([TIME(1), TIME(end)]);
    set(gca, 'xtick', xbounds(1):1:xbounds(2));
    xbounds = xlim();
    hold off;
    
    % Save plot as png
    if DoSavePlots
       print('EUROSTAT_NA_share.png', '-dpng');
    end

    % New object for next plot
    figure();
    
    % Plot deviation of real GDP from trend
    subplot(rows,cols,1);
    hold on;
    set (gca, 'ygrid', 'on');
    set (gca, 'xgrid', 'on');
    plot(TIME, GDP_cycle.*100, 'color', BfBlue, 'linewidth', StdLineWidth);
    title('Cyclical Real GDP');
    ylabel('percentage deviation');
    xlim([TIME(1), TIME(end)]);
    xbounds = xlim();
    set(gca, 'xtick', xbounds(1):1:xbounds(2));
    hold off;
    
    % Plot deviation from real government spending from trend
    subplot(rows,cols,2);
    hold on;
    set (gca, 'ygrid', 'on');
    set (gca, 'xgrid', 'on');
    plot(TIME, GOV_cycle.*100, 'color', BfBlue, 'linewidth', StdLineWidth);
    title('Cyclical Government Spending');
    ylabel('percentage deviation');
    xlim([TIME(1), TIME(end)]);
    xbounds = xlim();   
    set(gca, 'xtick', xbounds(1):1:xbounds(2));
    hold off;
    
    % Save plot as png
    if DoSavePlots
       print('EUROSTAT_NA_cyc.png', '-dpng');
    end

    % New object for next plot
    figure();

    % Plot correlation between GDP and government spending
    hold on;
    set (gca, 'ygrid', 'on');
    set (gca, 'xgrid', 'on');
    scatter(GDP_cycle, GOV_cycle, 'filled');
    title('Correlation between cyclical real GDP and GOV');
    xlabel('GDP');
    ylabel('GOV');
    hold off;
    
    % Save plot as png
    if DoSavePlots
       print('EUROSTAT_NA_Corr.png', '-dpng');
    end
end

