% ===============================================
% Ramsey Growth Model with Dynare
% This version: 02.12.2025
% Author: Oliver Holtemoeller
% Tested: MATLAB 2023b, MATLAB 2024a and Dynare 5.2
%         Octave 6.4 and Dynare 5.2
% ===============================================

clear all;
close all;

% addpath c:/dynare/5.2/matlab

DoSavePlots = 0;
ShockSpec   = 1;

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

disp(' ');
disp('******************************************');
disp('*** Ramsey Growth Model                ***');
disp('******************************************');
disp(' ');

% Call Dynare model
if ShockSpec==1
    dynare ramseygrowth_temp.mod noclearall;
end

if ShockSpec==2
    dynare ramseygrowth_perm noclearall;
end

if ShockSpec==3
    dynare ramseygrowth_expd noclearall;
end

% Baseline exogenous variables
T = length(k);
exog_0.Z = ones(T,1);
initval.A = 1;
initval.N = 1;
exog_0.N = (initval.N*(1+n).^(0:(T-1)))';
exog_0.A = (initval.A*(1+a).^(0:(T-1)))';

% Baseline
endog_0.k = k(1)*ones(T,1);
endog_0.c = c(1)*ones(T,1);
endog_0.y = y(1)*ones(T,1);
endog_0.i = i(1)*ones(T,1);
endog_0.Y = endog_0.y.*exog_0.A.*exog_0.N;
endog_0.K = endog_0.k.*exog_0.A.*exog_0.N;
endog_0.C = endog_0.c.*exog_0.A.*exog_0.N;

% Exogenous variables in the Alternative Scenario
exog_1.Z = oo_.exo_simul(:,1);
exog_1.N = exog_0.N;
exog_1.A = exog_0.A;

% Endogenous variables in the Alternative Scenario
% Endogenous variables from the workspace need to be transposed
% ... if send_endogenous_variables_to_workspace is used in the mod file
% ... (which is not necessary in Dynare 5.2 but in newer versions)
endog_1.k = k';
endog_1.c = c';
endog_1.y = y';
endog_1.i = i';
endog_1.Y = endog_1.y.*exog_1.N.*exog_1.A;
endog_1.K = endog_1.k.*exog_1.N.*exog_1.A;
endog_1.C = endog_1.c.*exog_1.N.*exog_1.A;

fig_counter = fig_counter + 1;
hf = figure(fig_counter);
Time = 1:min(T,100);
subplot(2,2,1);
hold on;
plot(Time, endog_1.K(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, endog_0.K(1:length(Time)), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Capital');
subplot(2,2,2);
hold on;
plot(Time, endog_1.C(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, endog_0.C(1:length(Time)), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Consumption');
subplot(2,2,3);
hold on;
plot(Time, endog_1.Y(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, endog_0.Y(1:length(Time)), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Output');
subplot(2,2,4);
hold on;
plot(Time, exog_1.Z(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, exog_0.Z(1:length(Time)), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Total Factor Productivity');

if DoSavePlots
    saveas(hf, ['../../figures/fig_Ramsey-Growth-',num2str(ShockSpec),'.png'], 'png');
end

fig_counter = fig_counter + 1;
hf = figure(fig_counter);
subplot(2,2,1);
hold on;
plot(Time, 100*endog_1.K(1:length(Time))./endog_0.K(1:length(Time))-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Capital');
subplot(2,2,2);
hold on;
plot(Time, 100*endog_1.C(1:length(Time))./endog_0.C(1:length(Time))-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Consumption');
subplot(2,2,3);
hold on;
plot(Time, 100*endog_1.Y(1:length(Time))./endog_0.Y(1:length(Time))-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Output');
subplot(2,2,4);
hold on;
plot(Time, 100*exog_1.Z(1:length(Time))./exog_0.Z(1:length(Time))-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Total Factor Productivity');

if DoSavePlots
    saveas(hf, ['../../figures/fig_Ramsey-Growth-Rel-',num2str(ShockSpec),'.png'], 'png');
end

disp('');
disp('******************************************');