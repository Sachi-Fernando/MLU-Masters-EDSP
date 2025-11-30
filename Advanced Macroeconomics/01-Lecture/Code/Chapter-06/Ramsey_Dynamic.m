% ===============================================
% Ramsey Dynamic Equilibrium
% This version (v1): 26.11.2025
% Author: Oliver Holtemoeler
% Tested: MATLAB 2023b, 2024a
% ===============================================

clear all;
close all;

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
disp('*** Ramsey Dynamic Equilibrium         ***');
disp('******************************************');
disp(' ');

% Parameters
params.alpha = 0.3;
params.rho   = 0.01;
params.delta = 0.02;
params.theta = 1;

% Exogenous: Productivity shock
exog.T = 300;

% Permanent Productivity Shock
if ShockSpec==1
    exog.Zpath = [ 1; ones(exog.T-1,1)*1.1 ];
end

% Temporary Productivity Shock
if ShockSpec==2
    exog.Zpath = [ 1; 1.1; ones(exog.T-2,1) ];
end

% Expected Productivity Shock
if ShockSpec==3
    exog.Zpath = [ ones(20,1); ones(exog.T-20,1)*1.1 ];
end

% Solve for initial steady state
exog.Z = exog.Zpath(1); 
startvals = [ 10; 2 ];

options = optimset('MaxIter', 10000, 'MaxFunEvals', 90000, 'TolX', 1e-16, 'TolFun', 1e-16);
[ fsopt, fval, efl ] = fsolve(@(x)ramseysteadystate(x, exog, params), startvals, options);

initvals = fsopt; % Store initial steady state solution in initvals

disp('Initial steady state:');
disp('---------------------');
disp(['k: ', num2str(initvals(1))]);
disp(['c: ', num2str(initvals(2))]);
disp(' ');
disp(['Checksum: ', num2str(max(abs(fval)))]);
disp(['Exit flag: ', num2str(efl)]);
disp(' ');

% Solve for final steady state
exog.Z = exog.Zpath(end);
startvals = initvals;
[ fsopt, fval, efl ] = fsolve(@(x)ramseysteadystate(x, exog, params), startvals, options);

endvals = fsopt; % Store final steady state solution in endvals

disp('Final steady state:');
disp('-------------------');
disp(['k: ', num2str(endvals(1))]);
disp(['c: ', num2str(endvals(2))]);
disp(' ');
disp(['Checksum: ', num2str(max(abs(fval)))]);
disp(['Exit flag: ', num2str(efl)]);
disp(' ');

% Solve for dynamic path
exog.Z = exog.Zpath; 
startvals = [ ones(exog.T,1)*initvals(1); ones(exog.T,1)*initvals(2) ];

disp('Dynamic path:');
disp('-------------');

[ fsopt, fval, efl ] = fsolve(@(x)ramseyloop(x, exog, initvals, endvals, params), startvals, options);

endog.k = fsopt(1:exog.T);
endog.c = fsopt((exog.T+1):(2*exog.T));

disp(['Checksum: ', num2str(max(abs(fval)))]);
disp(['Exit flag: ', num2str(efl)]);
disp(' ');

fig_counter = fig_counter + 1;
hf = figure(fig_counter);
Time = 1:min(exog.T,100);
subplot(3,1,1);
hold on;
plot(Time, endog.k(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, endog.k(1)*ones(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Capital');
subplot(3,1,2);
hold on;
plot(Time, endog.c(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, endog.c(1)*ones(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Consumption');
subplot(3,1,3);
hold on;
plot(Time, exog.Z(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, exog.Z(1)*ones(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Total Factor Productivity');

if DoSavePlots
    saveas(hf, ['../../figures/fig_Ramsey-',num2str(ShockSpec),'.png'], 'png');
end

fig_counter = fig_counter + 1;
hf = figure(fig_counter);
Time = 1:min(exog.T,100);
subplot(3,1,1);
hold on;
plot(Time, 100*endog.k(1:length(Time))/endog.k(1)-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Capital');
subplot(3,1,2);
hold on;
plot(Time, 100*endog.c(1:length(Time))/endog.c(1)-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Consumption');
subplot(3,1,3);
hold on;
plot(Time, 100*exog.Z(1:length(Time))/exog.Z(1)-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Total Factor Productivity');

if DoSavePlots
    saveas(hf, ['../../figures/fig_Ramsey-rel-',num2str(ShockSpec),'.png'], 'png');
end

fig_counter = fig_counter + 1;
hf = figure(fig_counter);
hold on;
plot(fval(1:exog.T));
plot(fval((exog.T+1):(2*exog.T)));
hold off;

disp('******************************************');