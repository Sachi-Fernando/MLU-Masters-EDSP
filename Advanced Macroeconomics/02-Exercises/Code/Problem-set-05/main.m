% Advanced Macroeconomics - Tutorial - Winter 25/26
% Alexandra Gutsch
% Date: 04/11/2025

clear all;
close all;

DoEqu = 1;
DoPlot = 1;
DoSens = 1;

SavePlot = 1;

fig_counter = 0;

% Parameters
pars.alpha = 0.3; 
pars.phi   = 1.5;               
pars.rho   = 0.05;  
pars.tau   = 0.2;          
pars.theta = 1.8;           

% Exogenous Variables
exo.A1 = 1.2;  
exo.A2 = 1;                
exo.k1 = 1;   

% Initial values for Endogenous Variables
    % x = [c1, c2, k2, T, n, y1, y2, r1, r2, w]
initval = [1.5; 1; 0.5; 0.5; 1; 1; 1; 0.3; 1; 0.7];                                 

% Solve nonlinear System
options = optimset('MaxIter', 10000, 'MaxFunEvals', 100000);

% Compute equilibrium
if DoEqu
    computeequ;
    if DoPlot
        % Display equilibrium as bar plots
        barplots;
    end
end

% Sensitivity analysis
if DoSens
    sensitivity;
end