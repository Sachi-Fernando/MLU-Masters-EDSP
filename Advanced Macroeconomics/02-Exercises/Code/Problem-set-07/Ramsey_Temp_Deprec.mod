// Ramsey Model: Temporary Productivity Shock
// Oliver Holtemoeller (v1) 26.11.2025
// Tested: Dynare 5.2

% Adjusted by Alexandra Gutsch
% Temporary shock on depreciation rate

// 1. Block: Definition of Variables and Parameters
// Endogenous variables:
var kV cV;
// Exogenous variables:
varexo ZV deltaV; % this is the shock variable for delta

// Parameters
parameters alphaP, deltaP, thetaP, rhoP, betaP;
thetaP = 1;
alphaP = 0.3;
deltaP = 0.02; % this is delta without shock
rhoP = 0.01;
betaP = 1/(1+rhoP);

// 2. Model Block
model;
// Consumption Euler Equation
betaP*(cV(+1)/cV)^(-thetaP)*(alphaP*ZV(+1)*kV^(alphaP-1)+1-deltaV) = 1; % change deltaP to deltaV
// Resource Constraint
ZV*kV(-1)^alphaP - cV - kV + (1-deltaV)*kV(-1); % change deltaP to deltaV
end;

// Initialization Block
initval;
ZV = 1;
deltaV = deltaP; % assign steady state value of delta
kV = 10;
cV = 2;
end;

// Compute Initial Steady State:
steady;

// Specify Temporary Productivity shock
shocks;
var deltaV; % shock on delta
periods 1:1;
values 0.05; % value of delta in schock period > deltaP
end;

// Compute Terminal Steady State
steady;

// Perfect Foresight Simulation of Adjustment Path
perfect_foresight_setup(periods=300);
perfect_foresight_solver;
send_endogenous_variables_to_workspace;
Z = oo_.exo_simul(:, 1); % extract time path of depreciation rate
delta = oo_.exo_simul(:, 2); % extract time path of depreciation rate

// Plot Results
StdLineWidth = 2;
fig_counter = 0;

// Colorblind barrier-free color pallet
BfBlack        = [   0,   0,   0 ]/255;
BfOrange       = [ 230, 159,   0 ]/255;
BfSkyBlue      = [  86, 180, 233 ]/255;
BfBluishGreen  = [   0, 158, 115 ]/255;
BfYellow       = [ 240, 228,  66 ]/255;
BfBlue         = [   0, 114, 178 ]/255;
BfVermillon    = [ 213,  94,   0 ]/255;
BfRedishPurple = [ 204, 121, 167 ]/255;

fig_counter = fig_counter + 1;
hf = figure(fig_counter);
Time = 1:min(length(Z),100);
subplot(3,1,1);
hold on;
plot(Time, kV(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, kV(1)*ones(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Capital');
subplot(3,1,2);
hold on;
plot(Time, cV(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, cV(1)*ones(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Consumption');
subplot(3,1,3);
hold on;
plot(Time, delta(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth); % change Z to delta
plot(Time, delta(1)*ones(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3); % change Z to delta
hold off;
title('Depreciation Rate'); % change plot title

fig_counter = fig_counter + 1;
hf = figure(fig_counter);
Time = 1:min(length(Z),100);
subplot(3,1,1);
hold on;
plot(Time, 100*kV(1:length(Time))/kV(1)-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Capital');
subplot(3,1,2);
hold on;
plot(Time, 100*cV(1:length(Time))/cV(1)-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Consumption');
subplot(3,1,3);
hold on;
plot(Time, 100*delta(1:length(Time))/delta(1)-100, 'color', BfBlue, 'LineWidth', StdLineWidth); % change Z to delta
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Depreciation Rate'); % change plot title