// Ramsey Model: Expected Productivity Shock
// Oliver Holtemoeller (v1) 26.11.2025
// Tested: Dynare 5.2

// 1. Block: Definition of Variables and Parameters
// Endogenous variables:
var kV cV;
// Exogenous variables:
varexo ZV;

// Parameters
parameters alphaP, deltaP, thetaP, rhoP, betaP;
thetaP = 1;
alphaP = 0.3;
deltaP = 0.02;
rhoP = 0.01;
betaP = 1/(1+rhoP);

// 2. Model Block
model;
// Consumption Euler Equation
betaP*(cV(+1)/cV)^(-thetaP)*(alphaP*ZV(+1)*kV^(alphaP-1)+1-deltaP) = 1;
// Resource Constraint
ZV*kV(-1)^alphaP - cV - kV + (1-deltaP)*kV(-1);
end;

// Initialization Block
initval;
ZV = 1;
kV = 10;
cV = 2;
end;

// Compute Initial Steady State:
steady;

// Specify Expected Productivity shock
endval;
ZV = 1.1;
end;

// Compute Terminal Steady State
steady;

shocks;
var ZV;
periods 1:19;
values 1;
end;

// Perfect Foresight Simulation of Adjustment Path
perfect_foresight_setup(periods=300);
perfect_foresight_solver;
send_endogenous_variables_to_workspace;
Z = oo_.exo_simul;

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
plot(Time, Z(1:length(Time)), 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, Z(1)*ones(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
title('Total Factor Productivity');

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
plot(Time, 100*Z(1:length(Time))/Z(1)-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/3);
hold off;
ylabel('% dev.');
title('Total Factor Productivity');
