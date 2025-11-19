% ===============================================
% Solow Model: Steady State
% This version: 16.11.2025
% Oliver Holtemoeller
% Tested: MATLAB 2024a, Octave 10.2.0
% ===============================================

clear all;
close all;

DoSavePlots = 0;

% Colorblind barrier-free color pallet
BfBlack        = [   0,   0,   0 ]/255;
BfOrange       = [ 230, 159,   0 ]/255;
BfSkyBlue      = [  86, 180, 233 ]/255;
BfBluishGreen  = [   0, 158, 115 ]/255;
BfYellow       = [ 240, 228,  66 ]/255;
BfBlue         = [   0, 114, 178 ]/255;
BfVermillon    = [ 213,  94,   0 ]/255;
BfRedishPurple = [ 204, 121, 167 ]/255;

disp(' --------------------------------');
disp(' Solow Growth Model: Steady State');
disp(' ');

% Set parameters and exogenous variables
BaselineParams.s     = 0.2;               % Savings ratio
BaselineParams.alpha = 0.3;               % capital elasticity of ouput
BaselineParams.delta = 0.03;              % depreciation rate
BaselineParams.n     = 0.01;              % Population growth rate
BaselineParams.a     = 0.01;              % Labor efficiency growth rate
BaselineParams.Z     = 1;                 % Total factor productivity

% Compute steady state values
[ kast, yast, cast ] = solowsteadystate(BaselineParams);

disp(' ');
disp('Check left-hand side and right-hand side of condition')
s     = BaselineParams.s;
alpha = BaselineParams.alpha;
delta = BaselineParams.delta;
n     = BaselineParams.n;
a     = BaselineParams.a;
Z     = BaselineParams.Z;
lhs = s*Z*kast^alpha;
rhs = (delta+n+a)*kast;
if (round(lhs*10^8)/10^8)==(round(rhs*10^8)/10^8)
  disp('Ok.')
else
  disp('Error.')
end

disp(' ');
disp('Steady state values:');
disp(['k = ', num2str(kast)]);
disp(['y = ', num2str(yast)]);
disp(['c = ', num2str(cast)]);

 % Sensitivity analysis: savings ratio (s)
 params = BaselineParams;
 sVec = 0:0.01:1;
 iMax = length(sVec);
 kVec_s = zeros(iMax,1);
 yVec_s = zeros(iMax,1);
 cVec_s = zeros(iMax,1);
 for ii=1:iMax
   params.s = sVec(ii);
   [kVec_s(ii), yVec_s(ii), cVec_s(ii) ] = solowsteadystate(params);
 end

 % Sensitivity analysis: total factor productivity (Z)
 params = BaselineParams;
 ZVec = 1:0.1:2;
 iMax = length(ZVec);
 kVec_Z = zeros(iMax,1);
 YVec_Z = zeros(iMax,1);
 CVec_Z = zeros(iMax,1);
 KVec_Z = zeros(iMax,1);
 for ii=1:iMax
   params.Z = ZVec(ii);
   [kVec_Z(ii), yVec_Z(ii), cVec_Z(ii)] = solowsteadystate(params);
 end

 % Sensitivity analysis: growth rate of labor efficiency (a)
 params = BaselineParams;
 aVec = 0.000:0.005:0.05;
 iMax = length(aVec);
 kVec_a = zeros(iMax,1);
 yVec_a = zeros(iMax,1);
 cVec_a = zeros(iMax,1);
  for ii=1:iMax
   params.a = aVec(ii);
   [kVec_a(ii), yVec_a(ii), cVec_a(ii)] = solowsteadystate(params);
 end

 % Sensitivity analysis: depreciation rate (delta)
 params = BaselineParams;
 deltaVec = 0.000:0.005:0.10;
 iMax = length(deltaVec);
 kVec_delta = zeros(iMax,1);
 yVec_delta = zeros(iMax,1);
 cVec_delta = zeros(iMax,1);
 for ii=1:iMax
   params.delta = deltaVec(ii);
   [kVec_delta(ii), yVec_delta(ii), cVec_delta(ii)] = solowsteadystate(params);
 end

 % Sensitivity analysis: growth rate of population (n)
 params = BaselineParams;
 nVec = 0.000:0.005:0.05;
 iMax = length(nVec);
 kVec_n = zeros(iMax,1);
 yVec_n = zeros(iMax,1);
 cVec_n = zeros(iMax,1);
  for ii=1:iMax
   params.n = nVec(ii);
   [kVec_n(ii), yVec_n(ii), cVec_n(ii)] = solowsteadystate(params);
 end

 % Sensitivity analysis: capital elasticity of output (alpha)
 params = BaselineParams;
 alphaVec = 0.1:0.05:0.6;
 iMax = length(alphaVec);
 kVec_alpha = zeros(iMax,1);
 yVec_alpha = zeros(iMax,1);
 cVec_alpha = zeros(iMax,1);
  for ii=1:iMax
   params.alpha = alphaVec(ii);
   [kVec_alpha(ii), yVec_alpha(ii), cVec_alpha(ii)] = solowsteadystate(params);
 end

% Display results
fig_counter = 0;

% Capital stock per efficient unit of labor
fig_counter = fig_counter + 1;
hf = figure(fig_counter);

subplot(2,3,1);
plot(sVec, kVec_s, 'color', BfBlue, 'LineWidth', 3);
title('Capital per efficiency unit of labor');
xlabel('savings ratio (s)');
ylabel('capital (k)');

subplot(2,3,2);
plot(ZVec, kVec_Z, 'color', BfBlue, 'LineWidth', 3);
title('Capital per efficiency unit of labor');
xlabel('total factor productivity (Z)');
ylabel('capital (k)');

subplot(2,3,3);
plot(aVec, kVec_a, 'color', BfBlue, 'LineWidth', 3);
title('Capital per efficiency unit of labor');
xlabel('growth rate of labor efficiency (a)');
ylabel('capital (k)');

subplot(2,3,4);
plot(deltaVec, kVec_delta, 'color', BfBlue, 'LineWidth', 3);
title('Capital per efficiency unit of labor');
xlabel('depreciation rate (delta)');
ylabel('capital (k)');

subplot(2,3,5);
plot(nVec, kVec_n, 'color', BfBlue, 'LineWidth', 3);
title('Capital per efficiency unit of labor');
xlabel('population growth rate (n)');
ylabel('capital (k)');

subplot(2,3,6);
plot(alphaVec, kVec_alpha, 'color', BfBlue, 'LineWidth', 3);
title('Capital per efficiency unit of labor');
xlabel('capital elasticity of output (alpha)');
ylabel('capital (k)');

if DoSavePlots
  saveas(hf, '../../figures/fig_SolowSteadyCapital.png', 'png');
end

% Output
fig_counter = fig_counter + 1;
hf = figure(fig_counter);

subplot(2,3,1);
plot(sVec, yVec_s, 'color', BfBlue, 'LineWidth', 3);
title('Output per efficiency unit of labor');
xlabel('savings ratio (s)');
ylabel('output (y)');

subplot(2,3,2);
plot(ZVec, yVec_Z, 'color', BfBlue, 'LineWidth', 3);
title('Output per efficiency unit of labor');
xlabel('total factor productivity (Z)');
ylabel('output (y)');

subplot(2,3,3);
plot(aVec, yVec_a, 'color', BfBlue, 'LineWidth', 3);
title('Output per efficiency unit of labor');
xlabel('growth rate of labor efficiency (a)');
ylabel('output (y)');

subplot(2,3,4);
plot(deltaVec, yVec_delta, 'color', BfBlue, 'LineWidth', 3);
title('Output per efficiency unit of labor');
xlabel('depreciation rate (delta)');
ylabel('output (y)');

subplot(2,3,5);
plot(nVec, yVec_n, 'color', BfBlue, 'LineWidth', 3);
title('Output per efficiency unit of labor');
xlabel('population growth rate (n)');
ylabel('output (y)');

subplot(2,3,6);
plot(alphaVec, yVec_alpha, 'color', BfBlue, 'LineWidth', 3);
title('Output per efficiency unit of labor');
xlabel('capital elasticity of output (alpha)');
ylabel('output (y)');

if DoSavePlots
  saveas(hf, '../../figures/fig_SolowSteadyOutput.png', 'png');
end

% Consumption
fig_counter = fig_counter + 1;
hf = figure(fig_counter);

subplot(2,3,1);
plot(sVec, cVec_s, 'color', BfBlue, 'LineWidth', 3);
title('Consumption per efficiency unit of labor');
xlabel('savings ratio (s)');
ylabel('consumption (C)');

subplot(2,3,2);
plot(ZVec, cVec_Z, 'color', BfBlue, 'LineWidth', 3);
title('Consumption per efficiency unit of labor');
xlabel('total factor productivity (Z)');
ylabel('consumption (C)');

subplot(2,3,3);
plot(aVec, cVec_a, 'color', BfBlue, 'LineWidth', 3);
title('Consumption per efficiency unit of labor');
xlabel('growth rate of labor efficiency (a)');
ylabel('consumption (C)');

subplot(2,3,4);
plot(deltaVec, cVec_delta, 'color', BfBlue, 'LineWidth', 3);
title('Consumption per efficiency unit of labor');
xlabel('depreciation rate (delta)');
ylabel('consumption (C)');

subplot(2,3,5);
plot(nVec, cVec_n, 'color', BfBlue, 'LineWidth', 3);
title('Consumption per efficiency unit of labor');
xlabel('population growth rate (n)');
ylabel('consumption (C)');

subplot(2,3,6);
plot(alphaVec, cVec_alpha, 'color', BfBlue, 'LineWidth', 3);
title('Consumption per efficiency unit of labor');
xlabel('capital elasticity of output (alpha)');
ylabel('consumption (C)');

if DoSavePlots
  saveas(hf, '../../figures/fig_SolowSteadyConsumption.png', 'png');
end

disp(' ');
disp(' Done. Bye.');
disp(' --------------------------------');
