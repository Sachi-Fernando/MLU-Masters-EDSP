% ====================================================================
% Solow Model: Competitive Markets, Real Interest Rate, and Real Wages
% This version: 16.11.2025
% Oliver Holtemoeller
% Tested: Octave 9.2.0, Octave 10.2.0, MATLAB 2024a
% =====================================================================

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

disp(' ---------------------------------------');
disp(' Solow Growth Model: Competitive Markets');
disp(' ');

% Set parameters and exogenous variables
BaselineParams.s     = 0.2;               % Savings ratio
BaselineParams.alpha = 0.3;               % capital elasticity of ouput
BaselineParams.delta = 0.03;              % depreciation rate
BaselineParams.n     = 0.01;              % Population growth rate
BaselineParams.a     = 0.01;              % Labor efficiency growth rate
BaselineParams.Z     = 1;                 % Total factor productivity

% Compute steady state values
[ kast, yast, cast, rast, wast ] = solowsteadystatecompetitive(BaselineParams);

disp('Steady state values:');
disp(['k = ', num2str(kast)]);
disp(['y = ', num2str(yast)]);
disp(['c = ', num2str(cast)]);
disp(['r = ', num2str(rast)]);
disp(['w = ', num2str(wast)]);

% Sensitivity analysis: savings ratio (s)
 params = BaselineParams;
 sVec = 0:0.01:1;
 iMax = length(sVec);
 kVec_s = zeros(iMax,1);
 yVec_s = zeros(iMax,1);
 cVec_s = zeros(iMax,1);
 rVec_s = zeros(iMax,1);
 wVec_s = zeros(iMax,1);
 for ii=1:iMax
   params.s = sVec(ii);
   [kVec_s(ii), yVec_s(ii), cVec_s(ii), rVec_s(ii), wVec_s(ii), ] = solowsteadystatecompetitive(params);
 end

 % Sensitivity analysis: total factor productivity (Z)
 params = BaselineParams;
 ZVec = 1:0.1:2;
 iMax = length(ZVec);
 kVec_Z = zeros(iMax,1);
 yVec_Z = zeros(iMax,1);
 cVec_Z = zeros(iMax,1);
 rVec_Z = zeros(iMax,1);
 wVec_Z = zeros(iMax,1);
 for ii=1:iMax
   params.Z = ZVec(ii);
   [kVec_Z(ii), yVec_Z(ii), cVec_Z(ii), rVec_Z(ii), wVec_Z(ii)] = solowsteadystatecompetitive(params);
 end

 % Sensitivity analysis: growth rate of labor efficiency (a)
 params = BaselineParams;
 aVec = 0.000:0.005:0.05;
 iMax = length(aVec);
 kVec_a = zeros(iMax,1);
 yVec_a = zeros(iMax,1);
 cVec_a = zeros(iMax,1);
 rVec_a = zeros(iMax,1);
 wVec_a = zeros(iMax,1);
  for ii=1:iMax
   params.a = aVec(ii);
   [kVec_a(ii), yVec_a(ii), cVec_a(ii), rVec_a(ii), wVec_a(ii)] = solowsteadystatecompetitive(params);
 end

 % Sensitivity analysis: depreciation rate (delta)
 params = BaselineParams;
 deltaVec = 0.000:0.005:0.10;
 iMax = length(deltaVec);
 kVec_delta = zeros(iMax,1);
 yVec_delta = zeros(iMax,1);
 cVec_delta = zeros(iMax,1);
 rVec_delta = zeros(iMax,1);
 wVec_delta = zeros(iMax,1);
 for ii=1:iMax
   params.delta = deltaVec(ii);
   [kVec_delta(ii), yVec_delta(ii), cVec_delta(ii), rVec_delta(ii), wVec_delta(ii)] = solowsteadystatecompetitive(params);
 end

 % Sensitivity analysis: growth rate of population (n)
 params = BaselineParams;
 nVec = 0.000:0.005:0.05;
 iMax = length(nVec);
 kVec_n = zeros(iMax,1);
 yVec_n = zeros(iMax,1);
 cVec_n = zeros(iMax,1);
 rVec_n = zeros(iMax,1);
 wVec_n = zeros(iMax,1);
  for ii=1:iMax
   params.n = nVec(ii);
   [kVec_n(ii), yVec_n(ii), cVec_n(ii), rVec_n(ii), wVec_n(ii)] = solowsteadystatecompetitive(params);
 end

 % Sensitivity analysis: capital elasticity of output (alpha)
 params = BaselineParams;
 alphaVec = 0.1:0.05:0.6;
 iMax = length(alphaVec);
 kVec_alpha = zeros(iMax,1);
 yVec_alpha = zeros(iMax,1);
 cVec_alpha = zeros(iMax,1);
 rVec_alpha = zeros(iMax,1);
 wVec_alpha = zeros(iMax,1);
  for ii=1:iMax
   params.alpha = alphaVec(ii);
   [kVec_alpha(ii), yVec_alpha(ii), cVec_alpha(ii), rVec_alpha(ii), wVec_alpha(ii)] = solowsteadystatecompetitive(params);
 end

% Display results
fig_counter = 0;

% Real Interest Rate
fig_counter = fig_counter + 1;
hf = figure(fig_counter);

subplot(2,3,1);
plot(sVec, rVec_s, 'color', BfBlue, 'LineWidth', 3);
title('Real interest rate');
xlabel('savings ratio (s)');
ylabel('real interest rate (r)');

subplot(2,3,2);
plot(ZVec, rVec_Z, 'color', BfBlue, 'LineWidth', 3);
title('Real interest rate');
xlabel('total factor productivity (Z)');
ylabel('real interest rate (r)');

subplot(2,3,3);
plot(aVec, rVec_a, 'color', BfBlue, 'LineWidth', 3);
title('Real interest rate');
xlabel('growth rate of labor efficiency (a)');
ylabel('real interest rate (r)');

subplot(2,3,4);
plot(deltaVec, rVec_delta, 'color', BfBlue, 'LineWidth', 3);
title('Real interest rate');
xlabel('depreciation rate (delta)');
ylabel('real interest rate (r)');

subplot(2,3,5);
plot(nVec, rVec_n, 'color', BfBlue, 'LineWidth', 3);
title('Real interest rate');
xlabel('population growth rate (n)');
ylabel('real interest rate (r)');

subplot(2,3,6);
plot(alphaVec, rVec_alpha, 'color', BfBlue, 'LineWidth', 3);
title('Real interest rate');
xlabel('capital elasticity of output (alpha)');
ylabel('real interest rate (r)');

if DoSavePlots
  saveas(hf, '../../figures/fig_SolowSteadyInterest.png', 'png');
end

% Real Wage
fig_counter = fig_counter + 1;
hf = figure(fig_counter);

subplot(2,3,1);
plot(sVec, wVec_s, 'color', BfBlue, 'LineWidth', 3);
title('Real wage per efficiency unit of labor');
xlabel('savings ratio (s)');
ylabel('real wage (w)');

subplot(2,3,2);
plot(ZVec, wVec_Z, 'color', BfBlue, 'LineWidth', 3);
title('Real wage per efficiency unit of labor');
xlabel('total factor productivity (Z)');
ylabel('real wage (w)');

subplot(2,3,3);
plot(aVec, wVec_a, 'color', BfBlue, 'LineWidth', 3);
title('Real wage per efficiency unit of labor');
xlabel('growth rate of labor efficiency (a)');
ylabel('real wage (w)');

subplot(2,3,4);
plot(deltaVec, wVec_delta, 'color', BfBlue, 'LineWidth', 3);
title('Real wage per efficiency unit of labor');
xlabel('depreciation rate (delta)');
ylabel('real wage (w)');

subplot(2,3,5);
plot(nVec, wVec_n, 'color', BfBlue, 'LineWidth', 3);
title('Real wage per efficiency unit of labor');
xlabel('population growth rate (n)');
ylabel('real wage (w)');

subplot(2,3,6);
plot(alphaVec, wVec_alpha, 'color', BfBlue, 'LineWidth', 3);
title('Real wage per efficiency unit of labor');
xlabel('capital elasticity of output (alpha)');
ylabel('real wage (w)');

if DoSavePlots
  saveas(hf, '../../figures/fig_SolowSteadyWage.png', 'png');
end

disp(' ');
disp(' Done. Bye.');
disp(' --------------------------------');
