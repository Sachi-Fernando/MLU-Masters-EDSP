% ====================================================================
% Solow Model: Golden Rule
% This version: 16.11.2025
% Oliver Holtemoeller
% Tested: Octave 10.2.0, MATLAB 2024a
% =====================================================================

clear all;
close all;

% Colorblind barrier-free color pallet
BfBlack        = [   0,   0,   0 ]/255;
BfOrange       = [ 230, 159,   0 ]/255;
BfSkyBlue      = [  86, 180, 233 ]/255;
BfBluishGreen  = [   0, 158, 115 ]/255;
BfYellow       = [ 240, 228,  66 ]/255;
BfBlue         = [   0, 114, 178 ]/255;
BfVermillon    = [ 213,  94,   0 ]/255;
BfRedishPurple = [ 204, 121, 167 ]/255;

disp(' -------------------------------');
disp(' Solow Growth Model: Golden Rule');
disp(' ');

% Set parameters and exogenous variables
BaselineParams.s     = 0.2;               % Savings ratio
BaselineParams.alpha = 0.3;               % capital elasticity of ouput
BaselineParams.delta = 0.03;              % depreciation rate
BaselineParams.n     = 0.01;              % Population growth rate
BaselineParams.a     = 0.01;              % Labor efficiency growth rate
BaselineParams.Z     = 1;                 % Total factor productivity

% Steady state and savings rate (s)
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
  [kVec_s(ii), yVec_s(ii), cVec_s(ii), rVec_s(ii), wVec_s(ii) ] = solowsteadystatecompetitive(params);
end
iiOpt = find(cVec_s==max(cVec_s));
sOpt  = sVec(iiOpt);
kOpt  = kVec_s(iiOpt);
cOpt  = cVec_s(iiOpt);
kOpt2 =  ((params.n+params.a+params.delta)/params.Z/params.alpha)^(1/(params.alpha-1));

disp(['Optimal savings rate: ',num2str(sOpt)]);
disp(['Optimal capital stock per efficiency unit of labor: ',num2str(kOpt)]);
disp(['Check (analytic solution for k): ', num2str(kOpt2)])

disp(' ');
disp(' Done. Bye.');
disp(' --------------------------------');
