% ===============================================
% Dynamic Simulation of Solow Model
% This version: 18.11.2025
% Oliver Holtemoeller
% Tested: Octave 9.2.0, 10.2.0, MATLAB 2024a
% ===============================================

clear all;
close all;

DoPlotConvergence = 1;
DoPlotSavingsRate = 1;
DoSavePlots = 0;

disp(' --------------------------------------');
disp(' Solow Growth Model: Dynamic Simulation');
disp(' ');

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

% Parameters
params.alpha = 0.3;
params.s     = 0.2;
params.delta = 0.03;
params.n     = 0.005;
params.a     = 0.01;
params.Z     = 1;

% Initial values
initvals.N = 1;
initvals.K = 5;
initvals.A = 1;
initvals.Y = params.Z*initvals.K^(params.alpha)*(initvals.A*initvals.N)^(1-params.alpha);
initvals.Q = params.s*initvals.Y;
initvals.C = initvals.Y - initvals.Q;

% Compute convergence path given initival values and parameters
params.T   = 400;
[ sim.N, sim.A, sim.K, sim.Y, sim.C, sim.Q ] = solowmodelloop(params, initvals);

% Steady state capital-output ratio
ky = (1+params.a)*(1+params.n)*params.s/((1+params.a)*(1+params.n)-1+params.delta);
sim.ky = sim.K./sim.Y;
kysim = sim.ky(end);
disp([' Analytical steady-state K-Y-ratio: ', num2str(ky)]);
disp([' Simulated steady-state K-Y-ratio: ', num2str(kysim)]);

% Simulate balanced growth paths starting from steady state
initvals.N = 1;
initvals.Z = 1;
initvals.Y = 1;
initvals.K = ky*initvals.Y;
initvals.A = (((initvals.K/(1+params.a)/(1+params.n))^(-params.alpha)*initvals.Y)/initvals.Z)^(1/(1-params.alpha))/initvals.N;
initvals.Q = params.s*initvals.Y;
initvals.C = initvals.Y - initvals.Q;
initvals.r = params.alpha*initvals.Y/initvals.K-params.delta;
initvals.w = (1-params.alpha)*initvals.Y/initvals.N;

params.T   = 100;
[ sim_0.N, sim_0.A, sim_0.K, sim_0.Y, sim_0.C, sim_0.Q, sim_0.r, sim_0.w ] = solowmodelcomploop(params, initvals);

% Simulate convergence from old steady state to new steady state: Increase in savings rate s
params_s = params;
params_s.s = params.s*1.1;
[ sim_1.N, sim_1.A, sim_1.K, sim_1.Y, sim_1.C, sim_1.Q, sim_1.r, sim_1.w, ] = solowmodelcomploop(params_s, initvals);

% Simulate convergence from old steady state to new steady state: Increase
% in TFP Z
params_Z = params;
params_Z.Z = params.Z*1.1;
[ sim_2.N, sim_2.A, sim_2.K, sim_2.Y, sim_2.C, sim_2.Q, sim_2.r, sim_2.w ] = solowmodelcomploop(params_Z, initvals);

% Simulate convergence from old steady state to new steady state: Increase
% in population growth rate n
params_n = params;
params_n.n = params.n*1.1;
[ sim_3.N, sim_3.A, sim_3.K, sim_3.Y, sim_3.C, sim_3.Q, sim_3.r, sim_3.w ] = solowmodelcomploop(params_n, initvals);

if DoPlotConvergence,
  Time = 1:length(sim.K);
  fig_counter = fig_counter + 1;
  hf = figure(fig_counter);
  subplot(2,3,1);
  plot(Time, sim.K, 'color', BfBlue, 'LineWidth', StdLineWidth);
  title('Capital');

  subplot(2,3,2);
  plot(Time, sim.K./sim.A./sim.N, 'color', BfBlue, 'LineWidth', StdLineWidth);
  title('Capital per efficiency unit of labor');

  subplot(2,3,3);
  plot(Time, sim.Y, 'color', BfBlue, 'LineWidth', StdLineWidth);
  title('Output');

  subplot(2,3,4);
  plot(Time, sim.Y./sim.A./sim.N, 'color', BfBlue, 'LineWidth', StdLineWidth);
  title('Output per efficiency unit of labor');

  subplot(2,3,5);
  plot(Time, sim.C, 'color', BfBlue, 'LineWidth', StdLineWidth);
  title('Consumption');

  subplot(2,3,6);
  plot(Time, sim.C./sim.N, 'color', BfBlue, 'LineWidth', StdLineWidth);
  title('Output per worker');
end

if DoPlotSavingsRate,
  TT = length(sim_0.K);
  Time = 1:TT;

  fig_counter = fig_counter + 1;
  hf = figure(fig_counter);
  subplot(2,3,1);
  hold on;
  plot(Time, sim_0.K, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, sim_1.K, 'color', BfOrange, 'LineWidth', StdLineWidth);
  hold off;
  xlim([0,TT]);
  title('Capital');
  legend('Baseline', 'Increase in s', 'Location', 'northwest');

  subplot(2,3,2);
  hold on;
  plot(Time, sim_0.K./sim_0.A./sim_0.N, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, sim_1.K./sim_1.A./sim_1.N, 'color', BfOrange, 'LineWidth', StdLineWidth);
  hold off;
  xlim([0,TT]);
  title('Capital per efficiency unit of labor');

  subplot(2,3,3);
  hold on;
  plot(Time, sim_0.Y, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, sim_0.Y, 'color', BfOrange, 'LineWidth', StdLineWidth);
  hold off;
  xlim([0,TT]);
  title('Output');

  subplot(2,3,4);
  hold on;
  plot(Time, sim_0.Y./sim_0.A./sim_0.N, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, sim_1.Y./sim_1.A./sim_1.N, 'color', BfOrange, 'LineWidth', StdLineWidth);
  hold off;
  xlim([0,TT]);
  title('Output per efficiency unit of labor');

  subplot(2,3,5);
  hold on;
  plot(Time, sim_0.C, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, sim_1.C, 'color', BfOrange, 'LineWidth', StdLineWidth);
  hold off;
  xlim([0,TT]);
  title('Consumption');

  subplot(2,3,6);
  hold on;
  plot(Time, sim_0.C./sim_0.N, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, sim_1.C./sim_1.N, 'color', BfOrange, 'LineWidth', StdLineWidth);
  hold off;
  xlim([0,TT]);
  title('Output per worker');

  fig_counter = fig_counter + 1;
  hf = figure(fig_counter);
  subplot(2,3,1);
  hold on;
  plot(Time, 100*sim_1.K./sim_0.K-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Capital');
  ylabel('Dev. in %');

  subplot(2,3,2);
  hold on;
  plot(Time, 100*sim_1.Y./sim_0.Y-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Output');
  ylabel('Dev. in %');

  subplot(2,3,3);
  hold on;
  plot(Time, 100*sim_1.C./sim_0.C-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Consumption');
  ylabel('Dev. in %');

  subplot(2,3,4);
  hold on;
  plot(Time, 100*(sim_1.r-sim_0.r), 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Real interest rate');
  ylabel('Dev. in pp.');

  subplot(2,3,5);
  hold on;
  plot(Time, 100*sim_1.w./sim_0.w-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Real wage');
  ylabel('Dev. in %');

  subplot(2,3,6);
  hold on;
  plot(Time, [0, 100*ones(1,TT-1)*(params_s.s-params.s)], 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Savings rate');
  ylabel('Dev. in pp');

  if DoSavePlots == 1,
    saveas(hf, '../../figures/fig_Solow-Dyn-Sim-s.png', 'png');
  end;

  fig_counter = fig_counter + 1;
  hf = figure(fig_counter);
  subplot(2,3,1);
  hold on;
  plot(Time, 100*sim_2.K./sim_0.K-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Capital');
  ylabel('Dev. in %');

  subplot(2,3,2);
  hold on;
  plot(Time, 100*sim_2.Y./sim_0.Y-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Output');
  ylabel('Dev. in %');

  subplot(2,3,3);
  hold on;
  plot(Time, 100*sim_2.C./sim_0.C-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Consumption');
  ylabel('Dev. in %');

  subplot(2,3,4);
  hold on;
  plot(Time, 100*(sim_2.r-sim_0.r), 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Real interest rate');
  ylabel('Dev. in pp.');

  subplot(2,3,5);
  hold on;
  plot(Time, 100*sim_2.w./sim_0.w-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Real wage');
  ylabel('Dev. in %');

  subplot(2,3,6);
  hold on;
  plot(Time, [0, ones(1,TT-1)*(100*params_Z.Z/params.Z-100)], 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Total factor productivity');
  ylabel('Dev. in %');

  if DoSavePlots == 1,
    saveas(hf, '../../figures/fig_Solow-Dyn-Sim-Z.png', 'png');
  end;

  fig_counter = fig_counter + 1;
  hf = figure(fig_counter);
  subplot(2,3,1);
  hold on;
  plot(Time, 100*sim_3.K./sim_0.K-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Capital');
  ylabel('Dev. in %');

  subplot(2,3,2);
  hold on;
  plot(Time, 100*sim_3.Y./sim_0.Y-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Output');
  ylabel('Dev. in %');

  subplot(2,3,3);
  hold on;
  plot(Time, 100*sim_3.C./sim_0.C-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Consumption');
  ylabel('Dev. in %');

  subplot(2,3,4);
  hold on;
  plot(Time, 100*(sim_3.r-sim_0.r), 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Real interest rate');
  ylabel('Dev. in pp.');

  subplot(2,3,5);
  hold on;
  plot(Time, 100*sim_3.w./sim_0.w-100, 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Real wage');
  ylabel('Dev. in %');

  subplot(2,3,6);
  hold on;
  plot(Time, [0, 100*ones(1,TT-1)*(params_n.n-params.n)], 'color', BfBlue, 'LineWidth', StdLineWidth);
  plot(Time, zeros(TT,1), 'color', BfBlack, 'LineWidth', 1);
  hold off;
  xlim([0,TT]);
  title('Population growth rate');
  ylabel('Dev. in pp.');

  if DoSavePlots == 1,
    saveas(hf, '../../figures/fig_Solow-Dyn-Sim-n.png', 'png');
  end;
end

disp(' ');
disp(' Done. Bye.');
disp(' --------------------------------------');
