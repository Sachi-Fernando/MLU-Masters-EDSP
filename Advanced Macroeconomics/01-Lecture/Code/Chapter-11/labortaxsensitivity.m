% Labor Tax Sensitivity
% 28.12.2025
% Dynare 5.2

clear all;
close all;

DoSavePlots = 0;

dynare labortax.mod noclearall;

ty_grid = 0:0.05:0.95;

ybar_grid = zeros(length(ty_grid),1);
nbar_grid = zeros(length(ty_grid),1);
cbar_grid = zeros(length(ty_grid),1);
cgbar_grid = zeros(length(ty_grid),1);

for ii = 1:length(ty_grid)
  set_param_value('tyP', ty_grid(ii));
  steady;
  
  % Store steady-state values
  ybar_grid(ii) = oo_.steady_state(strmatch('yV', M_.endo_names, 'exact'));
  nbar_grid(ii) = oo_.steady_state(strmatch('nV', M_.endo_names, 'exact'));
  cbar_grid(ii) = oo_.steady_state(strmatch('cV', M_.endo_names, 'exact'));
  cgbar_grid(ii) = oo_.steady_state(strmatch('cgV', M_.endo_names, 'exact'));
end

hf = figure(1);
subplot(2,2,1);
plot(ty_grid, nbar_grid, 'LineWidth', 2);
title('Hours Worked');
xlabel('Tax rate');
subplot(2,2,2);
plot(ty_grid, ybar_grid, 'LineWidth', 2);
title('Output');
xlabel('Tax rate');
subplot(2,2,3);
plot(ty_grid, cbar_grid, 'LineWidth', 2);
title('Private Consumption');
xlabel('Tax rate');
subplot(2,2,4);
plot(ty_grid, cgbar_grid, 'LineWidth', 2);
title('Tax Revenue');
xlabel('Tax rate');

if DoSavePlots
  saveas(hf, '../../figures/fig_labtaxsensitivity.png', 'png');
end