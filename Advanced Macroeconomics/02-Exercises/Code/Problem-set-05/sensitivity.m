% Vary values of tau  
MyResults = [];
tauVec = 0:0.05:0.9;
for tau = tauVec
    pars.tau = tau;
    % Solve nonlinear system
    [ fsopt, fval, efl ] = fsolve(@(x)equilibrium(x, exo, pars), initval, options);
    MyResults = [ MyResults, fsopt ];
end

% Plot transfers and labor input depending on tax rate
fig_counter = fig_counter + 1;
hf = figure(fig_counter);

myColor1 = [0.1 0.2 0.5]; % blue
myColor2 = [0.3 0.7 0.2]; % green
myColor3 = [0.8 0.1 0.3]; % red

plot(tauVec, MyResults(4,:), 'color', myColor3, 'Linewidth', 3); 
hold on;
plot(tauVec, MyResults(5,:), 'color', myColor2, 'Linewidth', 3); 
set(gca, 'ygrid', 'on');
set(gca, 'xgrid', 'on');
hold off;

xlabel('\tau');
ylabel('level');
title('Transfers and Labor Input Depending on Income Tax Rate');  
legend({'transfers', 'hours worked'}, 'location', 'southoutside', 'orientation', 'horizontal');
legend boxoff;
xlim([tauVec(1), tauVec(end)]);

if SavePlot
  set(hf, 'paperunits', 'centimeters'); % settings from lecture
  set(hf, 'papersize', [29/16*9, 29/16*9]);
  set(hf, 'paperposition',[0, 0, 29/16*9 ,29/16*9]);
  print('Two_Per_Retirement_Sens.pdf', '-dpdf');
end
