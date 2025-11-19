% Show results in bar plots

values1 = [fsopt(1) exo.k1 fsopt(6) fsopt(8); fsopt(2) fsopt(3) fsopt(7) fsopt(9)]; % load values into matrix
% matrix: c1 k1 y1 r1 => first row = period 1 values
%         c2 k2 y2 r2 => second row = period 2 values
% Compare Period 1 to Period 2

fig_counter = fig_counter + 1;
hf = figure(fig_counter);

subplot(2,2,1);
bar([values1(1,1), values1(2,1)]); % plot element row=1 and col=1 and element row=2 and col=1 which is c1 and c2
title('Consumption');
xlabel('period');
ylabel('consumption units');

subplot(2,2,2);
bar([values1(1,2), values1(2,2)]);
title('Capital');
xlabel('period');
ylabel('capital units');

subplot(2,2,3);
bar([values1(1,3), values1(2,3)]);
title('Output');
xlabel('period');
ylabel('output units');

subplot(2,2,4);
bar([100*values1(1,4), 100*values1(2,4)]);
title('Real Interest Rate');
xlabel('period');
ylabel('percent');

if SavePlot
  set(hf, 'paperunits', 'centimeters'); % settings from lecture
  set(hf, 'papersize', [29/16*9, 29/16*9]);
  set(hf, 'paperposition',[0, 0, 29/16*9 ,29/16*9]);
  print('Two_Per_Retirement_1.pdf', '-dpdf');
end

values2 = [fsopt(5) fsopt(10) fsopt(4)]; % load values into matrix
% matrix: n w T => one row for remaining variable values

fig_counter = fig_counter + 1;
hf = figure(fig_counter);

subplot(1,3,1);
bar(values2(1)); % plot element row=1 and col=1 and element row=2 and col=1 which is c1 and c2
title('Labour');
xlabel('period');
ylabel('hours');

subplot(1,3,2);
bar(values2(2));
title('Real Wage');
xlabel('period');
ylabel('wage units');

subplot(1,3,3);
bar(values2(3));
title('Transfers');
xlabel('period');
ylabel('transfer units');

if SavePlot
  set(hf, 'paperunits', 'centimeters'); % settings from lecture
  set(hf, 'papersize', [29/16*9, 29/16*9]);
  set(hf, 'paperposition',[0, 0, 29/16*9 ,29/16*9]);
  print('Two_Per_Retirement_2.pdf', '-dpdf');
end