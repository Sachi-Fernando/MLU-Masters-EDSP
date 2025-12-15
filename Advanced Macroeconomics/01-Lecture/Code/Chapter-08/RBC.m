% ==========================================================
% Calibrate RBC Model
% This version: 03.12.2025
% Oliver Holtemoeller
% Tested: MATLAB 2024a/Dynare 5.2
% ==========================================================

close all;
clear all;

% Specify what is to be done
% --------------------------
LoadData     = 1;
DefineVars   = 1;
PlotOverview = 1;
DoSavePlots  = 0;
DoDynare     = 1;

% Check for Matlab/Octave
% -----------------------
MyEnv.Octave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
MyEnv.Matlab = ~MyEnv.Octave;

% Load packages (Octave only)
% ---------------------------
if ~MyEnv.Matlab,
  pkg load io;
  pkg load dataframe;
end;

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

% Load data from German National Accounts
% ---------------------------------------
if LoadData,
  if MyEnv.Matlab,
    VGRDE=readtable('../../data/vgrqde.csv');
  end
  if MyEnv.Octave,
    VGRDE=dataframe('../../data/vgrqde.csv');
  end
  FirstYear = 1991;
  Nobs = length(VGRDE.BIP_SA_DE);
  Time = transpose(FirstYear:0.25:(1991+Nobs/4-0.25));
end

if DefineVars,
    % Employment
    obs.N = VGRDE.ETI_SA_DE;
    % Hours
    obs.H = VGRDE.AVETI_SA_DE;
    [obs.ntrend, obs.n] = hpfilter(log(1000*obs.H./obs.N/(90*24)), smoothing=1600);
    % GDP
    obs.YN = VGRDE.BIP_SA_DE;
    Baseyear = find(Time>=2020 & Time<2021);
    BIPMean = mean(obs.YN(Baseyear));
    obs.Y = VGRDE.BIPVOL_SA_DE*BIPMean/100;
    [obs.ytrend, obs.y] = hpfilter(log(obs.Y), smoothing=1600);
    % obs.ytrend = log(obs.Y)-log(obs.y);
    [ytrend_01, ycyc_01] = hpfilter(log(obs.Y), smoothing=1600);
    [ytrend_02, ycyc_02] = hpfilter(log(obs.Y), smoothing=100);
    [ytrend_03, ycyc_03] = hpfilter(log(obs.Y), smoothing=160000);
    % ytrend_01 = log(obs.Y)-ycyc_01;
    % ytrend_02 = log(obs.Y)-ycyc_02;
    % ytrend_03 = log(obs.Y)-ycyc_03;
    % Consumption
    obs.CN = VGRDE.CPR_SA_DE;
    CPRMean = mean(obs.CN(Baseyear));
    obs.C = VGRDE.CPRVOL_SA_DE*CPRMean/100;
    [obs.ctrend, obs.c] = hpfilter(log(obs.C), smoothing=1600);
    % Investment
    obs.IN = VGRDE.BAI_SA_DE;
    BAIMean = mean(obs.IN(Baseyear));
    obs.I = VGRDE.BAIVOL_SA_DE*BAIMean/100;
    [obs.itrend, obs.i] = hpfilter(log(obs.I), smoothing=1600);
    % Real wage
    obs.W = VGRDE.VERDH_SA_DE./VGRDE.BIPP_SA_DE*100;
    [obs.wtrend, obs.w] = hpfilter(log(obs.W), smoothing=1600);
    % Labor productivity
    [obs.xtrend, obs.x] = hpfilter(log(VGRDE.PRODH_SA_DE), smoothing=1600);
    
    MyData = [obs.y obs.n obs.c obs.i obs.w obs.x];
    
    Rho = corr(MyData);
    rhoobs = Rho(:,1);
    sdobs = transpose(std(MyData));
end

if PlotOverview,
    fig_counter = fig_counter + 1;
    hf = figure(fig_counter);
    figure(hf);
    hold on;
    plot(Time, log(obs.Y), 'color', BfBlue, 'LineWidth', StdLineWidth);
    plot(Time, ytrend_01, 'color', BfOrange, 'LineWidth', StdLineWidth);
    plot(Time, ytrend_02, 'color', BfSkyBlue, 'LineWidth', StdLineWidth);
    plot(Time, ytrend_03, 'color', BfVermillon, 'LineWidth', StdLineWidth);
    legend({'Observed','\lambda=1600','\lambda=100','\lambda=160000'});
    legend('Location','northwest')
    xlim([Time(1), Time(end)]);
    hold off;
  
    fig_counter = fig_counter + 1;
    hf = figure(fig_counter);
    subplot(2,3,1);
    hold on;
    plot(Time, obs.y, 'color', BfBlue, 'LineWidth', StdLineWidth);
    plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/4);
    hold off;
    ylabel('% dev.');
    title('Output');
    xlim([Time(1), Time(end)]);
    subplot(2,3,2);
    hold on;
    plot(Time, obs.c, 'color', BfBlue, 'LineWidth', StdLineWidth);
    plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/4);
    hold off;
    ylabel('% dev.');
    xlim([Time(1), Time(end)]);
    title('Consumption');
    subplot(2,3,3);
    hold on;
    plot(Time, obs.i, 'color', BfBlue, 'LineWidth', StdLineWidth);
    plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/4);
    hold off;
    ylabel('% dev.');
    xlim([Time(1), Time(end)]);
    title('Investment');
    subplot(2,3,4);
    hold on;
    plot(Time, obs.n, 'color', BfBlue, 'LineWidth', StdLineWidth);
    plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/4);
    hold off;
    ylabel('% dev.');
    xlim([Time(1), Time(end)]);
    title('Hours Worked');
    subplot(2,3,5);
    hold on;
    plot(Time, obs.w, 'color', BfBlue, 'LineWidth', StdLineWidth);
    plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/4);
    hold off;
    ylabel('% dev.');
    xlim([Time(1), Time(end)]);
    title('Real Wage');
    subplot(2,3,6);
    hold on;
    plot(Time, obs.x, 'color', BfBlue, 'LineWidth', StdLineWidth);
    plot(Time, zeros(length(Time),1), 'color', 'black', 'LineWidth', StdLineWidth/4);
    hold off;
    ylabel('% dev.');
    xlim([Time(1), Time(end)]);
    title('Labor Productivity');
    
    if DoSavePlots == 1,
      saveas(hf, '../../figures/fig_VGRQ_DEU.png', 'png');
    end
end

if DoDynare,
  dynare ramseygrowthext_stoch noclearall;
  Sigma = oo_.var;
  D=diag(1./sqrt(diag(Sigma)));
  Rho = D*Sigma*D;
  rhosim = Rho([4, 3, 2, 9, 5, 8],4);
  D = sqrt(diag(Sigma));
  sdsim = D([4, 3, 2, 9, 5, 8]);
  
  fig_counter = 9;
  
  fig_counter = fig_counter + 1;
  hf = figure(fig_counter);
  figure(hf);
  h = bar([sdsim, sdobs]);
  set(h(1), 'facecolor', BfBlue);
  set(h(2), 'facecolor', BfOrange);
  set(gca,'XTickLabel',{'y', 'c', 'i', 'h', 'w', 'x'});
  legend('Simulated', 'Observed');
  title('Standard Deviation');
  
    if DoSavePlots == 1,
      saveas(hf, '../../figures/fig_RBC-SD.png', 'png');
    end;
  
  fig_counter = fig_counter + 1;
  hf = figure(fig_counter);
  figure(hf);
  h = bar([rhosim, rhoobs]);
  set(h(1), 'facecolor', BfBlue);
  set(h(2), 'facecolor', BfOrange);
  set(gca,'XTickLabel',{'y', 'c', 'i', 'h', 'w', 'x'});
  legend({'Simulated', 'Observed'},'Location','southwest');
  title('Correlation with Output');
  
    if DoSavePlots == 1,
      saveas(hf, '../../figures/fig_RBC-Cor.png', 'png');
    end
end