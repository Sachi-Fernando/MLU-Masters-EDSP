// Ramsey Model with Distortionary Taxation
// Original Version: George Alogoskoufis
// https://mitpress.mit.edu/9780262043014/dynamic-macroeconomics/
//
// Modified and tested on Dynare 5.2
//
// We study an increase in the tax rate on income from capital tk
// and an increase in the tax rate on business profits tf
// 
// 1. The interest tax rate tk is increased from 0 to 10%
// 2. The profit tax rate tf is increased from 0 to 10%

// Definition of Variables
var y k c w r s gy rnet;
varexo Z tk tf;

// Definition of Parameters
parameters alpha delta n a rho sigma;

alpha=0.333;
delta=0.03;
n=0.01;
a=0.02;
rho=0.02;
sigma=1;

model;
// Production function
y=Z*(k(-1)^alpha);
// Profit-maximizing condition for capital:
r=(1-tf)*((alpha*Z*k(-1)^(alpha-1))-delta);
// Consumption Euler equation 
c(+1)=((((1+(r*(1-tk)))/(1+(rho)))^(1/sigma))*(1/(1+a)))*c;
// Capital accumulation
k=(1/((1+n)*(1+a)))*((y-c)+(1-delta)*k(-1));
// Competitive wage
w=(1-alpha)*Z*k(-1)^(alpha);
// Savings rate
s=(y-c)/y;
// Output growth rate
gy=(y-y(-1))/y(-1);
// Net real interest rate
rnet = (1-tf)*(1-tk)*r;

end;

initval;

k=5.3;
c=1.38;
y=1.7;
Z=1;
r=0.075;
w=1.16;
tf=0;
tk=0;

end;

steady;

endval;

k=5.3;
c=1.38;
y=1.7;
Z=1;
r=0.075;
w=1.16;
tf=0.10;
tk=0.0;

end;

steady;
check;

// Perfect Foresight Simulation of Adjustment Path
perfect_foresight_setup(periods=100);
perfect_foresight_solver;
send_endogenous_variables_to_workspace;

// Plotting Results
close all;
TMax = 80;
DoSavePlots = 0;
hf = figure(1);
subplot(2,3,1); hold on; plot(k(1:TMax), 'b-'); plot(k(1)*ones(TMax),'k-'); hold off; title('Capital');
subplot(2,3,2); hold on; plot(y(1:TMax), 'b-'); plot(y(1)*ones(TMax),'k-'); hold off; title('Output');
subplot(2,3,3); hold on; plot(c(1:TMax), 'b-'); plot(c(1)*ones(TMax),'k-'); hold off; title('Consumption');
subplot(2,3,4); hold on; plot(rnet(1:TMax), 'b-'); plot(rnet(1)*ones(TMax),'k-'); hold off; title('Net Real Interest Rate');
subplot(2,3,5); hold on; plot(r(1:TMax), 'b-'); plot(r(1)*ones(TMax),'k-'); hold off; title('Real Interest Rate');
subplot(2,3,6); hold on; plot(s(1:TMax), 'b-'); plot(s(1)*ones(TMax),'k-'); hold off; title('Savings Rate');

if DoSavePlots
  saveas(hf, '../../figures/fig_ramseyinttax.png', 'png');
end