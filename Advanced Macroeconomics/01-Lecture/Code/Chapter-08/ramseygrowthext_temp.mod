// Extended Ramsey Growth Model
// Oliver Holtemoeller 09.12.2025
// Temporary Productivity Shock
// Deterministic Extended Path Solution
// Tested: Dynare 5.2

var kV cV hV yV wV rV cyV lcV;
varexo ZV;
parameters alphaP, rhoP, deltaP, bP, gamma_aP, gamma_nP;
alphaP = 0.3;
rhoP = 0.02;
bP = 2;
deltaP = 0.03;
gamma_aP = 0.001;
gamma_nP = 0;

model;
(cV(+1)/cV) = (1+rV(+1))/(1+rhoP)/(1+gamma_aP);
kV = (yV + (1-deltaP)*kV(-1) - cV)/(1+gamma_nP)/(1+gamma_aP);
yV = ZV*kV(-1)^alphaP*hV^(1-alphaP);
cV = wV*(1-hV)/bP;
wV = (1-alphaP)*yV/hV;
rV = alphaP*yV/kV(-1)-deltaP;
cyV = cV/yV;
lcV = log(cV);
end;

initval;
kV = 3;
cV = 0.4;
hV = 0.25;
yV = 0.6;
wV = 1.5;
rV = 0.1;
cyV = cV/yV;
ZV = 1;
end;

steady;

// check;

// Specify temporary shock
shocks;
var ZV;
periods 1:1;
values 1.1;
end;

perfect_foresight_setup(periods=100);
perfect_foresight_solver;

rplot kV;
rplot hV;
rplot cV;
rplot cV yV;
