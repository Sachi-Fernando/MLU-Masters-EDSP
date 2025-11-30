// Ramsey Model
// Oliver Holtemoeller (v1) 26.11.2025
// Tested: Dynare 5.2

// 1. Block: Definition of Variables and Parameters
// Endogenous variables:
var kV cV;
// Exogenous variables:
varexo ZV;

// Parameters
parameters alphaP, deltaP, thetaP, rhoP, betaP;
thetaP = 1;
alphaP = 0.3;
deltaP = 0.02;
rhoP = 0.01;
betaP = 1/(1+rhoP);

// 2. Model Block
model;
// Consumption Euler Equation
betaP*(cV(+1)/cV)^(-thetaP)*(alphaP*ZV(+1)*kV^(alphaP-1)+1-deltaP) = 1;
// Resource Constraint
ZV*kV(-1)^alphaP - cV - kV + (1-deltaP)*kV(-1) = 0;
end;

// Initialization Block
initval;
ZV = 1;
kV = 10;
cV = 2;
end;

// Compute Static Equilibrium (Steady State):
steady;
resid;
