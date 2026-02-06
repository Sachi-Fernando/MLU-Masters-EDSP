// Non-Linear Static Equilibrium Model
// with Endogenous Labor and Income Tax
// Oliver Holtemoeller 28.12.2025
// Tested: Dynare 5.2

// 1. Block: Definition of Variables and Parameters
// Endogenous variables:
var wV nV yV cV cgV;
// Exogenous variables:
varexo aV;

// Parameters
parameters alphaP, sigmaP, varphiP, tyP;
alphaP = 0.3;
sigmaP = 1;
varphiP = 2;
tyP = 0.5;

// 2. Model Block
model;
// Labor supply
(1-tyP)*wV = cV^sigmaP * nV^varphiP;
// Labor demand
wV = aV * (1-alphaP) * nV^(-alphaP);
// Goods market equilibrium
cV + cgV = yV;
// Production function:
yV = aV * nV^(1-alphaP);
// Government budget constraint:
cgV = tyP*wV*nV;
end;

// Initialization Block
initval;
aV = 1;
wV = 0.5;
nV = 0.5;
yV = 0.5;
cV = 0.4;
cgV = 0.1;
end;

// Compute Static Solution (Steady State):
steady;
