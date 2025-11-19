function fun = equilibrium(endo, exo, pars)
  
  % Parameters
  alpha = pars.alpha;
  phi   = pars.phi;
  rho   = pars.rho;
  tau   = pars.tau;
  theta = pars.theta;
  
  % Endogenous Variables
  c1 = endo(1);
  c2 = endo(2);
  k2 = endo(3);
  T  = endo(4);
  n  = endo(5);
  y1 = endo(6);
  y2 = endo(7);
  r1 = endo(8);
  r2 = endo(9);
  w  = endo(10);
  
  % Exogenous Variables
  A1 = exo.A1;
  A2 = exo.A2;
  k1 = exo.k1;
  
  % Equations
  fun = ones(10,1); % Define empty matrix for the non-linear functions (10 rows)
  
  fun(1)  = c1 - (1+r1)*k1-(1-tau)*w*n+k2;
  fun(2)  = c2 - c1*((1+rho)/(1+r2))^(-1/theta);
  fun(3)  = k2 - (1/(1+r2))*(c2-T);
  fun(4)  = T  - tau*w*n;
  fun(5)  = w  - (1/(1-tau))*(n^phi*c1^theta);
  fun(6)  = y1 - A1*k1^alpha*n^(1-alpha);
  fun(7)  = y2 - A2*k2;
  fun(8)  = r1 - alpha*y1/k1;
  fun(9)  = r2 - A2;
  fun(10) = w  - (1-alpha)*y1/n;
  
end