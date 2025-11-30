function fun = ramseysteadystate(endog, exog, params)
  % Parameters
  alpha = params.alpha;
  delta = params.delta;
  beta  = 1/(1+params.rho);
  
  % Exogenous variables
  Z = exog.Z;
  
  % Endogenous variables
  k = endog(1);
  c = endog(2);
  
  % Function values
  fun(1) = beta*(alpha*Z*k^(alpha-1) + 1 - delta) - 1;
  fun(2) = c - (Z*k^alpha-delta*k);
end