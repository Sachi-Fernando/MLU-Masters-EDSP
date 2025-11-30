function fun = ramseyloop(endog, exog, initval, endval, params)
  % Parameters
  T     = exog.T;
  alpha = params.alpha;
  delta = params.delta;
  rho   = params.rho;
  theta = params.theta;
  beta  = 1/(1+rho);
  
  % Exogenous
  Z     = exog.Z;
  
  % Endogenous
  k     = endog(1:T);
  c     = endog((T+1):(2*T));
  
  % Initialize fun
  fun = ones(2*T,1);
  
  % Initial steady state;
  fun(1)   = k(1) - initval(1);
  fun(T+1) = c(1) - initval(2);
  
  % Function loop
  for t=2:(T-1)
    fun(t)   = beta*(c(t+1)/c(t))^(-theta)*(alpha*Z(t+1)*k(t)^(alpha-1)+1-delta) - 1;
    fun(T+t) = Z(t)*k(t-1)^alpha - c(t) - k(t) + (1-delta)*k(t-1);
  end
    
  % Final steady state
  fun(T)   = k(T) - endval(1);
  fun(2*T) = c(T) - endval(2);
end
