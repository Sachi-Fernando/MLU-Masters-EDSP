function [k, y, c ] = solowsteadystate(params)
  s     = params.s;
  Z     = params.Z;
  delta = params.delta;
  n     = params.n;
  a     = params.a;
  alpha = params.alpha;

  k = ((s*Z)/(delta+n+a))^(1/(1-alpha));
  y = Z*k^alpha;
  c = (1-s)*y;
end
