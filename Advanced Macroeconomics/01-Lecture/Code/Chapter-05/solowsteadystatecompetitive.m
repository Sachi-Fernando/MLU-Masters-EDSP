function [k, y, c, r, w ] = solowsteadystatecompetitive(params)
  s     = params.s;
  Z     = params.Z;
  delta = params.delta;
  n     = params.n;
  a     = params.a;
  alpha = params.alpha;

  k = ((s*Z)/(delta+n+a))^(1/(1-alpha));
  y = Z*k^alpha;
  c = (1-s)*y;
  r = Z*alpha*k^(alpha-1) - delta;
  w = Z*k^alpha - k*Z*alpha*k^(alpha-1);
end
