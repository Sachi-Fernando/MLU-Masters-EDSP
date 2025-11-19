function res = walrasequil(x, NumParams)
  w = x(1);
  c = x(2);
  n = x(3);
  y = x(4);
  alpha = NumParams(1);
  a = NumParams(2);
  sigma = NumParams(3);
  varphi = NumParams(4);
  res(1) = w - (c^sigma)*(n^varphi);
  res(2) = w - (1-alpha)*a*n^(-alpha);
  res(3) = y - c;
  res(4) = y - a*n^(1-alpha);
end
