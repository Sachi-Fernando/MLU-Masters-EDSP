function [ N, A, K, Y, C, Q, r, w ] = solowmodelcomploop(params, initvals)
  alpha   = params.alpha;
  s       = params.s;
  delta   = params.delta;
  a       = params.a;
  n       = params.n;
  Z       = params.Z;
  T       = params.T;

  N = [ initvals.N; zeros(T-1,1)*NaN ];
  A = [ initvals.A; zeros(T-1,1)*NaN ];
  K = [ initvals.K; zeros(T-1,1)*NaN ];
  Y = [ initvals.Y; zeros(T-1,1)*NaN ];
  C = [ initvals.C; zeros(T-1,1)*NaN ];
  Q = [ initvals.Q; zeros(T-1,1)*NaN ];
  r = [ initvals.r; zeros(T-1,1)*NaN ];
  w = [ initvals.w; zeros(T-1,1)*NaN ];

  for t=2:T,
      A(t)   = A(t-1)*(1+a);
      N(t)   = N(t-1)*(1+n);
      Y(t)   = Z*K(t-1)^alpha*(A(t)*N(t))^(1-alpha);
      Q(t)   = s*Y(t);
      K(t)   = (1-delta)*K(t-1) + Q(t);
      C(t)   = Y(t) - Q(t);
      r(t)   = alpha*Y(t)/K(t-1)-delta;
      w(t)   = (1-alpha)*Y(t)/N(t);
  end
end
