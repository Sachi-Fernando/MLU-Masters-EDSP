% Solve nonlinear System
[ fsopt, fval, efl ] = fsolve(@(x)equilibrium(x, exo, pars), initval, options);

% Display Equlibrium values (saved in vector fsopt)
disp('Endogenous variables:');
disp(['c1: ', num2str(fsopt(1))]);
disp(['c2: ', num2str(fsopt(2))]);
disp(['k2: ', num2str(fsopt(3))]);
disp(['T : ', num2str(fsopt(4))]);
disp(['n : ', num2str(fsopt(5))]);
disp(['y1: ', num2str(fsopt(6))]);
disp(['y2: ', num2str(fsopt(7))]);
disp(['r1: ', num2str(fsopt(8))]);
disp(['r2: ', num2str(fsopt(9))]);
disp(['w : ', num2str(fsopt(10))]);
disp(' ');
disp(['Checksum: ', num2str(sum(fval))]);
disp(['Exit flag: ', num2str(efl)]);
disp('******************************************');