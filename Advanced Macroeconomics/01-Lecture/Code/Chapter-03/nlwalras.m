% =============================
% nlwalras.m
% This version: 2024-10-21
% Oliver Holtemoeller
% Tested: MATLAB R2023b, R2024a
%         Octave 10.2
% =============================

disp('************************************************');
disp('Walrasian Equilibrium');

close all;
clear all;

disp('');
disp('Numerical solution');
disp('------------------');
NumParams = [0.3, 1, 1, 1];

xinit = [ 0.5, 0.5, 0.5, 0.5 ];
[xopt, fval, efl] = fsolve(@(x)walrasequil(x, NumParams),xinit);
disp(['w = ', num2str(xopt(1))]);
disp(['c = ', num2str(xopt(2))]);
disp(['n = ', num2str(xopt(3))]);
disp(['y = ', num2str(xopt(4))]);
disp('');
disp('************************************************');
