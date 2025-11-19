% Solving Linear Systems of Equations
% This version: 25.10.2025
% Tested: Octave 10.2, Matlab 2023b

% Define matrices
A = [ 2, 3; 3, -4 ];
b = [ 18; -7 ];

% Solve: Using inverse of A
AInv = inv(A);
x0 = inv(A)*b;

% Solve: Short cut
x1 = A\b;

% Display results
disp('Solution using the inverse of A:');
disp(x0);

disp('');
disp('Solution using the short cut:');
disp(x1);

% End of script
