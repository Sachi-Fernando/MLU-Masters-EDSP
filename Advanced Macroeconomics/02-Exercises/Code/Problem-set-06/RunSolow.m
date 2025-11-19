%------------------------------------------------------%
%------------- Simulate Solow Growth Model ------------%
%------------------------------------------------------%

disp('************************************************');
disp('Run Solow Growth Model');

clear all;
close all;

% Path to dynare toolbox
% ----------------------
addpath c:/dynare/6.2/matlab

% Excel file to save results
% --------------------------
excel_file = fullfile('results.xlsx');
sheet_names = {'z', 's', 'n'}; % scenarios with permanent shocks on TFP, savings rate and population growth

% Loop through all scenarios
% --------------------------
for Scenario = 1:3

    % clean up bytecode folder if needed 
    bytecode_path = fullfile(pwd, 'solow', 'model', 'bytecode');
    if exist(bytecode_path, 'dir')
        rmdir(bytecode_path, 's');
    end

    % run dynare code
    dynare solow noclearall

    % sheet name for this scenario
    sheet_name = sheet_names{Scenario};

    % endogenous variables
    endo_names = cellstr(M_.endo_names)';
    endo_results = oo_.endo_simul'; 

    % exogenous variables
    exo_names = cellstr(M_.exo_names)';
    exo_results = oo_.exo_simul; 

    % combine names and data
    all_names = [endo_names, exo_names];
    all_results = [endo_results, exo_results]; 

    % write header (variable names)
    writecell(all_names, excel_file, 'Sheet', sheet_name, 'Range', 'A1');

    % write data (time series)
    writematrix(all_results, excel_file, 'Sheet', sheet_name, 'Range', 'A2');
end


disp('************************************************');