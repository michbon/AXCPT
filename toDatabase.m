% add ling variables to dataset
clear all; close all; clc

% accuracy data
dataacc = table2cell(readtable('ACC_Ita-Eng.csv', 'ReadVariableNames', false));
lingvar = table2cell(readtable...
    ('ling_variables_IE_Ax.csv', 'ReadVariableNames', false));

dataaccvar = cell(length(dataacc)-1, 4 +size(lingvar, 2)-1);
for d = 1:length(dataacc)-1
    
    for l = 1:length(lingvar)-1
        if strcmp(dataacc(d+1, 2), lingvar(l+1, 1))
            dataaccvar(d, :) = [dataacc(d+1, [1 2 5 6])...
                lingvar(l+1, 2:end)];
        end
    end  
end

labels = ['group' 'subject' 'condition' 'accuracy' lingvar(1, 2:end)];
dataaccvart = table([labels; dataaccvar]);

% RTs data
datart = table2cell(readtable('RTI_Ita-Eng.csv', 'ReadVariableNames', false));
lingvar = table2cell(readtable...
    ('ling_variables_IE_Ax.csv', 'ReadVariableNames', false));

datartvar = cell(length(datart)-1, 4 + size(lingvar, 2)-1);
for d = 1:length(datart)-1
    
    for l = 1:length(lingvar)-1
        if strcmp(datart(d+1, 2), lingvar(l+1, 1))
            datartvar(d, :) = [datart(d+1, [1 2 5 6])...
                lingvar(l+1, 2:end)];
        end
    end  
end

labels = ['group' 'subject' 'condition' 'rti' lingvar(1, 2:end)];
datartvart = table([labels; datartvar]);


writetable(dataaccvart, 'AX_ACC_lingvar_IE.csv', 'WriteVariableNames', false);
writetable(datartvart, 'AX_RTI_lingvar_IE.csv', 'WriteVariableNames', false);

