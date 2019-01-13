 function [cond] = analyze_noTrimming (file, condition, converted, subjnum)

% % close all; clear all; clc;
% file = 'con_Ita-Eng_AX-1.csv';
% condition = 'AX';
% converted = 0;
% subjnum =  strrep(strrep(file, 'con_Ita-Eng_AX-', ''), '-1.csv', '');
 
% This function reads an xlsx log file of the AxCPT and summarizes the
% responses to analyse them. It takes the file name and the condition
% as input. It returns a structure containing the responses (accuracy
% and Rts, as well as Rts to be included only) and some summaries to plot
% data).
%   E.g. how to call the f: 
%   files = dir('AX-CPT-*.xlsx');
%   for f = 1: length(files)
%   AX(f) = analyze_AxCPT(files(f).name, 'AX');
%   ...
% With these data, different kinds of analyses can be carried out.
% As only probe responses are considered, you first need to make sure
% that the accuracy on the cue and on the distractors are fine. To do this
% automatically, read out the relative columns in the logs and apply
% this function on those data too.
 
if converted == 1
    
    probeacc    = xlsread(file, '', 'DR17:DR116');
    probert     = xlsread(file, '', 'DX17:DX116');
    [n, t, r]   = xlsread(file, '', 'EC17:EC116');
    type_trial  = t;
    
else
    
    data        = readtable(file, 'ReadVariableNames', false);
    probeacc    = table2array(data(:,3));
    probert     = table2array(data(:,4));
    type_trial  = table2cell(data(:,2));
    
end



% defines an index per condition (given as input)
for t = 1:length(type_trial)
    if strcmp(type_trial(t), condition) == 1
       type(t) = 1;
    else type(t) = 0;
    end
    type = logical(type);
end
    cond.acc = probeacc(type); 
    cond.rt = probert(type);

% extracts ### correct trials only
indincl = (cond.rt>0 & cond.acc==1); 
cond.rtIncl = cond.rt(indincl);

% sums up
cond.statsacc(1) = mean(cond.acc);
cond.statsacc(2) = std(cond.acc);

cond.statsrt(1) = mean(cond.rt); % summary Rts for plotting
cond.statsrt(2) = median(cond.rt);
cond.statsrt(3) = std(cond.rt);


cond.statsrtI(1) = mean(cond.rtIncl); % summary only Included Rts for BSI
cond.statsrtI(2) = median(cond.rtIncl);
cond.statsrtI(3) = std(cond.rtIncl);


% subject number
cond.subj = subjnum;

end


