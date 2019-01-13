  function [t] = averagedataset (groupname, cond, condname)
% 
% % % % produce tables of averaged variables with subj name, condition, and group. 
% clear all; close all; clc
% load condISM
% 
% % % % output param
% %t = a table of averaged variable per condition
% 
% % % % input param
% groupname   = 'ISM';      % a string, the name of the group
% cond        =  AX;         % a variable, the structure of interest
% condname    = 'AX';       % a string, the name of the condition


groups = repmat({groupname}, size(cond, 2), 1);
sbj = vertcat(cond.subj);
sbjs = cellfun(@strcat, groups, sbj, 'UniformOutput', false);
conds = repmat({condname}, size(cond, 2), 1);
acc = vertcat(cond.statsacc);
accs = acc(:,1);
rt = vertcat(cond.statsrtI);
rts = rt(:,1);

t = table(groups,sbjs,conds, accs, rts);
