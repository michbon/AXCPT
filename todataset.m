 function [t] = todataset (groupname, cond, condname, var)

% % % % produce concatenated dataset table of variable by condition
% clear all; close all; clc
% load condIE
% 
% % % % output param
% %t = concatenated dataset table of variable by condition
% 
% % % input param
% groupname   = 'IS';  % a string, the name of the group
% cond        = BX;     % a variable, the structure of interest
% condname    = 'BX';   % a string, the name of the condition
% var         = 'rtIncl';
% 'rtIncl';  % a string, the fieldname of the variable of interest


for s = 1:size(cond, 2)
    for f = 1:size(cond(s).(var), 1)
        groups(f,s) = {groupname};
        sbj (f,s) = {[groupname cond(s).subj{:}]};
        conds(f,s) = {condname};
        vars(f,s) = cond(s).(var)(f);   
    end  
end

groupt = reshape(groups, size(groups, 1)*size(groups, 2),1);
sbjt = reshape(sbj, size(sbj, 1)*size(sbj, 2),1);
condt = reshape(conds, size(conds, 1)*size(conds, 2),1);
vart = reshape(vars, size(vars, 1)*size(vars, 2),1);

if strcmp(var,'rtIncl')
    groupt = groupt(~cellfun(@isempty, groupt));
    sbjt = sbjt(~cellfun(@isempty, sbjt));
    condt = condt(~cellfun(@isempty, condt));
    vart = vart(vart~=0);
end

t = table(groupt,sbjt,condt, vart);

% end