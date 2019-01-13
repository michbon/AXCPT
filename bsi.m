function [bsi_err, bsi_rt, bsi_combined1, bsi_combined2] = ...
            bsi(data, nsubjects, ntrials, raw)
%      data       = readtable('fakedata.csv');
%      nsubjects  = 5;
%      ntrials    = 10;
%      raw        = true;
%Output = BSI for errors, reaction time, and 2 different methods of
%         combining the BSI (see comments)
%Input = data, number of subjects, number of trials, and whether or not
%        the data are in the raw format or not (logical true or false). 
%        If accuracy is either a 1 or 0, that's the raw form. If it's a 
%        decimal, that's % correct
% By Shelly Cooper, Braver's lab, personal communication august 2016.

if raw == true
    % empty matrix to store everything
    data_summary = zeros(nsubjects, 4);
    
    % get the # of correct trials per subject, store as first two columns
    % in the data_summary (col1 = AYcorrect, col2 = BXcorrect)
    data_summary(:,1:2) = varfun(@sum, data(:,1:3), 'GroupingVariables',...
        'SubjectID','OutputFormat','uniform');
    
    % get the mean reaction time per subject, store as last two columns
    % in data_summary (col3 = AYrt, col4 = BXrt)
    data_summary(:,3:4) = varfun(@mean, data(:,[1 4 5]), 'GroupingVariables',...
        'SubjectID','OutputFormat','uniform');
    
    % now get the error rate while applying the log-linear correction
    correctedAYerr = 1 - ((data_summary(:,1) + 0.5) / (ntrials + 1));
    correctedBXerr = 1 - ((data_summary(:,2) + 0.5) / (ntrials + 1));
    
else
    % empty matrix to store everything
    data_summary = zeros(nsubjects, 4);
    
    % since it's percent error, multiply by number of trials to get raw
    % number of correct vs. incorrect
    
    data_summary(:,1:2) = ntrials * data(:,1:2);
    % you don't need to do anything for the reaction time info
    data_summary(:,3:4) = data(:,3:4);
    
    % now get the error rate while applying the log-linear correction
    correctedAYerr = 1 - ((data_summary(:,1) + 0.5) / (ntrials + 1));
    correctedBXerr = 1 - ((data_summary(:,2) + 0.5) / (ntrials + 1));
end

% calculate the BSIs
bsi_err = (correctedAYerr - correctedBXerr) ./ ...
    (correctedAYerr + correctedBXerr);

bsi_rt = (data_summary(:,3) - data_summary(:,4)) ./ ...
    (data_summary(:,3) + data_summary(:,4));
    
% Different options for combining
% 1) Straight sum of the two BSIs
bsi_combined1 = bsi_err + bsi_rt;

% 2) z-score each BSI, then sum
bsi_err_z = zscore(bsi_err);
bsi_rt_z = zscore(bsi_rt);
bsi_combined2 = bsi_err_z + bsi_rt_z;

%% Plot them to compare which index you like most
% figure
% bsi_err_plot = plot(bsi_err, 'r-');
% hold on
% bsi_rt_plot = plot(bsi_rt, 'b-');
% hold on
% bsi_c1_plot = plot(bsi_combined1, 'g-');
% hold on
% bsi_c2_plot = plot(bsi_combined2, 'c-');
% legend([bsi_err_plot, bsi_rt_plot, bsi_c1_plot, bsi_c2_plot],'BSI-Errors', 'BSI-RT',...
%     'Sum of BSIs','Sum of Z-scored BSIs')

end

