% read, summarize and analyse AxCPT data. 
close all; clear all; clc;

group = 'IE';
plotindividual = 0;

%%
converted = 0; % or 1 if you converted edat2 with eprime
if converted == 1 
    files = dir('AX-CPT-*.xlsx');
    files = sort_nat({files.name});
else
    files = dir('con_Ita-Eng_AX-*.csv');
    files = sort_nat({files.name});
end

%% Read files and extract info with bespoke function

for f = 1: length(files)
    
    if converted == 1 
        subjnum{f} = strrep(strrep(char(files(f)), 'AX-CPT-ita-', ''), '-1.xlsx', '');
    else
        subjnum{f} = strrep(strrep(char(files(f)), 'con_Ita-Eng_AX-', ''), '.csv', '');
    end
    
   AX(f) = analyze_noTrimming(char(files(f)), 'AX', converted, subjnum(f));
   AY(f) = analyze_noTrimming(char(files(f)), 'AY', converted, subjnum(f));
   BX(f) = analyze_noTrimming(char(files(f)), 'BX', converted, subjnum(f));
   BY(f) = analyze_noTrimming(char(files(f)), 'BY', converted, subjnum(f));
end

%save ('dataIEcon.mat', 'AX', 'AY', 'BX', 'BY');
% load dataIE;
allcond = {'AX', 'AY', 'BX', 'BY'};
n = length(subjnum);

axaccIE = vertcat(AX.statsacc);
ayaccIE = vertcat(AY.statsacc);
bxaccIE = vertcat(BX.statsacc);
byaccIE = vertcat(BY.statsacc);

byrtIE = vertcat(BY.statsrtI);
axrtIE = vertcat(AX.statsrtI);
ayrtIE= vertcat(AY.statsrtI);
bxrtIE = vertcat(BX.statsrtI);

%% plot data overview
figa = figure;
set(figa, 'Position', [250 250 640 480])
subplot(1,2,1); % accuracy
    boxplot([axaccIE(:, 1) ayaccIE(:, 1)  bxaccIE(:, 1)  byaccIE(:, 1)]*100);
    ylim([-5 105])
    ylabel('Accuracy (proportion)');
    set(gca,'XTickLabel',allcond);
    title ('Accuracy on the Probe')
hold on
subplot(1,2,2); % RTs
    meanrt = nanmean([axrtIE(:, 1) ayrtIE(:, 1) bxrtIE(:, 1) byrtIE(:, 1)]);
    err = nanmean([axrtIE(:, 3) ayrtIE(:, 3) bxrtIE(:, 3) byrtIE(:, 3)]);
    bar(meanrt)
    ylim([0 600])
hold on
    h=errorbar(meanrt, err);
    set(h(1),'LineStyle','none')
  %  xlabel('Bars: Standard Deviation');
    set(gca,'XTickLabel',allcond);
    ylabel('RT(ms)');
    title ('RT on the Probe')
hold on
%suptitle (['Performance in the AXCPT. Ita-Eng. n = ' num2str(n)]);
suptitle (['Italian-English. N = ' num2str(n)]);
hold off
saveas(figa, ['overview_AXCPTcon_noTrimming' group], 'tif');


     %% print data to dataset

axtableacc = todataset(group, AX, 'AX', 'acc');
 aytableacc = todataset(group, AY, 'AY', 'acc');
 bxtableacc = todataset(group, BX, 'BX', 'acc');
 bytableacc = todataset(group, BY, 'BY', 'acc');
 
 tableAcc = [axtableacc; aytableacc; bxtableacc; bytableacc];
 writetable(tableAcc, ['dataACC_noTrimming' group '.csv'])
 
 axtablert = todataset(group, AX, 'AX', 'rtIncl');
 aytablert = todataset(group, AY, 'AY', 'rtIncl');
 bxtablert = todataset(group, BX, 'BX', 'rtIncl');
 bytablert = todataset(group, BY, 'BY', 'rtIncl');
 
 tableRT = [axtablert; aytablert; bxtablert; bytablert];
 writetable(tableRT, ['dataRT_noTrimming' group '.csv'])
 
 axtableave = averagedataset(group, AX, 'AX');
 aytableave = averagedataset(group, AY, 'AY');
 bxtableave = averagedataset(group, BX, 'BX');
 bytableave = averagedataset(group, BY, 'BY');
 
 tableave = [axtableave; aytableave; bxtableave; bytableave];
 writetable(tableave, ['dataAve_noTrimming' group '.csv'])





%% plot individual performance

if plotindividual == 1
    for sn = 1:n

        figsn = figure;
        subplot(1,2,1); % accuracy
            meanaccs(sn, :) = ([axaccIE(sn, 1) ayaccIE(sn, 1)  bxaccIE(sn, 1)  byaccIE(sn, 1)]*100);
            bar(meanaccs(sn, :))
            ylim([0 100])
        hold on
            set(gca,'XTickLabel',allcond);
            title ('Mean accuracy on the Probe')
        hold on
        subplot(1,2,2); % RTs
            meanrts(sn, :) = ([axrtIE(sn, 1) ayrtIE(sn, 1) bxrtIE(sn, 1) byrtIE(sn, 1)]);
            errs = ([axrtIE(sn, 3) ayrtIE(sn, 3) bxrtIE(sn, 3) byrtIE(sn, 3)]);
            bar(meanrts(sn, :))
            ylim([0 600])
        hold on
            h=errorbar(meanrts(sn, :), errs);
            set(h(1),'LineStyle','none')
            xlabel('Bars: Standard Deviation');
            set(gca,'XTickLabel',allcond);
            ylabel('RTs');
            title ('Mean RTs on the Probe')
        hold on
        suptitle (['Individual Performance in the AXCPT. Ita-Eng' char(subjnum(sn))]);
        hold off
        saveas(figsn, ['AXCPT_IE_subj' char(subjnum(sn))], 'tif');

    %     clear meanaccs
    %     clear meanrts
        clear errs

    end
end



%% display average and std by conditions
taverage = array2table(...
    [...
    round(mean([axaccIE(:, 1) ayaccIE(:, 1)  bxaccIE(:, 1)  byaccIE(:, 1)]), 2);...
    round(mean([axaccIE(:, 2) ayaccIE(:, 2)  bxaccIE(:, 2)  byaccIE(:, 2)]), 2);...
    round(mean([axrtIE(:, 1) ayrtIE(:, 1) bxrtIE(:, 1) byrtIE(:, 1)]), 0);...
    round(mean([axrtIE(:, 3) ayrtIE(:, 3) bxrtIE(:, 3) byrtIE(:, 3)]), 0)],...
    'VariableNames', allcond, 'Rownames',{'mean_ACC', 'std_ACC', 'mean_RT', 'std_RT'})

writetable(taverage, ['Report_Raw_Data_' group 'noTrim.csv'])
