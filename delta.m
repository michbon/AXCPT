function [may, mby, difference, meanquint] = delta (RTincl, AY, BY)
%% Delta Plot function
%
% order and divide RTs in 5 quintiles for each participant and condition
% mean RT in each quintile
% plot cumulative distribution
% compute difference between AY and BY in each quintile
% "if conditions differ in the amount of inhibition applied,...
%  RT differences between intereference conditions should not increase linearly"
%  ie if the plot tends to be linear, the difference between conditions is  homogeneous...
%  across conditions, suggesting the engagement of the same strategy or a similar...
%  modulation of control processes
%  Morales et al. 13
%
%  clear all; clc
%  load allcond


% ay
if RTincl == 0
    ay = sort(vertcat(AY.rt));   
else
    ay = sort(vertcat(AY.rtIncl));
end
qay = quantile(ay,[0:.2:1]);
ay_bin = discretize(ay, qay);


for i = 1:max(unique(ay_bin))
    r = 1;
    for l = 1:length(ay_bin)
        
        if ay_bin(l) == i
            ay_binned.(['w' num2str(i)])(r, 1) = ay(l);
            r = r+1;
%         else
%             
%             ay_binned.(['w' num2str(i)])(r, 1) = 0;
%             
         end
        
    end
    
        may(i) = mean(ay_binned.(['w' num2str(i)]));
end

% by
if RTincl == 0
    by = sort(vertcat(BY.rt));   
else
    by = sort(vertcat(BY.rtIncl));
end

qby = quantile(by,[0:.2:1]);
by_bin = discretize(by, qby);


for i = 1:max(unique(by_bin))
    r = 1;
    for l = 1:length(by_bin)
        
        if by_bin(l) == i
            by_binned.(['w' num2str(i)])(r, 1) = by(l);
            r = r+1;
%         else
%             
%             by_binned.(['w' num2str(i)])(1,r) = 0;
%             
         end
        
    end
    
        mby(i) = mean(by_binned.(['w' num2str(i)]));
end

% difference by quintile
difference = may - mby;
meanquint = mean([may;mby]);

% % plot delta
% figdelta = figure;
% plot(may, '- o','MarkerEdgeColor','b','MarkerFaceColor','b','MarkerSize',7)
% hold on
% plot(mby, '- o','MarkerEdgeColor','r','MarkerFaceColor','r','MarkerSize',7)
% ylim([0 800])
% xlim([0 5.3])
% set(gca,'XTick', 0:5);
% ylabel('Mean Response Time (ms)')
% xlabel('quintile')
% view(-270, 270)
% hold on
% legend({'AY' 'BY'}, 'location', 'southeast')
% hold on
% title('Delta Plot Analysis')
% saveas(figdelta, 'trydelta2', 'tif')
% 
% % plot difference
% figdiff = figure;
% plot(meanquint, difference, '- *','MarkerEdgeColor','b','MarkerFaceColor','b','MarkerSize',7)
% ylim([0 280])
% ylabel('Delta Response Time (ms)')
% xlabel('Mean Response Time (ms)')
% saveas(figdiff, 'trydiff2', 'tif')





