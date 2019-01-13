function [ay_cumacc, bx_cumacc] = SpeedAccuracyFx (AY, BX)
%% Speed-accuracy function
%
% bin RTs in 100ms windows
% compute cumulative accuracy for BX and AY in each window
% compute the difference
% reference: Braver et al., 2009, supporting information
%
% clear all; clc
% load aybx

rtwin = 0:100:1300;

% ay
ay = [vertcat(AY.acc) vertcat(AY.rt)];
ay = sortrows(ay,2);
i_bin = discretize(ay(:,2),rtwin);

for i = 1:max(unique(i_bin))
    r = 1;
    for l = 1:length(i_bin)
        
        if i_bin(l) == i
            ay_binned.(['w' num2str(rtwin(i))])(r,:) = ay(l, :);
            r = r+1;
        else
            
            ay_binned.(['w' num2str(rtwin(i))])(r,:) = [0 0];
            
        end
        
    end
    
    if i > 1
        ay_cumacc(i,1) = (sum(ay_binned.(['w' num2str(rtwin(i))])(:,1))/...
            length(ay)) + ay_cumacc(i-1,1);
    else
        ay_cumacc(i,1) = sum(ay_binned.(['w' num2str(rtwin(i))])(:,1))/...
              length(ay);
    end
end

% bx
bx = [vertcat(BX.acc) vertcat(BX.rt)];
bx = sortrows(bx,2);
i_bin = discretize(bx(:,2),rtwin);

for i = 1:max(unique(i_bin))
    
    r = 1;
    for l = 1:length(i_bin)
        
        if i_bin(l) == i
            bx_binned.(['w' num2str(rtwin(i))])(r,:) = bx(l, :);
            r = r+1;
        else
            
            bx_binned.(['w' num2str(rtwin(i))])(r,:) = [0 0];
        end
        
    end
    if i > 1
        bx_cumacc(i,1) = (sum(bx_binned.(['w' num2str(rtwin(i))])(:,1))/...
            length(bx)) + bx_cumacc(i-1,1);
    else
        bx_cumacc(i,1) = sum(bx_binned.(['w' num2str(rtwin(i))])(:,1))/...
              length(bx);
    end
end
