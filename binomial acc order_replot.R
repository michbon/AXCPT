### binomial analysis like morales et al anova: AX is analysed separately
## regress order of experiments

require(lme4)
require(lmerTest)
require(ggplot2)
require(multcomp)
require(afex)
library(aod)
library(Rcpp)
library(effects)
library(lattice)
library(sjPlot)




# accuracy data
ACC <- read.csv('DataACC_noTrim_all_var_order.csv')
ACC$ordervar <- factor(ACC$ordervar)

# RT data
RT <- read.csv('DataRT_noTrim_all_var_order.csv')


################################################################################################################
#### first analyse AX  #########################################################################################


AX_ACC <- ACC[ACC$condition== "AX", ]
AX_ACC$group <- factor(AX_ACC$group, levels = c("IS", "IE", "ISM", "IMM"))

AX_RT <- RT[RT$condition== "AX", ]
AX_RT$group <- factor(AX_RT$group, levels = c("IS", "IE", "ISM", "IMM"))




############## accuracy
AX_ACC_null <- glmer(accuracy ~  (1|subject), data = AX_ACC, family = "binomial")
AX_ACC_group <- glmer(accuracy ~ group + (1|subject), data = AX_ACC, family = "binomial")
summary(AX_ACC_group)
anova(AX_ACC_null, AX_ACC_group)

## confounds
modAXacc = glm(accuracy ~ scale(age) + scale(corr_edu) + ordervar, family = 'binomial', data = AX_ACC)
summary(modAXacc)
dvAXacc = modAXacc$residuals

cond_AX_nullconf <- lmer(dvAXacc ~ (1 |subject), data = AX_ACC,
                         control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)
cond_AX_groupconf <- lmer(dvAXacc ~ group+(1 |subject), data = AX_ACC,
                          control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)
summary(cond_AX_groupconf)
anova(cond_AX_nullconf, cond_AX_groupconf) # no effect of group on acc on AX p .129


############# RT
AX_RT_null <- lmer(rt ~ (1|subject), data = AX_RT)
AX_RT_group <- lmer(rt ~ group + (1|subject), data = AX_RT)
summary(AX_RT_group)
anova(AX_RT_null, AX_RT_group)

## confounds
AX_RT$ordervar <- factor(AX_RT$ordervar)
modAXrt = lm(rt ~ scale(age) + scale(corr_edu) + ordervar, data = AX_RT)
summary(modAXrt)
dvAXrt = modAXrt$residuals

cond_AX_rt_nullconf <- lmer(dvAXrt ~ (1 |subject), data = AX_RT,
                            control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)
cond_AX_rt_groupconf <- lmer(dvAXrt ~ group+(1 |subject), data = AX_RT,
                             control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)
summary(cond_AX_rt_groupconf)
anova(cond_AX_rt_nullconf, cond_AX_rt_groupconf) # no effect of group on RT in AX p .488




###################################################################################################################
#### analyse AY, BX, BY ###########################################################################################


####### accuracy ##################################################################################################

cond_ACC <- ACC[ACC$condition != "AX", ]
cond_ACC$group <- factor(cond_ACC$group, levels = c("IS", "IE", "ISM", "IMM"))
levels(cond_ACC$group) <- c("IS", "IE", "ISP", "ILP")

cond_ACC$subject <- factor(cond_ACC$subject)
cond_ACC$condition <- factor(cond_ACC$condition, levels = c("BY", "AY", "BX"))



summary(cond_ACC$ordervar)

# select random model
cond_ACC_null1 <- glmer(accuracy ~ (1|subject) + (0+condition|subject), data = cond_ACC,
                        family = "binomial", control = glmerControl(optimizer = "bobyqa"))
cond_ACC_null <- glmer(accuracy ~ (1+condition|subject), data = cond_ACC,
                       family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(cond_ACC_null2)
anova(cond_ACC_null1, cond_ACC_null2) # model 2 is not sgnft better, but has less DF, lower AIC and lower BIC

#

cond_ACC_condition <- glmer(accuracy ~ condition + (1+ condition|subject), data = cond_ACC,
                            family = "binomial", control = glmerControl(optCtrl=list(maxfun=6e4)))
ss <- getME(cond_ACC_condition,c("theta","fixef"))
cond_ACC_condition <- update(cond_ACC_condition, start=ss,control=glmerControl(optimizer = "bobyqa"))
summary(cond_ACC_condition)

cond_ACC_condandgroup <- glmer(accuracy ~ condition + group +(1+ condition|subject), data = cond_ACC,
                               family = "binomial", control = glmerControl(optCtrl=list(maxfun=6e4)))
ss <- getME(cond_ACC_condandgroup,c("theta","fixef"))
cond_ACC_condandgroup <- update(cond_ACC_condandgroup, start=ss,control=glmerControl(optimizer = "bobyqa"))
summary(cond_ACC_condandgroup)

cond_ACC_condgroup <- glmer(accuracy ~ condition * group +(1+ condition|subject), data = cond_ACC,
                            family = "binomial", control = glmerControl(optCtrl=list(maxfun=6e4)))
ss <- getME(cond_ACC_condgroup,c("theta","fixef"))
cond_ACC_condgroup <- update(cond_ACC_condgroup, start=ss,control=glmerControl(optimizer = "bobyqa"))
summary(cond_ACC_condgroup)

anova(cond_ACC_null, cond_ACC_condition) # condition p < .000
anova(cond_ACC_condition, cond_ACC_condandgroup) # group p .020
anova(cond_ACC_condandgroup, cond_ACC_condgroup) # interaction .019


 #effs <- as.data.frame(effect(c("group"), cond_ACC_condgroup))
 #ggplot(effs, aes(x = group, y = fit, ymin = lower, ymax = upper)) + 
#   geom_rect(xmax = Inf, xmin = -Inf, ymin = effs[effs$group == "IS", "lower"],
#             ymax = effs[effs$group == "IS", "upper"], alpha = 0.5, fill = "grey") +
#   geom_errorbar(width = 0.2) + geom_point() + theme_bw()




### pairwise comp
cond_ACC$groupcond <- interaction(cond_ACC$condition, cond_ACC$group)
cond_ACC_inter <- glmer(accuracy ~ groupcond + (1 + condition|subject),
                        data = cond_ACC, family = binomial, control = glmerControl(optCtrl=list(maxfun=6e4)))
ss <- getME(cond_ACC_inter,c("theta","fixef"))
cond_ACC_inter <- update(cond_ACC_inter, start=ss,control=glmerControl(optimizer = "bobyqa"))
#summary(cond_ACC_inter)

comp <- glht(cond_ACC_inter, linfct=mcp(groupcond="Tukey")) 
summary(comp)







# plot
library(ggplot2)
pcondacc <- ggplot(cond_ACC_condgroup@frame,aes(x=condition,color=group)) +stat_summary(aes(y=predict(cond_ACC_condgroup,type='response')))
pcondacc <- pcondacc + xlab("Condition") + ylab("Accuracy: fitted values")
pcondacc <- pcondacc+coord_cartesian(ylim = c(0.5, 1))
pcondacc <- pcondacc + scale_colour_discrete(breaks = c("IS", "ISM", "IE", "IMM"), labels=c("Ita-Sard", "Ita-Sard Pass", "Ita-Eng", "Italian"))
pcondacc



############### CONFOUNDING VARIABLES

##### residualize predictors to check for confounds: age, edu
## on residuals, run lmer with predictors of interest
## (quote from reading literature: residualize length of words or similar)

modcondacc = glm(accuracy ~ scale(age) + scale(corr_edu) + ordervar, family = 'binomial', data = cond_ACC)
summary(modcondacc)

#dvcondacc = modcondacc$residuals
residAcc = modcondacc$residuals


cond_ACC_nullconf <- lmer(residAcc ~ (1+condition|subject), data = cond_ACC,
                          control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)

cond_ACC_condconf <- lmer(residAcc ~ condition + (1+condition|subject), data = cond_ACC,
                          control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)

cond_ACC_condegroupconf <- lmer(residAcc ~ condition+group + (1+condition|subject), data = cond_ACC,
                                control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)

cond_ACC_condgroupconf <- lmer(residAcc ~ condition*group + (1+condition|subject), data = cond_ACC,
                               control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)
summary(cond_ACC_condgroupconf)
summary(cond_ACC_condconf)


anova(cond_ACC_nullconf, cond_ACC_condconf) # cond is  p < .000
anova(cond_ACC_condconf, cond_ACC_condegroupconf) # group is not sgnft  p .438
anova(cond_ACC_condegroupconf, cond_ACC_condgroupconf) # interaction is .019


# now run pairwise as before
cond_ACC$groupcond <- interaction(cond_ACC$condition, cond_ACC$group)
cond_ACC_confinter <- lmer(residAcc ~ groupcond + (1+condition|subject), data = cond_ACC,
                           control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)

compconf <- glht(cond_ACC_confinter, linfct=mcp(groupcond="Tukey")) 
summary(compconf)
# and AY comparison IS vs IMM is not sgnft anymore



### plot 1
sjp.setTheme(base = theme_bw())



sjp.int(cond_ACC_condgroupconf, type = "eff", vars = c("group", "condition"), swap.pred = TRUE,
        show.ci = TRUE, jitter.ci = TRUE, facet.grid = TRUE,
        lty = 2:6,
        geom.size = 1.2,
        ylim = c(-1.8, 1.5),
        legend.labels = c("Italian-Sardinian", "Italian-English", 
                         "Italian-Sardinian Passive", "Italian Late Passive"),
        title = (""))
      



## plot 2
#sjp.lmer(cond_ACC_condgroupconf, y.offset = .4)
#sjp.lmer(cond_ACC_condgroupconf, type = "fe", axis.lim = c(-5, 15))
#sjp.lmer(cond_ACC_condgroupconf, type = "pred", facet.grid = FALSE,
#         vars = c("condition", "group"))
cond_ACC$condition <- factor(cond_ACC$condition, levels = c( "AY", "BX", "BY"))
cond_ACC_condgroupconf <- lmer(residAcc ~ condition*group + (1+condition|subject), data = cond_ACC,
                               control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)
summary(cond_ACC_condgroupconf)
summary(cond_ACC_condconf)

### plot3
  plotacc <- plot(effect('condition:group', cond_ACC_condgroupconf,
                          #xlevels='condition'),
                         x.var = 'condition'),
                  
                   type="response",
                   #ylim = c(-1.8, 1.5),
                  
                  #key.args= c("Italian-Sardinian", "Italian-English", 
                  #                 "Italian-Sardinian Passive", "Italian Late Passive"),
                   multiline=TRUE, ci.style="bars",
                   #ylab = 'p of object fixations',
                   main = '',
                  # symbols = 10, 
                  lines=1:4, colors=c(1))
                  #key.args=list(x=0,y=0,corner=c(x=1, y=1)))
  plotacc


######### check what aspect of the random structure makes p > .05
cond_ACC_nullconfc <- lmer(dvcondacc ~ (1+condition|subject), data = cond_ACC)

cond_ACC_condconfc <- lmer(dvcondacc ~ condition + (1+condition|subject), data = cond_ACC,
                          verbose = T)

cond_ACC_condegroupconfc <- lmer(dvcondacc ~ condition+group + (1+condition|subject), data = cond_ACC,
                                verbose = T)

cond_ACC_condgroupconfc <- lmer(dvcondacc ~ condition*group + (1+condition|subject), data = cond_ACC,
                               verbose = T)
summary(cond_ACC_condgroupconfc)
summary(cond_ACC_condconfc)



## 1) without random effects
# cond is still p < .000
# group is sgnft p .003
# interaction is < 000

## 2) only subject intercept                                          <----- !!!
# cond is still p < .000                                              <----- !!!
# group is not sgnft p .259                                        <----- !!!
# interaction is < 000                                                <----- !!!
# pairwise shows IS better than IMM .009 and IE better than IMM .044  <----- !!!

## 3) subject intercept and slope, independent
# cond is still p < .000        <-- as above
# group is not sgnft p .438    <-- as above
# interaction is .019           <-- as above
# pairwise is as above, IMM != IS p .161

anova(cond_ACC_nullconfc, cond_ACC_condconfc) # 
anova(cond_ACC_condconfc, cond_ACC_condegroupconfc) # 
anova(cond_ACC_condegroupconfc, cond_ACC_condgroupconfc) # 

# pairwise chekcing random str
cond_ACC_confinterc <- lmer(dvcondacc ~ groupcond + (1+condition|subject), data = cond_ACC,
                            verbose = T)

compconfc <- glht(cond_ACC_confinterc, linfct=mcp(groupcond="Tukey")) 
summary(compconfc)

##########################################################################################

pcondaccconf <- ggplot(cond_ACC_condgroupconf@frame,aes(x=condition,color=group)) +stat_summary(aes(y=predict(cond_ACC_condgroupconf,type='response')))
pcondaccconf <- pcondaccconf + xlab("Condition") + ylab("Accuracy: fitted values")
pcondaccconf <- pcondaccconf + scale_colour_discrete(breaks = c("IS", "ISM", "IE", "IMM"), labels=c("Ita-Sard", "Ita-Sard Pass", "Ita-Eng", "Italian"))
pcondaccconf # what is on the Y axes? residuals of accuracy variation explained by condition and group...

###############################################################






############################################################################################################
#####  RT  #####################################################################################################

cond_RT <- RT[RT$condition != "AX", ]
cond_RT$rt <- scale(cond_RT$rt)
cond_RT$group <- factor(cond_RT$group, levels = c("IS", "IE", "ISM", "IMM"))
levels(cond_RT$group) <- c("IS", "IE", "ISP", "ILP")

#cond_RT$condition <- factor(cond_RT$condition, levels = c("BY", "AY", "BX"))
cond_RT$condition <- factor(cond_RT$condition, levels = c("AY", "BX", "BY"))
cond_RT$ordervar <- factor(cond_RT$ordervar)


cond_RT_null <- lmer(rt ~   (1 + condition|subject), data = cond_RT,
                     control = lmerControl(optCtrl=list(maxfun=6e4)))


cond_RT_condition <- lmer(rt ~ condition + (1+ condition|subject), data = cond_RT,
                          control = lmerControl(optCtrl=list(maxfun=6e4)))
summary(cond_RT_condition)

cond_RT_condandgroup <- lmer(rt ~ condition + group +(1+ condition|subject), data = cond_RT,
                             control = lmerControl(optCtrl=list(maxfun=6e4)))
summary(cond_RT_condandgroup)

cond_RT_condgroup <- lmer(rt ~ condition * group +(1+ condition|subject), data = cond_RT,
                          control = lmerControl(optCtrl=list(maxfun=6e4)))
summary(cond_RT_condgroup)


anova(cond_RT_null, cond_RT_condition) # condition p < .000
anova(cond_RT_condition, cond_RT_condandgroup) # group p .447
anova(cond_RT_condandgroup, cond_RT_condgroup) # interaction .062



# pairwise comp
cond_RT$groupcond <- interaction(cond_RT$condition, cond_RT$group)
cond_RT_inter <- lmer(rt ~ groupcond + (1 + condition|subject),
                      data = cond_RT, control = lmerControl(optCtrl=list(maxfun=6e4)))

#summary(cond_RT_inter)

comprt <- glht(cond_RT_inter, linfct=mcp(groupcond="Tukey")) 
summary(comprt)


library(ggplot2)
pcondRT <- ggplot(cond_RT_condgroup@frame,aes(x=condition,color=group)) +stat_summary(aes(y=predict(cond_RT_condgroup, type = "response")))
pcondRT <- pcondRT + xlab("Condition") + ylab("log odds of RT")
#pcondRT <- pcondRT+coord_cartesian(ylim = c(0.5, 1))
pcondRT <- pcondRT + scale_colour_discrete(breaks = c("IS", "ISM", "IE", "IMM"), labels=c("Ita-Sard", "Ita-Sard Pass", "Ita-Eng", "Italian"))
pcondRT


######### confounding variables on Rt

mod2 = lm(rt ~ scale(age) + scale(corr_edu)+ordervar, data = cond_RT)
summary(mod2)

residRT = mod2$residuals

cond_RT_nullconf <- lmer(residRT ~ (1+condition|subject), data = cond_RT,
                         control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)

cond_RT_condconf <- lmer(residRT ~ condition + (1+condition|subject), data = cond_RT,
                         control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)
summary(cond_RT_condconf)

cond_RT_condegroupconf <- lmer(residRT ~ condition+group + (1+condition|subject), data = cond_RT,
                               control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)

cond_RT_condgroupconf <- lmer(residRT ~ condition*group + (1+condition|subject), data = cond_RT,
                              control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)
summary(cond_RT_condgroupconf)

anova(cond_RT_nullconf, cond_RT_condconf) # cond is still p < .000
anova(cond_RT_condconf, cond_RT_condegroupconf) # group .743
anova(cond_RT_condegroupconf, cond_RT_condgroupconf) # interaction is .366


# plot rt
sjp.int(cond_RT_condgroupconf, type = "eff", vars = c("group", "condition"), swap.pred = FALSE,
        show.ci = TRUE, jitter.ci = TRUE, facet.grid = FALSE,
        geom.size = 1,
        ylim = c(-1, 1.3),
        legend.labels = c("Italian-Sardinian", "Italian-English", 
                          "Italian-Sardinian Passive", "Italian Late Passive"),
        title = (""),
        
        geom.colors = "gs")
       # title = ("RT on AY, BX and BY (fitted values of residuals)"))

### plot3
plotrt <- plot(effect('condition:group', cond_RT_condgroupconf,
                       #xlevels='condition'),
                       x.var = 'condition'),
                
                type="response",
                #ylim = c(-1.8, 1.5),
                
                #key.args= c("Italian-Sardinian", "Italian-English", 
                #                 "Italian-Sardinian Passive", "Italian Late Passive"),
                multiline=TRUE, ci.style="bars",
                #ylab = 'p of object fixations',
                main = '',
                # symbols = 10, 
                lines=1:4, colors=c(1))
#key.args=list(x=0,y=0,corner=c(x=1, y=1)))
plotrt



# now run pairwise as before
cond_RT$groupcond <- interaction(cond_RT$condition, cond_RT$group)
cond_RT_confinter <- lmer(residRT ~ groupcond + (1+condition|subject), data = cond_RT,
                          control = lmerControl(optCtrl=list(maxfun=6e4)), verbose = T)

compconfrt <- glht(cond_RT_confinter, linfct=mcp(groupcond="Tukey")) 
summary(compconfrt)



