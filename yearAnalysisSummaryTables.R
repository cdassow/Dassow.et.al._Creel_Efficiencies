# script to calculate some summary stats on the the year by year analysis that are published in Dassow et al. Creel Efficiencies paper

rm(list=ls())

library(tidyverse)

# set working directory to location of local copy of git repo
setwd("C:/Users/dassocju/Documents/OAS_git_repos/Dassow.et.al._Creel_Efficiencies")

# read in year by year RDS objects

expR=readRDS('yearLoopOutput_explRate_9.30.25.RData')
harvR=readRDS('yearLoopOutput_harvestR_9.29.25.RData')
eff=readRDS('yearLoopOutput_effort_9.23.25.RData')
catch=readRDS('yearLoopOutput_walleyecatch_9.23.25.RData')
hrv=readRDS('yearLoopOutput_harvest_9.23.25.RData')

# calculate a percentage of years with non-sig diffs by data scenario to present in a table.

# unpack RDS objects
bpval.self.y.expR=expR[[1]]
bpval.self.y.expR=bpval.self.y.expR[!is.na(bpval.self.y.expR$scenario),]
bpval.comp.y.expR=expR[[2]]
bpval.comp.y.expR=bpval.comp.y.expR[!is.na(bpval.comp.y.expR$scenario),]
grMetrics.y.expR=expR[[3]]
grMetrics.y.expR=grMetrics.y.expR[!is.na(grMetrics.y.expR$scenario),]

bpval.self.y.harvR=harvR[[1]]
bpval.self.y.harvR=bpval.self.y.harvR[!is.na(bpval.self.y.harvR$scenario),]
bpval.comp.y.harvR=harvR[[2]]
bpval.comp.y.harvR=bpval.comp.y.harvR[!is.na(bpval.comp.y.harvR$scenario),]
grMetrics.y.harvR=harvR[[3]]
grMetrics.y.harvR=grMetrics.y.harvR[!is.na(grMetrics.y.harvR$scenario),]

bpval.self.y.eff=eff[[1]]
bpval.self.y.eff=bpval.self.y.eff[!is.na(bpval.self.y.eff$scenario),]
bpval.comp.y.eff=eff[[2]]
bpval.comp.y.eff=bpval.comp.y.eff[!is.na(bpval.comp.y.eff$scenario),]
grMetrics.y.eff=eff[[3]]
grMetrics.y.eff=grMetrics.y.eff[!is.na(grMetrics.y.eff$scenario),]

bpval.self.y.catch=catch[[1]]
bpval.self.y.catch=bpval.self.y.catch[!is.na(bpval.self.y.catch$scenario),]
bpval.comp.y.catch=catch[[2]]
bpval.comp.y.catch=bpval.comp.y.catch[!is.na(bpval.comp.y.catch$scenario),]
grMetrics.y.catch=catch[[3]]
grMetrics.y.catch=grMetrics.y.catch[!is.na(grMetrics.y.catch$scenario),]

bpval.self.y.hrv=hrv[[1]]
bpval.self.y.hrv=bpval.self.y.hrv[!is.na(bpval.self.y.hrv$scenario),]
bpval.comp.y.hrv=hrv[[2]]
bpval.comp.y.hrv=bpval.comp.y.hrv[!is.na(bpval.comp.y.hrv$scenario),]
grMetrics.y.hrv=hrv[[3]]
grMetrics.y.hrv=grMetrics.y.hrv[!is.na(grMetrics.y.hrv$scenario),]

# combine dfs into relevant global dfs
metric=c(rep('expR',nrow(bpval.self.y.expR)),
         rep('harvR',nrow(bpval.self.y.harvR)),
         rep('eff',nrow(bpval.self.y.eff)),
         rep('catch',nrow(bpval.self.y.catch)),
         rep('hrv',nrow(bpval.self.y.hrv)))
global.bpval.self.y=as.data.frame(cbind(metric,
                                        rbind(bpval.self.y.expR[c('year','scenario','sd.pval')],
                                              bpval.self.y.harvR[c('year','scenario','sd.pval')],
                                              bpval.self.y.eff[c('year','scenario','sd.pval')],
                                              bpval.self.y.catch[c('year','scenario','sd.pval')],
                                              bpval.self.y.hrv[c('year','scenario','sd.pval')])))

metric=c(rep('expR',nrow(bpval.comp.y.expR)),
         rep('harvR',nrow(bpval.comp.y.harvR)),
         rep('eff',nrow(bpval.comp.y.eff)),
         rep('catch',nrow(bpval.comp.y.catch)),
         rep('hrv',nrow(bpval.comp.y.hrv)))
global.bpval.comp.y=as.data.frame(cbind(metric,
                                        rbind(bpval.comp.y.expR[c('year','scenario','sd.pval')],
                                              bpval.comp.y.harvR[c('year','scenario','sd.pval')],
                                              bpval.comp.y.eff[c('year','scenario','sd.pval')],
                                              bpval.comp.y.catch[c('year','scenario','sd.pval')],
                                              bpval.comp.y.hrv[c('year','scenario','sd.pval')])))
metric=c(rep('expR',nrow(grMetrics.y.expR)),
         rep('harvR',nrow(grMetrics.y.harvR)),
         rep('eff',nrow(grMetrics.y.eff)),
         rep('catch',nrow(grMetrics.y.catch)),
         rep('hrv',nrow(grMetrics.y.hrv)))
global.grMetrics.y=as.data.frame(cbind(metric,
                                        rbind(grMetrics.y.expR,
                                              grMetrics.y.harvR,
                                              grMetrics.y.eff,
                                              grMetrics.y.catch,
                                              grMetrics.y.hrv)))
global.grMetrics.y=global.grMetrics.y[!is.na(global.grMetrics.y$gr.prsf),]

# summary tables
selfCompTabl=global.bpval.self.y%>%
  group_by(metric,scenario)%>%
  summarise(percNONsig.sd=(sum(sd.pval>=0.1 & sd.pval<=0.9)/n())*100)
wideSelfCompTab.sd=pivot_wider(selfCompTabl,
                                 id_cols = scenario,
                                 names_from = metric,
                                 values_from = percNONsig.sd)
actualCompTabl=global.bpval.comp.y%>%
  group_by(metric,scenario)%>%
  summarise(percNONsig.sd=(sum(sd.pval>=0.1 & sd.pval<=0.9)/n())*100)
wideActualCompTab.sd=pivot_wider(actualCompTabl,
                                 id_cols = scenario,
                                 names_from = metric,
                                 values_from = percNONsig.sd)
grMetricCompTabl=global.grMetrics.y%>%
  group_by(metric,scenario)%>%
  summarise(percConverged=(sum(gr.prsf<1.1,na.rm = T)/n())*100)
widegrCompTab=pivot_wider(grMetricCompTabl,
                          id_cols = scenario,
                          names_from = metric,
                          values_from = percConverged)