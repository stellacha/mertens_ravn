library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)
library(scales)
library(dplyr)
library(xtable)
library(patchwork)
library(tidyverse)
library(stringr)
setwd("~/repo/psvar")

###### DATA   ###################################################################################

irf <- readRDS("~/repo/psvar/irf.RDS")
irfbs <- readRDS("~/repo/psvar/irfbs.RDS")
irfbsh <- readRDS("~/repo/psvar/irfbsh.RDS")
irfbsl <- readRDS("~/repo/psvar/irfbsl.RDS")

# Modifiaction ##############################################################################
# Personal income tax revenues : IRF of APITR/0.1667(Averge of APITR)+IRF of PITB
PITR <- cbind(irf[[1]][,1]/0.1667+irf[[1]][,3],irf[[3]][,1]/0.1667+irf[[3]][,3])
saveRDS(PITR , file="~/repo/psvar/PITR.RDS")
# Corporate income tax revenues : IRF of ACITR/0.2996(Averge of ACITR)+IRF of CITB
CITR <- cbind(irf[[2]][,2]/0.2996+irf[[2]][,4],irf[[4]][,2]/0.2996+irf[[4]][,4])
saveRDS(CITR , file="~/repo/psvar/CITR.RDS")


# Modifiaction ##############################################################################
# Personal income tax revenues : IRF of APITR/0.1667(Averge of APITR)+IRF of PITB
PITRbs <-lapply(c(1,3), function(x)(irfbs.temp[[x]][[1]]/0.1667+irfbs.temp[[x]][[3]]))
irfbs.pitr.h <- lapply(1:2, function(x) apply(PITRbs[[x]],1, quantile, prob = 0.025))
irfbs.pitr.l <- lapply(1:2, function(x) apply(PITRbs[[x]],1, quantile, prob = 0.975))
saveRDS(PITRbs , file="~/repo/psvar/PITRbs.RDS")
saveRDS(irfbs.pitr.h, file="~/repo/psvar/irfbs.pitr.h.RDS")
saveRDS(irfbs.pitr.l, file="~/repo/psvar/irfbs.pitr.l.RDS")

# Corporate income tax revenues : IRF of ACITR/0.2996(Averge of ACITR)+IRF of CITB

CITRbs <-lapply(c(2,4), function(x)(irfbs.temp[[x]][[2]]/0.2996+irfbs.temp[[x]][[4]]))
irfbs.citr.h <- lapply(1:2, function(x) apply(CITRbs[[x]],1, quantile, prob = 0.025))
irfbs.citr.l <- lapply(1:2, function(x) apply(CITRbs[[x]],1, quantile, prob = 0.975))
saveRDS(CITRbs , file="~/repo/psvar/CITRbs.RDS")
saveRDS(irfbs.citr.h, file="~/repo/psvar/irfbs.citr.h.RDS")
saveRDS(irfbs.citr.l, file="~/repo/psvar/irfbs.citr.l.RDS")




# Modifiaction ##############################################################################
# Personal income tax revenues : IRF of APITR/0.1667(Averge of APITR)+IRF of PITB
date <-c(1:20)

PITR <- cbind(irf[[1]][,1]/0.1667+irf[[1]][,3],irf[[3]][,1]/0.1667+irf[[3]][,3])
PITR.all<-cbind.data.frame(date,PITR[,1], irfbs.pitr.h[[1]], irfbs.pitr.l[[1]], PITR[,2], irfbs.pitr.h[[2]], irfbs.pitr.l[[2]])
# Corporate income tax revenues : IRF of ACITR/0.2996(Averge of ACITR)+IRF of CITB
CITR <- cbind(irf[[2]][,2]/0.2996+irf[[2]][,4],irf[[4]][,2]/0.2996+irf[[4]][,4])
CITR.all<-cbind.data.frame(date,CITR[,1], irfbs.citr.h[[1]], irfbs.citr.l[[1]], PITR[,2], irfbs.citr.h[[2]], irfbs.citr.l[[2]])

##### GRAPH ###################################################################################



#NewData_AP <-c("APITR:1","PITB:3","ACITR:2","RGDP:6","PITR:N","GOV:5")
#NewData_AC <-c("ACITR:2","CITB:4","APITR:1","RGDP:6","CITR:N","GOV:5")

irf.all <-lapply(1:4, function(y) {lapply(1:7,function(x){cbind.data.frame(irf[[y]][,x], irfbsh[[y]][[x]],irfbsl[[y]][[x]])})})
irf.ap <-lapply(1:7, function(x)cbind.data.frame(date,irf.all[[1]][[x]], irf.all[[3]][[x]]))
##irf.ap.select :irf.ap[[1]],irf.ap[[3]],irf.ap[[2]],irf.ap[[6]],PITR,irf.ap[[5]])

irf.ac <-lapply(1:7, function(x)cbind.data.frame(date,irf.all[[2]][[x]], irf.all[[4]][[x]]))
##irf.ac.select :irf.ac[[2]],irf.ac[[4]],irf.ac[[1]],irf.ac[[6]],CITR,irf.ac[[5]])
irf.ap.select1 <-melt(data.frame(irf.ap[[1]]), id="date")
irf.ap.select2 <-melt(data.frame(irf.ap[[6]]), id="date")
irf.ap.select3 <-melt(data.frame(irf.ap[[3]]), id="date")
irf.ap.select4 <-melt(data.frame(PITR.all), id="date")
irf.ap.select5 <-melt(data.frame(irf.ap[[2]]), id="date")
irf.ap.select6 <-melt(data.frame(irf.ap[[5]]), id="date")

irf.ac.select1 <-melt(data.frame(irf.ac[[1]]), id="date")
irf.ac.select2 <-melt(data.frame(irf.ac[[6]]), id="date")
irf.ac.select3 <-melt(data.frame(irf.ac[[3]]), id="date")
irf.ac.select4 <-melt(data.frame(CITR.all), id="date")
irf.ac.select5 <-melt(data.frame(irf.ac[[2]]), id="date")
irf.ac.select6 <-melt(data.frame(irf.ac[[5]]), id="date")

irf.select.ap=list(irf.ap.select1, irf.ap.select2, irf.ap.select3, irf.ap.select4, irf.ap.select5,irf.ap.select6)
irf.select.ac=list(irf.ac.select1, irf.ac.select2, irf.ac.select3, irf.ac.select4, irf.ac.select5,irf.ac.select6)

title.ap=list(
  "Average personal income tax rate","Output",
  "Personal income tax base",
  "Personal income tax Reveues",
  "Average coporate income tax rate",
  "Government purchases")
title.ac=list(
  "Average corporate income tax rate","Output",
  "Corporate income tax base",
  "Corporate income tax Reveues",
  "Average personal income tax rate",
  "Government purchases")
################ APITR SHOCK ON ECONOMY ACTIVITIES ###################################################################

irf.select.ap.g <-lapply(irf.select.ap, function(x) ggplot(x, aes(x=date, y=value ,linetype=variable))+ labs(y="IRF",x="Date")+geom_line()+scale_x_continuous(breaks=seq(0,20,2))+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())+geom_hline(yintercept=0, linetype="dashed", color = "red")+scale_linetype_manual(values=c("solid", "dotted","dotted", "solid", "dotdash","dotdash"))+geom_point(aes(shape=variable))+scale_shape_manual(values=c(NA,NA,NA,18,NA,NA))+scale_color_manual(values=c( "black", "#666666", "#666666","grey", "grey", "grey"))+ theme_bw()+theme(plot.title = element_text(face="bold", size=18 ,hjust = .5, vjust = 2), panel.background = element_rect(colour = "black"))+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.position="none"))

irf.select.ap.gg <- lapply(1:6,function(y) irf.select.ap.g[[y]]+ggtitle(title.ap[[y]]))
irf_apit_g<-do.call("grid.arrange",c(irf.select.ap.gg, nrow=3))
irf_apit_g
################ ACITR SHOCK ON ECONOMY ACTIVITIES ######################################################################


irf.select.ac.g <-lapply(irf.select.ac, function(x) ggplot(x, aes(x=date, y=value ,linetype=variable))+ labs(y="IRF",x="Date")+geom_line()+scale_x_continuous(breaks=seq(0,20,2))+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())+geom_hline(yintercept=0, linetype="dashed", color = "red")+scale_linetype_manual(values=c("solid", "dotted","dotted", "solid", "dotdash","dotdash"))+geom_point(aes(shape=variable))+scale_shape_manual(values=c(NA,NA,NA,18,NA,NA))+scale_color_manual(values=c( "black", "#666666", "#666666","grey", "grey", "grey"))+ theme_bw()+theme(plot.title = element_text(face="bold", size=18 ,hjust = .5, vjust = 2), panel.background = element_rect(colour = "black"))+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.position="none"))

irf.select.ac.gg <- lapply(1:6,function(y) irf.select.ac.g[[y]]+ggtitle(title.ac[[y]]))
irf_acit_g <-do.call("grid.arrange",c(irf.select.ac.gg, nrow=3))
irf_acit_g


