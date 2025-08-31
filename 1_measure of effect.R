#measure of effect
library(usethis)
use_github()

install.packages("fmsb")
install.packages("biostat3")
library(haven); library(tidyverse); library(fmsb); library(biostat3)

whitehall<-read_dta("whitehall.dta")

#Before we start the analysis, we first need to do some data manipulation. For variables
#grade4, smok, grade, cholgrp, and sbpgrp, we need to tell R to treat them as categorical
#variables, and to apply labels:

whitehall$grade4<-factor(whitehall$grade4, levels=c(1,2,3,4), 
                         labels=c("admin", "professional/executive", "clerical", "other"))
whitehall$grade<-factor(whitehall$grade, levels=c(1,2), 
                        labels=c("admin & professional", "clerical & other"))
whitehall$cholgrp<-factor(whitehall$cholgrp, levels=c(1,2,3,4), 
                          labels=c("<150", "150-199", "200-249", ">=250"))
whitehall$sbpgrp<-factor(whitehall$sbpgrp, levels=c(1,2,3,4), 
                         labels=c("<120", "120-139", "140-159", ">=160"))
whitehall$smok<-factor(whitehall$smok, levels=c(1,2,3,4,5), 
                       labels=c("never","ex", "1-14", "15-24", "25+"))

whitehall %>%
  group_by(all) %>%
  summarise(freq=n()) %>%
  mutate(percent.of.all=100*freq/sum(freq))

whitehall %>%
  group_by(smok) %>%
  summarise(freq=n()) %>%
  mutate(percent.of.all=100*freq/sum(freq))

n<-as.numeric(whitehall$smok)
whitehall<-whitehall %>%
  mutate(currsm=recode(n, 0,0,1,1,1))
whitehall$currsm<-factor(whitehall$currsm, levels=c(0,1), 
                         labels=c("never/ex", "current"))
table(whitehall$smok, whitehall$currsm)

#calculation of risks and risk ratios
whitehall %>%
  group_by(currsm, all) %>%
  summarise(freq=n()) %>%
  group_by(currsm) %>%
  mutate(percent.of.currsm=100*freq/sum(freq))

whitehall %>%
  group_by(currsm) %>%
  summarise(n=n(), mean = mean(all), SD = sd(all))

whitehall$ncurrsm<-(as.numeric(whitehall$currsm))-1
c1<-sum(whitehall$all==1 & whitehall$ncurrsm==1) #cases in exposed
c0<-sum(whitehall$all==1 & whitehall$ncurrsm==0) #cases in unexposed
N1<-sum(whitehall$ncurrsm==1) #exposed
N0<-sum(whitehall$ncurrsm==0) #unexposed
riskdifference(c1,c0,N1,N0, conf.level = 0.95)

fmsb::riskratio(c1,c0,N1,N0, conf.level=0.95)

#calculation of rates
whitehall$y<-(whitehall$timeout-whitehall$timein)/365.25
whitehall$y<-as.numeric(whitehall$y)
biostat3::survRate(Surv(whitehall$y,all) ~ ncurrsm, data=whitehall)

biostat3::survRate(Surv(whitehall$y/1000,all) ~ ncurrsm, data=whitehall)

biostat3::survRate(Surv(whitehall$y/1000,all) ~ smok, data=whitehall)

r<-biostat3::survRate(Surv(whitehall$y/1000,all) ~ smok, data=whitehall)
df<-data.frame(
  rate=c(r$rate),
  sm=factor(c(r$smok)),
  lower=c(r$lower),
  upper=c(r$upper)
)
p<-ggplot(data=df, aes(sm, rate))
p + geom_pointrange(aes(ymin=lower, ymax=upper)) + scale_x_discrete("smoking 
status", breaks=c(1,2,3,4,5)) + scale_y_continuous("mortality rate (per
1000")

#calculation of rate ratios
y0<-sum(whitehall$y[whitehall$ncurrsm==0])
y1<-sum(whitehall$y[whitehall$ncurrsm==1])
rateratio(c1, c0, y1, y0)

#calculation of odds ratios
mwanza<-read_dta("mwanza.dta")

mwanza %>%
  group_by(case) %>%
  summarise(freq=n()) %>%
  mutate(percent.of.case=100*freq/sum(freq))

mwanza %>%
  group_by(ed) %>%
  summarise(freq=n()) %>%
  mutate(percent.of.ed=100*freq/sum(freq))

mwanza %>%
  group_by(case, ed) %>%
  summarise(freq=n()) %>%
  group_by(case) %>%
  mutate(percent.of.case=100*freq/sum(freq))
chisq.test(table(mwanza$ed,mwanza$case))




