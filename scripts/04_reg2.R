#definer filsti
setwd("~/GitHub/lqrps17/")

#indl?s nogle pakker
require(haven)
require(dplyr)
require(stargazer)
require(multiwayvcov)
require(lmtest)
require(lme4)

#larsen et al data
ld<-read_dta("data/04_larsen.dta")

#overblik over data med glimpse()
glimpse(ld)

# estimer ren cross-sectional model
m1<-lm(incsupport~hp_1yr,data=ld)
summary(m1)

#estimer model med year FE
m2<-lm(incsupport~hp_1yr+factor(valgstedid)+factor(year),data=ld)
summary(m2) # impossible to read :-/

#clustered se's at precinct level
vcov_precinct<-cluster.vcov(m2,ld$valgstedid)
clse_precinct<-sqrt(diag(vcov_precinct))

#alternative: re model
m2re<-lmer(incsupport~hp_1yr+(1|valgstedid)+(1|year),data=ld)
summary(m2re)

#presentation in stargazer
stargazer(m1,m2,m2,m2re,se=list(NULL,NULL,clse_precinct,NULL),style="apsr",type="text",omit="factor")

