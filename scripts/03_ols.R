#definer filsti
setwd("~/GitHub/lqrps17/")

#indlæg nogle pakker
require(haven)
require(dplyr)

#simuler et meget enkelt datas?t
simdat<-data.frame(y=c(2,8,11),x=c(1,3,5))

#kovarians og varians
kovariansxy<-cov(simdat$x,simdat$y)
variansx<-var(simdat$x)
variansy<-var(simdat$y)

#udregn regressionskoefficient og bivariat korrelation
regkoefx<-kovariansxy/variansx
korrkoefxy<-kovariansxy/(sqrt(variansx)*sqrt(variansy))

#tjek
summary(lm(y~x,data=simdat))
cor.test(simdat$x,simdat$y)



#indlæs data
gd<-read_dta("data/2_gilens.dta")

#fjern missing i dep var
gd<-mutate(gd,outcome_rc=ifelse(OUTCOME==99,NA,OUTCOME))

#ols modeller
m1ols<-lm(outcome_rc~pred50_sw,data=gd)
m2ols<-lm(outcome_rc~pred90_sw,data=gd)
m3ols<-lm(outcome_rc~pred50_sw+pred90_sw,data=gd)

#resultat
summary(m1ols)
summary(m2ols)
summary(m3ols)

#lad os tjekke nogle typiske forudsætninger. er den funktionelle form lineær?
plot(gd$pred50_sw,jitter(gd$outcome_rc))

#heteroskedasticitet?
require(lmtest)
bptest(m3ols,studentize=F)

#normalfordelte residualer?
hist(m3ols$residuals)

#multikollinearitet?
require(car)
vif(m3ols)

#hvor meget korrelerer holdninger for forskellige indkomstgrupper?
plot(gd$pred50_sw,gd$pred90_sw)

#formel test af korrelation
cor.test(gd$pred50_sw,gd$pred90_sw)



### EKSTRA: robusthedstjek m binær afhængig

#binær afhængig
gd<-mutate(gd,outcome_rc01=ifelse(outcome_rc>0 & outcome_rc < 4,1,0))

#logit modeller
m1logit<-glm(outcome_rc01~pred50_sw,data=gd,family="binomial")
m2logit<-glm(outcome_rc01~pred90_sw,data=gd,family="binomial")
m3logit<-glm(outcome_rc01~pred90_sw+pred50_sw,data=gd,family="binomial")

#resultat
summary(m1logit)
summary(m2logit)
summary(m3logit)

#heteroskedasticitet?
bptest(m3logit,studentize=F)

#normalfordelte residualer?
hist(m3logit$residuals)

#multikollinearitet?
vif(m3logit)
