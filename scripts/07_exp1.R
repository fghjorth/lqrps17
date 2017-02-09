setwd("~/GitHub/lqrps17/")

require(readr)
require(stargazer)
require(dplyr)
require(coefplot)

### Randomisering i praksis

#1. lad os sige vi har et datasæt med 100 mennesker. 10 skal have treatment

df<-data.frame(id=1:100)
N<-nrow(df)
m<-10

#2. sæt et seed
set.seed(1234)

#3. tilfældigt tal for hver person
df$randomnum<-sample(10000,N,replace=F)

#4. sorter iht. det tilfældige tal (arrange() kommer fra dplyr)
df<-arrange(df,randomnum)

#5. assign treatment til de først m observationer
df$treat<-0
df$treat[1:m]<-1

#6. for god ordens skyld: sorter tilbage til oprindelig rækkefølge
df<-arrange(df,id)

### Gerber, Green & Larimer (2008)

#indlæs data
ggl<-read_csv("data/7_ggl.csv")

#kig på data
glimpse(ggl)

#regression på turnout af exp treatment
ols1<-lm(primary2006~messages,data=ggl)
summary(ols1)

#gør control til referencekategori
ggl$treatmentfac<-relevel(as.factor(ggl$messages),ref="Control")

#igen: regression på turnout af exp treatment
ols2<-lm(primary2006~treatmentfac,data=ggl)
summary(ols2)

#vis på tabelform
stargazer(ols2,type="text")

#vis som koefficientplot
coefplot(ols2,intercept=F,horizontal=T,color="black")

#vigtigt balancecheck: balance på pre-treatment turnout?
olsbc<-lm(primary2004~treatmentfac,data=ggl)
summary(olsbc)