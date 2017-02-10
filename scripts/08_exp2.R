setwd("~/GitHub/lqrps17")

require(readr)
require(dplyr)
require(broom)
require(ggplot2)
require(ri)

# RANDOMIZATION INFERENCE

set.seed(123456)
y <- rnorm(500,100,15)
Z <- sample(0:1,500,replace=T)
cluster <- rep(1:50,each=10)
perms <- genperms(Z, clustvar=cluster) # all possible permutations
probs <- genprobexact(Z, clustvar=cluster) # probability of treatment
ate <- estate(y,Z,prob=probs) # estimate the ATE
## Conduct Sharp Null Hypothesis Test of Zero Effect for Each Unit
Ys <- genouts(y,Z,ate=0) # generate potential outcomes under sharp null of no effect
distout <- gendist(Ys,perms, prob=probs) # generate sampling dist. under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

#indlæs data
ggd<-read_csv("data/8_gg.csv")

#overblik over data
glimpse(ggd)

#limit to one-person households i kontrol eller canvas
ggd<-subset(ggd,onetreat==1 & mailings==0 & phongotv==0 & persons==1)

#ift. bogen:
# VOTED hedder her v98
# ASSIGNED hedder her persngrp
# TREATED hedder her cntany

#model for ITT
ittmodel<-lm(v98~persngrp,data=ggd)
summary(ittmodel)
itt<-tidy(ittmodel)[2,2]

#model for ITTD
ittdmodel<-lm(cntany~persngrp,data=ggd)
summary(ittdmodel)
ittd<-tidy(ittdmodel)[2,2]

#beregn CACE
cace <- itt / ittd
