setwd("~/GitHub/lqrps17")

require(readr)
require(dplyr)
require(broom)

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
