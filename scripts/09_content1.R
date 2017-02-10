#set working dir
setwd("~/GitHub/lqrps17/")

require(pwr)

pwr.2p.test(h=.3,n=100,sig.level=.05,alternative="two.sided")

