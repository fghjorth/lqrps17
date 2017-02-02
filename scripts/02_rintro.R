# definer sti
setwd("~/GitHub/vkme16/scripts")

# brug R som en lommeregner

5+3

5*3

5^3

5*5

sqrt(5*5) #bemærk at sqrt() er en funktion!

# objekter

resultat <- 5+3

print(resultat)

class(resultat)

resultat <- "8"

class(resultat)

# vektorer

world.pop<-c(2525779,3026003,3691173,4449049,5320817,6127700,6916183)

pop.first<-world.pop[1:3] #indeksering!

pop.million<- world.pop / 1000

# funktioner

min(world.pop)

max(world.pop)

summary(world.pop)

year<-seq(from=1950,to=2010,by=10)

rev(rev(year)) #nestede funktioner

# data frames

worldpopdf<-data.frame(year=year,worldpop=world.pop)

class(worldpopdf)

# export og import af datafiler

require(readr) # bemærk: her indlæses en pakke. skal installeres først med install.package()

write_csv(worldpopdf,"../data/1_worldpop.csv")

worldpopdf<-read_csv("../data/1_worldpop.csv")

# subsetting af data frames

worldpopdf$year

worldpopdf[4,2]

worldpopdf[worldpopdf$year>1985,]

