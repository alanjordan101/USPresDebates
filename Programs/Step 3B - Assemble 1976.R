
setwd(Rfiles)

# Assemble 1976

load("D1976PWilliamsburgVA.Rdata")
d19761 <- deb
load("D1976PSanFranciscoCA.Rdata")
d19762 <- deb
load("D1976PPhiladelphiaPN.Rdata")
d19763 <- deb
load("D1976VHoustonTX.Rdata")
d19764 <- deb



E1976 <- rbind(d19761, d19762, d19763, d19764)
E1976 <- subset(E1976, person !="")

t(t(table(E1976$person)))

e1976names <- read.csv("E1976Names.csv")

E1976 <- merge(E1976, e1976names, by='person')
E1976$election <-1976

save(E1976, file="E1976.Rdata")