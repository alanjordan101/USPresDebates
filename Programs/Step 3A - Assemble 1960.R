
setwd(Rfiles)

# Assemble 1960

load("D1960PNewYorkNY.Rdata")
d19601 <- deb
load("D1960PNewYorkandLosAngeles.Rdata")
d19602 <- deb
load("D1960PWashingtonDC.Rdata")
d19603 <- deb
load("D1960PChicagoIL.Rdata")
d19604 <- deb

E1960 <- rbind(d19601, d19602, d19603, d19604)
E1960 <- subset(E1960, person !="")

t(t(table(E1960$person)))

e1960names <- read.csv("E1960Names.csv")

E1960 <- merge(E1960, e1960names, by='person')
E1960$election <-1960

save(E1960, file="E1960.Rdata")