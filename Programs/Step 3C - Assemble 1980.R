
setwd(Rfiles)

# Assemble 1980


load("D1980PClevelandOH.Rdata")
d19801 <- deb
load("D1980PBaltimoreMD.Rdata")
d19802 <- deb





E1980 <- rbind(d19801, d19802)
E1980 <- subset(E1980, person !="")

t(t(table(E1980$person)))

e1980names <- read.csv("E1980Names.csv")

E1980 <- merge(E1980, e1980names, by='person')
E1980$election <-1980

save(E1980, file="E1980.Rdata")