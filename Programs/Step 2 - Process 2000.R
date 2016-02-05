# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('2000') & form %in% c('A', 'B', 'C') ) # Keep only debate transcripts from 2000
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "2000", x = list.files())]
n <- length(a)

E2000 <- NULL
for (i in 1:n){
	#i=6

	debate <- deb_list[i,'debate']

	setwd(debTrans)
	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))






	deb <- gsub('<b> a\\?\\"</b>', "-", deb)


	deb <- gsub("<b> </b>", " ", deb)
	deb <- gsub("<b>.</b>", ".", deb)
	deb <- gsub("<b>...</b>", ".", deb)

setwd(Func)
source("gsublist.R")



	deb <- gsub("<br/>", "BAAB", deb)

	deb <-gsub('<i>','BAAB', deb)
	deb <-gsub('<b>','BAAB', deb)
	deb <-gsub('<p>','BAAB', deb)

	deb<-strsplit(deb, split='BAAB', fixed=TRUE)
	deb <- data.frame(matrix(unlist(deb), nrow=length(deb[[1]]), byrow=T),stringsAsFactors=FALSE)
	colnames(deb) <- '1'
	deb$delete <- ifelse( deb$'1'=="",1,0)
	deb <- subset(deb, delete==0)
	deb$delete <- NULL

	deb <- ParseDF(deb, nw=4 ) 

	deb$person <- gsub(":", "", deb$person)
	deb$message <- gsub("<br/>",'', deb$message)
	deb$message <- gsub("</p>",'', deb$message)
	deb$delete <- ifelse(deb$person %in% c("", "PARTICIPANTS", "MODERATORS"), 1,0)
	deb <- subset(deb, delete==0)
	deb$delete <- NULL
	deb$debate <- deb_list[i,'debate']
	deb$year <- deb_list[i,'year']
	deb$month <- deb_list[i,'month']
	deb$day <- deb_list[i,'day']
	deb$date <- as.POSIXct(trunc(ISOdate(deb$year, deb$month, deb$day), "day"))
	deb$turn <- 1:nrow(deb)

	deb$person
	deb$person <- trim(deb$person)

  	print(paste0("Debate ",a,  nrow(deb), " rows"))

  	E2000 <- rbind(E2000, deb)
  

}




setwd(Rfiles)
t(t(table(E2000$person)))


E2000 <- subset(E2000, person !="Note")

e2000names <- read.csv("E2000Names.csv")
E2000 <- merge(E2000, e2000names, by='person', all=TRUE)
E2000 <-arrange(E2000, debate, turn)





table(E2000$name,useNA ='always')

#E2000 <- subset(E2000, is.na(name) ==FALSE)

#save(E2000, file="E2000.Rdata")


#crap <-subset(E2000, grepl("\\[", E2000$message))
crap <-subset(E2000, is.na(name))
