# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('1996') ) # Keep only debate transcripts from 1996
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "1996", x = list.files())]
n <- length(a)

E1996 <- NULL
for (i in 1:n){
	#i=1

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
	deb$person <- trim(deb$person)

  	print(paste0("Debate ",a,  nrow(deb), " rows"))

  	E1996 <- rbind(E1996, deb)
  

}

setwd(Rfiles)
t(t(table(E1996$person)))


E1996 <- subset(E1996, person !="Note")

e1996names <- read.csv("E1996Names.csv")
E1996 <- merge(E1996, e1996names, by='person', all=TRUE)
E1996 <-arrange(E1996, debate, turn)





table(E1996$name,useNA ='always')

E1996 <- subset(E1996, is.na(name) ==FALSE)

save(E1996, file="E1996.Rdata")


crap <-subset(E1996, grepl("\\[", E1996$message))
