# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('1988') ) # Keep only debate transcripts from 1988
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "1988", x = list.files())]
n <- length(a)

E1988 <- NULL
for (i in 1:n){
	#i=1

	debate <- deb_list[i,'debate']

	setwd(debTrans)
	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))


	deb <- gsub("<br/>BUSH: What I think we ought to", "<p>BUSH: What I think we ought to", deb)
	deb <- gsub("BROKA W: Senator Quayle, all of us in our",  "<p>BROKAW: Senator Quayle, all of us in our", deb)


	deb <- gsub('<b> a\\?\\"</b>', "-", deb)

	deb <- gsub("<br/>", "", deb)
	deb <- gsub("<b> </b>", " ", deb)
	deb <- gsub("<b>.</b>", ".", deb)
	deb <- gsub("<b>...</b>", ".", deb)

setwd(Func)
source("gsublist.R")



	#deb <-gsub("</b>",' @ ', deb)

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
	deb$video <-0

  	print(paste0("Debate ",a,  nrow(deb), " rows"))

  	E1988 <- rbind(E1988, deb)
  

}

setwd(Rfiles)
t(t(table(E1988$person)))


E1988 <- subset(E1988, person !="Note")

e1988names <- read.csv("E1988Names.csv")
E1988 <- merge(E1988, e1988names, by='person', all=TRUE)
E1988 <-arrange(E1988, debate, turn)


E1988$message <- gsub("One final point; President Reagan", "One final point: President Reagan", E1988$message)
E1988$message <- gsub("Similarly, in Central America; What we're doing in Nicaragua", "Similarly, in Central America: What we're doing in Nicaragua", E1988$message)


table(E1988$name,useNA ='always')

save(E1988, file="E1988.Rdata")


crap <-subset(E1988, grepl("\\(", E1988$message))

crap <-subset(E1988, grepl(":", E1988$message))
crap$pos <- ifelse(  grepl("[A-Z'-]:",crap$message),1,0)

crap0<-subset(crap, pos==0)
crap1<-subset(crap, pos==1)

nrow(crap1)
writeLines(strwrap(   paste(crap1[1,]$debate,crap1[1,]$message)   , width = 120, indent=5))

#writeLines(strwrap(   deb   , width = 120, indent=5))

table(E1988$debate, E1988$turn)