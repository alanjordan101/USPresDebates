# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('2008') & form %in% c('A', 'B', 'C') ) # Keep only debate transcripts from 2008
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "2008", x = list.files())]
n <- length(a)

E2008 <- NULL
for (i in 1:n){
	#i=4

	debate <- deb_list[i,'debate']

	setwd(debTrans)
	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))


	deb <- gsub("The point is this: We can", "The point is this; We can", deb)
	deb <- gsub("And he asks: Before the", "And he asks; Before the", deb)
	deb <- gsub("And get this: If the Dem", "And get this; If the Dem", deb)
	deb <- gsub("Here's the policy question: if, in", "Here's the policy question; if, in", deb)
	deb <- gsub("The question is this: the", "The question is this; the", deb)
	deb <- gsub("But understand this: We also", "But understand this; We also", deb)
	deb <- gsub("But let's be clear:", "But let's be clear;", deb)



	deb <- gsub("<i>\\(Pittsburgh, Pennsylvania\\)</i>:", ":", deb)
	deb <- gsub("<p>Two-part question:", "Two-part question:", deb)
 	deb <- gsub("Thank you. BROKAW: Senator", "Thank you. <p>BROKAW: Senator", deb)
 	if (debate=="2008POxfordMS") {deb <- gsub("<br/>END<br/>", "<br/>END: Everything after this is shit.<br/>", deb)} # 2008POxfordMS


	deb <- gsub('<b> a\\?\\"</b>', "-", deb)


	deb <- gsub("<b> </b>", " ", deb)
	deb <- gsub("<b>.</b>", ".", deb)
	deb <- gsub("<b>\\.\\..</b>", ".", deb)

setwd(Func)
source("gsublist.R")

#	deb <- gsub("\\(", "", deb)
#	deb <- gsub("\\)", "", deb)
#	deb <- gsub("\\[", "", deb)
#	deb <- gsub("\\]", "", deb)

	deb <- gsub("<br/>", "BAAB", deb)

	deb <-gsub('<i>','BAAB', deb)
	deb <-gsub('<b>','BAAB', deb)
	deb <-gsub('<p>','BAAB', deb)
	deb <-gsub('<P>','BAAB', deb)
	deb <-gsub('<br>','BAAB', deb)
	deb <-gsub('BAABBAAB','BAAB', deb)

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

	if(debate=="2008POxfordMS") {deb <- deb[1:181,]} #**************** 

  	print(paste0("Debate ",a,  nrow(deb), " rows"))

  	E2008 <- rbind(E2008, deb)
  

}




setwd(Rfiles)
t(t(table(E2008$person)))


	E2008$message <- gsub("The point is this; We can", "The point is this: We can", E2008$message)
	E2008$message <- gsub("And he asks; Before the", "And he asks: Before the", E2008$message)
	E2008$message <- gsub("And get this; If the Dem", "And get this: If the Dem", E2008$message)
	E2008$message <- gsub("Here's the policy question; if, in", "Here's the policy question: if, in", E2008$message)
	E2008$message <- gsub("The question is this; the", "The question is this: the", E2008$message)
	E2008$message <- gsub("But understand this; We also", "But understand this: We also", E2008$message)
	E2008$message <- gsub("But let's be clear;", "But let's be clear:", E2008$message)


E2008 <- subset(E2008, person !="Transcription by" &  person !="SPEAKERS" & substr(message, 1,3) !="NA " )
#E2008$person <- gsub("-.", "", E2008$person)
#E2008$person <- gsub("-", "", E2008$person)


e2008names <- read.csv("E2008Names.csv")
E2008 <- merge(E2008, e2008names, by='person', all=TRUE)
E2008 <-arrange(E2008, debate, turn)


#2008POxfordMS


table(E2008$name,useNA ='always')

#E2008 <- subset(E2008, is.na(name) ==FALSE)

#save(E2008, file="E2008.Rdata")


crap <-subset(E2008, grepl("\\(", E2008$message))
#crap <-subset(E2008, is.na(name))
