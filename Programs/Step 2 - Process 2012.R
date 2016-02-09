# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('2012') & form %in% c('A', 'B', 'C') ) # Keep only debate transcripts from 2012
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "2012", x = list.files())]
n <- length(a)

E2012 <- NULL
for (i in 1:n){
	#i=4

	debate <- deb_list[i,'debate']

	setwd(debTrans)
	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))


	#deb <- gsub("Other programs we like:", "Other programs we like;", deb)
	#deb <- gsub("This was the question:",  "This was the question;", deb)


	deb <- gsub("Go ahead. OBAMA", "Go ahead. <p>OBAMA",  deb)
	deb <- gsub("TERRY PFAFF, FORMER NEW HAMPSHIRE STATE SENATE CANDIDATE: Yes, ma'am.", "TERRY PFAFF:", deb)
	deb <- gsub("<p><b>ROMNEY & GINGRICH:</b> Yes.", "<p>ROMNEY: Yes. <p>GINGRICH: Yes.", deb)
	deb <- gsub("<p><b>ROMNEY &amp; GINGRICH:</b> Yes.", "<p>ROMNEY: Yes. <p>GINGRICH: Yes.", deb)
	deb <- gsub("REP. MICHELE BACHMANN \\(R-MN\\), PRESIDENTIAL CANDIDATE:", "<p>MICHELE BACHMANN:", deb)
	deb <- gsub("SEN. JIM DEMINT , SOUTH CAROLINA:",  "<p>JIM DEMINT:", deb)
	deb <- gsub("REP. RON PAUL \\(R-TX\\), PRESIDENTIAL CANDIDATE:",  "<p>RON PAUL:", deb)


	deb <- gsub("Here's the question:", "Here's the question;",  deb)
	deb <- gsub("My question tonight is:", "My question tonight is;",  deb)
	deb <- gsub("Other programs we like:", "Other programs we like;",  deb)
	deb <- gsub("The choice is clear:", "The choice is clear;",  deb)
	deb <- gsub("The role of government:", "The role of government;",  deb)
	deb <- gsub("This was the question:", "This was the question;",  deb)
	deb <- gsub("What they said was:", "What they said was;",  deb)



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


  	print(paste0("Debate ",a,  nrow(deb), " rows"))

  	E2012 <- rbind(E2012, deb)
  

}


	E2012$message <- gsub("Here's the question;", "Here's the question:",  E2012$message)
	E2012$message <- gsub("My question tonight is;", "My question tonight is:",  E2012$message)
	E2012$message <- gsub("Other programs we like;", "Other programs we like:",  E2012$message)
	E2012$message <- gsub("The choice is clear;", "The choice is clear:",  E2012$message)
	E2012$message <- gsub("The role of government;", "The role of government:",  E2012$message)
	E2012$message <- gsub("This was the question;", "This was the question:",  E2012$message)
	E2012$message <- gsub("What they said was;", "What they said was:",  E2012$message)

setwd(Rfiles)
t(t(table(E2012$person)))

	#E2012$message <- gsub("Other programs we like;", "Other programs we like:", E2012$message)
	#E2012$message <- gsub("This was the question;",  "This was the question:", E2012$message)
 





E2012 <- subset(E2012, person !="Transcription by" &  person !="SPEAKERS" & substr(message, 1,3) !="NA " )
#E2012$person <- gsub("-.", "", E2012$person)
#E2012$person <- gsub("-", "", E2012$person)


e2012names <- read.csv("E2012Names.csv")
E2012 <- merge(E2012, e2012names, by='person', all=TRUE)
E2012 <-arrange(E2012, debate, turn)


#2012POxfordMS


table(E2012$name,useNA ='always')

E2012 <- subset(E2012, is.na(name) ==FALSE)

#save(E2012, file="E2012.Rdata")


crap <-subset(E2012, grepl(":", E2012$message))
#crap <-subset(E2012, is.na(name))
