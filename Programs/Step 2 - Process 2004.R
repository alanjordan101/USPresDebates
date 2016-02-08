# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('2004') & form %in% c('A', 'B', 'C') ) # Keep only debate transcripts from 2004
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "2004", x = list.files())]
n <- length(a)

E2004 <- NULL
for (i in 1:n){
	#i=4

	debate <- deb_list[i,'debate']

	setwd(debTrans)
	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))


	deb <- gsub("So the question is:", "So the question is;", deb)
	deb <- gsub("A reminder:", "A reminder;", deb)
	deb <- gsub("Affirmative action:", "Affirmative action;", deb)
	deb <- gsub("And so we said:", "And so we said;", deb)
	deb <- gsub("And third, credible:", "And third, credible;", deb)
	deb <- gsub("But I repeat again:", "But I repeat again;", deb)
	deb <- gsub("Here's the truth:", "Here's the truth;", deb)
	deb <- gsub("Here's what's happened:", "Here's what's happened;", deb)
	deb <- gsub("Here's what I'll do:", "Here's what I'll do;", deb)
	deb <- gsub("Here's what I believe:", "Here's what I believe", deb)
	deb <- gsub("Here's what I do:", "Here's what I do;", deb)
	deb <- gsub("I repeat:", "I repeat;", deb)
	deb <- gsub("Let's meet the candidates:", "Let's meet the candidates;", deb)
	deb <- gsub("<br/><br/>Now, flip-flops:", "Now, fliplops;", deb)
	deb <- gsub("People need to remember:", "People need to remember;", deb)
	deb <- gsub("Result:", "Result;", deb)
	deb <- gsub("The measurement is not:", "The measurement is not;", deb)
	deb <- gsub("The rules:", "The rules;", deb)
	deb <- gsub("The threats we face:", "The threats we face;", deb)
	deb <- gsub("Think about it:", "Think about it;", deb)

	deb <- gsub("Mr. Vice President? CHENEY:", "Mr. Vice President? <p>CHENEY:", deb)

	deb <- gsub("SEN. JOHN KERRY, \\(D-MA\\) PRESIDENTIAL CANDIDATE:", "KERRY", deb)
	deb <- gsub("HOWARD DEAN, \\(D-VT\\) PRESIDENTIAL CANDIDATE:", "DEAN", deb)
	deb <- gsub("SEN. JOE LIEBERMAN, \\(D-CT\\) PRESIDENTIAL CANDIDATE:", "LIEBERMAN", deb)
	deb <- gsub("GEN. WESELEY CLARK, \\(D\\) PRESIDENTIAL CANDIDATE:", "CLARK", deb)
	deb <- gsub("REP. DENNIS KUCINICH, \\(D-OH\\) PRESIDENTIAL CANDIDATE:", "KUCINICH", deb)
	deb <- gsub("SEN. JOHN EDWARDS, \\(D-NC\\) PRESIDENTIAL CANDIDATE:", "EDWARDS", deb)
	deb <- gsub("REV. AL SHARPTON, \\(D\\) PRESIDENTIAL CANDIDATE:", "SHARPTON", deb)

	deb <- gsub("IFILL: Mr. Vice President\\? CHENEY: Gwen, the story", "IFILL: Mr. Vice President\\? <br/><br/>CHENEY: Gwen, the story", deb)
	deb <- gsub("IFILL: Mr. Vice President\\? CHENEY: Well, Gwen, I can think of a lot of words", "IFILL: Mr. Vice President\\? <br/><br/>CHENEY: Well, Gwen, I can think of a lot of words", deb)

	deb <- gsub("IFILL: Well, I do, because", "<p>IFILL: Well, I do, because", deb)





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

  	E2004 <- rbind(E2004, deb)
  

}




setwd(Rfiles)
t(t(table(E2004$person)))



	E2004$message <- gsub("So the question is;", "So the question is:", E2004$message)
	E2004$message <- gsub("A reminder;", "A reminder:", E2004$message)
	E2004$message <- gsub("Affirmative action;", "Affirmative action:", E2004$message)
	E2004$message <- gsub("And so we said;", "And so we said:", E2004$message)
	E2004$message <- gsub("And third, credible;", "And third, credible:", E2004$message)
	E2004$message <- gsub("But I repeat again;", "But I repeat again:", E2004$message)
	E2004$message <- gsub("Here's the truth;", "Here's the truth:", E2004$message)
	E2004$message <- gsub("Here's what's happened;", "Here's what's happened:", E2004$message)
	E2004$message <- gsub("Here's what I'll do;", "Here's what I'll do:", E2004$message)
	E2004$message <- gsub("Here's what I believe;", "Here's what I believe:", E2004$message)
	E2004$message <- gsub("Here's what I do;", "Here's what I do:", E2004$message)
	E2004$message <- gsub("I repeat;", "I repeat:", E2004$message)
	E2004$message <- gsub("Let's meet the candidates;", "Let's meet the candidates:", E2004$message)
	E2004$message <- gsub("Now, fliplops;", "Now, fliplops:", E2004$message)
	E2004$message <- gsub("People need to remember;", "People need to remember:", E2004$message)
	E2004$message <- gsub("Result;", "Result:", E2004$message)
	E2004$message <- gsub("The measurement is not;", "The measurement is not:", E2004$message)
	E2004$message <- gsub("The rules;", "The rules:", E2004$message)
	E2004$message <- gsub("The threats we face;", "The threats we face:", E2004$message)
	E2004$message <- gsub("Think about it;", "Think about it:", E2004$message)



E2004 <- subset(E2004, person !="Note" &  person !="SPEAKERS" & substr(message, 1,3) !="NA " )
E2004$person <- gsub("-.", "", E2004$person)
E2004$person <- gsub("-", "", E2004$person)


e2004names <- read.csv("E2004Names.csv")
E2004 <- merge(E2004, e2004names, by='person', all=TRUE)
E2004 <-arrange(E2004, debate, turn)





table(E2004$name,useNA ='always')

#E2004 <- subset(E2004, is.na(name) ==FALSE)

save(E2004, file="E2004.Rdata")


crap <-subset(E2004, grepl(":", E2004$message))
#crap <-subset(E2004, is.na(name))
