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
	#i=19

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
	deb <- gsub("over the long term. SCHIEFFER: Governor", "over the long term. <p>SCHIEFFER: Governor", deb)
	deb <- gsub("someone else's. OBAMA: The", "someone else's. <p>OBAMA: The", deb)
	deb <- gsub("DAVID GERGEN, CNN SENIOR POLITICAL ANALYST:", "DAVID GERGEN:", deb)
	deb <- gsub("simply not accurate. LEHRER:", "simply not accurate. <p>LEHRER:", deb)
	deb <- gsub("argument against repeal\\? OBAMA:", "argument against repeal\\? <p>OBAMA:", deb)
	deb <- gsub("I'm going to do. CROWLEY:", "I'm going to do. <p>CROWLEY:", deb)
	deb <- gsub("Medicare and Medicaid\\.\\.\\. LEHRER:", "Medicare and Medicaid\\.\\.\\. <p>LEHRER:",  deb)
	deb <- gsub("we have to have. CROWLEY:", "we have to have. <p>CROWLEY:",  deb)
	deb <- gsub("it's for you. QUESTION: Governor", "it's for you. <p>QUESTION: Governor",   deb)
	deb <- gsub("Mexican drug lords. OBAMA:", "Mexican drug lords. <p>OBAMA:",   deb)
	deb <- gsub("JOHN MARCOUX, RETIRED STOCK TRADER:", "MARCOUX:",  deb)
	deb <- gsub("SEN\\. JIM DEMINT \\(R\\), SOUTH CAROLINA:", "JIM DEMINT:", deb)
	deb <- gsub("REP. STEVE KING \\(R\\), IOWA:", "STEVE KING:", deb)
	deb <- gsub("ROBERT GEORGE, PROFESSOR, PRINCETON UNIVERSITY:", "ROBERT GEORGE:", deb)
	deb <- gsub("REP\\. STEVE KING, \\(R\\) IOWA:", "STEVE KING:",  deb)
	deb <- gsub("HERMAN CAIN, \\(R\\) PRESIDENTIAL CANDIDATE:", "HERMAN CAIN:", deb)
	deb <- gsub("PROF\\. ROBERT GEORGE, PRINCETON UNIVERSITY:", "ROBERT GEORGE:", deb)
	deb <- gsub("SEN\\. JIM DEMINT, \\(R\\) SOUTH CAROLINA:", "JIM DEMINT:", deb)
	deb <- gsub("NEWT GINGRICH, \\(R\\) PRESIDENTIAL CANDIDATE:", "NEWT GINGRICH:", deb)
	deb <- gsub("REP\\. RON PAUL \\(R-TX\\), PRESIDENTIAL CANDIDATE:", "RON PAUL:",  deb)
	deb <- gsub("RON PAUL, \\(R\\) PRESIDENTIAL CANDIDATE:", "RON PAUL:",  deb)
	deb <- gsub("MITT ROMNEY, \\(R\\) PRESIDENTIAL CANDIDATE:", "MITT ROMNEY:",  deb)
	deb <- gsub("SENATOR JIM DEMINT \\(R\\), SOUTH CAROLINA:", "JIM DEMINT:",   deb)
	deb <- gsub("austerity question. ROMNEY:",  "austerity question. <p>ROMNEY:",  deb) 
	deb <- gsub("JOHN DISTASOS, SENIOR POLITICAL REPORTER, \"NEW HAMPSHIRE UNION LEADER\":", "JOHN DISTASOS:", deb) 
	deb <- gsub("<p>\\[<i>begin video clip</i>\\]<p><i>RONALD REAGAN, 40TH PRESIDENT OF THE UNITED STATES:</i>", "VIDEO REAGAN", deb) 
	deb <- gsub("<p>\\[<i>end video clip</i>\\]<p><b>ROSE:</b>", "<p>ROSE:", deb)
	deb <- gsub("</p><p>\\[<i>begin video clip</i>\\]</p><p><i>RONALD REAGAN, 40TH PRESIDENT OF THE UNITED STATES:</i>", "<p>VIDEO REAGAN:", deb)
	deb <- gsub("<p>\\[<i>end video clip</i>\\]</p><p><b>ROSE:", "<p>ROSE:", deb)
	deb <- gsub("<p>\\[<i>begin video clip</i>\\]<p><i>GEORGE W\\. BUSH, PRESIDENT OF THE UNITED STATES:</i>", "<p>VIDEO GEORGEBUSH:", deb)

	deb <- gsub("<p>\\[<i>begin video clip</i>\\]</p><p><i>GEORGE W. BUSH, PRESIDENT OF THE UNITED STATES:</i>", "<p>VIDEO GEORGEBUSH:", deb)
	deb <- gsub("<p>\\[<i>end video clip</i>\\] </p><p><b>ROSE:</b> Speaker Gingrich", "<p>ROSE: Speaker Gingrich", deb)


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

#E2012 <- subset(E2012, is.na(name) ==FALSE)
#save(E2012, file="E2012.Rdata")


crap <-subset(E2012, grepl(":", E2012$message))
#crap <-subset(E2012, is.na(name))


crap <-subset(E2012, grepl(":", E2012$message))
crap$pos <- ifelse(  grepl("[A-Z'-]:",crap$message),1,0)

crap0<-subset(crap, pos==0)
crap1<-subset(crap, pos==1)

nrow(crap1)
writeLines(strwrap(   paste(crap1[1,]$debate,crap1[1,]$message)   , width = 120, indent=5))

#writeLines(strwrap(   deb   , width = 120, indent=5))