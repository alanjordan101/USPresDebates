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
	#i=5

	debate <- deb_list[i,'debate']

	setwd(debTrans)
	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))


	deb <- gsub("And I asked him:", "And I asked him;",  deb)
	deb <- gsub("And one personal note:", "And one personal note;",  deb) 
	deb <- gsub("And that is:",  "And that is;",   deb)       
	deb <- gsub("Ask yourself this question:",   "Ask yourself this question;",   deb)
	deb <- gsub("From CNN.com:", "From CNN.com;",   deb)   
	deb <- gsub("He said:",  "He said;",   deb)  
	deb <- gsub("He says:",   "He says;",   deb) 
	deb <- gsub("I said:",  "I said;",  deb)   
	deb <- gsub("It's true:", "It's true;",   deb)  
	deb <- gsub("My question:", "My question;",  deb)
	deb <- gsub("My question is this:", "My question is this;",   deb)
	deb <- gsub("Now to the candidates:",  "Now to the candidates;",  deb)
	deb <- gsub("Videotape, December 16, 1999]:", "Videotape, December 16, 1999];",   deb) 
	deb <- gsub("SEN. JOHN MCCAIN \\(R-AZ\\), PRESIDENTIAL CANDIDATE:", "JOHN MCCAIN:", deb)
	deb <- gsub("SEN. ORRIN HATCH \\(R-UT\\), PRESIDENTIAL CANDIDATE:", "ORRIN HATCH", deb)
	deb <- gsub("GOV. GEORGE W. BUSH \\(R-TX\\), PRESIDENTIAL CANDIDATE:", "GEORGE W. BUSH", deb)
	deb <- gsub("<p><b>MODERATOR:</b><br/>Dennis Ryerson, Editor, The Des Moines Register</p>", "", deb)
	deb <- gsub("<span class=\"displaytext\"><b>PARTICIPANTS:</b><br/>Former Senator Bill Bradley \\(NJ\\), and;<br/>Vice President Al Gore<p><b>MODERATOR:</b><br/>Bernard Shaw, CNN</p><p><b>PANELISTS:</b><br/>  Ron Brownstein, Los Angeles Times; and<br/>Jeff Greenfield, CNN</p>",
		"", deb)
	deb <- gsub("\\[<i>off-mike</i>\\]", "|", deb)


	deb <- gsub('<b> a\\?\\"</b>', "-", deb)


	deb <- gsub("<b> </b>", " ", deb)
	deb <- gsub("<b>.</b>", ".", deb)
	deb <- gsub("<b>\\.\\..</b>", ".", deb)

setwd(Func)
source("gsublist.R")

	deb <- gsub("\\(", "", deb)
	deb <- gsub("\\)", "", deb)
	deb <- gsub("\\[", "", deb)
	deb <- gsub("\\]", "", deb)

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

  	E2000 <- rbind(E2000, deb)
  

}


	deb <- gsub("And I asked him;", "And I asked him:",  E2000$message)
	deb <- gsub("And one personal note;", "And one personal note:",  E2000$message) 
	deb <- gsub("And that is;",  "And that is:",   E2000$message)       
	deb <- gsub("Ask yourself this question;",   "Ask yourself this question:",   E2000$message)
	deb <- gsub("From CNN.com;", "From CNN.com:",   E2000$message)   
	deb <- gsub("He said;",  "He said:",   E2000$message)  
	deb <- gsub("He says;",   "He says:",   E2000$message) 
	deb <- gsub("I said;",  "I said:",  E2000$message)   
	deb <- gsub("It's true;", "It's true:",   E2000$message)  
	deb <- gsub("My question;", "My question:",  E2000$message)
	deb <- gsub("My question is this;", "My question is this:",   E2000$message)
	deb <- gsub("Now to the candidates;",  "Now to the candidates:",  E2000$message)
	deb <- gsub("Videotape, December 16, 1999];", "Videotape, December 16, 1999]:",   E2000$message) 

setwd(Rfiles)
t(t(table(E2000$person)))


E2000 <- subset(E2000, person !="Note" & person !="Videotape, December 16, 1999" & substr(message,1,3) !="NA " )
E2000$person <- gsub("-.", "", E2000$person)
E2000$person <- gsub("-", "", E2000$person)


e2000names <- read.csv("E2000Names.csv")
E2000 <- merge(E2000, e2000names, by='person', all=TRUE)
E2000 <-arrange(E2000, debate, turn)





table(E2000$name,useNA ='always')

#E2000 <- subset(E2000, is.na(name) ==FALSE)

save(E2000, file="E2000.Rdata")


crap <-subset(E2000, grepl(":", E2000$message))
#crap <-subset(E2000, is.na(name))
