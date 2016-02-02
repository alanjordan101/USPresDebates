# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('1984') ) # Keep only debate transcripts from 1984
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "1984", x = list.files())]
n <- length(a)

E1984 <- NULL
for (i in 1:n){
	#i=1

	debate <- deb_list[i,'debate']

	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))

	deb <- gsub("<p><br/>The Nation's Economy</p>", "", deb)
	deb <- gsub("One final point: President Reagan", "One final point; President Reagan", deb)
	deb <- gsub("<p><br/>Leadership Qualities</p>", "", deb)
	deb <- gsub("<p><br/>Religion</p>", "", deb)
	deb <- gsub("<p><br/>Political Issues</p>", "", deb)
	deb <- gsub("<p><br/>Abortion</p>", "", deb)	
	deb <- gsub("<p><br/>Federal Taxation</p>", "", deb)
	deb <- gsub("<p><br/>Social Welfare Programs</p>", "", deb)
	deb <- gsub("<p><br/>Presidential Campaign</p>", "", deb)
	deb <- gsub("<p><br/>Closing Statements</p>", "", deb)

	deb <- gsub("<p>Ed.</p>", "", deb)
	deb <- gsub("<p><br/>Central America</p>", "", deb)
	deb <- gsub("<p><br/>Soviet Union</p>", "", deb)
	deb <- gsub("<p><br/>Regions Vital to U.S. Interests</p>", "", deb)
	deb <- gsub("<p><br/>Eastern Europe</p>", "", deb)
	deb <- gsub("<p><br/>Use of Military Force</p>", "", deb)
	deb <- gsub("<p><br/>Nicaragua</p>", "", deb)
	deb <- gsub("<p><br/>Lebanon</p>", "", deb)
	deb <- gsub("<p><br/>The President's Age</p>", "", deb)
	deb <- gsub("<p><br/>Strategic Missiles</p>", "", deb)
	deb <- gsub("<p><br/>The President's Leadership</p>", "", deb)
	deb <- gsub("<p><br/>Illegal Immigration</p>", "", deb)
	deb <- gsub("<p><br/>Armageddon</p>", "", deb)
	deb <- gsub("<p><br/>Strategic Defense Initiative</p>", "", deb)
	deb <- gsub("<p><br/>Nuclear Freeze</p>", "", deb)
	deb <- gsub("<p><br/>Strategic Weapons</p>", "", deb)
	deb <- gsub("<p><br/>Support for U.S. Allies</p>", "", deb)
	deb <- gsub("<p><br/>Nuclear Weapons</p>", "", deb)
	deb <- gsub("<p><br/>Closing Statements</p>", "", deb)

	#deb <- gsub("\\(FOOTNOTE\\)", "", deb)
	deb <- gsub("\\\\1\\\\ \\(FOOTNOTE\\)", "", deb)
	deb <- gsub("<p>\\(FOOTNOTE\\) \\\\1\\\\Mr. Mondale was referring to an earlier debate between George Bush and Geraldine Ferarro, the Vice-Presidential candidates.</p>", "", deb)

	deb<- gsub("Similarly, in Central America: What we're doing in Nicaragua", "Similarly, in Central America; What we're doing in Nicaragua", deb)

	deb<- gsub("They delivered 21 A,AR percent", "They delivered twenty-one and a half percent", deb)
	#"Â®" - vp debate


	deb <- gsub('<b> a\\?\\"</b>', "-", deb)
	deb <- gsub("<b> </b>", " ", deb)
	deb <- gsub("<b>.</b>", ".", deb)
	deb <- gsub("<b>...</b>", ".", deb)
	deb <- gsub("\\[<i>applause</i><b>\\] </b>", "", deb)
	deb <- gsub("\\[<i>applause</i>\\]", "", deb)
	deb <- gsub("\\[<i>sic</i>\\]", "", deb)
	deb <- gsub("\\[<i>crosstalk</i>\\]", "", deb)
	deb <- gsub("\\[<i>laughter</i>\\]", "", deb)
	deb <- gsub("\\[<i>laughter and applause</i>\\]", "", deb)
	deb <- gsub("\\[<i>commercial break</i>\\]", "", deb)
	deb <- gsub("(COMMERCIAL BREAK)", "", deb)
	deb <- gsub("(APPLAUSE)", "", deb)
	deb <- gsub("(inaudible)", "", deb)
	deb <- gsub("\\(Laughter\\)", "", deb)
	deb <- gsub("(sic)", "", deb)
	deb <- gsub("\\n", "", deb)
	deb <- gsub("<br>", "", deb)
	deb <- gsub("<br/>", "", deb)
	deb <- gsub("<BR>", "", deb)
	deb <- gsub("<BR/>", "", deb)
	deb <- gsub('\"',"", deb)
	deb <- gsub('<strong>',"", deb)
	deb <- gsub('</strong>',"", deb)
	deb <- gsub('<h1>',"", deb)
	deb <- gsub('</h1>',"", deb) 
	deb <- gsub('</p>',"", deb) 
	deb <- gsub('<div id=content-sm>',"", deb) 
	deb <- gsub("\\(chuckle\\)", "", deb)
 	deb <- gsub('\\(barely audible\\)',"", deb) 
 	deb <- gsub('\\(laughter from audience\\)',"", deb) 
	deb <- gsub("\\[Laughter\\]", "", deb)
	deb <- gsub("\\[applause\\]", "", deb)
	deb <- gsub("\\[Laughter and applause\\]", "", deb)
	deb <- gsub("\\[Applause\\]", "", deb)
	deb <- gsub("\\[\\]", "", deb)
	deb <- gsub("\\[laughter\\]", "", deb)



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

  	print(paste0("Debate ",a,  nrow(deb), " rows"))

  	E1984 <- rbind(E1984, deb)
  

}

setwd(Rfiles)
t(t(table(E1984$person)))


E1984 <- subset(E1984, person !="Note")

e1984names <- read.csv("E1984Names.csv")
E1984 <- merge(E1984, e1984names, by='person', all=TRUE)
E1984 <-arrange(E1984, debate, turn)


E1984$message <- gsub("One final point; President Reagan", "One final point: President Reagan", E1984$message)
E1984$message <- gsub("Similarly, in Central America; What we're doing in Nicaragua", "Similarly, in Central America: What we're doing in Nicaragua", E1984$message)


table(E1984$name,useNA ='always')

save(E1984, file="E1984.Rdata")



