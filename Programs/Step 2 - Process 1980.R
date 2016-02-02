# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('1980') ) # Keep only debate transcripts from 1980
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "1980", x = list.files())]
n <- length(a)

E1980 <- NULL
for (i in 1:n){
	#i=1

	debate <- deb_list[i,'debate']

	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))


	deb <- gsub("RUTH HINERFELD, LEAGUE OF WOMEN VOTERS, EDUCATION FUND:", "RUTH HINERFELD:", deb)
	deb <- gsub("RUTH J. HINERFELD, CHAIR, LEAGUE OF WOMEN VOTERS EDUCATION FUND:", "RUTH HINERFELD:", deb)
	deb <- gsub("MARVIN STONE, U.S. NEWS AND WORLD REPORT:", "MARVIN STONE:", deb)
	deb <- gsub("MR. ELLIS, CHRISTIAN SCIENCE MONITOR:", "MR. ELLIS:", deb)
	deb <- gsub("MR. MOYERS, HOST AND EXECUTIVE EDITOR, \"BILL MOYERS' JOURNAL,\" PUBLIC BROADCASTING SYSTEM:", "MR. MOYERS:", deb)
	deb <- gsub("CAROL LOOMIS, BOARD OF EDITORS, FORTUNE MAGAZINE:", "CAROL LOOMIS:", deb)
	deb <- gsub("CHARLES CORDDRY, MILITARY CORRESPONDENT, THE SUN, BALTIMORE:", "CHARLES CORDDRY:", deb)
	deb <- gsub("LEE MAY, STAFF WRITER, THE LOS ANGELES TIMES - WASHINGTON BUREAU:", "LEE MAY:", deb)
	deb <- gsub("JANE BRYANT QUINN, CBS NEWS/NEWSWEEK/WASHINGTON POST:", "JANE BRYANT:", deb)
	deb <- gsub("GOLDEN, EDITORIAL WRITER, THE NEW YORK TIMES:", "GOLDEN:", deb)
	deb <- gsub("or prevent the burning of it:", "or prevent the burning of it", deb)

	deb <- gsub("<span class=\"displaytext\">", "", deb)
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
	deb <- gsub("\\(sic\\)", "", deb)
	deb <- gsub("\\[sic\\]", "", deb)
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
	deb <- gsub("\\[audience laughter\\]", "", deb)
	deb <- gsub("\\[cough\\]", "", deb)



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

  	E1980 <- rbind(E1980, deb)
  

}

setwd(Rfiles)
t(t(table(E1980$person)))

e1980names <- read.csv("E1980Names.csv")
E1980 <- merge(E1980, e1980names, by='person', all=TRUE)
E1980 <-arrange(E1980, debate, turn)

table(E1980$name,useNA ='always')

save(E1980, file="E1980.Rdata")



