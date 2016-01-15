library(plyr)
library(dplyr)
library(qdap)




################################################################################################################################################################
################################################################################################################################################################

# Set your directory here:

base<-file.path("C:\\Users\\alan\\Documents\\GitHub\\USPresDebates") # Set to your directory
setwd(base)
debTrans <- file.path(base,"Debate Transcripts") 
Func <- file.path(base,"Programs") 
Rfiles <- file.path(base,"R Data Files") 

################################################################################################################################################################
################################################################################################################################################################



setwd(Func)
source("Functions_USPresDebates.R")




# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character", "integer" ), "DebateList.csv")
deb_list <- subset(deb_list, form %in% c('3') ) # Keep only debate transcripts in forms 3




################################################################################################################################################################
################################################################################################################################################################

#Fix Individual Transcripts
setwd(debTrans)




################################################################################################################################################################
################################################################################################################################################################



n<-nrow(deb_list)

for (i in 1:n) {	

	#i <- 1
	pagenum <- deb_list[i,'pagenum']

	setwd(debTrans)
	deb <- read.table(paste0(pagenum,".txt"), stringsAsFactors=FALSE )


	if (pagenum='102344') {
					deb$x <- gsub("<p><i>Situation in the Middle East and North Africa/Al Qaida Terrorist Organization </i>", "", deb$x)
					deb$x <- gsub("<p><i>Syria </i>", "", deb$x)
				}

	if (pagenum='102317') {
					deb$x <- gsub("<p><i>National Economy </i>", "", deb$x)
				}	

	if (pagenum='102343') {
					deb$x <- gsub("<p><i>Education/College Affordability/Job Creation and Growth </i>", "", deb$x)
				}

	if (pagenum='52060') {
					deb$x <- gsub("<i>The President.</i> <p>You bet. <i>Senator Dole. </i>", "<i>The President.</i> You bet. <p><i>Senator Dole. </i>", deb$x)
				}

	#if (pagenum='52060') {}

	if (pagenum='63163') {
					deb$x <- gsub("<i>Homeland Security </i>", "", deb$x)
				}
	if (pagenum='72770') {
					deb$x <- gsub("<i>Preventing Future Terrorist Attacks </i>", "", deb$x)
				}

	#if (pagenum='72776') {}




	deb$x <- gsub("&mdash;", "-", deb$x)
	deb$x <- gsub("\\[", "", deb$x)  # Remove (any parenthesese and all their contents)
	deb$x <- gsub("\\]", "", deb$x)  # Remove (any parenthesese and all their contents)
	deb$x <- gsub("<i>applause</i>", "", deb$x)
	deb$x <- gsub("<i>sic</i>", "", deb$x)
	deb$x <- gsub("<i>crosstalk</i>", "", deb$x)
	deb$x <- gsub("<i>laughter</i>", "", deb$x)
	deb$x <- gsub("<i>laughter and applause</i>", "", deb$x)
	deb$x <- gsub("<i>commercial break</i>", "", deb$x)
	deb$x <- gsub("</span>", "", deb$x)


	deb$x<-gsub("}", "", deb$x)  # Remove (any parenthesese and all their contents)
	deb$x<-gsub("\\{", "", deb$x)  # Remove (any parenthesese and all their contents)


	deb$x <- gsub("(COMMERCIAL BREAK)", "", deb$x)
	deb$x <- gsub("(APPLAUSE)", "", deb$x)
	deb$x <- gsub("(inaudible)", "", deb$x)
	deb$x <- gsub("(sic)", "", deb$x)

	deb$x <-gsub("</i>",' @ ', deb$x)

	deb$x <-gsub("<i>",'AAAA', deb$x)
	deb$x <-gsub("<b>",'AAAA', deb$x)
	deb$x <-gsub("<p>",'AAAA', deb$x)

	deb<-strsplit(deb$x, split='AAAA', fixed=TRUE)
	deb <- data.frame(matrix(unlist(deb), nrow=length(deb[[1]]), byrow=T),stringsAsFactors=FALSE)
	colnames(deb) <- '1'
	deb$delete <- ifelse( deb$'1'=="",1,0)
	deb <- subset(deb, delete==0)
	deb$delete <- NULL

	deb <- ParseDF(deb, nw=15)

	deb$person <- gsub(":", "", deb$person)
	deb$person <- gsub("\\.", "", deb$person)
	deb$message <- gsub("<br/>",'', deb$message)
	deb$message <- gsub("</p>",'', deb$message)
	deb$delete <-0
	deb$delete[deb$person %in% c("", "PARTICIPANTS", "MODERATORS")] <-1
	deb$delete[deb$message == ""] <-1
	deb <- subset(deb, delete==0)
	deb$delete <- NULL

	setwd(Rfiles)
	save(deb, file=paste0('D',pagenum,".Rdata"))

	print(deb_list[i,'debate'])
	#print(i)
}




