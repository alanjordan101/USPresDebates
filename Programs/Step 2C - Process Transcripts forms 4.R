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
deb_list <- subset(deb_list, form %in% c('4') ) # Keep only debate transcripts in forms 4




################################################################################################################################################################
################################################################################################################################################################

#Fix Individual Transcripts
setwd(debTrans)




################################################################################################################################################################
################################################################################################################################################################



n<-nrow(deb_list)

for (i in 1:n) {	

	#i <- 1
	pagenum <- 76561
	#pagenum <- deb_list[i,'pagenum']

	setwd(debTrans)
	deb <- read.table(paste0(pagenum,".txt"), stringsAsFactors=FALSE )

	deb$x <- gsub("&mdash;", "-", deb$x)
	deb$x <- gsub("<i>", "", deb$x)
	deb$x <- gsub("</i>", "", deb$x)
	deb$x <- gsub("<b>", "", deb$x)
	deb$x <- gsub("</b>", "", deb$x)
	deb$x <- gsub("</p>", "", deb$x)
	deb$x <- gsub("<br/>", "", deb$x)
	deb$x <- gsub("<br>", " ", deb$x) # Space not blank
	deb$x <- gsub("PARTICIPANTS:", "<p>PARTICIPANTS:", deb$x)
	deb$x <- gsub("PARTICIPANTS:", "PARTICIPANTS: ", deb$x)
	deb$x <- gsub("MODERATORS:", "MODERATORS: ", deb$x)
	#deb$x <- gsub("\\(.*)", "", deb$x)  # Remove (any parenthesese and all their contents)
	#deb$x <- gsub("\\[.*]", "", deb$x)  # Remove [any brackets and all their contents]
 


	deb$x <- gsub("}", "", deb$x)  # Remove (any parenthesese and all their contents)
	deb$x <- gsub("\\{", "", deb$x)  # Remove (any parenthesese and all their contents)

	deb$x <-gsub("<p>",'AAAA', deb$x)

	deb<-strsplit(deb$x, split='AAAA', fixed=TRUE)
	deb <- data.frame(matrix(unlist(deb), nrow=length(deb[[1]]), byrow=T),stringsAsFactors=FALSE)
	colnames(deb) <- '1'
	deb$delete <- ifelse( deb$'1'=="",1,0)
	deb <- subset(deb, delete==0)
	deb$delete <- NULL

	deb <- ParseDF(deb, nw=3, sep=":")

	deb$person <- gsub(":", "", deb$person)
	deb$person <- gsub("\\.", "", deb$person)
	deb$delete <-0
	deb$delete[deb$person %in% c("", "PARTICIPANTS", "MODERATORS")] <-1
	deb$delete[deb$message == ""] <-1
	deb <- subset(deb, delete==0)
	deb$delete <- NULL
	deb$message <- gsub("\\(.*)", "", deb$message)  # Remove (any parenthesese and all their contents)
	deb$message <- gsub("\\[.*]", "", deb$message)  # Remove [any brackets and all their contents]

	setwd(Rfiles)
	save(deb, file=paste0('D',pagenum,".Rdata"))

	print(deb_list[i,'debate'])
	#print(i)
}




