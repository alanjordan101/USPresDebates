

################################################################################################################################################################
################################################################################################################################################################



# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, form =='4')  # Keep only debate transcripts in forms 4


# url for all debates
url <- "http://www.presidency.ucsb.edu/ws/index.php?pid="

################################################################################################################################################################
################################################################################################################################################################




n<-nrow(deb_list)

for (i in 1:n) {	

	#i <- 2
	pagenum <- deb_list[i,'pagenum']
	debate <- deb_list[i,'debate']

	deb <-read_html(paste0(url, pagenum )) %>% html_nodes('.displaytext') %>% as.character()

	deb <- gsub("&mdash;", "-", deb)
	deb <- gsub("<i>", "", deb)
	deb <- gsub("</i>", "", deb)
	deb <- gsub("<b>", "", deb)
	deb <- gsub("</b>", "", deb)
	deb <- gsub("</p>", "", deb)
	deb <- gsub("<br/>", "", deb)
	deb <- gsub("<br>", " ", deb) # Space not blank
	deb <- gsub("PARTICIPANTS:", "<p>PARTICIPANTS:", deb)
	deb <- gsub("PARTICIPANTS:", "PARTICIPANTS: ", deb)
	deb <- gsub("MODERATORS:", "MODERATORS: ", deb)
	#deb <- gsub("\\(.*)", "", deb)  # Remove (any parenthesese and all their contents)
	#deb <- gsub("\\[.*]", "", deb)  # Remove [any brackets and all their contents]
 


	deb <- gsub("}", "", deb)  # Remove (any parenthesese and all their contents)
	deb <- gsub("\\{", "", deb)  # Remove (any parenthesese and all their contents)

	deb <-gsub("<p>",'AAAA', deb)

	deb<-strsplit(deb, split='AAAA', fixed=TRUE)
	deb <- data.frame(matrix(unlist(deb), nrow=length(deb[[1]]), byrow=T),stringsAsFactors=FALSE)
	colnames(deb) <- '1'
	deb$delete <- ifelse( deb$'1'=="",1,0)
	deb <- subset(deb, delete==0)
	deb$delete <- NULL

	if (debate %in% c("2008RMiamiFL")) {
		deb <- ParseDF(deb, nw=1, sep=":") 
	} else
	{deb <- ParseDF(deb, nw=3, sep=":")
	}

	deb$person <- gsub(":", "", deb$person)
	deb$person <- gsub("\\.", "", deb$person)
	deb$delete <-0
	deb$delete[deb$person %in% c("", "PARTICIPANTS", "MODERATORS")] <-1
	deb$delete[deb$message == ""] <-1
	deb <- subset(deb, delete==0)
	deb$delete <- NULL
	deb$message <- gsub("\\(.*)", "", deb$message)  # Remove (any parenthesese and all their contents)
	deb$message <- gsub("\\[.*]", "", deb$message)  # Remove [any brackets and all their contents]
	deb$debate <- deb_list[i,'debate']
	deb$year <- deb_list[i,'year']
	deb$month <- deb_list[i,'month']
	deb$day <- deb_list[i,'day']
	deb$date <- trunc(ISOdate(deb$year, deb$month, deb$day), "day")
	deb$turn <- 1:nrow(deb)
	deb$person <- trim(deb$person)

	setwd(Rfiles)
	save(deb, file=paste0('D',debate,".Rdata"))

	print(deb_list[i,'debate'])
	#print(i)
}




