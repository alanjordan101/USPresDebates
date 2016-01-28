

################################################################################################################################################################
################################################################################################################################################################


url <- "http://www.debates.org/index.php?page=" 


# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, form =='7' ) # Keep only debate transcripts in forms 7





################################################################################################################################################################
################################################################################################################################################################

#Fix Individual Transcripts
setwd(debTrans)




################################################################################################################################################################
################################################################################################################################################################

setwd(Rfiles)

n<-nrow(deb_list)

for (i in 1:n) {	

	#i <- 2

	pagenum <- deb_list[i,'pagenum']
	debate <- deb_list[i,'debate']


	deb <-read_html(paste0(url, pagenum )) %>% html_nodes('p') 
	deb <- as.character(deb[[2]])



	if (debate=="2004VClevelandOH") {
		deb <- gsub("kind of attitude. IFILL: Senator Edwards, 30 seconds", "kind of attitude. <br/>IFILL: Senator Edwards, 30 seconds", deb)
		deb <- gsub("us. IFILL: Mr.", "us. <br/>IFILL: Mr.", deb)
		deb <- gsub("chief. IFILL: Mr.", "chief. <br/>IFILL: Mr.", deb)
		deb <- gsub("stop. IFILL: Well", "stop. <br/>IFILL: Well", deb)
		deb <- gsub("President? CHENEY: Gwen, the story", "President? <br/>CHENEY: Gwen, the story", deb)
		deb <- gsub("CHENEY: Well, Gwen, I can", "<br/>CHENEY: Well, Gwen, I can", deb)
	}

	if (debate=="2004PTempeAZ") {
		deb <- gsub("shadows. SCHIEFFER: Do you want to", "shadows. <br/>SCHIEFFER: Do you want to", deb)
	}



	deb <- gsub("<strong>", "", deb)
	deb <- gsub("</strong>", "", deb)
	deb <-gsub('<br/>','QAAQ', deb)


	deb<-strsplit(deb, split='QAAQ', fixed=TRUE)
	deb <- data.frame(matrix(unlist(deb), nrow=length(deb[[1]]), byrow=T),stringsAsFactors=FALSE)
	colnames(deb) <- '1'

	deb$delete <- ifelse( deb$'1'=="",1,0)
	deb <- subset(deb, delete==0)
	deb$delete <- NULL

	if (debate %in% c("2004PStLouisMO", "2004VClevelandOH", "2004PTempeAZ", "2008PNashvilleTN")) {
		deb <- ParseDF(deb, sep=':', nw=1)
	} else 
	{
	deb <- ParseDF(deb, sep=':')
	}

	deb$debate <- deb_list[i,'debate']
	deb$year <- deb_list[i,'year']
	deb$month <- deb_list[i,'month']
	deb$day <- deb_list[i,'day']
	deb$date <- trunc(ISOdate(deb$year, deb$month, deb$day), "day")
	deb$turn <- 1:nrow(deb)
	deb$person <- trim(deb$person)


	save(deb, file=paste0('D',debate,".Rdata"))

	print(debate)
}




