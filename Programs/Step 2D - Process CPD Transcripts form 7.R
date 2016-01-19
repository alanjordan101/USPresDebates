

################################################################################################################################################################
################################################################################################################################################################


url <- "http://www.debates.org/index.php?page=" 


# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, form =='7' ) # Keep only debate transcripts in forms 6





################################################################################################################################################################
################################################################################################################################################################

#Fix Individual Transcripts
setwd(debTrans)




################################################################################################################################################################
################################################################################################################################################################

setwd(Rfiles)

n<-nrow(deb_list)

for (i in 1:n) {	

	#i <- 1

	pagenum <- deb_list[i,'pagenum']
	debate <- deb_list[i,'debate']


	deb <-read_html(paste0(url, pagenum )) %>% html_nodes('p') 
	deb <- as.character(deb[[2]])


	deb <- gsub("<strong>", "", deb)
	deb <- gsub("</strong>", "", deb)
	deb <-gsub('<br/>','AAAA', deb)


	deb<-strsplit(deb, split='AAAA', fixed=TRUE)
	deb <- data.frame(matrix(unlist(deb), nrow=length(deb[[1]]), byrow=T),stringsAsFactors=FALSE)
	colnames(deb) <- '1'

	deb$delete <- ifelse( deb$'1'=="",1,0)
	deb <- subset(deb, delete==0)
	deb$delete <- NULL

	deb <- ParseDF(deb, sep=':')

	deb$debate <- deb_list[i,'debate']
	deb$year <- deb_list[i,'year']
	deb$month <- deb_list[i,'month']
	deb$day <- deb_list[i,'day']

	save(deb, file=paste0('D',debate,".Rdata"))

	print(debate)
}




