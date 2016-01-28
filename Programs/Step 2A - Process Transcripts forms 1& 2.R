

################################################################################################################################################################
################################################################################################################################################################




# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, form %in% c('1','2') ) # Keep only debate transcripts in forms 1 & 2
rownames(deb_list) <- 1:nrow(deb_list)

# url for all debates
url <- "http://www.presidency.ucsb.edu/ws/index.php?pid="


################################################################################################################################################################
################################################################################################################################################################

setwd(Rfiles)

n<-nrow(deb_list)

for (i in 1:n) {	

	#i <- 14
	pagenum <- deb_list[i,'pagenum']
	debate <- deb_list[i,'debate']
	deb <-read_html(paste0(url, pagenum )) %>% html_nodes('.displaytext') %>% as.character() %>% iconv(to='ASCII//TRANSLIT')

	if (pagenum == '111178') {
			deb <-gsub("SANDERS (?):", "<b>SANDERS (?):</b>", deb)  
			deb <-gsub("<b>MUIR:</b> (?):", "<b>MUIR (?):</b>", deb)  
	} 

	if (debate == '2008DChicagoIL') {
			deb <-gsub("<b>Number two:</b>", "", deb)  
			deb <-gsub("<b>Number three:</b>", "", deb)  
	} 




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



	deb <-gsub("</b>",' @ ', deb)

	deb <-gsub('<i>','AAAA', deb)
	deb <-gsub('<b>','AAAA', deb)
	deb <-gsub('<p>','AAAA', deb)

	deb<-strsplit(deb, split='AAAA', fixed=TRUE)
	deb <- data.frame(matrix(unlist(deb), nrow=length(deb[[1]]), byrow=T),stringsAsFactors=FALSE)
	colnames(deb) <- '1'
	deb$delete <- ifelse( deb$'1'=="",1,0)
	deb <- subset(deb, delete==0)
	deb$delete <- NULL

	if (debate=="2008RSimiValleyCA1") {deb <- ParseDF(deb, 10)} else { 	
	deb <- ParseDF(deb)}

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
	deb$date <- trunc(ISOdate(deb$year, deb$month, deb$day), "day")
	deb$turn <- 1:nrow(deb)
	deb$person <- trim(deb$person)
table(deb$person)

	save(deb, file=paste0('D',debate,".Rdata"))

	print(deb_list[i,'debate'])
	#print(i)
}




