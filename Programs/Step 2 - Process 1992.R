# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('1992') ) # Keep only debate transcripts from 1992
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

#a <- list.files()[grepl(pattern = "1992", x = list.files())]
#n <- length(a)

E1992 <- NULL
for (i in 1:n){
	#i=4

	debate <- deb_list[i,'debate']

	setwd(debTrans)
	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))



	deb<- gsub("<p class=\"MsoNormal\" style=\"mso-margin-top-alt: auto; margin-bottom: 7.5pt; line-height: 14.25pt; background: white;\"><span style=\"font-size: 9pt; font-family: Verdana;\">", "<p>", deb)
	deb <- gsub("Verdana; font-size: 9pt; background-color: white; line-height: 14.25pt;>", "", deb)
	deb <- gsub("<span style=\"font-family: Verdana; font-size: 9pt; background-color: white; line-height: 14.25pt;\">", "", deb)
	deb <- gsub("Let me tell you:", "Let me tell you,", deb)

	deb <- gsub("\\(holding hand up at Gore\\)", "", deb)


	deb <- gsub('<b> a\\?\\"</b>', "-", deb)


	deb <- gsub("<b> </b>", " ", deb)
	deb <- gsub("<b>.</b>", ".", deb)
	deb <- gsub("<b>...</b>", ".", deb)

setwd(Func)
source("gsublist.R")



	#deb <-gsub("</b>",' @ ', deb)

	deb <-gsub('<i>','BAAB', deb)
	deb <-gsub('<b>','BAAB', deb)
	deb <-gsub('<br/>','BAAB', deb) # Not for use on all transcripts
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

  	#E1992 <- rbind(E1992, deb)
	assign(paste0('d',debate), deb)
  

}



  
d1992PRichmondVA <- rbind(d1992PRichmondVA_A, d1992PRichmondVA_B)
d1992PRichmondVA$turn <- 1:nrow(d1992PRichmondVA)
d1992PRichmondVA$debate <- "1992PRichmondVA"

d1992PStLouisMO <- rbind(d1992PStLouisMO_A, d1992PStLouisMO_B)   
d1992PStLouisMO$turn <- 1:nrow(d1992PStLouisMO)
d1992PStLouisMO$debate <- "1992PStLouisMO"




E1992 <- rbind(d1992PEastLansingMI, d1992PRichmondVA, d1992PStLouisMO, d1992VAtlantaGA)


setwd(Rfiles)
t(t(table(E1992$person)))


E1992 <- subset(E1992, person !="Note")

e1992names <- read.csv("E1992Names.csv")
E1992 <- merge(E1992, e1992names, by='person', all=TRUE)
E1992 <-arrange(E1992, debate, turn)


E1992$message <- gsub("One final point; President Reagan", "One final point: President Reagan", E1992$message)
E1992$message <- gsub("Similarly, in Central America; What we're doing in Nicaragua", "Similarly, in Central America: What we're doing in Nicaragua", E1992$message)


table(E1992$name,useNA ='always')

save(E1992, file="E1992.Rdata")


crap <-subset(E1992, grepl("\\(", E1992$message))
