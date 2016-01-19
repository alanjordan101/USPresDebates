

################################################################################################################################################################
################################################################################################################################################################



url <- "http://www.debates.org/index.php?page=" 


# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, form =='6' ) # Keep only debate transcripts in forms 6





################################################################################################################################################################
################################################################################################################################################################

#Fix Individual Transcripts
setwd(debTrans)




################################################################################################################################################################
################################################################################################################################################################

setwd(Rfiles)

n<-nrow(deb_list)

for (i in 1:n) {	

	i <- 17

	pagenum <- deb_list[i,'pagenum']
	debate <- deb_list[i,'debate']

	if (debate %in% c("1980PClevelandOH", "1980PBaltimoreMD")) {

	deb <- rht(Page=pagenum, urlbase=url)

	deb$'1' <- gsub("RUTH HINERFELD, LEAGUE OF WOMEN VOTERS, EDUCATION FUND:", "RUTH HINERFELD:", deb$'1')
	deb$'1' <- gsub("RUTH J. HINERFELD, CHAIR, LEAGUE OF WOMEN VOTERS EDUCATION FUND:", "RUTH HINERFELD:", deb$'1')
	deb$'1' <- gsub("MARVIN STONE, U.S. NEWS AND WORLD REPORT:", "MARVIN STONE:", deb$'1')
	deb$'1' <- gsub("MR. ELLIS, CHRISTIAN SCIENCE MONITOR:", "MR. ELLIS:", deb$'1')
	deb$'1' <- gsub("MR. MOYERS, HOST AND EXECUTIVE EDITOR, \"BILL MOYERS' JOURNAL,\" PUBLIC BROADCASTING SYSTEM:", "MR. MOYERS:", deb$'1')
	deb$'1' <- gsub("CAROL LOOMIS, BOARD OF EDITORS, FORTUNE MAGAZINE:", "CAROL LOOMIS:", deb$'1')
	deb$'1' <- gsub("CHARLES CORDDRY, MILITARY CORRESPONDENT, THE SUN, BALTIMORE:", "CHARLES CORDDRY:", deb$'1')
	deb$'1' <- gsub("LEE MAY, STAFF WRITER, THE LOS ANGELES TIMES - WASHINGTON BUREAU:", "LEE MAY:", deb$'1')
	deb$'1' <- gsub("JANE BRYANT QUINN, CBS NEWS/NEWSWEEK/WASHINGTON POST:", "JANE BRYANT:", deb$'1')
	deb$'1' <- gsub("GOLDEN, EDITORIAL WRITER, THE NEW YORK TIMES:", "GOLDEN:", deb$'1')

	deb <- ParseDF(deb, nw=5, sep=':')

	deb$message<-gsub("\\(.*)", "", deb$message)  # Remove (any parenthesese and all their contents)
	deb$message<-gsub("\\[.*]", "", deb$message)  # Remove [any brackets and all their contents]
	deb$message<-gsub("\\{.*}", "", deb$message)  # Remove {any brackets and all their contents}

	deb$debate <- deb_list[i,'debate']
	deb$year <- deb_list[i,'year']
	deb$month <- deb_list[i,'month']
	deb$day <- deb_list[i,'day']

	}
	else {

	deb <- rht(Page=pagenum, urlbase=url) %>% ParseDF(nw=5, sep=':')
	deb$message<-gsub("\\(.*)", "", deb$message)  # Remove (any parenthesese and all their contents)
	deb$message<-gsub("\\[.*]", "", deb$message)  # Remove [any brackets and all their contents]
	deb$message<-gsub("\\{.*}", "", deb$message)  # Remove {any brackets and all their contents}

	deb$debate <- deb_list[i,'debate']
	deb$year <- deb_list[i,'year']
	deb$month <- deb_list[i,'month']
	deb$day <- deb_list[i,'day']
	}

	save(deb, file=paste0('D',debate,".Rdata"))

	print(debate)
}




