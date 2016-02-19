# Set working directory to get debate list from csv file

setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, election %in% c('1960') ) # Keep only debate transcripts from 1960
rownames(deb_list) <- 1:nrow(deb_list)


################################################################################################################################################################
################################################################################################################################################################

setwd(debTrans)

a <- list.files()[grepl(pattern = "1960", x = list.files())]
n <- length(a)

E1960 <- NULL
for (i in 1:n){
	#i=4

	debate <- deb_list[i,'debate']

	a <- list.files()[grepl(pattern = debate, x = list.files())]

  	deb <-  readLines(a)
  	ln <- nchar(deb)
  	print(paste0("Debate ",a, " imported. ", ln, " characters"))

	deb <- gsub("From the standpoint of MR, KENNEDY:", "From the standpoint of - <p>MR. KENNEDY:", deb)
	deb <- gsub("\\[introducing themselves: \"I'm Sander Vanocur, NBC News;\" \"I'm Charles Warren, Mutual News;\" \"I'm Stuart Novins, CBS News;\" \"Bob Fleming, ABC News.\"] The first question to Senator Kennedy from Mr. Fleming.</p>",
"</p> <p>MR. VANOCUR: I'm Sander Vanocur, NBC News</p> <p>MR. WARREN: I'm Charles Warren, Mutual News</p> <p>MR. NOVINS: I'm Stuart Novins, CBS News</p> <p> MR. FLEMING: Bob Fleming, ABC News.</p> <p>MR. SMITH: The first question to Senator Kennedy from Mr. Fleming.</p>", deb)
	deb <- gsub("we've b- gaining", "we've been gaining", deb)



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
	deb$video <-0

  	print(paste0("Debate ",a,  nrow(deb), " rows"))

  	E1960 <- rbind(E1960, deb)
  

}

setwd(Rfiles)
t(t(table(E1960$person)))

e1960names <- read.csv("E1960Names.csv")
E1960 <- merge(E1960, e1960names, by='person', all=TRUE)
E1960 <-arrange(E1960, debate, turn)

table(E1960$name,useNA ='always')

save(E1960, file="E1960.Rdata")



crap <-subset(E1960, grepl(":", E1960$message))
crap$pos <- ifelse(  grepl("[A-Z'-]:",crap$message),1,0)

crap0<-subset(crap, pos==0)
crap1<-subset(crap, pos==1)

nrow(crap1)
writeLines(strwrap(   paste(crap1[1,]$debate,crap1[1,]$message)   , width = 120, indent=5))

#writeLines(strwrap(   deb   , width = 120, indent=5))