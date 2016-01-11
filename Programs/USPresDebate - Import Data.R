# The point of this program is to process all the debate files at USC Santa Barbara's web page The American Presidency Project http://www.presidency.ucsb.edu/
# Once they are brought in and processed (speaker and message separated), they are output individually into a directory for transcripts

library(rvest)
library(plyr)
library(dplyr)
library(stringi)



base<-file.path("C:\\Users\\alan\\Documents\\GitHub\\USPresDebates") # Set to your directory
setwd(base)




parsevec <- function(vec) {
	l <- length(strsplit(vec, ' ')[[1]])
	n<-ifelse( 	length(grep(':',strsplit(vec, ' ')[[1]][1:nw]))==0,
			0, 
			grep(':',strsplit(vec, ' ')[[1]][1:nw]))
	person <- paste(gsub(':','',strsplit(vec, ' ')[[1]][0:n]), collapse=' ')
	message <- paste(strsplit(vec, ' ')[[1]][(n+1):l], collapse=' ')
	return(cbind(person,message))	
}


parsedf <- function(df,num=6) {
	nw<<-num
	newdf <- data.frame(t(apply(df, MARGIN=1, parsevec)),stringsAsFactors=FALSE)
	colnames(newdf) <-c('person', 'message')


	if (nrow(newdf) > 1) {
	  	for (j in 2:nrow(newdf)) { 
	  	if (newdf[j,'person']=='' ) 
			{newdf[j,'person'] <-  newdf[(j-1),'person'] }
		}


		for (j in 2:nrow(newdf)) {
			if (newdf[j,'person']==newdf[(j-1),'person']   ) { 
				newdf[j,'message'] <- paste(newdf[(j-1),'message'], newdf[j,'message'])
				newdf[(j-1),'person'] <-'DeleteMe'
			}
		}
	}



	newdf<-newdf[newdf$person!='DeleteMe',]
	return(newdf)
}


# Function to read in debate transcripts from url and php page
rht <- function(nodes="p", urlbase, Page ) {
	newdf <-read_html(paste0(urlbase, Page)) %>% html_nodes("p") %>% html_text() %>% ldply(rbind) 
	#newdf$'1' <- as.character(newdf$'1')
	return(newdf)
	}


trim <- function (x) gsub("^\\s+|\\s+$", "", x) # Stolen from f3lix http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r 





# Importing debates --- 
# url for all debates
url <- "http://www.presidency.ucsb.edu/ws/index.php?pid="








debListFP <- file.path(getwd(),"DebateList") 
setwd(debListFP)

deb_list0<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character" ), "DebateList.csv")
deb_list <- subset(deb_list0, pagenum !='' & usable=='1'), # Remove debates in the list that haven't happened yet and those where we need edited transcripts



n<-nrow(deb_list)

for (i in 1:n) {	
	assign(paste0('d_', deb_list[i,'debate']),  rht(Page=deb_list[i,'pagenum'], urlbase=url) %>% parsedf() 
	)  
		
	dat<-get(paste0('d_', deb_list[i,'debate']))  
	dat$debate <- deb_list[i,'debate']
	dat$pagenum <- deb_list[i, 'pagenum']
	dat$person <- as.character(dat$person) %>% trim() %>% toupper()
	dat$year <- deb_list[i,'year']
	dat$month <- deb_list[i,'month']
	dat$day <- deb_list[i,'day']

	dat$election <- substr(dat$debate, 1,4)
	dat$type <- substr(dat$debate, 5,5)
	assign(paste0('d_', deb_list[i,'debate']), dat)

	print(deb_list[i,'debate'])
	}
 


# Join into large d.f.
listOfDataFrames <- paste0("d_", deb_list$debate)
all_debates1<-do.call("rbind", lapply(listOfDataFrames , get))

















################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Process edited transcripts

# Some transcripts from the site are directly usable.
# At least not at my skill level

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################




deb_list <- subset(deb_list0, pagenum !='' & usable=='T'), # Remove debates in the list that haven't happened yet and those where we need edited transcripts


debTrans <- file.path(base,"Debate Transcripts") 
setwd(debTrans)



n<-nrow(deb_list)

for (i in 1:n) {	
	assign(paste0('d_', deb_list[i,'debate']),  ***********************************************
	)  
		
	dat<-get(paste0('d_', deb_list[i,'debate']))  
	dat$debate <- deb_list[i,'debate']
	dat$pagenum <- deb_list[i, 'pagenum']
	dat$person <- as.character(dat$person) %>% trim() %>% toupper()
	dat$year <- deb_list[i,'year']
	dat$month <- deb_list[i,'month']
	dat$day <- deb_list[i,'day']

	dat$election <- substr(dat$debate, 1,4)
	dat$type <- substr(dat$debate, 5,5)
	assign(paste0('d_', deb_list[i,'debate']), dat)

	print(deb_list[i,'debate'])
	}

 



# Join into large d.f.
listOfDataFrames <- paste0("d_", deb_list$debate)
all_debates2<-do.call("rbind", lapply(listOfDataFrames , get))

  












################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

all_debates <- rbind(all_debates1, all_debates2)

# On some machines weird symbols pop up like â€”. These are hard to get rid without converting the encoding. You may need to play around with this.
all_debates$message<-iconv(all_debates$message, to='ASCII//TRANSLIT')
 
# Fix the dashes
all_debates$message<-gsub(iconv("—",  to = "UTF-8"), "-", all_debates$message)

# Remove things like (APPLAUSE) and [VIDEO] that are not speech
all_debates$message<-gsub("\\(.*)", "", all_debates$message)  # Remove (any parenthesese and all their contents)
all_debates$message<-gsub("\\[.*]", "", all_debates$message)  # Remove [any brackets and all their contents]

alldeb1 <- file.path(base,"R Data Files") 










setwd(alldeb)

write.csv(all_debates, file = "all_debates.csv")
save(all_debates, file = "all_debates.Rdata")
