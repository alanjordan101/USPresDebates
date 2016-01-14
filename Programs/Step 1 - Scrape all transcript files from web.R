library(rvest)
library(plyr)
library(dplyr)

################################################################################################################################################################
################################################################################################################################################################

# Set your directory here:

base<-file.path("C:\\Users\\alan\\Documents\\GitHub\\USPresDebates") # Set to your directory
setwd(base)

################################################################################################################################################################
################################################################################################################################################################


# url for all debates
url <- "http://www.presidency.ucsb.edu/ws/index.php?pid="


# Set working directory to get debate list from csv file
debListFP <- file.path(getwd(),"DebateList") 
setwd(debListFP)


# Pull in debate list from csv file and remove lines for debates without php page number.
deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character", "integer" ), "DebateList.csv")
deb_list <- subset(deb_list, pagenum !='' ) # Remove debates in the list that haven't happened yet and those where we need edited transcripts

# I want to keep a copy of each transcript just in case it gets changed. 
# Set working directory to place copy of the transcript in a folder labeled Debate Transcripts.
debTrans <- file.path(base,"Debate Transcripts") 
setwd(debTrans)


# Loop to go through the debate list and get a copy with transcripts.
# I grab only the section of the web page where the debate transcript is kept <span class="displaytext">  to </span>.
# I keep all html tags at this point. Many will be helpful in processing the transcript.

n<-nrow(deb_list)

for (i in 1:n) {	

	pagenum <- deb_list[i,'pagenum']
	
	deb <-as.character(read_html(paste0(url, pagenum )) %>% html_nodes('.displaytext'))
	write.table(deb, paste0(pagenum,".txt") )
	print(deb_list[i,'debate'])
	#print(i)
}




