# The point of this program is to process all the debate files at USC Santa Barbara's web page The American Presidency Project http://www.presidency.ucsb.edu/
# Once they are brought in and processed (speaker and message separated), they are output individually into a directory for transcripts

library(rvest)
library(plyr)
library(dplyr)
library(stringi)



base<-file.path("C:\\Users\\alan\\Documents\\GitHub\\USPresDebates") # Set to your directory
setwd(base)




# function to partially separate and clean into a data.frame a debate from the presidency project
MakeDebateDF<-function(df){
  newdf <- data.frame(
    person = apply(df, 
                   MARGIN = 1, 
                   function(x){
                     stri_extract_first_regex(x, 
                                              "[A-Z'-]+(?=(:\\s))")
                   }),
    message = apply(df, 
                    MARGIN = 1, 
                    function(x){
                      stri_replace_first_regex(x,
                                               "[A-Z'-]+:\\s+", 
                                               "")
                    }),
    stringsAsFactors=FALSE
  )

  	for (j in 2:nrow(newdf)) { 
  	if (is.na(newdf[j,'person'])) 
		{newdf[j,'person'] <-  newdf[(j-1),'person'] }
	}


	newdf$person[is.na(newdf$person)] <-'x'
	for (j in 2:nrow(newdf)) {
		if (newdf[j,'person']==newdf[(j-1),'person']   ) { 
			newdf[j,'message'] <- paste(newdf[(j-1),'message'], newdf[j,'message'])
			newdf[(j-1),'person'] <-'DeleteMe'
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







# Data Frame listing debates, php page numbers and short description
# The program will cycle through this table to construct a data frame for each debate in their directory.



# variable debate 
#	columns 1-4 - Election year
#	column  5 - P for Presidential , V for Vice-Presidential, D for Democratic primary and R for Republican primary
#	column  6-end - City and State Abrv. Occaisionally other descriptors when more than one debate per city or city not listed  



debListFP <- file.path(getwd(),"DebateList") 
setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character", "Date", "character",  "character"), "DebateList.csv")
deb_list <- deb_list[deb_list$pagenum !='',] # Remove debates in the list that haven't happened yet

debRF <- file.path(base,"IndDebateFiles") 
setwd(debRF)

n<-nrow(deb_list)

for (i in 1:n) {	
	assign(paste0('d_', deb_list[i,'debate']),  rht(Page=deb_list[i,'pagenum'], urlbase=url) %>% MakeDebateDF() 
	)  
		
	dat<-get(paste0('d_', deb_list[i,'debate']))  
	dat$debate <- deb_list[i,'debate']
	dat$person <- as.character(dat$person) %>% trim() %>% toupper()
	dat$date <- deb_list[i,'date']
	assign(paste0('d_', deb_list[i,'debate']), dat)

	#write.csv(dat, file = paste0(deb_list[i,"debate"],".csv"))
	print(deb_list[i,'debate'])
	}
 

#d_2016D1$person<-ifelse(d_2016D1$person=="INTON", "CLINTON", d_2016D1$person)

# Join into large d.f.
listOfDataFrames <- paste0("d_", deb_list$debate)
all_debates<-do.call("rbind", lapply(listOfDataFrames , get))

# On some machines weird symbols pop up like â€”. These are hard to get rid without converting the encoding. You may need to play around with this.
all_debates$message<-iconv(all_debates$message, to='ASCII//TRANSLIT')
 
# Fix the dashes
all_debates$message<-gsub(iconv("—",  to = "UTF-8"), "-", all_debates$message)

# Remove things like (APPLAUSE) and [VIDEO] that are not speech
all_debates$message<-gsub("\\(.*)", "", all_debates$message)  # Remove (any parenthesese and all their contents)
all_debates$message<-gsub("\\[.*]", "", all_debates$message)  # Remove [any brackets and all their contents]

alldeb <- file.path(base,"R Data Files") 
setwd(alldeb)

write.csv(all_debates, file = "all_debates.csv")
save(all_debates, file = "all_debates.Rdata")
