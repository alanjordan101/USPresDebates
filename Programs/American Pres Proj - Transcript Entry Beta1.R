# The point of this program is to process all the debate files at USC Santa Barbara's web page The American Presidency Project http://www.presidency.ucsb.edu/
# At this point it processing all the debates for the 2016 presidential elections so far. I will add more later.

library(rvest)
library(plyr)
library(dplyr)
library(stringi)
library(ggplot2)
# devtools::install_github("eflores89/eem")
library(eem)

#library("tm")
#library("SnowballC")
#library("wordcloud")

library(qdap)



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

  return(newdf)
}


# Function to read in debate transcripts from url and php page
rht <- function(nodes="p", urlbase, Page ) {
	newdf <-read_html(paste0(urlbase, Page)) %>% html_nodes("p") %>% html_text() %>% ldply(rbind) 
	newdf$'1' <- as.character(newdf$'1')
	return(newdf)
	}


trim <- function (x) gsub("^\\s+|\\s+$", "", x) # Stolen from f3lix http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r 





# Importing debates --- 
# url for all debates
url <- "http://www.presidency.ucsb.edu/ws/index.php?pid="

# Data Frame listing debates, php page numbers and short description
# The program will cycle through this table to construct a data frame for each debate.
# Later the program will put all these data frames together into all_debates.
# This data frame shouldn't be in the program. I'll move it to a csv file later.

# Might want to add date in date format
# variable debate 
#	columns 1-4 - Election year
#	column  5 - Party
#	column  6 - Debate number within election year
#	column  7 - A is for poll leaders, B is for those at the bottom of the polls

deb_list<-read.table(header=TRUE, colClasses='character', text="
debate pagenum description
2016R1A 110489 'First Republican Debate Poll Leaders'
2016R1B 110757 'First Republican Debate Kiddy Table'
2016R2A 110756 'Second Republican Debate Poll Leaders'
2016R2B 110758 'Second Republican Debate Kiddy Table'
2016R3A 110906 'Third Republican Debate Poll Leaders'
2016R3B 110907 'Third Republican Debate Kiddy Table'
2016R4A 110908 'Fourth Republican Debate Poll Leaders'
2016R4B 110909 'Fourth Republican Debate Kiddy Table'
2016R5A 111177 'Fifth Republican Debate Poll Leaders'
2016R5B 111176 'Fifth Republican Debate Kiddy Table'
2016D1  110903 'First Democratic Debate'
2016D2  110910 'Second Democratic Debate'
2016D3  111178 'Third Democratic Debate'
")



for (i in 1:nrow(deb_list)) {	
		assign(paste0('d_', deb_list[i,'debate']),  rht(Page=deb_list[i,'pagenum'], urlbase=url) %>% MakeDebateDF() 
		 )  

		
		dat<-get(paste0('d_', deb_list[i,'debate']))  
		dat$debate <- deb_list[i,'debate']
		dat$person <- as.character(dat$person) %>% trim() %>% toupper()
		assign(paste0('d_', deb_list[i,'debate']), dat)

			} 









 
 


######################### Fix Clinton's Name in First Debate
d_d1$person<-ifelse(d_D1$person=="INTON", "CLINTON", d_D1$person)
 
 




######################## Remove Non Candidate Speakers
d_R1a<-subset(d_R1A, person %in% c("BUSH",  "CARSON",  "CHRISTIE", "CRUZ",  "HUCKABEE", "KASICH",  "PAUL", "RUBIO", "TRUMP",  "WALKER") )
d_R1b<-subset(d_R1B, person %in% c("GILMORE", "FIORINA", "GRAHAM", "JINDAL", "PATAKI", "PERRY", "SANTORUM") )

d_R2a<-subset(d_R2A, person %in% c("BUSH",  "CARSON",  "CHRISTIE", "CRUZ", "HUCKABEE", "KASICH",  "PAUL", "RUBIO", "TRUMP",  "WALKER", "FIORINA") )
d_R2b<-subset(d_R2B, person %in% c("GRAHAM", "JINDAL", "PATAKI",  "SANTORUM") )

d_R3a<-subset(d_R3A, person %in% c("BUSH",  "CARSON",  "CHRISTIE", "CRUZ",  "HUCKABEE", "KASICH",  "PAUL", "RUBIO", "TRUMP",  "FIORINA") )
d_R3b<-subset(d_R3B, person %in% c("GRAHAM", "JINDAL", "PATAKI", "SANTORUM") )

d_R4a<-subset(d_R4A, person %in% c("BUSH",  "CARSON",  "CRUZ",   "KASICH",  "PAUL", "RUBIO", "TRUMP",  "FIORINA") )
d_R4b<-subset(d_R4B, person %in% c("CHRISTIE", "HUCKABEE", "JINDAL", "SANTORUM") )

d_R5a<-subset(d_R5A, person %in% c("BUSH",  "CARSON", "CHRISTIE", "CRUZ",   "FIORINA", "KASICH",  "PAUL", "RUBIO", "TRUMP" ) )
d_R5b<-subset(d_R5B, person %in% c("GRAHAM", "HUCKABEE", "PATAKI",  "SANTORUM") )




d_D1<-subset(d_D1, person %in% c("CHAFEE", "CLINTON", "O'MALLEY", "SANDERS", "WEBB") )
d_D2<-subset(d_D2, person %in% c("CLINTON", "O'MALLEY", "SANDERS") )
d_D3<-subset(d_D2, person %in% c("CLINTON", "O'MALLEY", "SANDERS") )


# Join into large d.f.
listOfDataFrames <- paste0("d_", deb_list$debate)
all_debates<-do.call("rbind", lapply(listOfDataFrames , get))

# On some machines weird symbols pop up like â€”. These are hard to get rid without converting the encoding. You may need to play around with this.
all_debates$message<-iconv(debates$message, to='ASCII//TRANSLIT')
 
# Fix the dashes
all_debates$message<-gsub(iconv("—",  to = "UTF-8"), "-", all_debates$message)

# Remove things like (APPLAUSE) and [VIDEO] that are next speech
all_debates$message<-gsub("\\(.*)", "", all_debates$message)  # Remove (any parenthesese and all their contents)
all_debates$message<-gsub("\\[.*]", "", all_debates$message)  # Remove [any brackets and all their contents]











cand<- c("BUSH",  "CARSON",  "CHRISTIE", "CRUZ", "HUCKABEE", "KASICH",  "PAUL", "RUBIO", "TRUMP",  "WALKER", "FIORINA", "GRAHAM", "JINDAL", "PATAKI", "PERRY", "SANTORUM",
	"CHAFEE", "CLINTON", "O'MALLEY", "SANDERS", "WEBB")



debates<-subset(debates, person %in% cand) 

