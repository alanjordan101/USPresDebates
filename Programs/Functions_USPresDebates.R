ParseDF <- function(df,nw=3, sep='@') {



	parsevec <- function(vec) {
		l <- length(strsplit(vec, ' ')[[1]])
		n<-ifelse( 	length(grep(sep,strsplit(vec, ' ')[[1]][1:nw]))==0,
				0, 
				grep(sep,strsplit(vec, ' ')[[1]][1:nw]))
		person <- paste(gsub(sep,'',strsplit(vec, ' ')[[1]][0:n]), collapse=' ')
		message <- paste(strsplit(vec, ' ')[[1]][(n+1):l], collapse=' ')
		return(cbind(person,message))	
	}


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


trim <- function (x) gsub("^\\s+|\\s+$", "", x) # Stolen from f3lix http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r 


# Function to read in debate transcripts from url and php page
rht <- function(nodes="p", urlbase, Page ) {
	newdf <-read_html(paste0(urlbase, Page)) %>% html_nodes("p") %>% html_text() %>% ldply(rbind) 
	#newdf$'1' <- as.character(newdf$'1')
	return(newdf)
	}