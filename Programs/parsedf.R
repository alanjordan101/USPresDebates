


stuff<-data.frame(rbind(
"Bob Dylan: I don't understand regexp.",
"Edward G. Robinson: That's funny I don't understand regexp either, of course I'm dead.",
"In fact I was long dead before regexp was created: that's a long time",
"Carrot-top: Hi everybody!"
))
colnames(stuff) <-'1'




parsevec <- function(vec,nw=3) {
	l <- length(strsplit(vec, ' ')[[1]])
	n<-ifelse( 	length(grep(':',strsplit(vec, ' ')[[1]][1:nw]))==0,
			0, 
			grep(':',strsplit(vec, ' ')[[1]][1:nw]))
	person <- paste(gsub(':','',strsplit(vec, ' ')[[1]][0:n]), collapse=' ')
	message <- paste(strsplit(vec, ' ')[[1]][(n+1):l], collapse=' ')
	return(cbind(person,message))	
}


parsedf <- function(df) {
	newdf <- data.frame(t(apply(stuff, MARGIN=1, parsevec)),stringsAsFactors=FALSE)
	colnames(newdf) <-c('person', 'message')
	return(newdf)
}
parsedf(stuff)
