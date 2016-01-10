# I'm toying around with a new function that will split transcripts on colons.
# This would be a replacement for Eduardo Flores' MakeDebateDF.
# I want it to be able to split on the colon so long as the colon appears in the first nw words.
# I also want to pick up words with apostrophe's (O'MALLEY).
# I also to ignore case.
# I want to pick up hyphens.

# I think this does what I want.
# I don't like that I have two functions and I don't know how to set an argument for the inner function 
# from the outer. I can probably get by with that option fixed...hopefully.


# Ex. 1
# nw=3
# "Bob Dylan: I don't understand regexp."
# Should break out into "Bob Dylan" and "I don't understand regexp" if n is 3.


# Ex. 2
# nw=3
# "In fact I was long dead before regexp was created: that's a long time"
# Should not break out into two separate strings because the colon appears at the 10th word.


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
