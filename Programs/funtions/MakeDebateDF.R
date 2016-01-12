# function to partially separate and clean into a data.frame a debate from the presidency project
# This function was orginally written by Eduardo Flores
# I have made changes to it.
MakeDebateDF<-function(df){
  newdf <- data.frame(
    person = apply(df, 
                   MARGIN = 1, 
                   function(x){
                     stri_extract_first_regex(x, 
                                              "[A-Z'-\\[\\]]+(?=(:\\s))")
                   }),
    message = apply(df, 
                    MARGIN = 1, 
                    function(x){
                      stri_replace_first_regex(x,
                                               "[A-Z'-\\[\\]]+:\\s+", 
                                               "")
                    }),
    stringsAsFactors=FALSE
  )
	if (nrow(newdf) > 1) {
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
	}
	newdf<-newdf[newdf$person!='DeleteMe',]
  	return(newdf)
}

