


# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)
 
deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, form %in% c('1','2','4') ) # Keep only debate transcripts in forms 1 , 2 & 4
rownames(deb_list) <- 1:nrow(deb_list)

# url for all debates
url <- "http://www.presidency.ucsb.edu/ws/index.php?pid="


setwd(debTrans)


for (i in 1:nrow(deb_list)) {

	#i = 1

	pagenum <- deb_list[i,'pagenum']
	debate <- deb_list[i,'debate']
	deb <-read_html(paste0(url, pagenum )) %>% html_nodes('.displaytext') %>% as.character() %>% iconv(to='ASCII//TRANSLIT')

	write(deb, paste0(debate,".trans")    )

}









# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)
 
deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, form %in% c('6','7') ) # Keep only debate transcripts in forms 6, 7
rownames(deb_list) <- 1:nrow(deb_list)

# url for all debates
url <- "http://www.debates.org/index.php?page="


setwd(debTrans)


for (i in 1:nrow(deb_list)) {

	#i = 1

	pagenum <- deb_list[i,'pagenum']
	debate <- deb_list[i,'debate']
	deb <-read_html(paste0(url, pagenum )) %>% html_nodes('#content-sm') %>% as.character() %>% iconv(to='ASCII//TRANSLIT')

	print(substr(deb,1,50))
	write(deb, paste0(debate,".trans")    )

}

