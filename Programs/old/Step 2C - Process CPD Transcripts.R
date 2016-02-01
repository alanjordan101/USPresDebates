

################################################################################################################################################################
################################################################################################################################################################



url <- "http://www.debates.org/index.php?page=" 


# Set working directory to get debate list from csv file
debListFP <- file.path(base,"DebateList") 
setwd(debListFP)

deb_list<-read.csv(header=TRUE, colClasses=c("character",  "character", "integer", "integer", "integer", "character", "character",  "character", "character", "integer", "character" ), "dl3.csv")
deb_list <- subset(deb_list, form =='6' ) # Keep only debate transcripts in forms 6
rownames(deb_list)<-1:nrow(deb_list)





################################################################################################################################################################
################################################################################################################################################################



setwd(Rfiles)

n<-nrow(deb_list)

for (i in 1:n) {	

	#i <- 14

	pagenum <- deb_list[i,'pagenum']
	debate <- deb_list[i,'debate']


	deb <- rht(Page=pagenum, urlbase=url)


	deb$'1' <- gsub("RUTH HINERFELD, LEAGUE OF WOMEN VOTERS, EDUCATION FUND:", "RUTH HINERFELD:", deb$'1')
	deb$'1' <- gsub("RUTH J. HINERFELD, CHAIR, LEAGUE OF WOMEN VOTERS EDUCATION FUND:", "RUTH HINERFELD:", deb$'1')
	deb$'1' <- gsub("MARVIN STONE, U.S. NEWS AND WORLD REPORT:", "MARVIN STONE:", deb$'1')
	deb$'1' <- gsub("MR. ELLIS, CHRISTIAN SCIENCE MONITOR:", "MR. ELLIS:", deb$'1')
	deb$'1' <- gsub("MR. MOYERS, HOST AND EXECUTIVE EDITOR, \"BILL MOYERS' JOURNAL,\" PUBLIC BROADCASTING SYSTEM:", "MR. MOYERS:", deb$'1')
	deb$'1' <- gsub("CAROL LOOMIS, BOARD OF EDITORS, FORTUNE MAGAZINE:", "CAROL LOOMIS:", deb$'1')
	deb$'1' <- gsub("CHARLES CORDDRY, MILITARY CORRESPONDENT, THE SUN, BALTIMORE:", "CHARLES CORDDRY:", deb$'1')
	deb$'1' <- gsub("LEE MAY, STAFF WRITER, THE LOS ANGELES TIMES - WASHINGTON BUREAU:", "LEE MAY:", deb$'1')
	deb$'1' <- gsub("JANE BRYANT QUINN, CBS NEWS/NEWSWEEK/WASHINGTON POST:", "JANE BRYANT:", deb$'1')
	deb$'1' <- gsub("GOLDEN, EDITORIAL WRITER, THE NEW YORK TIMES:", "GOLDEN:", deb$'1')
	deb$'1' <- gsub("\\(CROSSTALK\\)", "", deb$'1')
	deb$'1' <- gsub("MARTHA RADDATZ, MODERATOR\\[\\*\\]", "", deb$'1')
	deb$'1' <- gsub("\\(LAUGHTER\\)", "", deb$'1')
	deb$'1' <- gsub("\\[\\*\\]", "", deb$'1')
	deb$'1' <- gsub('\"',"", deb$'1')


	if (debate %in% c("1980PClevelandOH", "1980PBaltimoreMD")) {
		deb <- ParseDF(deb, nw=5, sep=':')
	} else if (debate %in% c("1984PKansasCityMO", "1984PLouisvilleKY") )  	{
		deb <- ParseDF(deb, nw=2, sep=':')

	} else if (debate %in% c("2012VDanvilleKY", "2012PHempsteadNY", "2012PDenverCO") )  	{
		deb <- ParseDF(deb, nw=1, sep=':')
	} else 
	{
		deb <- ParseDF(deb, nw=5, sep=':')
	}




#	deb$message<-gsub("\\(.*)", "", deb$message)  # Remove (any parenthesese and all their contents)
#	deb$message<-gsub("\\[.*]", "", deb$message)  # Remove [any brackets and all their contents]
#	deb$message<-gsub("\\{.*}", "", deb$message)  # Remove {any brackets and all their contents}

	deb <- deb[-1,]
	deb$debate <- deb_list[i,'debate']
	deb$year <- deb_list[i,'year']
	deb$month <- deb_list[i,'month']
	deb$day <- deb_list[i,'day']
	deb$date <- as.POSIXct(trunc(ISOdate(deb$year, deb$month, deb$day), "day"))
	deb$turn <- 1:nrow(deb)
	deb$person <- trim(deb$person)

	deb$person[deb$person=="OBAM"] <-"OBAMA"
	deb$person[deb$person=="ROMNEHY"] <-"ROMNEY"


	if (debate=='1960PWashingtonDC') {

	deb[17,]$message <-"Senator, on the same subject, in the past you have emphasized the president's responsibility as a moral leader as well as an executive on civil rights questions. What specifically might the next president do uh - in the event of an uh - an occurrence such as Little Rock or the lunch-counter sit-ins? From the standpoint of..."
	debb <- deb[24,]
	debb$message <- "Well let me say that I think that the president operates in a number of different areas. First, as a legislative leader. And as I just said that I believe that the passage of the so-called Title Three, which gives the Attorney General the power to protect Constitutional rights in those cases where it's not possible for the person involved to bring the suit. Secondly, as an executive leader. There have been only six cases brought by this Attorney General under the voting bill passed in 1957 and the voting bill passed in 1960. The right to vote is basic. I do not believe that this Administration has implemented those bills which represent the will of the majority of the Congress on two occasions with vigor. Thirdly, I don't believe that the government contracts division is operated with vigor. Everyone who does business with the government should have the opportunity to make sure that they do not practice discrimination in their hiring. And that's in all sections of the United States. And then fourthly, as a moral leader. There is a very strong moral basis for this concept of equality of opportunity. We are in a very difficult time. We need all the talent we can get. We sit on a conspicuous stage. We are a goldfish bowl before the world. We have to practice what we preach. We set a very high standard for ourselves. The Communists do not. They set a low standard of materialism. We preach in the Declaration of Independence and in the Constitution, in the statement of our greatest leaders, we preach very high standards; and if we're not going to be s- charged before the world with hypocrisy we have to meet those standards. I believe the president of the United States should indicate it. Now lastly, I believe in the case of Little Rock. I would have hoped that the president of the United States would have been possible for him to indicate it clearly that uh - the Supreme Court decision was going to be carried out. I would have hoped that it would have been possible to use marshals to do so. But it wou- uh - evidently uh - under the handling of the case it was not. I would hope an incident like that would not happen. I think if the president is responsible, if he consults with those involved, if he makes it clear that the Supreme Court decision is going to be carried out in a way that the Supreme Court planned - with deliberate speed - then in my judgment, providing he's behind action, I believe we can make uh - progress. Now the present Administration - the President - has said - never indicated what he thought of the 1954 decision. Unless the president speaks, then of course uh - the country doesn't speak, and Franklin Roosevelt said: 'The pre - uh - the presidency of the United States is above all a place of moral leadership.' And I believe on this great moral issue he should speak out and give his views clearly."
 	debb$turn <- 17.1
	deb <- rbind(deb, debb)
	deb <- arrange(deb, turn)
	deb$turn <- 1:nrow(deb)
	}

	save(deb, file=paste0('D',debate,".Rdata"))

	print(debate)
}




