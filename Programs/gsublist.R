	deb <- gsub("\\[<i>applause</i><b>\\] </b>", "", deb)
	deb <- gsub("\\[<i>applause</i>\\]", "", deb)
	deb <- gsub("\\[<i>Applause</i>\\]", "", deb)
	deb <- gsub("\\[<i>cheering and applause</i>\\]", "", deb)
	deb <- gsub("\\[<i>Pause</i>\\]", "", deb)
	deb <- gsub("\\[<i>pause</i>\\]", "", deb)
	deb <- gsub("\\[<i>Laughter</i>\\]", "", deb)
	deb <- gsub("\\[<i>Murmurs from audience</i>\\]", "", deb)
	deb <- gsub("\\[<i>Crosstalk</i>\\]", "", deb)
	deb <- gsub("\\[<i>Crosstalk</i>\\}", "", deb)
	deb <- gsub("\\[<i>Booing</i>\\]", "", deb)
	deb <- gsub("\\[<i>booing</i>\\]", "", deb)
	deb <- gsub("\\[<i>joined in progress</i>\\]", "", deb)
	deb <- gsub("\\[<i>Bell sounds</i>\\]", "", deb)
	deb <- gsub("\\[<i>speaking to Santorum</i>\\]", "", deb)
	deb <- gsub("\\[<i>Inaudible</i>\\]", "", deb)
	deb <- gsub("\\[<i>inaudible</i>\\]", "", deb)
	deb <- gsub("\\[<i>inaubible</i>\\]", "", deb)
	deb <- gsub("\\[<i>unintelligible</i>\\]", "", deb)
	deb <- gsub("\\[<i>overtalk</i>\\]", "", deb)
	deb <- gsub("\\[<i>audience booing</i>\\]", "", deb)
	deb <- gsub("\\[<i>laugh</i>\\]", "", deb)
	deb <- gsub("\\[<i>chuckle</i>\\]", "", deb)
	deb <- gsub("\\[<i>cheering</i>\\]", "", deb)
	deb <- gsub("\\[<i>music</i>\\]", "", deb)
	deb <- gsub("\\[<i>boos</i>\\]", "", deb)
	deb <- gsub("\\[<i>background voice</i>\\]", "", deb)
	deb <- gsub("\\[<i>singing of the National Anthem</i>\\]", "", deb)
	deb <- gsub("\\[<i>off-mike</i>\\]", "", deb)
	deb <- gsub("\\[<i>cheers</i>\\]", "", deb)

#	deb <- gsub("\\[<i>begin video clip</i>\\]", "", deb) ########################################
#	deb <- gsub("\\[<i>end video clip</i>\\]", "", deb) #######################################

	deb <- gsub("\\[<i>Cheers and Applause</i>\\]", "", deb)
	deb <- gsub("\\[<i>ph</i>\\]", "", deb)

	deb <- gsub("\\[<i>Sings The Star Spangled Banner</i>\\]", "", deb)
	deb <- gsub("\\[<i>noise from audience</i>\\]", "", deb)
	deb <- gsub("\\[<i>feedback noise</i>\\]", "", deb)
	deb <- gsub("\\[<i>laughing</i>\\]", "", deb)
	deb <- gsub("\\[<i>national anthem</i>\\]", "", deb)
	deb <- gsub("</div>", "", deb)

	deb <- gsub("a\\?", "", deb)




	deb <- gsub("\\[<i>Commercial Break</i>\\]", "", deb)
	deb <- gsub("\\[<i>sic</i>\\]", "", deb)
	deb <- gsub("\\[<i>crosstalk</i>\\]", "", deb)
	deb <- gsub("\\[<i>laughter</i>\\]", "", deb)
	deb <- gsub("\\[<i>inaudible</i>\\]", "", deb)
	deb <- gsub("\\[<i>chuckles</i>\\]", "", deb)
	deb <- gsub("\\[<i>laughter and applause</i>\\]", "", deb)
	deb <- gsub("\\[<i>speaking in Spanish</i>\\]", "", deb)
	deb <- gsub("\\[<i>commercial break</i>\\]", "", deb)
	deb <- gsub("\\(COMMERCIAL BREAK\\)", "", deb)
	deb <- gsub("\\(APPLAUSE\\)", "", deb)
	deb <- gsub("\\(inaudible\\)", "", deb)
	deb <- gsub("\\(Laughter\\)", "", deb)
	deb <- gsub("\\(sic\\)", "", deb)
	deb <- gsub("\\n", "", deb)
	deb <- gsub("<br>", "", deb)

	deb <- gsub("<BR>", "", deb)
	deb <- gsub("<BR/>", "", deb)
	deb <- gsub('\"',"", deb)
	deb <- gsub('<strong>',"", deb)
	deb <- gsub('</strong>',"", deb)
	deb <- gsub('<h1>',"", deb)
	deb <- gsub('</h1>',"", deb) 
	deb <- gsub('</p>',"", deb) 
	deb <- gsub('<div id=content-sm>',"", deb) 
	deb <- gsub("\\(chuckle\\)", "", deb)
 	deb <- gsub('\\(barely audible\\)',"", deb) 
 	deb <- gsub('\\(laughter from audience\\)',"", deb) 
	deb <- gsub("\\[Laughter\\]", "", deb)
	deb <- gsub("\\[applause\\]", "", deb)
	deb <- gsub("\\[Laughter and applause\\]", "", deb)
	deb <- gsub("\\[Applause\\]", "", deb)
	deb <- gsub("\\[\\]", "", deb)
	deb <- gsub("\\[laughter\\]", "", deb)
	deb <- gsub("\\(Applause\\)", "", deb)
	deb <- gsub("\\(Shouts and applause\\)", "", deb)
	deb <- gsub("\\(Prolonged shouts and applause\\)", "", deb)
	deb <- gsub("\\(Laughter, boos\\)", "", deb)
	deb <- gsub("\\(Laughter and applause\\)", "", deb)
	deb <- gsub("\\(Cheers and Applause\\)", "", deb)
	deb <- gsub("\\(phonetic\\)", "", deb)
	deb <- gsub("\\(Scattered applause\\)", "", deb)
	deb <- gsub("\\(Shouts, laughter\\)", "", deb)
	deb <- gsub("\\(Equal amounts of cheering and booing\\)", "", deb)
	deb <- gsub("\\(Laughter, scattered applause\\)", "", deb)
	deb <- gsub("\\(Laughter, applause\\)", "", deb)
	deb <- gsub("\\(Scattered laughter\\)", "", deb)
	deb <- gsub("\\(Boos and applause\\)", "", deb)
	deb <- gsub("\\[Laughter.\\]", "", deb)
	deb <- gsub("\\(Laughter.\\)", "", deb)
	deb <- gsub("\\(Audience: No.\\)", "", deb)
	deb <- gsub("\\(Inaudible\\)", "", deb)
	deb <- gsub("\\(Laughs\\)", "", deb)
	deb <- gsub("</span>", "", deb)
	deb <- gsub("\\(APPLAUSE.\\)", "", deb)
	deb <- gsub("\\(Simultaneous conversation\\)", "", deb)
	deb <- gsub("\\(Jeers.\\)", "", deb)
	deb <- gsub("\\(laughter\\)", "", deb)
	deb <- gsub("\\(ph\\)", "", deb)
	deb <- gsub("\\[ Laughter \\]", "", deb)
	deb <- gsub("</b>", "", deb)
	deb <- gsub("<span class=displaytext>", "", deb)
	deb <- gsub("</i>", "", deb)
	deb <- gsub("\\.\\.\\.", "-", deb)
	deb <- gsub("\\(LAUGHTER\\)", "", deb)
	deb <- gsub("\\(CROSSTALK\\)", "", deb)
	deb <- gsub("\\(J\\)", "", deb)
	deb <- gsub("\\(OFF-MIKE\\)", "", deb)
	deb <- gsub("\\(R\\)", "", deb)
	deb <- gsub("\\(j\\)", "", deb)
	deb <- gsub("\\(AUDIENCE BOOING\\)", "", deb)
	deb <- gsub("\\(Speaks in Spanish.\\)", "", deb)
	deb <- gsub("\\(Cheers, applause.\\)", "", deb)
	deb <- gsub("\\(Applause.\\)", "", deb)
	deb <- gsub("\\(cheers, applause\\)", "", deb)
	deb <- gsub("\\(off mike\\)", "", deb)
	deb <- gsub("\\(applause\\)", "", deb)
	deb <- gsub("\\(dollars\\)", "", deb)
	deb <- gsub("\\(interrupted by continued cheers, applause\\)", "", deb)
	deb <- gsub("\\(Remarks in Spanish.\\)", "", deb)
	deb <- gsub("\\(by cheers, applause\\)", "", deb)
	deb <- gsub("\\(chuckles\\)", "", deb)
	deb <- gsub("\\(laughter, cheers, applause\\)", "", deb)
	deb <- gsub("\\(Laughter, applause.\\)", "", deb)
	deb <- gsub("a\\(n\\)", "an", deb)
	deb <- gsub("\\(interrupted by applause\\)", "", deb)
	deb <- gsub("\\(remarks in Spanish\\)", "", deb)
	deb <- gsub("\\(Announcements.\\)", "", deb)
	deb <- gsub("\\(Off mike\\)", "", deb)
	deb <- gsub("\\(jeers from the audience\\)", "", deb)
	deb <- gsub("\\(Thank you \\?\\)", "", deb)
	deb <- gsub("\\(Off mike.\\)", "", deb)
	deb <- gsub("\\(interrupted by cheers, applause\\)", "", deb)
	deb <- gsub("\\(laughter, applause\\)", "", deb)
	deb <- gsub("\\(Applause continues.\\)", "", deb)
	deb <- gsub("\\(Chuckles.\\)", "", deb)
	deb <- gsub("\\(Laughs.\\)", "", deb)
	deb <- gsub("\\(Laughter, cheers, applause.\\)", "", deb)
	deb <- gsub("\\(Laughter, boos.\\)", "", deb)
	deb <- gsub("\\(can/can't \\?\\)", "can't", deb) 
	deb <- gsub("\\(trillion dollars\\)", "", deb)
	deb <- gsub("\\(Scattered applause.\\)", "", deb)
	deb <- gsub("\\(million\\)", "", deb)
	deb <- gsub("\\(Democratic/democratic \\?\\)", "democratic", deb)
	deb <- gsub("\\(sp\\)", "", deb)
	deb <- gsub("\\(Inaudible.\\)" , "", deb)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
	deb <- gsub("\\(Joined in progress from the source\\)", "", deb) 
	deb <- gsub("\\(laughs, cheers, applause\\)", "", deb)
	deb <- gsub("\\(Applauds.\\)", "", deb)
	deb <- gsub("\\(be \\?\\)", "be", deb)
	deb <- gsub("\\(Pause, laughter.\\)", "", deb)
	deb <- gsub("\\(scattered applause\\)", "", deb)
	deb <- gsub("\\(Booing, shouting.\\)", "", deb)
	deb <- gsub("\\(,000\\)", "", deb)
	deb <- gsub("\\(name inaudible\\)", "", deb)
	deb <- gsub("401\\(k\\)", "401k", deb)
	deb <- gsub("\\(billion dollars\\)", "", deb)

	deb <- gsub("\\[Applause\\]", "", deb)
	deb <- gsub("\\[Commercial Break\\]", "", deb)
	deb <- gsub("\\[ joined in progress\\]", "", deb)
	deb <- gsub("\\(speaking mandarin\\)", "", deb)
	deb <- gsub("\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*", "", deb)


	#deb <- gsub("\\(Pre-recorded remarks.\\)", "", deb) # Use later after debates have been put together.
	#deb <- gsub("\\(From videotape.\\)", "", deb)