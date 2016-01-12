# USPresDebates
Process US presidential debate transcripts from http://www.presidency.ucsb.edu/


A work in progress here.
I'm trying to read in all the presidential debate transcripts and put them in an R data frame for text analysis. 


Notes:
1. Current code for scraping misses the first sentence/paragraph because there is no "<"p">" tag.
2. Transcripts are divided divided into two groups: 
A - Transcripts that can be scraped from the website and processed with code.
B - Transcripts that are so far beyond my ability that I will edit them by hand and save edited copies to a folder for further
     processing.




Process

Track A
Step 1 - Scrape transcripts from website and save as r files for further processing.
Step 2 - Pull r files back in and use sub function to make individual edits so transcripts can be split by parsedf function into person           and message. Save r files.
Step 3 - Process r files

Track B
Step 1 - Hand edit transcripts from website and save as word .docx files for further processing.
Step 2 - Process word files and save r files into same directory as track A r files

Join all rfiles from Track A and Track B
Rename speakers (there are 3 bushes, two Kennedy's and two Clintons possibly other duplicate names).
Create variable for type of speaker (candidate, moderator and other)


