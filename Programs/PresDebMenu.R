library(rvest)
library(plyr)
library(dplyr)

################################################################################################################################################################
################################################################################################################################################################

# Set your directory here:

base<-file.path("C:\\Users\\admin\\Documents\\GitHub\\USPresDebates") # Set to your directory



setwd(base)
debTrans <- file.path(base,"Debate Transcripts") 
Func <- file.path(base,"Programs") 
Rfiles <- file.path(base,"R Data Files") 
setwd(Func)
source("Functions_USPresDebates.R")
################################################################################################################################################################
################################################################################################################################################################



setwd(Func)
source("Step 1 - Scrape all transcript files from web.R")

setwd(Func)
source("Step 2A - Process Transcripts forms 1& 2.R")

setwd(Func)
source("Step 2B - Process Transcripts forms 4.R")

setwd(Func)
source("Step 2C - Process CPD Transcripts.R")

setwd(Func)
source("Step 2D - Process CPD Transcripts form 7.R")


setwd(Func)
#source("Step 3 - Join data files")
