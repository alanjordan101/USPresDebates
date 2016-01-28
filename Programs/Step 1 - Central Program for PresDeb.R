library(rvest)
library(plyr)
library(dplyr)


################################################################################################################################################################
################################################################################################################################################################

# Set your directory here:

base<-file.path("C:\\Users\\alan\\Documents\\GitHub\\USPresDebates") # Set to your directory



setwd(base)
debTrans <- file.path(base,"Debate Transcripts") 
Func <- file.path(base,"Programs") 
Rfiles <- file.path(base,"R Data Files") 
debListFP <- file.path(base,"DebateList") 
setwd(Func)
source("Functions_USPresDebates.R")
################################################################################################################################################################
################################################################################################################################################################




setwd(Func)
source("Step 2A - Process Transcripts forms 1& 2.R")

setwd(Func)
source("Step 2B - Process Transcripts forms 4.R")

setwd(Func)
source("Step 2C - Process CPD Transcripts.R")

setwd(Func)
source("Step 2D - Process CPD Transcripts form 7.R")


setwd(Func)
#source("Step 3 - Assemble Debates.R")
