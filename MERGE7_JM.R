###############################################################################
###############################################################################
#Requires running EddyPro to calculate fluxes
#--> Burba correction
#--> No Burba Correction

#############################IMPORTANT##################################
#This will substitute Burba corrected fluxes for 30 min periods below 5 C

#--------------------------------------------------------------------------
#MERGE biomet data
#Requires that Biomet data was entered into EddyPro 
#----> this means that the biomet data was also written by EddyPro

#pull in Biomet data and processed fluxes that were both output by EddyPro

#the data files should not have any gaps 
#--> this was chosen in EddyPro to facilitate lining up the files
rm(list = ls())
###############################################################################
#############################IMPORTANT#########################################
###############################################################################
#update site & year & the EddyPro output you want to use
#site <- '176d'
#site <- '176'
site <- '138h08'
#site <- '138i23'
version <- 'NoBurba'

#-----------------------------------------------------------------------------#
#directory structure
homebase <- 'C:/Users/aaron.fellows/Desktop/DataProcessing/ECprocessing/FastProc/EddyProOut/'
outbase  <- 'C:/Users/aaron.fellows/Desktop/DataProcessing/ECprocessing/'
MERGEext <- 'MergeProc/MERGE/'
#-----------------------------------------------------------------------------#

#NOTE: updated to new processing on 5/13/2015 - awf
#particular file name
if (site == '176d') {
  file <- 'eddypro_NoBurba_full_output_2015-03-23T100216.csv'
}

if (site == '176') {
  #file <- 'All.csv'  -- updated to new processing on 5/13/2015
  file <- 'eddypro_NoBurba2_full_output_2015-04-17T182242.csv'
}

if (site == '138i23') {
  file <- 'eddypro_NoBurba_full_output_2015-03-26T190551.csv'
}

if (site == '138h08') {
  file <- 'eddypro_NoBurba_full_output_2015-03-30T151450.csv'
}
###############################################################################
#-----------------------------------------------------------------------------#
#EC data 
#-----------------------------------------------------------------------------#
###############################################################################

#This functions reads in the EC data and puts a header on it
GrabEddyCovData <- function(f2g,version){
  #ECddd <- read.table(f2g, header=FALSE, sep = ",", skip = 3, as.is = TRUE)# strip header & get the data - 5/13/2015
  ECddd <- read.table(f2g, header=FALSE, sep = ",", skip = 3, stringsAsFactors = FALSE)# strip header & get the data
  D <- as.data.frame(ECddd) #matrix to data frame -> data is D  
  hhh <- read.table(f2g, sep = ",", skip = 1, nrows = 1) # get the header
  
  if (version == 'NoBurba'){
    print(version)
    header <- lapply(hhh, as.character)
  }  else {
    print('No Version of EC data')
  }
  
  colnames(D) <- header #put the header on D
  
  return(D) # spit it out
}


GrabMetData <- function(f2g){
  #Metddd <- read.table(f2g, header=FALSE, sep = ",", skip = 2, as.is = TRUE)# strip header & get the data
  Metddd <- read.table(f2g, header=FALSE, sep = ",", skip = 2,, stringsAsFactors = FALSE) # awf - 5/13/2015
  D <- as.data.frame(Metddd) #matrix to data frame -> data is D  
  hhh <- read.table(f2g, sep = ",", skip = 0, nrows = 1) # get the header
  header <- lapply(hhh, as.character)

  colnames(D) <- header #put the header on D
  
  return(D) # spit it out
}

#fraction of a day to 4 decimal places 
#since 2002 - 2004 , 2008, and 2012 were leap years
Time4Merge <- function(jday, ihr, imn, yr){
  
  #account for the year
  bufferdays <- yr * NA
  focus <- yr == 2002
  bufferdays[focus] <- 0
  focus <- yr == 2003
  bufferdays[focus] <- 365
  focus <- yr == 2004
  bufferdays[focus] <- 365+365
  focus <- yr == 2005
  bufferdays[focus] <- 365+365+366
  focus <- yr == 2006
  bufferdays[focus] <- 365+365+366+365
  focus <- yr == 2007
  bufferdays[focus] <- 365+365+366+365+365
  focus <- yr == 2008
  bufferdays[focus] <- 365+365+366+365+365+365
  focus <- yr == 2009
  bufferdays[focus] <- 365+365+366+365+365+365+366
  focus <- yr == 2010
  bufferdays[focus] <- 365+365+366+365+365+365+366+365
  focus <- yr == 2011
  bufferdays[focus] <- 365+365+366+365+365+365+366+365+365
  focus <- yr == 2012
  bufferdays[focus] <- 365+365+366+365+365+365+366+365+365+365
  focus <- yr == 2013
  bufferdays[focus] <- 365+365+366+365+365+365+366+365+365+365+366
  
  #account for hr
  hrd <- ihr/24
  
  #account for mn
  mnd <- imn/(60*24)
  
  #total days
  D <- jday + hrd + mnd + bufferdays
  D <- (round(D * 10000))/10000
  
  return(D) # spit it out
}

#-----------------------------------------------------------------------------#
#get the EC data WITHOUT the Burba Correction
#-----------------------------------------------------------------------------#
version <- 'NoBurba'
ECfile2get <- paste0(homebase, version, '/', site, '/', file)
ECDATA <- GrabEddyCovData(ECfile2get, version)

#-------------------------------------------------------------------------
#EC time - build a time stamp for merging the data 
timetime  <- strsplit(ECDATA$date, "-")
tme <- as.data.frame(timetime)
ecyr <- t(tme[1,])
ecyr <- as.double(ecyr)


#for 176 - time stamp is read in differently
# timetime  <- strsplit(ECDATA$date, "/")
# tme <- as.data.frame(timetime)
# ecyr <- t(tme[3,])
# ecyr <- as.double(ecyr)

#days - eddypro labels by the end of the hour
ecjday <- floor(ECDATA$DOY) #check for rounding errors on floor

#hours & min
timetime  <- strsplit(ECDATA$time, ":")
tme <- as.data.frame(timetime)
echr <- t(tme[1,]) #slow but strait forward
echr <- as.double(echr)

ecmn <- t(tme[2,]) #slow but strait forward
ecmn <- as.double(ecmn)

ectime <- Time4Merge(ecjday, echr, ecmn, ecyr) 

#put the time col on the EC data
ECDATA <- cbind(ECDATA, ectime)
#-----------------------------------------------------------------------------#
#R can not deal with the some of the notation --> rewrite the names 
#-----------------------------------------------------------------------------#
#ust name
i <- which(colnames(ECDATA) == 'u*')
colnames(ECDATA)[i] <- "ust"

#footprint
i <- which(colnames(ECDATA) == 'x_10%')
colnames(ECDATA)[i] <- "footprint10per"
i <- which(colnames(ECDATA) == 'x_30%')
colnames(ECDATA)[i] <- "footprint30per"
i <- which(colnames(ECDATA) == 'x_50%')
colnames(ECDATA)[i] <- "footprint50per"
i <- which(colnames(ECDATA) == 'x_70%')
colnames(ECDATA)[i] <- "footprint70per"
i <- which(colnames(ECDATA) == 'x_90%')
colnames(ECDATA)[i] <- "footprint90per"

#stability
i <- which(colnames(ECDATA) == '(z-d)/L')
colnames(ECDATA)[i] <- "stability"

#-----------------------------------------------------------------------------#
#- BIOMET DATA ---------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#updated the biomet data on 5/13/2015 - included new radiation data posted by Gerald ~ 1 week earlier - awf
#now get the met data
if (site == '176d') {
  methome <- 'C:/Users/aaron.fellows/Desktop/DataProcessing/ECprocessing/MergeProc/METfields/EddyProFormat/176d/'
  
  file2get <- paste0(methome, 'y2007/01MET176d2007.csv')
  met2007 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2008/01MET176d2008.csv')
  met2008 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2009/01MET176d2009.csv')
  met2009 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2010/01MET176d2010.csv')
  met2010 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2011/01MET176d2011.csv')
  met2011 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2012/01MET176d2012.csv')
  met2012 <- GrabMetData(file2get)
  
  BIOMETDATA <- rbind(met2007,met2008,met2009,met2010,met2011,met2012)
  print('met data from 176d')
}

#now get the met data
if (site == '176') {
  methome <- 'C:/Users/aaron.fellows/Desktop/DataProcessing/ECprocessing/MergeProc/METfields/EddyProFormat/176/'
  
  file2get <- paste0(methome, 'y2004/01MET1762004.csv')
  met2004 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2005/01MET1762005.csv')
  met2005 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2006/01MET1762006.csv')
  met2006 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2007/01MET1762007.csv')
  met2007 <- GrabMetData(file2get) 
  
  
  BIOMETDATA <- rbind(met2004,met2005,met2006,met2007)
  print('met data from 176')
}


#now get the met data
if (site == '138i23') {
  methome <- 'C:/Users/aaron.fellows/Desktop/DataProcessing/ECprocessing/MergeProc/METfields/EddyProFormat/138i23/'
  
  file2get <- paste0(methome, 'y2004/01MET138i232004.csv')
  met2004 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2005/01MET138i232005.csv')
  met2005 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2006/01MET138i232006.csv')
  met2006 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2007/01MET138i232007.csv')
  met2007 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2008/01MET138i232008.csv')
  met2008 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2009/01MET138i232009.csv')
  met2009 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2010/01MET138i232010.csv')
  met2010 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2011/01MET138i232011.csv')
  met2011 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2012/01MET138i232012.csv')
  met2012 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2013/01MET138i232013.csv')
  met2013 <- GrabMetData(file2get)
  
  BIOMETDATA <- rbind(met2004,met2005,met2006,met2007,met2008,met2009,met2010,met2011,met2012,met2013)
  print('met data from 138i23')
}

#now get the met data
if (site == '138h08') {
  methome <- 'C:/Users/aaron.fellows/Desktop/DataProcessing/ECprocessing/MergeProc/METfields/EddyProFormat/138h08/'
  
  file2get <- paste0(methome, 'y2005/01MET138h082005.csv')
  met2005 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2006/01MET138h082006.csv')
  met2006 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2007/01MET138h082007.csv')
  met2007 <- GrabMetData(file2get) 
  
  file2get <- paste0(methome, 'y2008/01MET138h082008.csv')
  met2008 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2009/01MET138h082009.csv')
  met2009 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2010/01MET138h082010.csv')
  met2010 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2011/01MET138h082011.csv')
  met2011 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2012/01MET138h082012.csv')
  met2012 <- GrabMetData(file2get)
  
  file2get <- paste0(methome, 'y2013/01MET138h082013.csv')
  met2013 <- GrabMetData(file2get)
  
  BIOMETDATA <- rbind(met2005,met2006,met2007,met2008,met2009,met2010,met2011,met2012,met2013)
  print('met data from 138h08')
}

#get a time for merging
biomettime <- Time4Merge(BIOMETDATA$Timestamp_1, BIOMETDATA$Timestamp_2, BIOMETDATA$Timestamp_3, BIOMETDATA$Timestamp_4) 

#put the time col on the BIOMETDATA
BIOMETDATA <- cbind(BIOMETDATA, biomettime)
###############################################################################
#-----------------------------------------------------------------------------#
#do the merge 
#-----------------------------------------------------------------------------#
#MERGE behavior works as expected - awf 4/9/2015
#see example
# A <- matrix(c(1,3,4,5,5.5, 6,8,9,10,20), nrow = 5, ncol = 2, byrow = FALSE)
# colnames(A) <- c("a", "b")
# B <- matrix(c(2,3.2,5,5.5, 7,8,10,21), nrow = 4, ncol = 2, byrow = FALSE)
# colnames(B) <- c("a", "b")
# C <- merge(A, B, by.x = "a", by.y = "a", all = TRUE)
###############################################################################
MERGE <- merge(ECDATA, BIOMETDATA, by.x = "ectime", by.y = "biomettime", all = TRUE)

###############################################################################
#save
###############################################################################
#this is the working file
foutcurrent <- paste0('EC', site, 'MERGE')

#write over the working file as a csv
file2save <- paste0(outbase, MERGEext, site, '/', foutcurrent, '.RData')
save(MERGE, file = file2save)

file2save <- paste0(outbase, MERGEext, site, '/', foutcurrent, '.csv')
write.csv(MERGE, file = file2save) 

#-----------------------------------------------------------------------------#
#---now make a backup file
#-----------------------------------------------------------------------------#
#set the date on the output
procData <- gsub(" ", "", format(Sys.time(), "%b %d %Y")) # gives month day year - collapses
fout <- paste0('EC', site, 'MERGE')

#as a data.frame - backup
file2save <- paste0(outbase, MERGEext, fout, procData, '.RData')
save(MERGE, file = file2save)

#as a csv - backup
file2save <- paste0(outbase, MERGEext, fout, procData, '.csv')
write.csv(MERGE, file = file2save) 

###############################################################################
###############################################################################
