####
#merge and filter EC and biomet data for
#ogletree and ponder stations
#
#
#last edit 10/02/2017


###functions

#function to read in data

GrabData <- function(f2g){
  #Metddd <- read.table(f2g, header=FALSE, sep = ",", skip = 2, as.is = TRUE)# strip header & get the data
  Metddd <- read.csv(f2g, header=FALSE, sep = ",", skip = 2, stringsAsFactors = FALSE) # awf - 5/13/2015
  D <- as.data.frame(Metddd) #matrix to data frame -> data is D  
  hhh <- read.table(f2g, sep = ",", skip = 0, nrows = 1) # get the header
  header <- lapply(hhh, as.character)
  
  colnames(D) <- header #put the header on D
  
  return(D) 
}

#stations


#get station EC out
EC_Data<-GrabData("ogletreeEddyproOut.csv")

# get station biomet
Met_Data<-GrabData("ogletreeBiomet.csv")

#creat POSIX time stamp for both files to merge on



#filter EC data

#max min filters

#direction filters


