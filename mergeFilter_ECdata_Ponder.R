####
#merge and filter EC and biomet data for
#ponder 
#
#
#last edit 10/02/2017


rm(list = ls())
#libraries
library("tidyverse")
library("lubridate")
###functions

#function to read in data

GetData <- function(f2g){
  #Metddd <- read.table(f2g, header=FALSE, sep = ",", skip = 2, as.is = TRUE)# strip header & get the data
  Metddd <- read.csv(f2g, header=FALSE, sep = ",", skip = 2, stringsAsFactors = FALSE) # awf - 5/13/2015
  D <- as.data.frame(Metddd) #matrix to data frame -> data is D  
  hhh <- read.table(f2g, sep = ",", skip = 0, nrows = 1) # get the header
  header <- lapply(hhh, as.character)
  
  colnames(D) <- header #put the header on D
  
  return(D) 
}

#get station EC out
EC_Data1<-GetData("data/ponderEddyproOut.csv")

# get station biomet
Met_Data1<-GetData("data/ponderBiomet.csv")


#creat posix time for Biomet
#dt <- as.POSIXct(Met_Data1[, "TIMESTAMP"],"%m/%d/%Y %H:%M:%S")
dt <- as.POSIXct(Met_Data1[, "TIMESTAMP"],"%Y-%m-%d %H:%M:%S")
dt<-as.data.frame(dt,tz="America/New_York")
Met_Data<-bind_cols(dt,Met_Data1)

#create posix time for both files to merge on
#create posix time for EC
EC_Data<-unite_(EC_Data1,"date",c("date","time")," ")
dt <- as.POSIXct(EC_Data[, "date"],"%Y-%m-%d %H:%M")
dt<-as.data.frame(dt,tz="America/New_York")
EC_Data<-bind_cols(dt,EC_Data)



#the cambell biomet data has many calculations including 
#all the sonic calculations subset to only the biomet data

#plotting
#ggplot(Met_Data,aes(x=dt,y=RH_tmpr_rh_mean))+geom_point()
#ggplot(W_Data,aes(x=dt,y=RH))+geom_point()

#W_Data$RH

#ggplot(Met_Data,aes(x=dt,y=Rn_Avg))+geom_point()

#ggplot(Met_Data,aes(x=dt,y=Rs_incoming_Avg))+geom_point()


# ggplot(W_Data,aes(x=dt,y=cs65x_wcr_Avg1))+
#   geom_point()
# ggplot(W_Data,aes(x=dt,y=cs65x_wcr_Avg2))+
#   geom_point()






#join on POSIX time

W_Data<-full_join(Met_Data,EC_Data,by="dt")
#sort by date
W_Data<-arrange(W_Data,dt)



#filter EC data ogletree sensor facing 190
#sum(is.na(W_Data))
#recode -9999 and NaN as NA
W_Data[W_Data==-9999] <- NA
#W_Data[W_Data==NaN] <- NA

#change column names that R cant work with
names(W_Data)[names(W_Data) == 'u*'] <- 'Ustar'
names(W_Data)[names(W_Data) == 'Tsoil_mean(1)'] <- 'Tsoil_mean1'
names(W_Data)[names(W_Data) == 'Tsoil_mean(2)'] <- 'Tsoil_mean2'
names(W_Data)[names(W_Data) == 'cs65x_wcr_Avg(1)'] <- 'cs65x_wcr_Avg1'
names(W_Data)[names(W_Data) == 'cs65x_wcr_Avg(2)'] <- 'cs65x_wcr_Avg2'
names(W_Data)[names(W_Data) == 'x_70%'] <- 'x_70'
names(W_Data)[names(W_Data) == 'x_90%'] <- 'x_90'
#d<-select(W_Data,x_70)


#set up filters
sum(is.na(W_Data$co2_flux))
sum(!is.na(W_Data$co2_flux))

#explore
# tibble(W_Data$x_70>120)
# sum(W_Data$wind_dir>350, na.rm = TRUE)
# sum(W_Data$wind_dir<20, na.rm = TRUE)
# sum(!is.na(W_Data$wind_dir))


#remove fluxes flagged >1
#sum(W_Data$qc_LE>1, na.rm = TRUE)
#sum(W_Data$qc_co2_flux>1, na.rm = TRUE)

W_Data$LE[W_Data$qc_LE>1] <- NA
W_Data$co2_flux[W_Data$qc_co2_flux>1] <- NA

#filter flux on footprint 260 to 90 between 5-19-2016 and 8-5-2016
#                       wind_dir > 260 ~ NA,
#                       wind_dir < 90  ~ NA
#ggplot(W_Data,aes(x=dt,y=co2_flux))+geom_point()


ft<-filter(W_Data,(wind_dir>260 & dt >= start & dt <= end))

start="2016-5-19 00:00"
end="2016-8-5 23:30"  
W_Data$co2_flux[W_Data$wind_dir>260 & W_Data$dt >= start & W_Data$dt <= end] <- NA
W_Data$co2_flux[W_Data$wind_dir<90 & W_Data$dt >= start & W_Data$dt <= end] <- NA


W_Data$LE[W_Data$wind_dir>260 & W_Data$dt >= start & W_Data$dt <= end] <- NA
W_Data$LE[W_Data$wind_dir<90 & W_Data$dt >= start & W_Data$dt <= end] <- NA


# EddyP.f %>% mutate(EddyP.f$LE,LE1 = case_when(
#                       dt <= start | dt >= end ~ LE,
#                       wind_dir > 260 ~ NA,
#                       wind_dir < 90  ~ NA
# 
#    
#                    )
# )
# ?mutate
# ?case_when

#remove flux between 5/11/2016 and 5/19/2016 becasue irgason was to low compared to plant 
end="2016-5-19 16:00"
start="2016-5-11 00:00"  
W_Data$LE[W_Data$dt >= start & W_Data$dt <= end] <- NA
W_Data$LE[W_Data$dt >= start & W_Data$dt <= end] <- NA


W_Data$LE[W_Data$wind_dir>260 & dt >= start & dt <= end] <- NA
W_Data$LE[W_Data$wind_dir<90 & dt >= start & dt <= end] <- NA




#max min filters

#remove C02 fluxes <-70 and >30 umol m-2 s-1
# sum(W_Data$co2_flux>30, na.rm = TRUE)
# sum(W_Data$co2_flux<(-70), na.rm = TRUE)

W_Data$co2_flux[W_Data$co2_flux>30] <- NA
W_Data$co2_flux[W_Data$co2_flux<(-50)] <- NA


#remove nightime -NEE
#Rg<=0 & NEE<
W_Data$co2_flux[W_Data$Rs_incoming_Avg<=0 & W_Data$co2_flux<0] <- NA


#remove LE flux <-20 and >600 
#sum(W_Data$LE>600, na.rm = TRUE)
#sum(W_Data$LE<(-20), na.rm = TRUE)
W_Data$LE[W_Data$LE>600] <- NA
W_Data$LE[W_Data$LE<(-20)] <- NA


#Check Rg set minimum to 0
max(W_Data$Rs_incoming_Avg, na.rm = TRUE)
min(W_Data$Rs_incoming_Avg, na.rm = TRUE)
W_Data$Rs_incoming_Avg[W_Data$Rs_incoming_Avg<0] <- 0




#ggplot(W_Data,aes(x=dt,y=T_tmpr_rh_mean))+geom_point()

#check max and min air
#remove air temp < -30 and > 50 C
#temp<-select(W_Data,T_tmpr_rh_mean)
#sum(W_Data$T_tmpr_rh_mean>28, na.rm = TRUE)
#W_Data$T_tmpr_rh_mean[W_Data$T_tmpr_rh_mean>50] <- NA
#W_Data$T_tmpr_rh_mean[W_Data$T_tmpr_rh_mean<(-30)] <- NA
max(W_Data$T_tmpr_rh_mean,na.rm=TRUE)
min(W_Data$T_tmpr_rh_mean,na.rm=TRUE)

#check max and min soil
#remove soil temp <-30 and >50 C
max(W_Data$Tsoil_mean1,na.rm=TRUE)
min(W_Data$Tsoil_mean1,na.rm=TRUE)
max(W_Data$Tsoil_mean2,na.rm=TRUE)
min(W_Data$Tsoil_mean2,na.rm=TRUE)


#several bad data points on both water sensors
#remove data if % water greater than 50
max(W_Data$cs65x_wcr_Avg1,na.rm=TRUE)
min(W_Data$cs65x_wcr_Avg1,na.rm=TRUE)
max(W_Data$cs65x_wcr_Avg2,na.rm=TRUE)
min(W_Data$cs65x_wcr_Avg2,na.rm=TRUE)

sum(W_Data$cs65x_wcr_Avg1>.5, na.rm = TRUE)
sum(W_Data$cs65x_wcr_Avg2>.5, na.rm = TRUE)

W_Data$cs65x_wcr_Avg1[W_Data$cs65x_wcr_Avg1>.5] <- NA
W_Data$cs65x_wcr_Avg2[W_Data$cs65x_wcr_Avg2>.5] <- NA


#check max and min rH

max(W_Data$RH,na.rm=TRUE)
min(W_Data$RH,na.rm=TRUE)
max(W_Data$RH_tmpr_rh_mean,na.rm=TRUE)
min(W_Data$RH_tmpr_rh_mean,na.rm=TRUE)

W_Data$RH_tmpr_rh_mean[W_Data$RH_tmpr_rh_mean>100] <- 100

#ggplot(W_Data,aes(x=dt,y=RH_tmpr_rh_mean))+geom_point()
#ggplot(W_Data,aes(x=dt,y=RH))+geom_point()



#count %NA in flux
sum(is.na(W_Data$co2_flux))
sum(!is.na(W_Data$co2_flux))

#get data into REddyProc Format
#create year day hr colums from Posix

Year<-year(W_Data$dt)
DoY<-yday(W_Data$dt)
Hour<-hour(W_Data$dt)
Minute<-minute(W_Data$dt)/6

#time Data frame
time<-as.tibble(cbind(Year,DoY,Hour,Minute))
time<-unite(time,Hour,c(Hour,Minute),sep=".")
            
#Eddy Pro starts the day at 0 and records the data from 0-.5hr
#Reddy proc starts the day at .5 and this is record form 0-.5
#to change time format to reddy proc add 

# time$Hour<-as.numeric(time$Hour)
# time$Hour<-time$Hour+.5
# time$Hour[time$Hour==24.0] <- 0.0

W_Data<-bind_cols(time,W_Data)


#select data from the merged file and rename for ReddyProc
#have 2 soil water and soil temp sensors so take avg
#plotting
# ggplot(W_Data,aes(x=dt,y=Tsoil_mean1))+
#        geom_point()
# ggplot(W_Data,aes(x=dt,y=Tsoil_mean2))+
#   geom_point()
# ggplot(W_Data,aes(x=dt,y=cs65x_wcr_Avg1))+
#   geom_point()
# ggplot(W_Data,aes(x=dt,y=cs65x_wcr_Avg2))+
#   geom_point()

                  
sum(is.na(W_Data$Tsoil_mean1))
sum(is.na(W_Data$Tsoil_mean2))
sum(is.na(W_Data$cs65x_wcr_Avg1))
sum(is.na(W_Data$cs65x_wcr_Avg2))


W_Data<-mutate(W_Data,Tsoil=(rowMeans(cbind2(Tsoil_mean1,Tsoil_mean2),na.rm=TRUE)))

W_Data<-mutate(W_Data,Wsoil=(rowMeans(cbind2(cs65x_wcr_Avg1,cs65x_wcr_Avg2),na.rm=TRUE)))

#select data for reddyProc

S_Data<-select(W_Data,Year,DoY,Hour,NEE=co2_flux,LE,H,Ustar,Rg=Rs_incoming_Avg,Rnet=Rn_Avg,rH=RH_tmpr_rh_mean,Tair=T_tmpr_rh_mean,Tsoil,Wsoil,Tcrop=ir_tmpr_Avg,Rain=precip_Tot)


#ReddyProc needs full days start at 0 end at 23.5
# trim to start and end
S_Data<-filter(S_Data,Year==2015 & DoY>223 | Year==2016 )
#S_Data<-slice(S_Data,2:nrow(S_Data))

#ReddyProc header
head1 <- c('Year','DoY' ,'Hour','NEE'       ,'LE'  ,'H'   ,'Ustar','Rg'  ,'Rnet','rH','Tair','Tsoil','Wsoil','Tcrop','Rain')
head2 <- c('--'  ,'--'  , '--' ,'umolm-2s-1','Wm-2','Wm-2','ms-1' ,'Wm-2','Wm-2','%' ,'degC','degC' ,'%'    ,'degC' ,'mm')

#write it to file

#function writes a tab delimited text file with no row names and no quotes around the numbers
write.csv3 <- function(d, file, hd, ut) {
  opts <- options(useFancyQuotes = FALSE)
  on.exit(options(opts))
  h1 <- gsub(",", "\t", paste(c(names(d)), collapse = ","))
  h2 <- gsub(",", "\t", paste(rbind(ut), collapse = ","))
  writeLines(paste(h1, h2, sep = "\n"), file)
  write.table(d, file, sep="\t", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
}

write.csv3(S_Data, "out/ponderFiltRprocIn.txt", head1[1:15], head2[1:15]) 






















# trim first to start where you want
start <- as.POSIXct("2016-04-01 00:00")
lookat <- which(dt > start)
dt <- dt[lookat]
Flux.data <- Flux.data[lookat, ]
dtdf <- as.data.frame(dt)







#creat POSIX time for biomet
dt <- as.POSIXct(Met_Data1[, "TIMESTAMP"], "%Y-%m-%d %H:%M:%S")
dt<-as.data.frame(dt)
Met_Data<-bind_cols(dt,Met_Data1)





#creat POSIX time stamp for both files to merge on
EC_dt  <- strsplit(EC_Data$date, "-")
EC_dt <- as.data.frame(EC_dt)

#EC year
ECyr <- t(EC_dt[1,])
ECyr <- as.double(ECyr)

#ECdays - eddypro labels by the end of the hour
ecjday <- floor(ECdata$DOY) #check for rounding errors on floor

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

#creat POSIX time for biomet

dt <- as.POSIXct(Met_Data1[, "TIMESTAMP"], "%Y-%m-%d %H:%M:%S")
dt<-as.data.frame(dt)
Met_Data<-bind_cols(dt,Met_Data1)



dt <- as.POSIXct(Met_Data[, "TIMESTAMP"])
ncol <- length(Met_Data[1, ])
# sort because file order isn't always right
sorted <- order(dt)
dt <- dt[sorted]
Flux.data <- Flux.data[sorted, ]
# Done with concatenating files 

# trim first to start where you want
start <- as.POSIXct("2016-04-01 00:00")
lookat <- which(dt > start)
dt <- dt[lookat]
Flux.data <- Flux.data[lookat, ]
dtdf <- as.data.frame(dt)





#filter EC data

#max min filters

#direction filters


