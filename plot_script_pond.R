
#plotting scripts


rm(list = ls())
#libraries
library(tidyverse)
library(ggthemes)
library(scales)
library(lubridate)

#read in data

data2 <- read_tsv(
  #file = "out/OgeltreeResults.txt",
  file = "out/PonderResults.txt",
  col_names = FALSE,
  skip = 2
)

#get header
hhh <- read.table("out/OgeltreeResults.txt",sep="\t", skip = 0, nrows = 1) # get the header
header <- lapply(hhh, as.character)
colnames(data2) <- header #put the header on Data

#recode -9999  NA
data2[data2==-9999] <- NA

data2

#change some column names
#change column names that R cant work with
names(data2)[names(data2) == 'Date Time'] <- 'dt'

# convert to g /m2 /hh
data2 %>% mutate(NEEgmhh=((NEE_WithUstar_f*44.01*60*30) /(10^6)))->data2

#rho=1000 kg/m3 Lv = 2.5*10^6    1000*2.5*10^6*(1/1000)

#  H/rho = E conver to mm per m2 per hh

data2 %>% mutate(LEgmhh=((LE_WithUstar_f*60*30) /(2.5*10^6)))->data2

#annual sums
sum(data2$LEgmhh)
sum(data2$NEEgmhh)


maizePlant=as.numeric(strptime("2016-03-14","%Y-%m-%d"))
maizeHarvest=as.numeric(strptime("2016-08-08","%Y-%m-%d"))

miscanthusHarvest1
miscanthusHarvest1
miscanthusHarvest1


#simple scatter plots of data
p<-ggplot(data2,aes(x=dt,y=NEE_WithUstar_f))+
   geom_point()+geom_vline(xintercept=maizePlant, linetype="dotdash")+
  geom_vline(xintercept=maizeHarvest, linetype="dotdash")
 
p + theme_tufte() + geom_rangeframe()+ggtitle("b) Maize")+
  xlab("")+ylab("NEE (umol/m2/s)")
 

p<-ggplot(data2,aes(x=dt,y=LE_WithUstar_f))+
  geom_point()

p

p<-ggplot(data2,aes(x=dt,y=Reco_WithUstar))+
  geom_point()

p

p<-ggplot(data2,aes(x=dt,y=GPP_WithUstar_f))+
  geom_point()


#cumulative sum

data2 %>% group_by(Year) %>% mutate (cumNEE=cumsum(NEEgmhh)) ->data2

data2_2016<-filter(data2,Year==2016)

p<-ggplot(data2_2016,aes(x=dt,y=cumNEE*.01))+
  geom_point()+
  geom_vline(xintercept=maizePlant, linetype="dotdash")+
  geom_vline(xintercept=maizeHarvest, linetype="dotdash")

p + theme_tufte() + geom_rangeframe()+ggtitle("b) Maize")+
  xlab("")+ylab("cumulative NEE (Mg C ha-1)")




ggsave("out/PonderCumNEE.pdf",useDingbats=FALSE)



p


summarise(data2,mean(NEE, na.rm=TRUE))




data2 %>% group_by(Year) %>% summarise(cumsum(NEE_WithUstar_f))

?cumsum





 
 p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
   geom_point() +
   ggtitle("Cars")
 
 p2 <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(gear))) +
   geom_point() +
   ggtitle("Cars")
 
 p3 <- p2 + facet_wrap(~ am)
 
 
 












as.tibble(data2)
#barchart


#set factors
data2$End.Depth.cm <- factor(data2$End.Depth.cm)
data2$YEAR <- factor(data2$YEAR)
data2$Plot.. <- factor(data2$Plot..)

#summarize data

data2_summary <- data2 %>% 
  group_by(End.Depth.cm,YEAR) %>%   
  summarise(mean_C = mean(SD.Carbon..Kg.Ha.),  # calculates the mean of each group
            sd_C = sd(SD.Carbon..Kg.Ha.), # calculates the standard deviation of each group
            n_C = n(),  # calculates the sample size per group
            SE_C = sd(SD.Carbon..Kg.Ha.)/sqrt(n())) # calculates the standard error of each group

#plot data
SoilCPlot <- ggplot(data2_summary, aes(End.Depth.cm, mean_C, fill=YEAR)) + 
  geom_col(position=position_dodge()) +  
  geom_errorbar(aes(ymin = mean_C - sd_C, ymax = mean_C + sd_C),
                position=position_dodge(.9),width=0.2)

SoilCPlot  + labs(y="Soil Carbon (kg/ha) ± s.d.", x = "depth") + theme_classic()

write.csv(data2_summary,file="out/sCarbonSum.csv")

#total at each plot
data2_summary2 <- data2 %>% 
  group_by(Plot..,YEAR) %>% mutate (sumC=sum(SD.Carbon..Kg.Ha.) ) %>%
  summarise(YearC = mean(sumC))  # calculates the mean of each group


yearSummary<-group_by(data2_summary2,YEAR) %>%
  summarise(mean_C = mean(YearC),  # calculates the mean of each group
            sd_C = sd(YearC), # calculates the standard deviation of each group
            n_C = n(),  # calculates the sample size per group
            SE_C = sd(YearC)/sqrt(n())) # calculates the standard error of each group


#plot data
SoilCPlot <- ggplot(yearSummary, aes(YEAR, mean_C)) + 
  geom_col(position=position_dodge()) +  
  geom_errorbar(aes(ymin = mean_C - sd_C, ymax = mean_C + sd_C),
                position=position_dodge(.9),width=0.2)

SoilCPlot  + labs(y="Soil Carbon (kg/ha) ± s.d.", x = "depth") + theme_classic()


#summarize data soil mass

data2_summary <- data2 %>% 
  group_by(End.Depth.cm,YEAR) %>%   
  summarise(mean_C = mean(SD.Soil.Mass..Kg.Ha.),  # calculates the mean of each group
            sd_C = sd(SD.Soil.Mass..Kg.Ha.), # calculates the standard deviation of each group
            n_C = n(),  # calculates the sample size per group
            SE_C = sd(SD.Soil.Mass..Kg.Ha.)/sqrt(n())) # calculates the standard error of each group

#plot data
SoilCPlot <- ggplot(data2_summary, aes(End.Depth.cm, mean_C, fill=YEAR)) + 
  geom_col(position=position_dodge()) +  
  geom_errorbar(aes(ymin = mean_C - sd_C, ymax = mean_C + sd_C),
                position=position_dodge(.9),width=0.2)

SoilCPlot  + labs(y="Soil Mass (kg/ha) ± s.d.", x = "depth") + theme_classic()

write.csv(data2_summary,file="out/sCarbonSum.csv")

#total at each plot
data2_summary2 <- data2 %>% 
  group_by(Plot..,YEAR) %>% mutate (sumC=sum(SD.Carbon..Kg.Ha.) ) %>%
  summarise(YearC = mean(sumC))  # calculates the mean of each group


yearSummary<-group_by(data2_summary2,YEAR) %>%
  summarise(mean_C = mean(YearC),  # calculates the mean of each group
            sd_C = sd(YearC), # calculates the standard deviation of each group
            n_C = n(),  # calculates the sample size per group
            SE_C = sd(YearC)/sqrt(n())) # calculates the standard error of each group


#plot data
SoilCPlot <- ggplot(yearSummary, aes(YEAR, mean_C)) + 
  geom_col(position=position_dodge()) +  
  geom_errorbar(aes(ymin = mean_C - sd_C, ymax = mean_C + sd_C),
                position=position_dodge(.9),width=0.2)

SoilCPlot  + labs(y="Soil Carbon (kg/ha) ± s.d.", x = "depth") + theme_classic()






















diffence = filter(data2, YEAR == "2017")$SD.Carbon..Kg.Ha.  - filter(data2, YEAR == "2012")$SD.Carbon..Kg.Ha.
mean(diffence)
sd(diffence)




data2 %>% group_by(YEAR) %>% mutate()






mutate(diff=SD.Carbon..Kg.Ha.~2012-SD.Carbon..Kg.Ha.~2017)


difference <- data2 %>% 
  group_by(YEAR) %>% mutate(diff=SD.Carbon..Kg.Ha.~2012-SD.Carbon..Kg.Ha.~2017)
 
mutate(data2,diffence = filter(data2, GROUP == "2012")$SD.Carbon..Kg.Ha.  - filter(data2, GROUP == "2017")$SD.Carbon..Kg.Ha.)  

mutate(data2,diffence = filter(data2, GROUP == "2012")$SD.Carbon..Kg.Ha.  - filter(data2, GROUP == "2017")$SD.Carbon..Kg.Ha.)  

  
   summarise(mean_C = mean(SD.Carbon..Kg.Ha.),  # calculates the mean of each group
          sd_C = sd(SD.Carbon..Kg.Ha.), # calculates the standard deviation of each group
          n_C = n(),  # calculates the sample size per group
          SE_C = sd(SD.Carbon..Kg.Ha.)/sqrt(n())) # calculates the standard error of each group

   
 mutate_each(data2,)
   
   diffence = filter(data2, YEAR == "2012")$SD.Carbon..Kg.Ha.  - filter(data2, YEAR == "2017")$SD.Carbon..Kg.Ha.
   
   

   
   
   
   
   

SoilCPlot <- ggplot(data2_summary, aes(End.Depth.cm, mean_C, fill=YEAR)) + 
  geom_col(position=position_dodge()) +  
  geom_errorbar(aes(ymin = mean_C - sd_C, ymax = mean_C + sd_C),
                position=position_dodge(.9),width=0.2)

SoilCPlot  + labs(y="Soil Carbon (kg/ha) ± s.d.", x = "depth") + theme_classic()

write.csv(data2_summary,file="out/sCarbonSum.csv")


?write.table()


# creating a small data.frame
GROUP <- rep(c("A","B"),each=10)
NUMBE <- rnorm(20,50,10)
datf <- data.frame(GROUP,NUMBE)

datf2 <- datf %>% group_by(GROUP) %>% mutate(cent = (NUMBE - mean(NUMBE))/sd(NUMBE))

gA <- datf2 %>% ungroup() %>% filter(GROUP == "A") %>% select(cent)
gB <- datf2 %>% ungroup() %>% filter(GROUP == "B") %>% select(cent)

gA - gB

mutate_(datf2,diffence = filter(datf2, GROUP == "A")$cent  - filter(datf2, GROUP == "B")$cent)

diffence = filter(datf2, GROUP == "A")$cent-filter(datf2, GROUP == "B")$cent
diffence

iris

summarise(iris,mean_PL = mean(Petal.Length))

Iris_summary <- iris %>% # the names of the new data frame and the data frame to be summarised
  group_by(Species) %>%   # the grouping variable
  summarise(mean_PL = mean(Petal.Length),  # calculates the mean of each group
            sd_PL = sd(Petal.Length), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(Petal.Length)/sqrt(n())) # calculates the standard error of each group


IrisPlotse <- ggplot(Iris_summary, aes(Species, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2)

IrisPlot 

IrisPlot + labs(y="Petal length (cm) ± s.d.", x = "Species") + theme_classic()

IrisPlotse <- ggplot(Iris_summary, aes(Species, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2)





Iris_summary <- iris %>% # the names of the new data frame and the data frame to be summarised
  group_by(Species) %>%   # the grouping variable
  summarise(mean_PL = mean(Petal.Length)),  # calculates the mean of each group
            sd_PL = sd(Petal.Length)), # calculates the standard deviation of each group
            n_PL = n),  # calculates the sample size per group
            SE_PL = sd(Petal.Length)/sqrt(n(Petal.Length))) # calculates the standard error of each group
?summarise
?n()

if (require("nycflights13")) {
  carriers <- group_by(flights, carrier)
  summarise(carriers, n())
  mutate(carriers, n = n())
  filter(carriers, n() < 100)
}





#summarize data
dataw <- summarySE(data2, measurevar="SD.Carbon..Kg.Ha.", groupvars=c("YEAR","End.Depth.cm"))
dataw


# Error bars represent standard error of the mean
dataw$End.Depth.cm <- factor(dataw$End.Depth.cm)
dataw$YEAR <- factor(dataw$YEAR)

ggplot(dataw, aes(x=End.Depth.cm, y=SD.Carbon..Kg.Ha., fill=YEAR)) + 
  geom_bar(position=position_dodge(),stat="identity") +
  geom_errorbar(aes(ymin=SD.Carbon..Kg.Ha.-se, ymax=SD.Carbon..Kg.Ha.+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))



t.test(dataw$SD.Carbon..Kg.Ha.~dataw$End.Depth.cm)



#calculate the average value for each depth and year with the *aggregate* function
bilan=aggregate(cbind(cond_A,cond_B,cond_C)~specie , data=data , mean)
rownames(bilan)=bilan[,1]
bilan=as.matrix(bilan[,-1])

?aggregate



#worked example
#Let's build a dataset : height of 10 sorgho and poacee sample in 3 environmental conditions (A, B, C)
A=c(rep("sorgho" , 10) , rep("poacee" , 10) )
B=rnorm(20,10,4)
C=rnorm(20,8,3)
D=rnorm(20,5,4)
data=data.frame(A,B,C,D)
colnames(data)=c("specie","cond_A","cond_B","cond_C")

#Let's calculate the average value for each condition and each specie with the *aggregate* function
bilan=aggregate(cbind(cond_A,cond_B,cond_C)~specie , data=data , mean)
rownames(bilan)=bilan[,1]
bilan=as.matrix(bilan[,-1])

#Then it is easy to make a classical barplot :
lim=1.2*max(bilan)
ze_barplot = barplot(bilan , beside=T , legend.text=T , col=c("blue" , "skyblue") , ylim=c(0,lim))

#I becomes a bit more tricky when we want to add the error bar representing the confidence interval.

#First I create a smell function that takes...in entry
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#Then I calculate the standard deviation for each specie and condition :
stdev=aggregate(cbind(cond_A,cond_B,cond_C)~specie , data=data , sd)
rownames(stdev)=stdev[,1]
stdev=as.matrix(stdev[,-1]) * 1.96 / 10

#I am ready to add the error bar on the plot using my "error bar" function !
ze_barplot = barplot(bilan , beside=T , legend.text=T,col=c("blue" , "skyblue") , ylim=c(0,lim) , ylab="height")
error.bar(ze_barplot,bilan, stdev)












#tried raster style plots but points too small

ggplot(grass,aes(x,y)) +  
  geom_tile(aes(fill=ndvi))

ggplot(grass,aes(x,y)) +  
  geom_raster(aes(fill=ndvi))

#point scatter plot

ggplot(grass2,aes(POINT_X,y=POINT_Y))+
  geom_point()+geom_label(aes(label=PlotID_4))

#make points bigger

ggplot(grass,aes(x,y,color=ndvi)) +
  geom_point(size=4)

#makes points partially transparent

ggplot(grass,aes(x,y,color=ndvi)) +
  geom_point(size=4)+
  geom_point(alpha=.5)

#add jitter to the poitns so they dont overlap

ggplot(grass,aes(x,y,color=ndvi)) +
  geom_jitter(size=1,width=.000005, height=.000005)

#change color scale
ggplot(grass,aes(x,y,color=ndvi)) +
  geom_jitter(aes(color=ndvi), size=1,width=.000005, height=.000005)+
  scale_colour_gradient(low='black', high='white')

ggplot(grass,aes(x,y,color=ndvi)) +
  geom_jitter(size=1,width=.000005, height=.000005)+
  scale_colour_gradient(low='red', high='green')

ggplot(grass,aes(x,y,color=ndvi)) +
  geom_jitter(size=1,width=.000005, height=.000005)+
  scale_colour_gradient(low='yellow', high='darkgreen')



#plot histogram

ggplot(grass,aes(ndvi))+
  geom_histogram(binwidth=0.005)


ggplot(grass,aes(ndvi))+
  geom_histogram(aes(fill = ..count..),binwidth=0.005)
  


## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}





