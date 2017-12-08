
#plotting scatterplot graphic


######





grass <- read.csv(
  file = "C:/Users/jmaleski/Downloads/070217AB.csv",
  header = TRUE, 
  stringsAsFactors = FALSE
)


library(tidyr)
library(dplyr)
library(ggplot2)

#tried raster style plots but points too small

ggplot(grass,aes(x,y)) +  
  geom_tile(aes(fill=ndvi))

ggplot(grass,aes(x,y)) +  
  geom_raster(aes(fill=ndvi))

#point scatter plot

ggplot(grass,aes(x,y,color=ndvi))+
  geom_point()

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

#add boundaries
yedge=rbind(31.4765496,31.476599,31.4764883,31.4764461)

xedge=rbind(-83.5269438,-83.5277103,-83.5277159,-83.5269549)

edges=cbind(xedge,yedge)

plot(edges)
miny<-min(edges[,2])
maxy<-max(edges[,2])
minx<-min(edges[,1])
maxx<-max(edges[,1])

#filter on boundaries
a<-filter(grass,y>miny & y<maxy & x>minx &x<maxx,ndvi)
ggplot(a,aes(x,y,color=ndvi)) +
  geom_jitter(size=1,width=.000005, height=.000005)+
  scale_colour_gradient(low='yellow', high='darkgreen')


#rotate graph

#rotation matrix (function)
rot <- function(angle) matrix(
  c(cos(angle),-sin(angle),sin(angle),cos(angle)),nrow=2) #CCW rotation matrix


#angle to rotate in radians
rad<-0.06

#extract x y coordinates
c<-as.matrix(cbind(a$x,a$y))

plot(c)

#offsets data so first line of matrix is moved to (0,0)
d.offset <- apply(c,2,function(z) z - z[1]) #offset data

plot(d.offset)

#apply rotation
d.offset.rotate <- d.offset %*% rot(rad) #rotation

plot(d.offset.rotate)

#make new matrix with new coordinates and NDVI
a<-select(a,ndvi)
mndvi<-cbind(d.offset.rotate,a)
mndvi<-as.data.frame(mndvi) #make data frame
mndvi<-tbl_df(mndvi) #make table for printing
mndvi<-select(mndvi, x = 1,y=2,ndvi=ndvi) #name variables

mndvi<-transmute(mndvi,x=x*100000000,y=y*100000000,ndvi=ndvi) #the values are so small make them bigger

ggplot(mndvi,aes(x,y,color=ndvi)) +
  geom_jitter(size=1,width=500, height=500)+
  scale_colour_gradient(low='yellow', high='darkgreen')


#cut into plots

minx<-min(mndvi$x)
minx

ggplot(mndvi,aes(x,y,color=ndvi)) +
  geom_jitter(size=1,width=500, height=500)+
  scale_colour_gradient(low='yellow', high='darkgreen')+
  geom_hline(yintercept = -1200)+geom_hline(yintercept = 1200)+geom_hline(yintercept = 4000)+geom_hline(yintercept = 7000)+geom_hline(yintercept = 10000)+
  geom_vline(xintercept = 0)+geom_vline(xintercept = -77000)

plotXcut<-seq(0, -77000, -3080) #vector of 25 divisions (steps of 3000)
plotYcut<-c(-1200,1200,4000,7000,10000) #vector of 5 divisions 

ggplot(mndvi,aes(x,y,color=ndvi)) +
  geom_jitter(size=1,width=500, height=500)+
  scale_colour_gradient(low='yellow', high='darkgreen')+
  geom_hline(yintercept = plotYcut)+
  geom_vline(xintercept = plotXcut)

#filter plots
plotXcut[1]

plot1<-filter(mndvi,y<plotYcut[2] & y>plotYcut[1] & x<(plotXcut[1]) & x>(plotXcut[2]))
plot2<-filter(mndvi,y<plotYcut[2] & y>plotYcut[1] & x<(plotXcut[2]) & x>(plotXcut[3]))
plot3<-filter(mndvi,y<plotYcut[2] & y>plotYcut[1] & x<(plotXcut[3]) & x>(plotXcut[4]))

#loop to cut plot into 4 rows 24 col

#matrix for results
plots=matrix(nrow = 0, ncol = 5)


for (r in 1:5){
  
rplots=matrix(nrow = 0, ncol = 4)

for (i in 1:25) {
  a<-filter(mndvi,y<plotYcut[r+1] & y>plotYcut[r] & x<(plotXcut[i]) & x>(plotXcut[i+1]))
  
  nrow <-  dim(a)[1]
  idp <- rep(i, nrow)
  
  iplot<-cbind(idp,a)
  
  rplots=rbind(rplots,iplot)
  
  i+1
}
  nrow <-  dim(rplots)[1]
  idr <- rep(r, nrow)

  iplot<-cbind(idr,rplots)
  plots=rbind(plots,iplot)
  
  }

####
#how to select
#row1
row1<-filter(plots,idr==1)

#plot1row1
plot1r1<-filter(plots,idr==1,idp==1)



#plot example

#plot

boxplot(ndvi~idp,data=row1, main="Row 1 NDVI", 
        xlab="plot #", ylab="NDVI")



#analize

# Analysis of variance 
?aov

fit <- aov(ndvi ~ idp, data=row1)
plot(fit)
summary(fit)


?TukeyHSD
# Tukey Honestly Significant Differences
TukeyHSD(fit) # where fit comes from aov()



