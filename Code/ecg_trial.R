#install.packages("devtools")
library(devtools)
#install_github('jMotif/jmotif-R')
library(jmotif)
#install.packages("readxl")
#install.packages('readMat')
library(readxl)
library(R.matlab)
## SET WORKING DIRECTORY
setwd('/Users/mosesowusu/Desktop/Fall 2022/UTRGV thesis/Excel trial files')


# Loading the data
C0 <-read_excel("C0.xlsx") #C0
#x<-readMat('C0.mat')
T12<-read_excel("T12.xlsx") #T12
T61<-read_excel("T61.xlsx") #T61

#make files time series

x1<-ts(C0)   
x2<-ts(T12)   ###
x3<-ts(T61)    ###

#?znorm

## z normalization of the data 
x1z<-znorm(x1)
x2z<-znorm(x2)
x3z<-znorm(x3)

head(x1z)
head(x2z)
head(x3z)
summary(x1z)
summary(x2z)
summary(x3z)
## plot of the time series
plot(x1, type="l", col="blue", main="Control group time series plot")
plot(x2,  type="l", col="blue", main="Treatment group 1 time series plot")
plot(x3,  type="l", col="blue", main="Treatment group 2 time series plot")


### trial for 2 segments######################
#plot(x1z, type="l", col="blue", main="3600-points time series and it PAA transform into 5 points")
#points(x1z, pch=16, lwd=5, col="blue")
#abline(v=c(1,1800,3600), lty=3, lwd=2, col="gray70")

#y_paa3 = paa(x1z, 2)

#segments(1,y_paa3[1],1800,y_paa3[1],lwd=1,col="red")
#points(x=1800/2,y=y_paa3[1],col="red",pch=23,lwd=5)

#segments(1800,y_paa3[2],3600,y_paa3[2],lwd=1,col="red")
#points(x=1800+1800/2,y=y_paa3[2],col="red",pch=23,lwd=5)
#####################

## C0 5 SEGMENT PAA
plot(x1z, type="l", col="blue", main="3600-points time series and it PAA transform into 5 points")
#points(x1z, pch=16, lwd=5, col="blue")  #plot the observations
abline(v=c(1,720,1440,2160,2880,3600), lty=3, lwd=2, col="gray70")
y_paa3 = paa(x1z, 5)

segments(1,y_paa3[1],720,y_paa3[1],lwd=1,col="red")
points(x=720/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(720,y_paa3[2],1440,y_paa3[2],lwd=1,col="red")
points(x=720+720/2,y=y_paa3[2],col="red",pch=23,lwd=5)

segments(1440,y_paa3[3],2160,y_paa3[3],lwd=1,col="red")
points(x=1440+720/2,y=y_paa3[3],col="red",pch=23,lwd=5)

segments(2160,y_paa3[4],2880,y_paa3[4],lwd=1,col="red")
points(x=2160+720/2,y=y_paa3[4],col="red",pch=23,lwd=5)

segments(2880,y_paa3[5],3600,y_paa3[5],lwd=1,col="red")
points(x=2880+720/2,y=y_paa3[5],col="red",pch=23,lwd=5)

#####R function to do the PAA 
?linspace
plot(x1z, type="l", col="blue", main="3600-points time series and it PAA transform into 5 points")
nseq<-c(linspace(720,3600,5))
abline(v=c(1,nseq),lty=3, lwd=2,col="gray70")
pd<-5 #define number of PAA divisions for the normalized time series
ml<-3600  # length of the series
segts<-c(linspace(1,pd,5))    #sequence of PAA splits from 1:pd
y_paa3 = paa(x1z,pd)

mylist<-list(segts,nseq)
lapply(mylist, segments(1, y_paa3[segts],nseq[],y_paa3[segts],lwd=1,col="red"))
#function to do the segment splitting
#paa_split<-function(x){
  #k<-1 #assign 1 to k
#  for (k in segments, n in nseq){
#  return(segments(k,y_paa3[k],nseq[2],y_paa3[k],lwd=1,col="red")
#         points(nseq[2]/2,y=y_paa3[1],col="red",pch=23,lwd=5))
#}
#paa_split()


nseq[2]
## SAX TRANSFORM 

y_paa1 = paa(x1z, 5) #C0 DATA
y_paa2 = paa(x2z, 5) #T12 DATA
y_paa3 = paa(x3z, 5) #T61 DATA


#plot time series first
plot(x1z, type="l", col="blue")
abline(v=c(1,720,1440,2160,2880,3600), lty=3, lwd=2, col="gray70")

y_paa1 = paa(x1z, 5)
segments(1,y_paa3[1],720,y_paa3[1],lwd=1,col="red")
points(x=720/2,y=y_paa3[1],col="red",pch=23,lwd=5)
segments(720,y_paa3[2],1440,y_paa3[2],lwd=1,col="red")
points(x=720+720/2,y=y_paa3[2],col="red",pch=23,lwd=5)
segments(1440,y_paa3[3],2160,y_paa3[3],lwd=1,col="red")
points(x=1440+720/2,y=y_paa3[3],col="red",pch=23,lwd=5)
segments(2160,y_paa3[4],2880,y_paa3[4],lwd=1,col="red")
points(x=2160+720/2,y=y_paa3[4],col="red",pch=23,lwd=5)
segments(2880,y_paa3[5],3600,y_paa3[5],lwd=1,col="red")
points(x=2880+720/2,y=y_paa3[5],col="red",pch=23,lwd=5)

y<-x1 #y assigned to original time series
#x <- dnorm(y, mean=0, sd=1)
lines(x1z,y, type="l", lwd=5, col="magenta")
abline(h = alphabet_to_cuts(5)[2:5], lty=2, lwd=2, col="magenta")
text(0.7,-1.602743,"a",cex=2,col="magenta")
text(0.7,0.243257 ,"b",cex=2,col="magenta")
text(0.7, 2.089257,"c",cex=2,col="magenta")
text(0.7, 3.935257,"d",cex=2,col="magenta")
text(0.7, max(x1z),"e",cex=2,col="magenta")
series_to_string(y_paa1, 5)


#min(x3z)
#max(x3z)
series_to_chars(y_paa1, 5)
