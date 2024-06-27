#install.packages("devtools")
library(devtools)
#install_github('jMotif/jmotif-R')
library(jmotif)
#install.packages("readxl")
#install.packages('readMat')
library(readxl)
library(R.matlab)
#library(pracma) for pretty()
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
graphics.off()
par(mfrow=c(2,2))
## plot of the time series
plot(x1, type="l", col="blue", main="Control group ", xlab = "observation", ylab = "frequency")
plot(x2,  type="l", col="blue", main="Disorder 1 ", xlab = "observation", ylab = "frequency")
plot(x3,  type="l", col="blue", main="Disorder 2 ", xlab = "observation", ylab = "frequency")


##plot of time series and its normalization
#plot(x1,x1z, type="l", col="blue", main="A scaled sine wave with a random noise and its z-normalization")
#plot(x2, x2z, type="l", col="blue", main="A scaled sine wave with a random noise and its z-normalization")
#plot(x3, x3z, type="l", col="blue", main="A scaled sine wave with a random noise and its z-normalization")

#lines(x1, x1z, type="l", col="red")
#abline(h=c(1,-1), lty=2, col="gray50")
#legend(0, -4, c("scaled sine wave","z-normalized wave"), lty=c(1,1), lwd=c(1,1), 
#       col=c("blue","red"), cex=0.8)



## piecewise aggregate approximation

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
## v= c... specifies the x values for the vertical line

#?abline
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



## C0 10 SEGMENT PAA

plot(x1z, type="l", col="blue", main="3600-points time series and it PAA transform into 10 points")

points(x1z, pch=16, lwd=5, col="blue")

abline(v=c(1,360,720,1080,1440,1800,2160,2520,2880,3240,3600), lty=3, lwd=2, col="gray70")

y_paa3 = paa(x1z, 10)

segments(1,y_paa3[1],360,y_paa3[1],lwd=1,col="red")
points(x=360/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(360,y_paa3[2],720,y_paa3[2],lwd=1,col="red")
points(x=360+360/2,y=y_paa3[2],col="red",pch=23,lwd=5)


segments(720,y_paa3[3],1080,y_paa3[3],lwd=1,col="red")
points(x=720+360/2,y=y_paa3[3],col="red",pch=23,lwd=5)

segments(1080,y_paa3[4],1440,y_paa3[4],lwd=1,col="red")
points(x=1080+360/2,y=y_paa3[4],col="red",pch=23,lwd=5)

segments(1440,y_paa3[5],1800,y_paa3[5],lwd=1,col="red")
points(x=1440+360/2,y=y_paa3[5],col="red",pch=23,lwd=5)

segments(1800,y_paa3[6],2160,y_paa3[6],lwd=1,col="red")
points(x=1880+360/2,y=y_paa3[6],col="red",pch=23,lwd=5)

segments(2160,y_paa3[7],2520,y_paa3[7],lwd=1,col="red")
points(x=2160+360/2,y=y_paa3[7],col="red",pch=23,lwd=5)


segments(2520,y_paa3[8],2880,y_paa3[8],lwd=1,col="red")
points(x=2520+360/2,y=y_paa3[8],col="red",pch=23,lwd=5)

segments(2880,y_paa3[9],3240,y_paa3[9],lwd=1,col="red")
points(x=2880+360/2,y=y_paa3[9],col="red",pch=23,lwd=5)

segments(3240,y_paa3[10],3600,y_paa3[10],lwd=1,col="red")
points(x=3240+360/2,y=y_paa3[10],col="red",pch=23,lwd=5)


## C0 15 SEGMENT PAA

plot(x1z, type="l", col="blue", main="3600-points time series and it PAA transform into 15 points")

points(x1z, pch=16, lwd=5, col="blue")

abline(v=c(1,240,480,720,960,1200,1440, 1680,1920,2160,2400,2640,2880,3120,3360,3600), lty=3, lwd=2, col="gray70")

y_paa3 = paa(x1z, 15)

segments(1,y_paa3[1],240,y_paa3[1],lwd=1,col="red")
points(x=240/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(240,y_paa3[2],480,y_paa3[2],lwd=1,col="red")
points(x=240+240/2,y=y_paa3[2],col="red",pch=23,lwd=5)


segments(480,y_paa3[3],720,y_paa3[3],lwd=1,col="red")
points(x=480+240/2,y=y_paa3[3],col="red",pch=23,lwd=5)

segments(720,y_paa3[4],960,y_paa3[4],lwd=1,col="red")
points(x=720+240/2,y=y_paa3[4],col="red",pch=23,lwd=5)

segments(960,y_paa3[5],1200,y_paa3[5],lwd=1,col="red")
points(x=960+240/2,y=y_paa3[5],col="red",pch=23,lwd=5)

segments(1200,y_paa3[6],1440,y_paa3[6],lwd=1,col="red")
points(x=1200+240/2,y=y_paa3[6],col="red",pch=23,lwd=5)

segments(1440,y_paa3[7],1680,y_paa3[7],lwd=1,col="red")
points(x=1440+240/2,y=y_paa3[7],col="red",pch=23,lwd=5)


segments(1680,y_paa3[8],1920,y_paa3[8],lwd=1,col="red")
points(x=1680+240/2,y=y_paa3[8],col="red",pch=23,lwd=5)

segments(1920,y_paa3[9],2160,y_paa3[9],lwd=1,col="red")
points(x=1920+240/2,y=y_paa3[9],col="red",pch=23,lwd=5)

segments(2160,y_paa3[10],2400,y_paa3[10],lwd=1,col="red")
points(x=2160+240/2,y=y_paa3[10],col="red",pch=23,lwd=5)

segments(2400,y_paa3[11],2640,y_paa3[11],lwd=1,col="red")
points(x=2400+240/2,y=y_paa3[11],col="red",pch=23,lwd=5)

segments(2640,y_paa3[12],2880,y_paa3[12],lwd=1,col="red")
points(x=2640+240/2,y=y_paa3[12],col="red",pch=23,lwd=5)

segments(2880,y_paa3[13],3120,y_paa3[13],lwd=1,col="red")
points(x=2880+240/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(3120,y_paa3[14],3360,y_paa3[14],lwd=1,col="red")
points(x=3120+240/2,y=y_paa3[14],col="red",pch=23,lwd=5)

segments(3360,y_paa3[15],3600,y_paa3[15],lwd=1,col="red")
points(x=3360+240/2,y=y_paa3[15],col="red",pch=23,lwd=5)




## PAA FOR T12
## T12 5 SEGMENT PAA
plot(x2z, type="l", col="blue", main="3600-points time series and it PAA transform into 5 points")
points(x2z, pch=16, lwd=5, col="blue")
abline(v=c(1,720,1440,2160,2880,3600), lty=3, lwd=2, col="gray70")

y_paa3 = paa(x2z, 5)

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


## T12 10 SEGMENT PAA

plot(x2z, type="l", col="blue", main="3600-points time series and it PAA transform into 10 points")

points(x2z, pch=16, lwd=5, col="blue")

abline(v=c(1,360,720,1080,1440,1800,2160,2520,2880,3240,3600), lty=3, lwd=2, col="gray70")

y_paa3 = paa(x2z, 10)

segments(1,y_paa3[1],360,y_paa3[1],lwd=1,col="red")
points(x=360/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(360,y_paa3[2],720,y_paa3[2],lwd=1,col="red")
points(x=360+360/2,y=y_paa3[2],col="red",pch=23,lwd=5)


segments(720,y_paa3[3],1080,y_paa3[3],lwd=1,col="red")
points(x=720+360/2,y=y_paa3[3],col="red",pch=23,lwd=5)

segments(1080,y_paa3[4],1440,y_paa3[4],lwd=1,col="red")
points(x=1080+360/2,y=y_paa3[4],col="red",pch=23,lwd=5)

segments(1440,y_paa3[5],1800,y_paa3[5],lwd=1,col="red")
points(x=1440+360/2,y=y_paa3[5],col="red",pch=23,lwd=5)

segments(1800,y_paa3[6],2160,y_paa3[6],lwd=1,col="red")
points(x=1880+360/2,y=y_paa3[6],col="red",pch=23,lwd=5)

segments(2160,y_paa3[7],2520,y_paa3[7],lwd=1,col="red")
points(x=2160+360/2,y=y_paa3[7],col="red",pch=23,lwd=5)


segments(2520,y_paa3[8],2880,y_paa3[8],lwd=1,col="red")
points(x=2520+360/2,y=y_paa3[8],col="red",pch=23,lwd=5)

segments(2880,y_paa3[9],3240,y_paa3[9],lwd=1,col="red")
points(x=2880+360/2,y=y_paa3[9],col="red",pch=23,lwd=5)

segments(3240,y_paa3[10],3600,y_paa3[10],lwd=1,col="red")
points(x=3240+360/2,y=y_paa3[10],col="red",pch=23,lwd=5)


## T12 15 SEGMENT PAA

plot(x2z, type="l", col="blue", main="3600-points time series and it PAA transform into 15 points")

points(x2z, pch=16, lwd=5, col="blue")

abline(v=c(1,240,480,720,960,1200,1440, 1680,1920,2160,2400,2640,2880,3120,3360,3600), lty=3, lwd=2, col="gray70")

y_paa3 = paa(x2z, 15)

segments(1,y_paa3[1],240,y_paa3[1],lwd=1,col="red")
points(x=240/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(240,y_paa3[2],480,y_paa3[2],lwd=1,col="red")
points(x=240+240/2,y=y_paa3[2],col="red",pch=23,lwd=5)


segments(480,y_paa3[3],720,y_paa3[3],lwd=1,col="red")
points(x=480+240/2,y=y_paa3[3],col="red",pch=23,lwd=5)

segments(720,y_paa3[4],960,y_paa3[4],lwd=1,col="red")
points(x=720+240/2,y=y_paa3[4],col="red",pch=23,lwd=5)

segments(960,y_paa3[5],1200,y_paa3[5],lwd=1,col="red")
points(x=960+240/2,y=y_paa3[5],col="red",pch=23,lwd=5)

segments(1200,y_paa3[6],1440,y_paa3[6],lwd=1,col="red")
points(x=1200+240/2,y=y_paa3[6],col="red",pch=23,lwd=5)

segments(1440,y_paa3[7],1680,y_paa3[7],lwd=1,col="red")
points(x=1440+240/2,y=y_paa3[7],col="red",pch=23,lwd=5)


segments(1680,y_paa3[8],1920,y_paa3[8],lwd=1,col="red")
points(x=1680+240/2,y=y_paa3[8],col="red",pch=23,lwd=5)

segments(1920,y_paa3[9],2160,y_paa3[9],lwd=1,col="red")
points(x=1920+240/2,y=y_paa3[9],col="red",pch=23,lwd=5)

segments(2160,y_paa3[10],2400,y_paa3[10],lwd=1,col="red")
points(x=2160+240/2,y=y_paa3[10],col="red",pch=23,lwd=5)

segments(2400,y_paa3[11],2640,y_paa3[11],lwd=1,col="red")
points(x=2400+240/2,y=y_paa3[11],col="red",pch=23,lwd=5)

segments(2640,y_paa3[12],2880,y_paa3[12],lwd=1,col="red")
points(x=2640+240/2,y=y_paa3[12],col="red",pch=23,lwd=5)

segments(2880,y_paa3[13],3120,y_paa3[13],lwd=1,col="red")
points(x=2880+240/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(3120,y_paa3[14],3360,y_paa3[14],lwd=1,col="red")
points(x=3120+240/2,y=y_paa3[14],col="red",pch=23,lwd=5)

segments(3360,y_paa3[15],3600,y_paa3[15],lwd=1,col="red")
points(x=3360+240/2,y=y_paa3[15],col="red",pch=23,lwd=5)




## PAA FOR T61

plot(x3z, type="l", col="blue", main="3600-points time series and it PAA transform into 5 points")
#points(x3z, pch=16, lwd=5, col="blue")
abline(v=c(1,720,1440,2160,2880,3600), lty=3, lwd=2, col="gray70")

y_paa3 = paa(x3z, 5)

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


## T12 10 SEGMENT PAA

plot(x3z, type="l", col="blue", main="3600-points time series and it PAA transform into 10 points")

#points(x3z, pch=16, lwd=5, col="blue")

abline(v=c(1,360,720,1080,1440,1800,2160,2520,2880,3240,3600), lty=3, lwd=2, col="gray70")

y_paa3 = paa(x3z, 10)

segments(1,y_paa3[1],360,y_paa3[1],lwd=1,col="red")
points(x=360/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(360,y_paa3[2],720,y_paa3[2],lwd=1,col="red")
points(x=360+360/2,y=y_paa3[2],col="red",pch=23,lwd=5)


segments(720,y_paa3[3],1080,y_paa3[3],lwd=1,col="red")
points(x=720+360/2,y=y_paa3[3],col="red",pch=23,lwd=5)

segments(1080,y_paa3[4],1440,y_paa3[4],lwd=1,col="red")
points(x=1080+360/2,y=y_paa3[4],col="red",pch=23,lwd=5)

segments(1440,y_paa3[5],1800,y_paa3[5],lwd=1,col="red")
points(x=1440+360/2,y=y_paa3[5],col="red",pch=23,lwd=5)

segments(1800,y_paa3[6],2160,y_paa3[6],lwd=1,col="red")
points(x=1880+360/2,y=y_paa3[6],col="red",pch=23,lwd=5)

segments(2160,y_paa3[7],2520,y_paa3[7],lwd=1,col="red")
points(x=2160+360/2,y=y_paa3[7],col="red",pch=23,lwd=5)


segments(2520,y_paa3[8],2880,y_paa3[8],lwd=1,col="red")
points(x=2520+360/2,y=y_paa3[8],col="red",pch=23,lwd=5)

segments(2880,y_paa3[9],3240,y_paa3[9],lwd=1,col="red")
points(x=2880+360/2,y=y_paa3[9],col="red",pch=23,lwd=5)

segments(3240,y_paa3[10],3600,y_paa3[10],lwd=1,col="red")
points(x=3240+360/2,y=y_paa3[10],col="red",pch=23,lwd=5)


## T12 15 SEGMENT PAA

plot(x3z, type="l", col="blue", main="3600-points time series and it PAA transform into 15 points")

#points(x3z, pch=16, lwd=5, col="blue")

#abline( lty=3, lwd=2, col="gray50")
abline(v=c(1,240,480,720,960,1200,1440, 1680,1920,2160,2400,2640,2880,3120,3360,3600), lty=3, lwd=2, col="gray70")

y_paa3 = paa(x3z, 15)

segments(1,y_paa3[1],240,y_paa3[1],lwd=1,col="red")
points(x=240/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(240,y_paa3[2],480,y_paa3[2],lwd=1,col="red")
points(x=240+240/2,y=y_paa3[2],col="red",pch=23,lwd=5)


segments(480,y_paa3[3],720,y_paa3[3],lwd=1,col="red")
points(x=480+240/2,y=y_paa3[3],col="red",pch=23,lwd=5)

segments(720,y_paa3[4],960,y_paa3[4],lwd=1,col="red")
points(x=720+240/2,y=y_paa3[4],col="red",pch=23,lwd=5)

segments(960,y_paa3[5],1200,y_paa3[5],lwd=1,col="red")
points(x=960+240/2,y=y_paa3[5],col="red",pch=23,lwd=5)

segments(1200,y_paa3[6],1440,y_paa3[6],lwd=1,col="red")
points(x=1200+240/2,y=y_paa3[6],col="red",pch=23,lwd=5)

segments(1440,y_paa3[7],1680,y_paa3[7],lwd=1,col="red")
points(x=1440+240/2,y=y_paa3[7],col="red",pch=23,lwd=5)


segments(1680,y_paa3[8],1920,y_paa3[8],lwd=1,col="red")
points(x=1680+240/2,y=y_paa3[8],col="red",pch=23,lwd=5)

segments(1920,y_paa3[9],2160,y_paa3[9],lwd=1,col="red")
points(x=1920+240/2,y=y_paa3[9],col="red",pch=23,lwd=5)

segments(2160,y_paa3[10],2400,y_paa3[10],lwd=1,col="red")
points(x=2160+240/2,y=y_paa3[10],col="red",pch=23,lwd=5)

segments(2400,y_paa3[11],2640,y_paa3[11],lwd=1,col="red")
points(x=2400+240/2,y=y_paa3[11],col="red",pch=23,lwd=5)

segments(2640,y_paa3[12],2880,y_paa3[12],lwd=1,col="red")
points(x=2640+240/2,y=y_paa3[12],col="red",pch=23,lwd=5)

segments(2880,y_paa3[13],3120,y_paa3[13],lwd=1,col="red")
points(x=2880+240/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(3120,y_paa3[14],3360,y_paa3[14],lwd=1,col="red")
points(x=3120+240/2,y=y_paa3[14],col="red",pch=23,lwd=5)

segments(3360,y_paa3[15],3600,y_paa3[15],lwd=1,col="red")
points(x=3360+240/2,y=y_paa3[15],col="red",pch=23,lwd=5)


###T61 30 paa
a = 5 #alphabet size
p = 30 ## paa size
b = linspace(1,p,p) ##number of paa divisions
seq = linspace(0,3600,30)  ## sequence of breakpoints
plot(x3z, type="l", col="blue", main="3600-points time series and it PAA transform into 30 points")
abline(v=c(0,linspace(0,3600,p)), lty=3, lwd=2, col="gray70")  #draw vertical lines to split the series into equal sizes
y_paa30 = paa(x3z, p)  ### does the piecewise aggregate approximation

##to show segments and their midpoints
for (i in b) {
  for (j in seq) {
    segments(seq[i],y_paa30[i],seq[i+1],y_paa30[i],lwd=1,col="red")
    points(x = seq[i] + 120/2,y=y_paa30[i],col="red",pch=23,lwd=5)
  }
}  

graphics.off()
par(mfrow=c(2,2))


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
text(min(x1z),-1.602743,"a",cex=2,col="magenta")
text(-1.602743,0.243257 ,"b",cex=2,col="magenta")
text(0.243257, 2.089257,"c",cex=2,col="magenta")
text(2.089257, 3.935257,"d",cex=2,col="magenta")
text(3.935257, max(x1z),"e",cex=2,col="magenta")
series_to_string(y_paa1, 5)


#?text
#?alphabet_to_cuts

min(x3z)
max(x3z)
series_to_chars(y_paa1, 5)

 
#5.0 SAX-VSM classifier
 # load Cylinder-Bell-Funnel data
 data("C0.xlsx")
 str(CBF)
 
 #5.1 Pre-processing and bags of words construction
 
 # set the discretization parameters
 #
 w <- 60 # the sliding window size
 p <- 6  # the PAA size
 a <- 6  # the SAX alphabet size
 
 # convert the train classes to wordbags (the dataset has three labels: 1, 2, 3)
 #
 cylinder <- manyseries_to_wordbag(CBF[["data_train"]][CBF[["labels_train"]] == 1,], w, p, a, "exact", 0.01)
 bell <- manyseries_to_wordbag(CBF[["data_train"]][CBF[["labels_train"]] == 2,], w, p, a, "exact", 0.01)
 funnel <- manyseries_to_wordbag(CBF[["data_train"]][CBF[["labels_train"]] == 3,], w, p, a, "exact", 0.01)
 head(cylinder)
 
 #5.2 TF*IDF weighting
 
 # compute tf*idf weights for three bags
 #
 tfidf = bags_to_tfidf( list("cylinder" = cylinder, "bell" = bell, "funnel" = funnel) )
 tail(tfidf)
 
 library(dplyr)
 head(arrange(tfidf, desc(cylinder)))
 head(arrange(tfidf, desc(funnel)))
 
 
 # make up a sample time-series
 #
 sample = (CBF[["data_train"]][CBF[["labels_train"]] == 3,])[1,]
 sample_bag = sax_via_window(sample, w, p, a, "exact", 0.01)
 df = data.frame(index = as.numeric(names(sample_bag)), words = unlist(sample_bag))
 
 # weight the found patterns
 #
 weighted_patterns = merge(df, tfidf)
 specificity = rep(0, length(sample))
 for(i in 1:length(weighted_patterns$words)){
   pattern = weighted_patterns[i,]
   for(j in 1:w){
     specificity[pattern$index+j] = specificity[pattern$index+j] +
       pattern$funnel - pattern$bell - pattern$cylinder
   }
 }
 
 # plot the weighted patterns
 #
 library(ggplot2)
 library(scales)
 ggplot(data=data.frame(x=c(1:length(sample)), y=sample, col=rescale(specificity)),
        aes(x=x,y=y,color=col)) + geom_line(size=1.2) + theme_bw() +
   ggtitle("The funnel class-characteristic pattern example") +
   scale_colour_gradientn(name = "Class specificity:  ",limits=c(0,1),
                          colours=c("red","yellow","green","lightblue","darkblue"),
                          breaks=c(0,0.5,1),labels=c("negative","neutral","high"),
                          guide = guide_colorbar(title.theme=element_text(size=14, angle=0),title.vjust=1,
                                                 barheight=0.6, barwidth=6, label.theme=element_text(size=10, angle=0))) +
   theme(legend.position="bottom",plot.title=element_text(size=18),
         axis.title.x=element_blank(), axis.title.y=element_blank(),
         axis.text.x=element_text(size=12),axis.text.y=element_blank(),
         panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
         axis.ticks.y = element_blank())
#5.3 SAX-VSM CLASSIFIER
 
 # classify the test data
 #
 labels_predicted = rep(-1, length(CBF[["labels_test"]]))
 labels_test = CBF[["labels_test"]]
 data_test = CBF[["data_test"]]
 for (i in c(1:length(data_test[,1]))) {
   series = data_test[i,]
   bag = series_to_wordbag(series, w, p, a, "exact", 0.01)
   cosines = cosine_sim(list("bag"=bag, "tfidf" = tfidf))
   labels_predicted[i] = which(cosines$cosines == max(cosines$cosines))
 }
 
 # compute the classification error
 #
 error = length(which((labels_test != labels_predicted))) / length(labels_test)
 error
 
 # findout which time series were misclassified
 #
 which((labels_test != labels_predicted))

 
 #6.0 SAX-VSM discretization parameters optimization
 
 library(plyr)
 #install.packages('cvTools')
 library(cvTools)
 library(nloptr)
 
 # the cross-validation error function
 # uses the following global variables
 #   1) nfolds -- specifies folds for the cross-validation
 #                if equal to the number of instances, then it is
 #                LOOCV
 #
 cverror <- function(x) {
   
   # the vector x suppos to contain reational values for the
   # discretization parameters
   #
   w = round(x[1], digits = 0)
   p = round(x[2], digits = 0)
   a = round(x[3], digits = 0)
   
   # few local vars to simplify the process
   m <- length(train_labels)
   c <- length(unique(train_labels))
   folds <- cvFolds(m, K = nfolds, type = "random")
   
   # saving the error for each folds in this array
   errors <- list()
   
   # cross-valiadtion business
   for (i in c(1:nfolds)) {
     
     # define data sets
     set_test <- which(folds$which == i)
     set_train <- setdiff(1:m, set_test)
     
     # compute the TF-IDF vectors
     bags <- alply(unique(train_labels),1,function(x){x})
     for (j in 1:c) {
       ll <- which(train_labels[set_train] == unique(train_labels)[j])
       bags[[unique(train_labels)[j]]] <-
         manyseries_to_wordbag( (train_data[set_train, ])[ll,], w, p, a, "exact", 0.01)
     }
     tfidf = bags_to_tfidf(bags)
     
     # compute the eror
     labels_predicted <- rep(-1, length(set_test))
     labels_test <- train_labels[set_test]
     data_test <- train_data[set_test,]
     
     for (j in c(1:length(labels_predicted))) {
       bag=NA
       if (length(labels_predicted)>1) {
         bag = series_to_wordbag(data_test[j,], w, p, a, "exact", 0.01)
       } else {
         bag = series_to_wordbag(data_test, w, p, a, "exact", 0.01)
       }
       cosines = cosine_sim(list("bag" = bag, "tfidf" = tfidf))
       if (!any(is.na(cosines$cosines))) {
         labels_predicted[j] = which(cosines$cosines == max(cosines$cosines))
       }
     }
     
     # the actual error value
     error = length(which((labels_test != labels_predicted))) / length(labels_test)
     errors[i] <- error
   }
   
   # output the mean cross-validation error as the result
   err = mean(laply(errors,function(x){x}))
   print(paste(w,p,a, " -> ", err))
   err
 }
 
 # define the data for CV
 train_data <- CBF[["data_train"]]
 train_labels <- CBF[["labels_train"]]
 nfolds = 15
 
 # perform the parameters optimization
 S <- directL(cverror, c(10,2,2), c(120,60,12),
              nl.info = TRUE, control = list(xtol_rel = 1e-8, maxeval = 10))

 w = round(S$par[1], digits = 0)
 p = round(S$par[2], digits = 0)
 a = round(S$par[3], digits = 0)
 
 # compute the TF-IDF vectors
 #
 bags <- alply(unique(train_labels),1,function(x){x})
 for (j in 1:length(unique(train_labels))) {
   ll <- which(train_labels == unique(train_labels)[j])
   bags[[unique(train_labels)[j]]] <-
     manyseries_to_wordbag( train_data[ll,], w, p, a, "exact", 0.01)
 }
 tfidf = bags_to_tfidf(bags)
 
 # classify the test data
 #
 labels_predicted = rep(-1, length(CBF[["labels_test"]]))
 labels_test = CBF[["labels_test"]]
 data_test = CBF[["data_test"]]
 for (i in c(1:length(data_test[,1]))) {
   print(paste(i))
   series = data_test[i,]
   bag = series_to_wordbag(series, w, p, a, "exact", 0.01)
   cosines = cosine_sim(list("bag"=bag, "tfidf" = tfidf))
   if (!any(is.na(cosines$cosines))) {
     labels_predicted[i] = which(cosines$cosines == max(cosines$cosines))
   }
 }
 
 # compute the classification error
 #
 error = length(which((labels_test != labels_predicted))) / length(labels_test)
 error
 
 # findout which time series were misclassified
 #
 which((labels_test != labels_predicted))
 par(mfrow=c(3,1))
 plot(data_test[316,], type="l")
 plot(data_test[589,], type="l")
 plot(data_test[860,], type="l") 

 
 ##7.0 HOT-SAX algorithm for time series discord discovery
 
 
 lineprof( find_discords_brute_force(ecg0606, 100, 5) )
 
 ##### r codes link
 Paper link
 https://www.sciencedirect.com/science/article/pii/S0957417417306292?via%3Dihub 
 
 Database for ECG data
 https://www.physionet.org/about/database/
   
   https://github.com/jMotif/jmotif-R (codes link)
 
 https://github.com/jMotif/jmotif-R=	`