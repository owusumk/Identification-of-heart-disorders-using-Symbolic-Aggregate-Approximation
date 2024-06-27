library(devtools)
#install_github('jMotif/jmotif-R')
library(jmotif)
library(readxl)
library(R.matlab)
library(pracma)    #for linspace

## SET WORKING DIRECTORY
setwd('/Users/mosesowusu/Desktop/Fall 2022/UTRGV thesis/Excel trial files')


# Loading the data
C0 <-read_excel("C0.xlsx") #C0 is the control group
T12<-read_excel("T12.xlsx") #T12 is the first heart disorder
T61<-read_excel("T61.xlsx") #T61 is the second heart disorder
nas<-read_excel("nas.xlsx") ##nas is the 9 by 3600 data(1st3 rows for CO, next 3 for T12...)

#make files time series
####x1, x2 & x3 represent CO, T12 & T61 respectively
x1<-ts(C0)   
x2<-ts(T12) 
x3<-ts(T61)    
x4<-ts(nas)

## z normalization of the data 
###x1z, x2z & x3z represent the normalized series
x1z<-znorm(x1)
x2z<-znorm(x2)
x3z<-znorm(x3)

#viewing the normalized data
head(x1z)
summary(x1)
summary(x1z)

## plot of the time series
par(mfrow=c(2,2))
plot(x1, type="l", col="blue", main="Normal Sinus Rhythm plot")
plot(x2,  type="l", col="blue", main="Disorder 1 series plot")
plot(x3,  type="l", col="blue", main="Disorder 2 series plot")
#plot(nas[1,])


##PAA divisions
y_paa30 = paa(x1z, 30) #C0 DATA
y_paa31 = paa(x2z, 30) #T12 DATA
y_paa32 = paa(x3z, 30) #T61 DATA

## C0 30 SEGMENT PAA

##define parameters:
a = 5 #alphabet size
p = 30 ## paa size
b = linspace(1,p,p) ##number of paa divisions
seq = linspace(0,3600,30)  ## sequence of breakpoints
plot(x1z, type="l", col="blue", main="3600-points time series and it PAA transform into 30 points")
abline(v=c(0,linspace(0,3600,p)), lty=3, lwd=2, col="gray70")  #draw vertical lines to split the series into equal sizes
y_paa30 = paa(x1z, p)  ### does the piecewise aggregate approximation

##to show segments and their midpoints
for (i in b) {
  for (j in seq) {
    segments(seq[i],y_paa30[i],seq[i+1],y_paa30[i],lwd=1,col="red")
    points(x = seq[i] + 120/2,y=y_paa30[i],col="red",pch=23,lwd=5)
  }
}  


##PAA FOR T12- disorder 1
plot(x2z, type="l", col="blue", main="3600-points time series and it PAA transform into 30 points")
abline(v=c(0,linspace(0,3600,p)), lty=3, lwd=2, col="gray70")  #draw vertical lines to split the series into equal sizes
y_paa31 = paa(x2z, p)  ### does the piecewise aggregate approximation

##to show segments and their midpoints
for (i in b) {
  for (j in seq) {
    segments(seq[i],y_paa31[i],seq[i+1],y_paa31[i],lwd=1,col="red")
    points(x = seq[i] + 120/2,y=y_paa31[i],col="red",pch=23,lwd=5)
  }
}  

graphics.off()

###PAA for disorder 2 T61
plot(x3z, type="l", col="blue", main="3600-points time series and it PAA transform into 30 points")
abline(v=c(0,linspace(0,3600,p)), lty=3, lwd=2, col="gray70")  #draw vertical lines to split the series into equal sizes
y_paa32 = paa(x3z, p)  ### does the piecewise aggregate approximation

##to show segments and their midpoints
for (i in b) {
  for (j in seq) {
    segments(seq[i],y_paa32[i],seq[i+1],y_paa32[i],lwd=1,col="red")
    points(x = seq[i] + 120/2,y=y_paa32[i],col="red",pch=23,lwd=5)
  }
}  



### showing the alphabet cuts on the plot
## T61 data used here
y3<-x3 #y assigned to original time series    #change to y1,y2 for CO and T12
lines(x3z,y3, type="l", lwd=5, col="magenta")    #change to x1z and x2z for CO and T12
abline(h = alphabet_to_cuts(5)[2:5], lty=2, lwd=2, col="magenta")
text(0.4,-1,"a",cex=2,col="magenta")
text(0.4,-0.5 ,"b",cex=2,col="magenta")
text(0.4, 0,"c",cex=2,col="magenta")
text(0.4, 0.5,"d",cex=2,col="magenta")
text(0.4, 1,"e",cex=2,col="magenta")

#if any of the PAA divisions does not fall in the range of a given alphabet, 
#the series to string and series to character won't contain it


CO_sts<-series_to_string(y_paa30, 5)   # series to 5 letter string  #change to y_paa31 or 32 for T12 and T61
CO_stc<-series_to_chars(y_paa30,5)

T12_sts<-series_to_string(y_paa31,5)   
T12_stc<-series_to_chars(y_paa31,5)

T61_sts<-series_to_string(y_paa32,5)  
T61_stc<-series_to_chars(y_paa32,5)


### series to wordbag 
#arguments:
#(time series, window size, paa size, alphabet size, numerosity reduction strategy, normalization threshold)
#?series_to_wordbag
#this gives word string sequences and their respective counts
#the bigger the window size, the more word strings we get
CO2_swb<-series_to_wordbag(y_paa30,3,30,5, "exact",0.01)  #change the sliding window size
T12_swb<-series_to_wordbag(y_paa31,3,30,5, "exact",0.01)  #change the sliding window size
T61_swb<-series_to_wordbag(y_paa32,3,30,5, "exact",0.01)  #change the sliding window size


#?sax_via_window
#arguments:
#(ts, window size, paa size, alphabet size, nr strategy, n threshold)
T12_sliwin<-(sax_via_window(x2, 3, 30, 5, nr_strategy="exact", 0.01))
head(T12_sliwin)

#?manyseries_to_wordbag converts a set of time-series into a single bag of words
#CO_trial<-manyseries_to_wordbag(x1,5,30,5,"exact",0.01)  #my R crashes each time I run this 
#head(CO_trial)


### computing TF*IDF weights
#bags_to_tfidf(list("words" = ,))
#first create bag words from the output of series_to_wordbag for the 3 data sets
bag1 = data.frame(
  "words" = c(CO2_swb$words),
  "counts" = c(CO2_swb$counts),
  stringsAsFactors = FALSE
)


bag2 = data.frame(
  "words" = c(T12_swb$words),
  "counts" = c(T12_swb$counts),
  stringsAsFactors = FALSE
)


bag3 = data.frame(
  "words" = c(T61_swb$words),
  "counts" = c(T61_swb$counts),
  stringsAsFactors = FALSE
)

ll = list("bag1" = bag1, "bag2" = bag2, "bag3" = bag3)  # creat a list of the bags
tfidf = bags_to_tfidf(ll)  ## compute the weights
head(tfidf)

### finding most contributing patterns
library(dplyr)
## arrange the weights in order according to specified bag
arrange(tfidf, desc(bag1))
arrange(tfidf, desc(bag2))
arrange(tfidf, desc(bag3))




# VISUALIZING for CO  and T12
library(ggplot2)
library(scales)
sample = x1 #time series for CO
# set the discretization parameters
#
w <- 3 # the sliding window size # higher values give o
p <- 30  # the PAA size
a <- 5  # the SAX alphabet size
sample_bag = sax_via_window(sample, w, p, a, "exact", 0.01)
df = data.frame(index = as.numeric(names(sample_bag)), words = unlist(sample_bag))

# weight the found patterns
weighted_patterns = merge(df, tfidf)  ##tfidf is the data with weighted output for the series to wordbag 
specificity = rep(0, length(sample))    ###rep repeats the indicated list/items the number of times. Zero is repeated 3599 times
for(i in 1:length(weighted_patterns$words)){     #length of weighted_patterns is 1891
  pattern = weighted_patterns[i,]
  for(j in 1:w){
    specificity[pattern$index+j] = specificity[pattern$index+j] +
      pattern$bag1 - pattern$bag2 
  }
}
#?rep
#?as.numeric
#?unlist
# plot the weighted patterns 
ggplot(data=data.frame(x=c(1:length(sample)), y=sample, col=rescale(specificity)),
       aes(x=x,y=X1016,color=col)) + geom_line(size=1.2) + theme_bw() +
  ggtitle("Weighted pattern for normal sinus rhythm and disorder (T12)") +
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

#graphics.off()
#par(mfrow =c(2,2))
#par(mfrow=c(2,2))
## VISUALIZING FOR CO and T61 

sample1 = x2 #time series for T12
sample_bag1 = sax_via_window(sample1, w, p, a, "exact", 0.01)
df1 = data.frame(index = as.numeric(names(sample_bag1)), words = unlist(sample_bag1))

# weight the found patterns
weighted_patterns1 = merge(df1, tfidf)
specificity1 = rep(0, length(sample1))
for(i in 1:length(weighted_patterns1$words)){
  pattern1 = weighted_patterns1[i,]
  for(j in 1:w){
    specificity1[pattern1$index+j] = specificity1[pattern1$index+j] +
      pattern1$bag1 - pattern1$bag2
  }
}

# plot the weighted patterns CO and T61
ggplot(data=data.frame(x=c(1:length(sample1)), y=sample1, col=rescale(specificity1)),
       aes(x=x,y=X1175,color=col)) + geom_line(size=1.2) + theme_bw() +
  ggtitle("Weighted pattern for normal sinus rhythm and disorder (T61)") +
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





##### VISUALIZING FOR T12 and T61

sample3 = x2 #time series for T12
sample_bag3 = sax_via_window(sample3, w, p, a, "exact", 0.01)
df3 = data.frame(index = as.numeric(names(sample_bag3)), words = unlist(sample_bag3))

# weight the found patterns
weighted_patterns3 = merge(df3, tfidf)
specificity3 = rep(0, length(sample3))
for(i in 1:length(weighted_patterns3$words)){
  pattern3 = weighted_patterns3[i,]
  for(j in 1:w){
    specificity3[pattern3$index+j] = specificity3[pattern3$index+j] +
      pattern3$bag2 - pattern3$bag3
  }
}


# plot the weighted patterns
ggplot(data=data.frame(x=c(1:length(sample3)), y=sample3, col=rescale(specificity3)),
       aes(x=x,y=X979,color=col)) + geom_line(size=1.2) + theme_bw() +
  ggtitle("Weighted pattern plot between the two disorders") +
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



gc()
memory.limit(999999999999999999)


###makinglist
my_list <- list(nas[1:3,], nas[1:3,], nas[1:3,])
names(my_list) <- c("normal", "apb","svta")
str(my_list)

sample<-my_list$normal
sample2<-my_list$apb
sample3<-my_list$svta


my_list <- list(nas[1:3,], nas[1:3,], nas[1:3,])
names(my_list) <- c("normal", "apb","svta")


sample<-my_list$normal
sample2<-my_list$apb
sample3<-my_list$svta

nmat <- sample 
nmat <- matrix(nmat,nrow=3,ncol=3600,byrow=TRUE)
nmat

apbmat <- sample2 
apbmat <- matrix(apbmat,nrow=3,ncol=3600,byrow=TRUE)
apbmat

svtamat <- sample3
svtamat <- matrix(svtamat,nrow=3,ncol=3600,byrow=TRUE)
svtamat

mylist<-list(nmat, apbmat, svtamat)

data("CBF")
dim(CBF[["data_test"]])
dim(apbmat)

w<-5  #3 
a<-5
p<-30 #20
labels_predicted = rep(-1, length(nmat[1,]))
labels_test = apbmat#nmat[1,]
data_test = nmat[1,]  ## sample is time series for CO
for (i in c(1:length(nmat))) { 
  #for (i in c(1:90)) {            
  series = data_test[i,]
  bag = series_to_wordbag(series, w, p, a, "exact", 0.01)
  cosines = cosine_sim(list("bag"=bag, "tfidf" = tfidf))
  labels_predicted[i] = which(cosines$cosines == max(cosines$cosines))
}

#length(CBF$labels_test)
#### SAX-VSM CLASSIFICATION
# classify the test data
###getting errors for the whole length (reduce accordingly)
# specifying new parameters
w<-5  #3 
a<-5
p<-30 #20
labels_predicted = rep(-1, length(sample3))
labels_test = sample3
data_test = sample  ## sample is time series for CO
for (i in c(1:length(sample))) { 
#for (i in c(1:90)) {            
  series = data_test[i,]
  bag = series_to_wordbag(series, w, p, a, "exact", 0.01)
  cosines = cosine_sim(list("bag"=bag, "tfidf" = tfidf))
  labels_predicted[i] = which(cosines$cosines == max(cosines$cosines))
}

# compute the classification error
#error = length(which((labels_test != labels_predicted))) / length(labels_test)  #errooooorrrr!!!
#error



# findout which time series were misclassified
which((labels_test != labels_predicted))