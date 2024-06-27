#SET WD
setwd("/Users/mosesowusu/Desktop/Fall 2022/UTRGV thesis/")

#LOAD PACKAGES
library(devtools)
library(jmotif)
library(readxl)
library(R.matlab)
library(plyr)
library(cvTools)
library(nloptr)

#LOAD INTACT EXCEL TRAIN AND TEST FILE
trainData<-read.csv("TRAIN.csv",header = FALSE)

mytraindata<-as.matrix(trainData[,-c(1,2)]) #REMOVES 1ST 2 COLUMNS INDICATING CLASS 

testData<-read.csv("TEST.csv",header = FALSE)


mytestdata<-as.matrix(testData[,-c(1,2)]) 
dim(mytestdata)

my_list <- list(trainData[,2],mytraindata,testData[,2],mytestdata)
names(my_list) <- c("labels_trainM", "data_trainM","labels_testM","data_testM")

# set the discretization parameters
w <-62 # the sliding window size
p <- 31  # the PAA size
a <- 6  # the SAX alphabet size

# convert the train classes to wordbags (the dataset has TWELVE labels: 1, 2, 3,...,12)
nsr <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 1,], w, p, a, "exact", 0.01) 
head(nsr)

# compute tf*idf weights for three bags
tfidf_M = bags_to_tfidf( list("nsr" = nsr, "apb" = apb, "afl" = afl,"afib" = afib, "svta" = svta, "pvc" = pvc,
                              "bigenimy" = bigenimy, "trigenimy" = trigenimy, "vt" = vt, 
                              "fusion" = fusion, "lbbbb" = lbbbb,"rbbbb" = rbbbb) )
tail(tfidf_M) #this yields a data frame of five variables: the words which are "important" in TF*IDF terms 
#(i.e. not presented at least in one of the bags) and their class-corresponding weights:
dim(tfidf_M)  #11851 by 5

library(dplyr)
head(arrange(tfidf_M, desc(nsr)))
head(arrange(tfidf_M, desc(apb)))
head(arrange(tfidf_M, desc(afl)))
head(arrange(tfidf_M, desc(svta)))


################ HOT-SAX FOR DISCORD DISCOVERY ################


##brute force discord discovery
find_discords_brute_force(mytraindata[87,],62,2)


#hotsax discord discovery
#arguments (ts, w_size, paa_size, a_size, n_threshold, discords_num)
find_discords_hotsax(mytraindata[2,], 62, 31, 6, 0.01,1)


### FINDING MULTIPLE DISCORDS ###
discords = find_discords_hotsax(mytraindata[289,], 62, 31, 6, 0.01, 3)  
discords

#### SELECTING THE BEST DISCORD AND PLOT #####

####################### mutli discord with plot#####
discords = find_discords_hotsax(mytraindata[544,], 62, 31, 6, 0.01, 3)
plot(mytraindata[544,], type = "l", col = "cornflowerblue", main = "ECG RBBBB - PATIENT 1 ")
lines(x=c(discords[1,2]:(discords[1,2]+100)),
      y= mytraindata[544,][discords[1,2]:(discords[1,2]+100)], col="red")  #add disorder to plot
lines(x=c(discords[2,2]:(discords[2,2]+100)),
      y= mytraindata[544,][discords[2,2]:(discords[2,2]+100)], col="black")
lines(x=c(discords[3,2]:(discords[3,2]+100)),
      y= mytraindata[544,][discords[3,2]:(discords[3,2]+100)], col="lawngreen")


discords = find_discords_hotsax(mytraindata[583,], 62, 31, 6, 0.01, 3)
plot(mytraindata[583,], type = "l", col = "cornflowerblue", main = "ECG RBBBB - PATIENT 2")
lines(x=c(discords[1,2]:(discords[1,2]+100)),
      y= mytraindata[583,][discords[1,2]:(discords[1,2]+100)], col="red")
lines(x=c(discords[2,2]:(discords[2,2]+100)),
      y= mytraindata[583,][discords[2,2]:(discords[2,2]+100)], col="black")
lines(x=c(discords[3,2]:(discords[3,2]+100)),
      y= mytraindata[583,][discords[3,2]:(discords[3,2]+100)], col="lawngreen")


discords = find_discords_hotsax(mytraindata[601,], 62, 31, 6, 0.01, 3)
plot(mytraindata[601,], type = "l", col = "cornflowerblue", main = "ECG RBBBB - PATIENT 3 ")
lines(x=c(discords[1,2]:(discords[1,2]+100)),
      y= mytraindata[601,][discords[1,2]:(discords[1,2]+100)], col="red")
lines(x=c(discords[2,2]:(discords[2,2]+100)),
      y= mytraindata[601,][discords[2,2]:(discords[2,2]+100)], col="black")
lines(x=c(discords[3,2]:(discords[3,2]+100)),
      y= mytraindata[601,][discords[3,2]:(discords[3,2]+100)], col="lawngreen")



#####SERIES TO CHARACTER STRING###

#arguments(ts, a_size)
nsr_str<-series_to_string(mytraindata[289,],6) #values in [] can be changed using table sent
apb_str<-series_to_string(mytraindata[127,],6) 
afl_str<-series_to_string(mytraindata[108,],6) 


#series_to_wordbag(mytraindata[289,],62,31,6,"exact",0.01)
z_norm <-znorm(mytraindata[108,])
nsr_paa = paa(z_norm, 31) 
nsr_sts<-series_to_string(nsr_paa, 6)   # series to 6 letter string  #change to y_paa31 or 32 for T12 and T61
nsr_stc<-series_to_chars(nsr_paa,6)
