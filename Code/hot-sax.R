setwd("/Users/mosesowusu/Desktop/Fall 2022/UTRGV thesis/")
library(devtools)
library(jmotif)
library(readxl)
library(R.matlab)
library(plyr)
library(cvTools)
library(nloptr)

trainData<-read.csv("TRAIN.csv",header = FALSE)
head(trainData,10)
trainData[1:5,1:5]

mytraindata<-as.matrix(trainData[,-c(1,2)])## Important: Please convert these training data to a matrix. 
#In my first two columns I had saved ClassName (NSR,APB,AFL,IVR) and ClassLabels (NSR==1,APB==2,AFL==3,IVR==4) of training data.
dim(mytraindata) # 610 by 3600

testData<-read.csv("TEST.csv",header = FALSE)
head(testData,10)
testData[1:5,1:5]

mytestdata<-as.matrix(testData[,-c(1,2)]) # Important: Please convert these training data to a matrix.
##In my first two columns I had saved ClassName (NSR,APB,AFL,IVR) and ClassLabels (NSR==1,APB==2,AFL==3,IVR==4) of test data.
dim(mytestdata)# 266 by 3600

my_list <- list(trainData[,2],mytraindata,testData[,2],mytestdata)
names(my_list) <- c("labels_trainM", "data_trainM","labels_testM","data_testM")
str(my_list)


attributes(my_list)

# set the discretization parameters
w <-62 #60 # the sliding window size
p <- 31 #6  # the PAA size
a <- 6 #6  # the SAX alphabet size

# convert the train classes to wordbags (the dataset has three labels: 1, 2, 3)
nsr <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 1,], w, p, a, "exact", 0.01) #8024 by 2
apb <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 2,], w, p, a, "exact", 0.01) #4822 by 2
afl <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 3,], w, p, a, "exact", 0.01)
afib <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 4,], w, p, a, "exact", 0.01)
svta <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 5,], w, p, a, "exact", 0.01)
pvc <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 6,], w, p, a, "exact", 0.01)
bigenimy <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 7,], w, p, a, "exact", 0.01)
trigenimy <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 8,], w, p, a, "exact", 0.01)
vt <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 9,], w, p, a, "exact", 0.01)
fusion <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 10,], w, p, a, "exact", 0.01)
lbbbb <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 11,], w, p, a, "exact", 0.01)
rbbbb <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 12,], w, p, a, "exact", 0.01)
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


#SAX-VSM classification 
# classify the test data
labels_predicted_M = rep(-1, length(my_list[["labels_testM"]]))
labels_test_M = my_list[["labels_testM"]]
data_test_M = my_list[["data_testM"]]
for (i in c(1:length(data_test_M[,1]))) {
  series_M = data_test_M[i,]
  bag_M = series_to_wordbag(series_M, w, p, a, "exact", 0.01)
  cosines_M = cosine_sim(list("bag"=bag_M, "tfidf" = tfidf_M))
  labels_predicted_M[i] = which(cosines_M$cosines == max(cosines_M$cosines))
}

labels_predicted_M


# compute the classification error
error_M = length(which((labels_test_M != labels_predicted_M))) / length(labels_test_M)
error_M

#findout which time series were misclassified
which((labels_test_M != labels_predicted_M))


###SAX PARAMETER OPTIMIZATION FOR MY DATA###
x = c(3,30,5)  #c(w,p,a)
cverror_M <- function(x) {
  
  # the vector x suppose to contain reational values for the
  # discretization parameters
  w = round(x[1], digits = 0)
  p = round(x[2], digits = 0)
  a = round(x[3], digits = 0)
  
  
  # define the data for CV
  train_dataM <- my_list$data_trainM
  train_labelsM <- my_list$labels_trainM    #labels_trainM
  nfolds = 10
  # few local vars to simplify the process
  m<- length(train_labelsM)
  c <- length(unique(train_labelsM))
  folds <- cvFolds(m, K = nfolds, type = "random")
  
  # saving the error for each folds in this array
  errors_M <- list()
  
  # cross-valiadtion business
  for (i in c(1:nfolds)) {
    
    # define data sets
    set_testM <- which(folds$which == i)
    set_trainM <- setdiff(1:m, set_testM)
    
    # compute the TF-IDF vectors
    bags_M <- alply(unique(train_labelsM),1,function(x){x})
    for (j in 1:c) {
      ll_M <- which(train_labelsM[set_trainM] == unique(train_labelsM)[j])
      bags_M[[unique(train_labelsM)[j]]] <-
        manyseries_to_wordbag( (train_dataM[set_trainM, ])[ll_M,], w, p, a, "exact", 0.01)
    }
    tfidf_M = bags_to_tfidf(bags_M)
    
    # compute the eror
    labels_predicted_M <- rep(-1, length(set_testM))
    labels_test_M <- train_labelsM[set_testM]
    data_testM <- train_dataM[set_testM,]
    
    for (j in c(1:length(labels_predicted_M))) {
      bag_M=NA
      if (length(labels_predicted_M)>1) {
        bag_M = series_to_wordbag(data_testM[j,], w, p, a, "exact", 0.01)
      } else {
        bag_M = series_to_wordbag(data_testM, w, p, a, "exact", 0.01)
      }
      cosines_M = cosine_sim(list("bag" = bag_M, "tfidf" = tfidf_M))
      if (!any(is.na(cosines_M$cosines))) {
        labels_predicted_M[j] = which(cosines_M$cosines == max(cosines_M$cosines))
      }
    }
    
    # the actual error value
    error_M = length(which((labels_test_M != labels_predicted_M))) / length(labels_test_M)
    errors_M[i] <- error_M
  }
  
  # output the mean cross-validation error as the result
  err_M = mean(laply(errors_M,function(x){x}))
  print(paste(w,p,a, " -> ", err_M))
  err_M
}


# PERFORM THE PARAMETER OPTIMIZATION 
#S <- directL(cverror_M, c(10,2,2), c(68,60,12),
# nl.info = TRUE, control = list(xtol_rel = 1e-8, maxeval = 10))
S <- directL(cverror_M, c(3,2,2), c(68,60,10),
             nl.info = TRUE, control = list(xtol_rel = 1e-8, maxeval = 10))


####use optimal sax parameters for classification
w = round(S$par[1], digits = 0)
p = round(S$par[2], digits = 0)
a = round(S$par[3], digits = 0)
para<-c(w,p,a)   #list of optimal parameters
para
w = 62
p = 31
a = 6
#compute the TF-IDF vectors
bags_M <- alply(unique(train_labelsM),1,function(x){x})
for (j in 1:length(unique(train_labelsM))) {
  ll_M <- which(train_labelsM == unique(train_labelsM)[j])
  bags_M[[unique(train_labelsM)[j]]] <-
    manyseries_to_wordbag( train_dataM[ll_M,], w, p, a, "exact", 0.01)
}
tfidf_M = bags_to_tfidf(bags_M)

# classify the test data
labels_predicted_M = rep(-1, length(my_list[["labels_testM"]]))
labels_test_M = my_list[["labels_testM"]]
data_test_M = my_list[["data_testM"]]
for (i in c(1:length(data_test_M[,1]))) {
  #print(paste(i))
  series_M = data_test_M[i,]
  bag_M = series_to_wordbag(series_M, w, p, a, "exact", 0.01)
  cosines_M = cosine_sim(list("bag"=bag_M, "tfidf" = tfidf_M))
  if (!any(is.na(cosines_M$cosines))) {
    labels_predicted_M[i] = which(cosines_M$cosines == max(cosines_M$cosines))
  }
}


# compute the classification error
error_M = length(which((labels_test_M != labels_predicted_M))) / length(labels_test_M)
error_M

# findout which time series were misclassified
which((labels_test_M != labels_predicted_M))
par(mfrow=c(3,1))
plot(my_list[["data_testM"]][28,], type="l")
plot(my_list[["data_testM"]][118,], type="l")
plot(my_list[["data_testM"]][253,], type="l")


################ HOT-SAX FOR DISCORD DISCOVERY ################
?find_discords_brute_force
#arguments(ts, w_size, discords_num) discords_num is the number of discords to report
#install.packages("lineprof")
#library("profvis")
find_discords_brute_force(mytraindata[87,],62,2)

?find_discords_hotsax
# arguments (ts, w_size, paa_size, a_size, n_threshold, discords_num)
find_discords_hotsax(mytraindata[2,], 62, 31, 6, 0.01,1)


### FINDING MULTIPLE DISCORDS ####
discords = find_discords_hotsax(mytraindata[289,], 62, 31, 6, 0.01, 3)  #
discords

## SELECTING THE BEST DISCORD AND PLOT ##### the one with largest nn_distance
discords = find_discords_hotsax(mytraindata[289,], 62, 31, 6, 0.01, 3)
plot(mytraindata[289,], type = "l", col = "cornflowerblue", main = "ECG NSR ")
lines(x=c(discords[1,2]:(discords[1,2]+100)),
      y=mytraindata[289,][discords[1,2]:(discords[1,2]+100)], col="red")

discords = find_discords_hotsax(mytraindata[289,], 62, 31, 6, 0.01, 3)
plot(mytraindata[289,], type = "l", col = "cornflowerblue", main = "ECG NSR")
lines(x=c(discords[2,2]:(discords[2,2]+100)),
      y=mytraindata[289,][discords[2,2]:(discords[2,2]+100)], col="red")

discords = find_discords_hotsax(mytraindata[289,], 62, 31, 6, 0.01, 3)
plot(mytraindata[289,], type = "l", col = "cornflowerblue", main = "ECG NSR ")
lines(x=c(discords[3,2]:(discords[3,2]+50)),
      y=mytraindata[289,][discords[3,2]:(discords[3,2]+100)], col="red")

####################### mutli discord with plot#####
discords = find_discords_hotsax(mytraindata[544,], 62, 31, 6, 0.01, 3)
plot(mytraindata[544,], type = "l", col = "cornflowerblue", main = "ECG RBBBB - PATIENT 1 ")
lines(x=c(discords[1,2]:(discords[1,2]+100)),
      y= mytraindata[544,][discords[1,2]:(discords[1,2]+100)], col="red")
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
plot(mytraindata[601,], type = "l", col = "cornflowerblue", main = "ECG VT - PATIENT 3 ")
lines(x=c(discords[1,2]:(discords[1,2]+100)),
      y= mytraindata[601,][discords[1,2]:(discords[1,2]+100)], col="red")
lines(x=c(discords[2,2]:(discords[2,2]+100)),
      y= mytraindata[601,][discords[2,2]:(discords[2,2]+100)], col="black")
lines(x=c(discords[3,2]:(discords[3,2]+100)),
      y= mytraindata[601,][discords[3,2]:(discords[3,2]+100)], col="lawngreen")



#####SERIES TO CHARACTER STRING###


nsr_str<-series_to_string(mytraindata[127,],6) 
nsr_str<-series_to_string(paa (mytraindata[122,],p),6) 
nsr_str<-series_to_string(paa (mytraindata[120,],p),6) 
#nsr_char<-series_to_chars(paa (mytraindata[292,],p),5)
################trial end############



### SORT DISCORD BY NEAREST NEIGHBOUR DISTANCE #####
library(dplyr)
arrange(discords,desc(nn_distance))


###### ERROR BETWEEN CLASS COMPUTATION####
#LOAD DATA
##TRAIN
############################
trainData<-read.csv("TRAIN.csv",header = FALSE)
testData<-read.csv("TEST.csv",header = FALSE)
nsrAFL_train<-trainData[c(540:586, 279:453),] 
nsrapb_test<-testData[c(241:255,112:193),] #change row numbers for each disorder
mytraindata<-as.matrix(nsrapb_train[,-c(1,2)])
mytestdata<-as.matrix(nsrapb_test[,-c(1,2)])
dim(mytraindata) 
dim(mytestdata)
my_list <- list(nsrapb_train[,2],mytraindata,nsrapb_test[,2],mytestdata)
names(my_list) <- c("labels_trainM", "data_trainM","labels_testM","data_testM")
attributes(my_list)

############################
nsr_data <- my_list[["data_trainM"]][my_list[["labels_trainM"]] == 1,]
apb_data <-  my_list[["data_trainM"]][my_list[["labels_trainM"]] == 12,] #change the no for classes
#svta_data[,1:5] # view data


###TEST
#test_nsr  <- my_list[["data_testM"]][my_list[["labels_testM"]] == 1,]
#test_apb  <- my_list[["data_testM"]][my_list[["labels_testM"]] == 2,]


#SPECIFY PARAMETERS
w = 62
p = 31
a = 6

##CLASSES TO WORDBAGS
# convert the train classes to wordbags (the dataset has three labels: 1, 2, 3)
nsr <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 1,], w, p, a, "exact", 0.01)
apb <- manyseries_to_wordbag(my_list[["data_trainM"]][my_list[["labels_trainM"]] == 2,], w, p, a, "exact", 0.01) 



##TFIDF FOR CLASSES (2 EACH)####
#tfidf = bags_to_tfidf( list("nsr" = nsr, "apb" = apb, "afl" = afl, "afib" = afib, "svta" = svta, "pvc"= pvc,
#                            "bige" = bige, "trig" = trig, "vt"= vt, "fus"= fus,"lb"= lb, "rb"=rb) ) # for all
tfidf = bags_to_tfidf( list("nsr" = nsr, "apb" = apb) )  #REPLACE APB BY THE OTHER CLASSES


####WEIGHTED PATTERN PLOT####
# make up a sample time-series
sample = nsr_data[1,]
sample_bag = sax_via_window(sample, w, p, a, "exact", 0.01)
df = data.frame(index = as.numeric(names(sample_bag)), words = unlist(sample_bag))

# weight the found patterns
weighted_patterns = merge(df, tfidf)
specificity = rep(0, length(sample))
for(i in 1:length(weighted_patterns$words)){
  pattern = weighted_patterns[i,]
  for(j in 1:w){
    specificity[pattern$index+j] = specificity[pattern$index+j] +
      pattern$nsr - pattern$apb - pattern$afl - pattern$afib - pattern$svta - pattern$pvc - pattern$bige - pattern$trig - pattern$vt - pattern$fus - pattern$lb -pattern$rb
  }
}

# plot the weighted patterns
library(ggplot2)
library(scales)
ggplot(data=data.frame(x=c(1:length(sample)), y=sample, col=rescale(specificity)),
       aes(x=x,y=y,color=col)) + geom_line(size=1.2) + theme_bw() +
  ggtitle("WEIGHTED PATTERN PLOT") +
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


###CLASSIFY THE DATA 

labels_predicted = rep(-1, length(my_list[["labels_testM"]]))
labels_test = my_list[["labels_testM"]]
data_test = my_list[["data_testM"]]
for (i in c(1:length(data_test[,1]))) {
  series = data_test[i,]
  bag = series_to_wordbag(series, w, p, a, "exact", 0.01)
  cosines = cosine_sim(list("bag"=bag, "tfidf" = tfidf))
  labels_predicted[i] = which(cosines$cosines == max(cosines$cosines))
}

#compute the classification error
error = length(which((labels_test != labels_predicted))) / length(labels_test)
error

#findout which time series were misclassified
which((labels_test != labels_predicted))

#plot some misclassified time series
par(mfrow=c(3,1))
plot(my_list[["data_testM"]][1,], type="l")
plot(my_list[["data_testM"]][6,], type="l")
plot(my_list[["data_testM"]][12,], type="l")
#save.image("CBF_SAX.RData")

