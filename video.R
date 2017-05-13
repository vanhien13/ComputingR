# Set the working directory
library(caret)
library(kernlab)
library(MASS)
rm(list=ls())
setwd("D:/videogamesales")
mydata = read.csv("vgsales.csv")
mydata<-na.omit(mydata)
mydata<-mydata[mydata$Year != "N/A", ]
mydata<-mydata[mydata$Platform != "N/A", ]
mydata<-mydata[mydata$Genre != "N/A", ]
mydata<-mydata[mydata$Global_Sales != "N/A", ]
# use fullRank to avoid the 'dummy trap'
dmy_P <- dummyVars(" ~ Platform", data = mydata, fullRank=T)
dummy_data_P <- data.frame(predict(dmy_P, newdata = mydata))
head(dummy_data_P)
dmy_Y <- dummyVars(" ~ Year", data = mydata, fullRank=T)
dummy_data_Y <- data.frame(predict(dmy_Y, newdata = mydata))
head(dummy_data_Y)
table(mydata$Year)
dmy_G <- dummyVars(" ~ Genre", data = mydata, fullRank=T)
dummy_data_G <- data.frame(predict(dmy_G, newdata = mydata))
dummy_data_Y$Year.N.A <- NULL
globalsales<-mydata$Global_Sales
data_fin<-cbind(globalsales,dummy_data_G,dummy_data_P,dummy_data_Y)

folds <- cut(seq(1,nrow(data_fin)),breaks=10,labels=FALSE)
test_sets <- list()
train_sets <- list()
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data_fin[testIndexes, ]
  trainData <- data_fin[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  test_sets <- c(test_sets,list(testData))
  train_sets <- c(train_sets,list(trainData))
}

Testset1<-test_sets[[1]]
Trainset1<-train_sets[[1]]
Testset2<-test_sets[[2]]
Trainset2<-train_sets[[2]]
Testset3<-test_sets[[3]]
Trainset3<-train_sets[[3]]
Testset4<-test_sets[[4]]
Trainset4<-train_sets[[4]]
Testset5<-test_sets[[5]]
Trainset5<-train_sets[[5]]
Testset6<-test_sets[[6]]
Trainset6<-train_sets[[6]]
Testset7<-test_sets[[7]]
Trainset7<-train_sets[[7]]
Testset8<-test_sets[[8]]
Trainset8<-train_sets[[8]]
Testset9<-test_sets[[9]]
Trainset9<-train_sets[[9]]
Testset10<-test_sets[[10]]
Trainset10<-train_sets[[10]]

ols1 <- lm(Trainset1$globalsales~.,Trainset1)
