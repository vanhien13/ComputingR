library(kernlab)
cre_card <- read.table("D:/machine-learning-coursera-master/creditcard.csv/creditcard.csv", header=TRUE,
   sep=",")

#Undersample the data set
#Randomly shuffle the data
cre_card<-cre_card[sample(nrow(cre_card)),]
fraud_trans <- sum(cre_card$Class == '1')
non_fraud_trans <- sum(cre_card$Class == '0')
cre_card_non_fraud_50<-cre_card[ sample( which(cre_card$Class==0), fraud_trans), ]
cre_card_fraud <- cre_card[ which(cre_card$Class=='1'), ]
cre_card <- rbind(cre_card_fraud,cre_card_non_fraud_50)
cre_card <- cre_card[sample(nrow(cre_card)),]
folds <- cut(seq(1,nrow(cre_card)),breaks=10,labels=FALSE)
test_sets <- list()
train_sets <- list()
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- cre_card[testIndexes, ]
  trainData <- cre_card[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  test_sets <- c(test_sets,list(testData))
  train_sets <- c(train_sets,list(trainData))
}

Testset1<-test_sets[[1]]
Trainset1<-train_sets[[1]]
svmmodel<-ksvm(Class~.,data=Trainset1,type = "C-bsvc", kernel = "rbfdot",kpar = list(sigma = 0.1), C = 10,prob.model = TRUE, cross = 5)
predict(svmmodel, Testset1, type ="probabilities")
y_hat<- predict(svmmodel, Testset1)
sum(y_hat==Testset1$Class)/length(Testset1$Class)

#Oversample the data set
