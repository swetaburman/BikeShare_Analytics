library("e1071")
library("MASS")
library("klaR")
library("caret")
library(ggplot2)
library(rpart)

bike = read.csv(file.choose())
set.seed(1234)
head(bike)



#Randomly shuffle the data
bike<-bike[sample(nrow(bike)),]
bike$AboveMA = as.factor(bike$AboveMA)
head(bike)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(bike)),breaks=7,labels=FALSE)
head(folds)
tail(folds)
BayesoutputData = 0
LogoutputData=0

#Perform 7 fold cross validation
for(i in 1:7){
  #Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- bike[testIndexes, ]
  trainData <- bike[-testIndexes, ]
  
  #test bayes
  classifier = NaiveBayes(AboveMA ~ .-Rented-MovingAverage, data=trainData)
  pred = predict(classifier, testData)
  misClassifyError = mean(pred$class != testData$AboveMA)
  Accuracy = 1-misClassifyError
  Accuracy
  BayesoutputData[i] = Accuracy
  confusionMatrix(testdata$AboveMA,pred$Class)

  
  #test glm
  classifier2=glm(AboveMA ~ .-Rented-MovingAverage, data=trainData, family="binomial"(link='logit'))
  pred=predict(classifier2,newdata=testData,type="response")
  pred<-ifelse(pred > 0.5,1,0)
  misClassifyError = mean(pred != testData$AboveMA)
  Accuracy1 = 1-misClassifyError
  LogoutputData[i] = Accuracy1
  Accuracy1
  confusionMatrix(newdata$AboveMA,pred$Class)
  

}
  head(BayesoutputData,7)
  head(LogoutputData,7)
  
  
  summary(BayesoutputData)
  summary(LogoutputData)
  
 

