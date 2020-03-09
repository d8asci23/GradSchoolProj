#Final Project

#read data
creditcard = read.csv("creditcard.csv", header = T)
head(creditcard)

#change the status of class to factor
creditcard$Class = as.factor(creditcard$Class)

#Get rid of time because we do not want a time series model
creditcard = creditcard[,-1]

#number of nonfraud and fraud transactions, and choose seperately
sum(creditcard$Class==0)
sum(creditcard$Class==1)
fraud = creditcard[creditcard$Class==1,]
nonfraud = creditcard[creditcard$Class==0,]

#set seed and choose 492 non-frauds or 4428 non-frauds
#Fraud:Total(Fraud+NonFraud_Sample) = 1:2
set.seed(12345)
nindex1 = sample(284315, 492, replace = F)
nf1 = nonfraud[nindex1,]
#Fraud:Total(Fraud+NonFraud_Sample) = 1:10
set.seed(12345)
nindex2 = sample(284315, 4428, replace = F)
nf2 = nonfraud[nindex2,]

#sample sets
sampleset1 = rbind(nf1, fraud)
sampleset2 = rbind(nf2, fraud)

#training indexes and validation indexes
set.seed(12345)
trainindex1 = sample(984, 984*0.8, replace = F)
set.seed(12345)
trainindex2 = sample(4920, 4920*0.8, replace = F)

#training sets and validation sets
training1 = sampleset1[trainindex1,]
validation1 = sampleset1[-trainindex1,]
training2 = sampleset2[trainindex2,]
validation2 = sampleset2[-trainindex2,]

##Logistic Regression
#training1
mylogit1<-glm(Class~., data=training1, family=binomial, maxit = 100)
mylogit1.probs<-predict(mylogit1,validation1,type="response")
library(SDMTools)
matrix1 = confusion.matrix(validation1$Class,mylogit1.probs,threshold=0.5)
matrix1
AccuMeasures1=accuracy(validation1$Class,mylogit1.probs,threshold=0.5)
AccuMeasures1

mydf1<-cbind(validation1, mylogit1.probs)
mydf1$response<-as.factor(ifelse(mydf1$mylogit1.probs>0.5,1,0))
library(ROCR)
logit_scores1<-prediction(predictions=mydf1$mylogit1.probs,labels=mydf1$Class)
logit_perf1<-performance(logit_scores1,"tpr", "fpr")
plot(logit_perf1,main="ROC Curves", xlab="1-Specificity:False Positive Rate", ylab="Sensitivity: True Positive Rate", col="darkblue", lwd=3)
abline(0,1,lty=300, col="green", lwd=3)
grid(col="aquamarine")
logit_auc1<-performance(logit_scores1,"auc")
as.numeric(logit_auc1@y.values)

##Logistic Regression
#training2
mylogit2<-glm(Class~., data=training2, family=binomial, maxit = 100)
mylogit2.probs<-predict(mylogit2,validation2,type="response")
library(SDMTools)
matrix2 = confusion.matrix(validation2$Class,mylogit2.probs,threshold=0.5)
matrix2
AccuMeasures2=accuracy(validation2$Class,mylogit2.probs,threshold=0.5)
AccuMeasures2

mydf2<-cbind(validation2, mylogit2.probs)
mydf2$response<-as.factor(ifelse(mydf2$mylogit2.probs>0.5,1,0))
library(ROCR)
logit_scores2<-prediction(predictions=mydf2$mylogit2.probs,labels=mydf2$Class)
logit_perf2<-performance(logit_scores2,"tpr", "fpr")
plot(logit_perf2,main="ROC Curves", xlab="1-Specificity:False Positive Rate", ylab="Sensitivity: True Positive Rate", col="darkblue", lwd=3)
abline(0,1,lty=300, col="green", lwd=3)
grid(col="aquamarine")
logit_auc2<-performance(logit_scores2,"auc")
as.numeric(logit_auc2@y.values)

#random forest
library(randomForest)
set.seed(1)
rf1 = randomForest(Class~., data = sampleset1, subset = trainindex1)
set.seed(1)
rf2 = randomForest(Class~., data = sampleset2, subset = trainindex2)
#random forest cross validation
cv.rf1 = predict(rf1, newdata = validation1, type = "class")
cv.rf2 = predict(rf2, newdata = validation2, type = "class")
table.rf1 = table(cv.rf1, validation1$Class)
table.rf2 = table(cv.rf2, validation2$Class)

#svm
install.packages(e1071) 
library(e1071)
svm1 = svm(Class~., data = training1)
svm2 = svm(Class~., data = training2)
#tuning svm
tune(svm, Class~., data = training1, ranges = list(gamma = 2^(-5:5), cost = 2^(0:10)))
tune(svm, Class~., data = training2, ranges = list(gamma = 2^(-5:5), cost = 2^(0:10)))
#tuning using two size of training data yeilds to similar parameters
#use the new cost and gamma
svm1 = svm(Class~., data = training1, kernel = "radial", cost = 4, gamma = 0.03125)
svm2 = svm(Class~., data = training2, kernel = "radial", cost = 8, gamma = 0.03125)
#svm cross validation
cv.svm1 = predict(svm1, newdata = validation1, type = "class")
cv.svm2 = predict(svm2, newdata = validation2, type = "class")
table.svm1 = table(cv.svm1, validation1)
table.svm2 = table(cv.svm2, validation2)

