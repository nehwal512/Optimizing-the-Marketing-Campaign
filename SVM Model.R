#------------------------------------------------------------------------------##############
# svm
library(e1071)
model3<-svm(TargetB ~.,data=train1,kernel="linear",cost=10)
summary(model3)
#confusion matrix and pridiction
p3<-predict(model3,test1)
plot(p3)
cf3<-table(predicted=p3,actual=test1$TargetB)
cf3
accuracy3<-sum(diag(cf3))/sum(cf3)
accuracy3
#miss classification
1-sum(diag(cf3))/sum(cf3)

# Precision
precision3 = (cf3[1,1])/(cf3[1,1]+cf3[2,1])
precision3

# Recall
recall3 = (cf3[1,1])/(cf3[1,1]+cf3[1,2])
recall3

# F-1 score 
f3= (2 * recall3 * precision3)/(recall3 + precision3)
f3


# SVM model with kernel "Radia"
model4<-svm(TargetB ~.,data=train1,kernel="radial",cost=10)
summary(model4)
#confusion matrix and pridiction
p4<-predict(model4,test1)
plot(p4)
cf4<-table(predicted=p4,actual=test1$TargetB)
cf4
accuracy4<-sum(diag(cf4))/sum(cf4)
accuracy4
#miss classification
1-sum(diag(cf4))/sum(cf4)

# Precision
precision4 = (cf4[1,1])/(cf4[1,1]+cf4[2,1])
precision4

# Recall
recall4 = (cf4[1,1])/(cf4[1,1]+cf4[1,2])
recall4

# F-1 score 
f4 = (2 * recall4 * precision4)/(recall4 + precision4)
f4


# K fold Cross validation for SVM 
library(caret)
folds = createFolds(train1$TargetB, k=10)
cv5 = lapply(folds, function(x){
  train1_fold = train1[-x, ]
  test1_fold = train1[x, ]
  
  # SVM Model 
  library(e1071)
  model5 = svm(formula=TargetB ~.,
               data= train1,
               type='C-classification',
               kernel= 'linear')
  y_pred5 = predict(model5, newdata= test1_fold[-1])
  cm5 = table(test1_fold[,1],y_pred5)
  accuracy5 = (cm5[1,1]+cm5[2,2])/(cm5[1,1]+cm5[2,2]+cm5[1,2]+cm5[2,1])
  return(accuracy5)
})
accuracy5 = mean(as.numeric(cv5))
accuracy5


