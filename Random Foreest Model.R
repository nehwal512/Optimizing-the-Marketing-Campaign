# random forest

library(randomForest)
model8<- randomForest(TargetB~.-(DemCluster), data=train1,mtry=22)  # mtry shoud be change  it is an no of variables that we selected
model8                                                                    #if mtry is equal to no of variables so it is called bagging 
model8$importance
varImpPlot(model8)
#prediction
# prediction
p8<-predict(model8,test1,type="class")
#confusion matrix
cf8<-table(predicted=p8,actual=test1$TargetB)
cf8
# accuracy
accuracy8<-sum(diag(cf8))/sum(cf8)
accuracy8


# Precision
precision8 = (cf8[1,1])/(cf8[1,1]+cf8[2,1])
precision8

# Recall
recall8 = (cf8[1,1])/(cf8[1,1]+cf8[1,2])
recall8

# F-1 score 
f8 = (2 * recall8 * precision8)/(recall8 + precision8)
f8


# RF wit k fold 
library(caret)
folds9 = createFolds(train1$TargetB, k=10)
cv9 = lapply(folds9, function(x){
  train1_fold = train1[-x, ]
  test1_fold = train1[x, ]
  library(randomForest)
  rf9<- randomForest(TargetB~.-(DemCluster),
                     data=train1,mtry=22)  # mtry shoud be change  it is an no of variables that we selected
  
  y_pred9 = predict(rf9, newdata= test1_fold[-1])
  cm9 = table(test1_fold[,1],y_pred9)
  accuracy9 = (cm9[1,1]+cm9[2,2])/(cm9[1,1]+cm9[2,2]+cm9[1,2]+cm9[2,1])
  return(accuracy9)
})
accuracy9 = mean(as.numeric(cv9))
accuracy9