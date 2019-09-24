# decesion tree
# library for tree
library(tree)
model6=tree(TargetB~.-(DemCluster),train1)
plot(model6)
text(model6,pretty=0)
# prediction
p6<-predict(model6,test1,type="class")
#confusion matrix
cf6<-table(predicted=p6,actual=test1$TargetB)
cf6
# accuracy
accuracy6<-sum(diag(cf6))/sum(cf6)
accuracy6


# Precision
precision6 = (cf6[1,1])/(cf6[1,1]+cf6[2,1])
precision6

# Recall
recall6 = (cf6[1,1])/(cf6[1,1]+cf6[1,2])
recall6

# F-1 score 
f6 = (2 * recall6 * precision6)/(recall6 + precision6)
f6

# prune

# CV for decision tree 
library(caret)
folds = createFolds(train1$TargetB, k=10)
cv7 = lapply(folds, function(x){
  train1_fold = train1[-x, ]
  test1_fold = train1[x, ]
  
  # model Decision tree
  library(tree)
  model7=tree(TargetB~.-(DemCluster),train1)
  y_pred7 = predict(model7, newdata= test1_fold[-1])
  cm7 = table(test1_fold[,1],y_pred7)
  accuracy7 = (cm7[1,1]+cm7[2,2])/(cm7[1,1]+cm7[2,2]+cm7[1,2]+cm7[2,1])
  return(accuracy7)
})
accuracy7 = mean(as.numeric(cv7))
accuracy7

