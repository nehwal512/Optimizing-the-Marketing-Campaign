# Spliting data  
library(caTools) # spliting
split<-sample.split(data$TargetB,SplitRatio = 0.80)
train1<-subset(data,split == "TRUE")
test1<-subset(data,split == "FALSE")

# Logistic Regression 
model1<- glm(TargetB~.,family=binomial,data=train1)
summary(model1)
# predict 
library(caTools)
fitted.results1 <- predict(model1,newdata=test1[,-1],type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.5,1,0)

# Confussion Matrix
cf1<-table(fitted.results1 , test1[,1])
cf1

# Accuracy of the model 
accuracy1 = (cf1[1,1]+cf1[2,2])/(cf1[1,1]+cf1[2,2]+cf1[1,2]+cf1[2,1])
accuracy1

# Precision
precision1 = (cf1[1,1])/(cf1[1,1]+cf1[2,1])
precision1

# Recall
recall1 = (cf1[1,1])/(cf1[1,1]+cf1[1,2])
recall1

# F-1 score 
f1= (2 * recall1 * precision1)/(recall1 + precision1)
f1

# plot Area Under Curve 
library(ROCR)
p <- predict(model1, newdata=test1[,-1], type="response")
pr <- prediction(p, test1[,1])
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc1 <- auc@y.values[[1]]
auc1

# K fold Cross validation for logistic Regression 
library(caret)
folds = createFolds(train1$TargetB, k=10)
cv2 = lapply(folds, function(x){
  train1_fold = train1[-x, ]
  test1_fold = train1[x, ]
  library(randomForest)
  
  # logestic regression 
  model2<- glm(TargetB~.,family=binomial,
               data=train1)
  y_pred2 = predict(model2, newdata= test1_fold[-1])
  cm2 = table(test1_fold[,1],y_pred2)
  accuracy2 = (cm2[1,1]+cm2[2,2])/(cm2[1,1]+cm2[2,2]+cm2[1,2]+cm2[2,1])
  return(accuracy2)
})
accuracy2 = mean(as.numeric(cv2))
accuracy2


