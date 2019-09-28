#######  KNN Classifier
install.packages("class")       #KNN classifier
install.packages("gmodels")
library(class)
library(gmodels)            #Cross tables
# 1) Data setup
wbcd = read.table(file = "D:/Ganesh Pondi/Pondicherry university/2019/Data Science/Python/Machine learning with python/KNN Classifier/wisc_bc_data-KNN.csv",header = T, sep = ",")
str(wbcd)
dim(wbcd)
wbcd=wbcd[,-1]
dim(wbcd)
table(wbcd$diagnosis)    #how many B and M
wbcd$diagnosis = factor(wbcd$diagnosis, levels = c("B","M"),labels = c("Benign","malignant"))

# 2) Normalize the data the values will be come 0 to 1
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wbcd_n=as.data.frame(apply(wbcd[,2:31],MARGIN=2,normalize))

# 3) creation of train and test data
wbcd_train = wbcd_n[1:469,]
wbcd_test = wbcd_n[470:569,]
wbcd_train_labels = wbcd_n[1:469,1]
wbcd_test_labels = wbcd_n[470:569,1]

# 4) Builiding the classifier
wbcd_pred = knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=60)
##create confusion matrix
tab <- table(wbcd_pred,wbcd_test_labels)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))}
accuracy(tab)


Performance=CrossTable(wbcd_test_labels,wbcd_pred,prop.t=F,prop.c=F,prop.r=T,dnn=c("Actual","Predict"))
Accuracy=(Performance$t[1,1]+Performance$t[2,2])/length(wbcd_test_labels)
Accuracy

mean(wbcd_pred == wbcd_test_labels)
# test_pred
test_pred <- predict(knn_fit, newdata = test)


# confusionMatrix(test_pred, test$left )











# Fit the model on the training set  #sthda#
set.seed(123)
model <- train(
  diagnosis ~., data = wbcd_train, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
)
# Plot model accuracy vs different values of k
plot(model)











