
library('utiml')
library('e1071')
source("yeast/function/binarizer.R")
source("yeast/function/models.R")
yeast_train <- read.csv("yeast/dataset/yeast-train.arff", sep=',', header=FALSE, comment.char = "@", dec=".")
yeast_test <- read.csv("yeast/dataset/yeast-test.arff", sep=',', header=FALSE, comment.char = "@", dec=".")
labels_yeast <- 104:117
attributes_yeast <- 1:103

train_data <- mldr_from_dataframe(yeast_train, labelIndices = labels_yeast)
test_data <- mldr_from_dataframe(yeast_test, labelIndices = labels_yeast)
model <- mlknn(train_data, k= 5, distance = "euclidean", cores = getOption("utiml.cores", 2))
data_generalisation <- predict(model, train_data)
data_predict <- predict(model, test_data)
matrix_confusion <- multilabel_confusion_matrix(test_data, data_predict)
metric_test <- multilabel_evaluate(test_data, data_predict, measures = "all")
metric_test <- multilabel_evaluate(test_data,data_predict,measures = c("accuracy", "F1"))
metric_validation <- multilabel_evaluate(train_data, data_generalisation, measures = "all")
data_generalisation[1:20,]
train_data
data_predict[data_predict<0.6] <- 0
data_predict[data_predict>=0.6] <- 1
data_generalisation[data_predict<0.6] <- 0
data_generalisation[data_predict>=0.6] <- 1
data_predict
hamming_loss(yeast_test[,labels_yeast],data_predict)
hamming_loss(yeast_train[,labels_yeast],data_generalisation)
names(metric)<-c('K',"hamming_loss_test","accuracy_test","f1_test","hamming_loss_train","accuracy_train","f1_train")
# names(metric)<-c('n',"hamming_loss_test","accuracy_test","f1_test","hamming_loss_train","accuracy_train","f1_train")
plot(metric[,1],metric$hamming_loss_test,col="red", type='l',xlab="K voisin",ylab="metric")
par(new=TRUE)
plot(metric[,1],metric$hamming_loss_train,col="blue", type='l')
# lines(metric[,1],metric[,5], col="blue",type="l")
# text("plot generation")
title("Metric ML-Knn lazy")
decode_el <- debinarizer(yeast_test[1:10,],labels_yeast)
decode_el[[10]]
yeast_test[1:10,labels_yeast]
data
decode_el[1:10,nrow(decode_el)]
?legend
?append
?plot
?multilabel_evaluate
?abline
?lines
bar
metric