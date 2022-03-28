library('utiml')
library('e1071')
library("lubridate")
source("yeast/function/binarizer.R")
source("yeast/function/models.R")
yeast_train <- read.csv("yeast/dataset/yeast-train.arff", sep=',', header=FALSE, comment.char = "@", dec=".")
yeast_test <- read.csv("yeast/dataset/yeast-test.arff", sep=',', header=FALSE, comment.char = "@", dec=".")
labels_yeast <- 104:117
attributes_yeast <- 1:103
distance <- c("euclidean","maximum", "manhattan")
# train_data <-fit_transform(yeast_train,labels = labels_yeast)
# test_data <- fit_transform(yeast_test,labels = labels_yeast)
metric<-models(12,distance = distance,data_train = yeast_train,data_test = yeast_test,labels_data = labels_yeast)
metric_euclien <-metric[[1]]
metric_maximun <-metric[[2]]
metric_manhattan <-metric[[3]]
plot(metric_euclien$K,metric_euclien$hamming_loss_test,type="l",col="red",
     xlab = "k voisin",ylab = "hamming loss",
      xlim = c(7,15),ylim = c(0.15,0.25))
lines(metric_euclien$K,metric_maximun$hamming_loss_test,col="green")
lines(metric_euclien$K,metric_manhattan$hamming_loss_test,col="blue")
title("Metric ML-Knn lazy")
legend(12, 0.25, legend=c("euclidienne", " Maximun","manhattan"),
       col=c("red", "green","blue"), lty=1:2, cex=0.8)
# points(x=10,col="cyan",type='p')
train_data <-fit_transform(yeast_train,labels = labels_yeast)
test_data <- fit_transform(yeast_test,labels = labels_yeast)
model_final <- mlknn(train_data,k=11,distance = "manhattan",s=1,cores=getOption("utiml.cores",2))
predict_final <- predict(model_final,test_data)
test_pourcen <- seq(from=0.2,to=0.8,by=0.1)
predict_value <- data.frame()
for(i in test_pourcen ){
  predict6 <- predict_final
  predict6[predict6<i]<-0
  predict6[predict6>i]<-1
  hamming_loss_value <-hamming_loss(yeast_test[,labels_yeast],predict6)
  predict_value <- rbind(predict_value,c(i,hamming_loss_value))
}
names(predict_value)<-c('pourcentage',"erreur")
predict_value
predict6 <- predict_final
predict6[predict6<0.7]<-0
predict6[predict6>0.7]<-1
plot(predict_value$pourcentage,predict_value$erreur,type='l',
     xlab = "Probabilite entre 0.1 et 0.8",ylab = "Erreur")
hamming_loss6 <-hamming_loss(yeast_test[,labels_yeast],predict6)

yeast_test[1:20,labels_yeast]
