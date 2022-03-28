source("yeast/function/binarizer.R")
models <- function(k,distance,data_train,data_test,labels_data){
  n_iter1<- length(distance)
  n_iter2 <- k
  pb1 <- txtProgressBar(min = 0, max =n_iter1, style = 3,width = 50,char = "*")
  init1 <- numeric(n_iter1)
  end1 <- numeric(n_iter1)
  init2 <- numeric(n_iter2)
  end2 <- numeric(n_iter2)
  train_data <-fit_transform(data_train,labels = labels_data)
  test_data <- fit_transform(data_test,labels = labels_data)
  list_metric <- list()
  indice<-1
  for(j in distance){
    print(j)
    pb2 <- txtProgressBar(min = 0, max = n_iter2,style = 3,width = 50,char = "=")
    init1[indice] <- Sys.time()
    metric <- data.frame()
    for(i in 7:k){
      init2[i] <- Sys.time()
      model <- mlknn(train_data,k= i,distance = j,cores = getOption("utiml.cores",2))
      data_generalisation <- predict(model,train_data)
      data_predict <- predict(model,test_data)
      data_predict[data_predict<0.6]<-0
      data_predict[data_predict>=0.6]<-1
      data_generalisation[data_predict<0.6]<-0
      data_generalisation[data_predict>=0.6]<-1
      hamming_loss_test <-hamming_loss(data_test[,labels_data],data_predict)
      hamming_loss_train <- hamming_loss(data_train[,labels_data],data_generalisation)
      metric_test <- multilabel_evaluate(test_data,data_predict,measures = c("accuracy", "F1"))
      metric_validation <- multilabel_evaluate(train_data,data_generalisation,measures = c("accuracy", "F1"))
      metric <- rbind(metric,c(i,hamming_loss_test,metric_test['accuracy'],metric_test['F1'],hamming_loss_train,metric_validation['accuracy'],metric_validation['F1']))
      end2[i] <- Sys.time()
      setTxtProgressBar(pb2, i)
      time <- round(seconds_to_period(sum(end2 - init2)), 0)
      est <- n_iter2 * (mean(end2[end2 != 0] - init2[init2 != 0])) - time
      remainining <- round(seconds_to_period(est), 0)

      cat(paste(" // Temps Excecution:", time," // Temps Estimer:", remainining), "")
    }
    close(pb2)
    names(metric)<-c('K',"hamming_loss_test","accuracy_test","f1_test","hamming_loss_train","accuracy_train","f1_train")
    list_metric[[indice]] <- metric
    end1[indice]<-Sys.time()
    setTxtProgressBar(pb1, indice)
    time1 <- round(seconds_to_period(sum(end1 - init1)), 0)
    est1 <- n_iter1 * (mean(end1[end1 != 0] - init1[init1 != 0])) - time1
    remainining1 <- round(seconds_to_period(est1), 0)

    cat(paste(" // Temps Execution:", time1,
              " // Temps Estime time remaining:", remainining1), "")
    indice <- indice + 1
  }
  close(pb1)
    return (list_metric)
}