propocessing_na <- function (data){

}
fit_transform <- function (data,labels){
 return(mldr_from_dataframe(data, labelIndices = labels))
}
export_mldr <- function(data,filename){
  return (write_arff(data,filename = filename,write.xml = True))
}
list_element <- function (datas){
  result <- list()
  for(i in seq_along(datas)){
    for(j in datas[i]){
      for(k in j){
        if(!(k %in% result)){
          print(k)
          result <- c(result,k)
        }
      }
    }
  }
  return(result)
}

binarizer <- function(x,y,elements){
  col_number <- ncol(x)
  for( i in elements){
    col_number <- col_number+1
    x<-cbind(x,y %in% i)
    names(x)[col_number]<- i
  }
  return (x)
}
debinarizer <- function(x,labels){
  b<-list()
  for(i in seq_len(nrow(x))){
    a<-list()
    for(k in labels){
      if(x[i,k]==1){
        a<-c(a,names(x)[k])
      }
    }
    b[[i]]<-a
  }
  x<-cbind(x,b)
  names(x)[ncol(x)]<- "Classe"
  return(x)
}

?apply