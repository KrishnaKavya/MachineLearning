
bayes <- function(train, test ){
  if(missing(train)||missing(test)){
    return('files are missing')
  }
  
  trainData <- read.csv(train,header = TRUE, sep = ',', dec = '.')
  testData <- read.csv(test,header = TRUE, sep = ',', dec = '.')
  NumberOfInstances<- nrow(trainData)
  class0<- sum(trainData$class==0)
  class1<-sum(trainData$class==1)
  cat('P(Class=0)',class0/NumberOfInstances, sep='=')
  
  for(col in colnames(trainData[,-7])){
    frequency<-count(trainData, col)
    classone=frequency[1,1]
    classonefreq=frequency[1,2]
    pone=round(classonefreq/class0, digits = 3)
    cat(paste(' P(',col,'=',classone,'|',0,') =',pone))
    
  }
  cat('\n')
  cat(' P(Class=1)',class1/NumberOfInstances, sep='=')
  
  for(col in colnames(trainData[,-7])){
    classtwo=frequency[2,1]
    classtwofreq=frequency[2,2]
    ptwo=round(classtwofreq/class1, digits = 3)
    cat(paste('P(',col,'=',classtwo,'|',1,') = ',ptwo))
  }
}