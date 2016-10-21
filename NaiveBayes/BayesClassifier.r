
bayes <- function(train, test ){
  if(missing(train)||missing(test)){
    return('files are missing')
  }
  
  trainData <- read.csv(train,header = TRUE, sep = ',', dec = '.')
  testData <- read.csv(test,header = TRUE, sep = ',', dec = '.')
  
#Question 1:
  
  #Training a Naive Bayes model on the training data. 
  nb_model<-naiveBayes(as.factor(class)~.,data=trainData)
 
  
  
   print("Class Prior and Class Conditional Probabilities")
   print("Prior and conditional probability for class 0")
  
   #Compute the class prior first.
   NumberOfInstances<- nrow(trainData)
   class0<- sum(trainData$class==0)
   class1<-sum(trainData$class==1)
   cat('P(Class=0)',class0/NumberOfInstances, sep='=')
   
   #Prior and Probability for the class 0.
   #The conditional probabilities are calculated. 
   #For each column . The (value=0 and class=0)/(class=0) 2. the value=1 and class=0/class=0 is calculated. 
   # Attribute/column  class=0
   # value=0             cv0
   #value=1              cv1
   for(col in colnames(trainData[,-7])){
     cv0=(sum(trainData[,col]==0 & trainData$class==0)/sum(trainData$class==0))
     cv1=(sum(trainData[,col]==1 & trainData$class==0)/sum(trainData$class==0))
     cat(paste(' P(',col,'=0|0)=',round(cv0,digits = 3)))
     cat(paste(' P(',col,'=1|0)=',round(cv1,digits = 3)))
   }
   
 cat("\n")
   print("Prior and conditional probability for class 1")
   #Prior and Probability for the class 1.
   #The conditional probabilities are calculated. 
   #For each column . The (value=0 and class=1)/(class=1) 2. the value=1 and class=1/class=1 is calculated. 
   #Attribute/column  class=1
   #value=0             cv0
   #value=1              cv1
   
   cat('P(Class=1)',class1/NumberOfInstances, sep='=')
   for(col in colnames(trainData[,-7])){
     cv0=(sum(trainData[,col]==0 & trainData$class==1)/sum(trainData$class==1))
     cv1=(sum(trainData[,col]==1 & trainData$class==1)/sum(trainData$class==1))
     cat(paste(' P(',col,'=0|1)=',round(cv0,digits = 3)))
     cat(paste(' P(',col,'=1|1)=',round(cv1,digits = 3)))
   }
  

  #Predicting based on the Training data 
  nb_train_predict<-predict(nb_model,trainData[,-7])
  summary(nb_model)
  #confusion Matrix
  #table(pred=nb_train_predict,true=trainData[,7])
  
  cat("\n")
  #fraction of correct predictions
  Training_Data_accuracy_percentage <- mean(nb_train_predict==trainData$class)*100
  cat(paste("The Training dataset accuracy is",Training_Data_accuracy_percentage))
  
  #Predicting based on the Testing data
  nb_test_predict<-predict(nb_model,testData[,-7])
  
  #confusion Matrix for the Test Data.
  cat("\n")
  Testing_Data_accuracy_percentage<-mean(nb_test_predict==testData$class)*100
  cat(paste("The Testing dataset accuracy is ",Testing_Data_accuracy_percentage))
}