# load libraries
library(caret)
# load the dataset
data(iris)
# summarize data
summary(iris[,1:5])

print(NAValues<- is.na(iris))
print(duplicated(iris))
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(iris[,1:5], method=c("scale"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, iris[,1:5])
# summarize the transformed dataset
summary(transformed)

###############################################3
## correlation Plots #############################
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

pairs(transformed[1:5], lower.panel=panel.smooth, upper.panel=panel.cor)
transformed$Species<-as.numeric(transformed$Species)
hist(cor(transformed))

b <- transformed
b$Species <- as.numeric(b$Species)
apply(b, MARGIN = 2, FUN = function(x) sum(is.na(x)))
maxs = apply(b, MARGIN = 2, max)
mins = apply(b, MARGIN = 2, min)
scaled = as.data.frame(scale(b, center = mins, scale = maxs - mins))


#install.packages("cvTools")
library(cvTools)
dataset<-scaled
k <- 10 #the number of folds
folds <- cvFolds(NROW(dataset), K=k)
dataset$holdoutpred <- rep(0,nrow(dataset))


########################################
####Logistic Regression#################
########################################


#training the model
print("Logistic Regression")

for(i in 1:k){
  train<-dataset[folds$subsets[folds$which != i], ] 
  validation <- dataset[folds$subsets[folds$which == i], ]
  cl<-factor(train$Species)
  model<- glm(Species~.,family = quasibinomial ,data = train)
  fitted.results <- predict(model,validation)
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  #Acccuracy of the model 
  misClasificError <- mean(fitted.results !=testing.data$class)
  print(paste('Error',misClasificError))
}


########################################
#################KNN####################
########################################

library(class)
library(gmodels)

for(i in 1:10){
train <- dataset[folds$subsets[folds$which != i], ] #Set the training set
  validation <- dataset[folds$subsets[folds$which == i], ] #Set the validation set
  train.labels <- train$Species
  test.labels <- validation$Species
  knn <- knn(train, validation,k=5,cl=train.labels)
  dataset[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use
  CrossTable(x= test.labels, y=knn , prop.chisq = FALSE)
}


########################################
#################Bagging################
########################################

library(ipred)
library(rpart)

print('Bagging.')

for(i in 1:k){
  train <- dataset[folds$subsets[folds$which != i], ] #Set the training set
  validation <- dataset[folds$subsets[folds$which == i], ] #Set the validation set
  adaboost<-boosting(Species~., data=train,control=(minsplit=-1), maxdepth=5, mfinal=3)
  newpred <- predict(model,validation)
  fitted.results <- ifelse(newpred > 0.5,1,0)
  ref <- ifelse(validation$Species > 0.5,1,0)
  #confusionMatrix(data=fitted.results, reference=ref, positive = '1' )
}


########################################
#############Random Forest##############
########################################


library(randomForest)

for(i in 1:k){
  train <- dataset[folds$subsets[folds$which != i], ] #Set the training set
  validation <- dataset[folds$subsets[folds$which == i], ] #Set the validation set
  model <- randomForest(Species ~ ., data=train, boos = TRUE, mfinal = 10	)
  newpred <- predict(model,validation)
  dataset[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use
  predictionVal<- ifelse(newpred > 0.5,1,0)
  ref <- ifelse(validation$Species > 0.5,1,0)
  # calculating error. 
  error <- mean(ref!=predictionVal)
  print(paste('Error',error))
}



########################################
#################Boosting###############
########################################

print("Boosting")
library(adabag)
library(rpart)
dataset<- as.data.frame(dataset)

for(i in 1:k){
  train <- dataset[folds$subsets[folds$which != i], ] #Set the training set
  validation <- dataset[folds$subsets[folds$which == i], ] #Set the validation set
  cl<-train$Species
  adaboost <- boosting(Species~., data=train,control=(minsplit=-1), mfinal=3)
  predict<-predict(adaboost, training.data)
  #here we get weights for all trees.
  adaboost$weights
  #here we get the importance for all variables
  adaboost$importance
  #here we get the evolution of error. 
  errorevol(adaboost,training.data)
}
