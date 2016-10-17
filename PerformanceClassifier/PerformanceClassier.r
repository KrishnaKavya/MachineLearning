#library(caret)
#Cosidering Iris Dataset.
#reading data
t <- iris

#converting categorical variable to numeric
t$Species <- as.numeric(t$Species)

#Preprocessing the Data

# removing the NA instances from the dataframe. na.omit all the 
na.omit(t)

#Scaling data.
t$Sepal.Length <- scale(t$Sepal.Length, center = FALSE, scale = TRUE)
t$Sepal.Width <-scale(t$Sepal.Length, center = FALSE, scale = TRUE)
t$Petal.Length <- scale(t$Petal.Length, center = FALSE, scale = TRUE)
t$Petal.Width <-scale(t$Petal.Width,center=FALSE, scale= TRUE)


#Removing duplicates The unique method returns the dataframe without duplicate rows.
t <-unique(ts, incomparables = FALSE)

#Correlation Coeeficient: It is the measurement of how the attributes are linerly related.
# if the value is close to 1: variables are positively linearly related.(scatter plot with +ve slope with point around)
# if the value is close to -1: variables are negatively linearly related.(scatter plot with -ve slope with points around)
# if the value is close to 0: variables have weal linear relationship. (points scattered)

cor(t$Sepal.Length,t$Species)
#[1] 0.7825612
cor(t$Sepal.Width,t$Species)
#[1] -0.4266576
cor(t$Petal.Length,t$Species)
#[1] 0.9490347
cor(t$Petal.Width,t$Species)
#[1] 0.9565473


#scatterplots of attributes. 
pairs((iris[1:4]), main = "Iris Data",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

#Section: 4  Classifiers

#1 Decision Tree.
#install.packages("rpart")
library(rpart)
#fetching data
data("iris")

#creating a Decision Tree, class- Classification Problem, and split is performed based on Information gain
fit<- rpart(Species~Sepal.Width+Sepal.Length+Petal.Width+Petal.Length, data=iris, method = 'class', parms = list(split="information"))
print(fit)
summary(fit)

#Display Decision Tree.
plot(fit)
text(fit, all=TRUE)






#3. Neural Net
#install.packages("neuralnet", dependencies = T)
library(neuralnet)
#normalising the data.Finding the min and max values column wise. 
t<-iris
t$Species <- as.numeric(t$Species)
apply(t, MARGIN = 2, FUN = function(x)sum(is.na(x)))
maxs <-	apply(t,	MARGIN	=	2,	max)
mins <-	apply(t,	MARGIN	=	2,	min)
scaled <-	 as.data.frame(scale(t,	center=mins,	scale	=	maxs	- mins))

#creating Training and test datasets:
trainIndex	<- sample(1:nrow(scaled),	0.8 *	nrow(scaled))
train	<- scaled[trainIndex,	]
test	<- scaled[-trainIndex,	]

#training neuralnet.
#In neural net to create models we need a formula of type y~x 
#y- predicted value, x- attributes. NN needs the column names. 

#fetching column names
n <-names(train)
#finding column names except for the predicted value.
f <- as.formula(paste("Species ~", paste(n[!n%in%"Species"],collapse="+")))

#creating a neutal network with 1 hidden layer and 1 inner node. 
nn <-neuralnet(f, data=train)

#Analysis of Neural net. 
plot(nn)
#summary
nn$result.matrix
#predicting the output using the test data
pred	<- compute(nn,test[,1:4])

#fetching the original scale values to 
pred.scaled	<- pred$net.result *(max(t$Species)-min(t$Species))+min(t$Species)
real.values	<- (test$Species)*(max(t$Species)-min(t$Species))+min(t$Species)

#Mean Square Error 
MSE.nn	<- sum((real.values	- pred.scaled)^2)/nrow(test)
#0.037534

#Plot
plot(real.values, pred.scaled, col='red',main='Real	vs	predicted	NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red',	bty='n')


#Increasing the hidden layers and nodes.  2 layers 5 nodes and 3 nodes in each layer respectively.
nn <-neuralnet(f, data=train, hidden=c(2,2))
# after repeating the process of Analysis of neural net. The mean Square error was 0.036472


#4. SVM

#install.packages("e1071")
library(e1071)

data("iris")
#seperating training data and testing data.
index	<- 1:nrow(iris)
testindex	<- sample(index,	trunc(length(index)/3))
testset	<- iris[testindex,]
trainset	<- iris[-testindex,]

#creating the model
svm.model	<- svm(Species	~	.,	data	=	trainset,	cost	=	100,	gamma	=	1)
#Predicting values
svm.pred	<- predict(svm.model,	testset[,-10])

summary(svm.pred)
plot(svm.pred)
