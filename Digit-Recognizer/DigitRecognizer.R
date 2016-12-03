
#Reading data
data <- read.csv("train.csv")

#Splitting input data to 80% training data and 20% testing.
trainIndex <- sample(1:nrow(data), 0.8 * nrow(data))

#Assigning data.
train <- data[trainIndex, ]
test <- data[-trainIndex, ]


#Method to rotate a matrix. This helps in displaying the picture.
rotate <- function(x) t(apply(x,2,rev)) #Reverse or rotate a matrix

#Plot some images from input
#First 6 rows are taken from the training data and ploted.in a matrix of size 28*28. 
par(mfrow=c(2,3))
lapply(1:6,
       function(x) image(rotate(matrix(unlist(train[x,-1]),nrow = 28, byrow = T))
                         ,col=grey.colors(255),xlab = train[x,1]
                         )
)

par(mfrow=c(1,1)) # set plot options back to default



#Deep Learning

#install.packages("h2o")
library(h2o)

#start the local cluster with usage of 6GB RAM and all CPU for processing. 
localH2o=h2o.init(max_mem_size='7g')

#MNIST data to H2O
#convert digit labels to factor for classification.
train[,1]=as.factor(train[,1])
test[,1]=as.factor(test[,1])

#converting training and test data to h2o
train_h2o=as.h2o(train)
test_h2o=as.h2o(test)


#Training model
model <- h2o.deeplearning(x=2:785, # column range for predictor
                          y=1, #Column number of label
                          training_frame = train_h2o, #Training data in H2O format
                          activation = "RectifierWithDropout", #Algorithm for neural nets
                          input_dropout_ratio = 0.2, # input drop percentage.
                          hidden_dropout_ratios = c(0.5,0.5), #node drop percentage
                          balance_classes = TRUE, 
                          hidden = c(120,120), #Two layer neural net with 120 nodes in each hidden layer
                          momentum_stable = 0.99, 
                          nesterov_accelerated_gradient = TRUE,
                          epochs = 15 
                        )

#Confusion Matrix
h2o.confusionMatrix(model)


#Predicting the test data
predict <- h2o.predict(model, test_h2o)


#Plot of predicted labels and Test data.
par(mfrow=c(4,4))
lapply(1:16,
       function(x) {
                        predict <- h2o.predict(model, test_h2o[x,])
                        image(rotate(matrix(unlist(test[x,-1]),nrow = 28, byrow = T))
                         ,col=grey.colors(255),xlab = predict[1,1])
        }
)

par(mfrow=c(1,1)) # set plot options back to default

#Calculation the mis classified predictions
misClasificError <- mean(predict[,1] !=test_h2o[,1])

#Printing the Accuracy 
print(paste('Accuracy',1-misClasificError))

#Shutting down the virtual machine.
h2o.shutdown(prompt=F)
