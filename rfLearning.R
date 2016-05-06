# Load libraries
library(randomForest)
library(caret)
library(plotly)

#######
# We will be splitting the dataset into a training, testing and cross validation dataset with a 60/20/20 split.
# Training will obviously be used for training the model, and the CV will be used to refine the training model,
# the testing set will not be touched at all until we are completely finished, to avoid accidentally fitting it
# indirectly by switching methods halfway through, because we see better results with another machine learning
# algorithm or features (that's what the CV dataset is for).
#######

# Load the data from our createDatamart.R file
data <- readRDS("learningDatamart.RDS")

# Set a seed so that the training, testing and CV datasets always containt he same matches.
set.seed(322)

## Split into training and testing 60/40
trainIndex<- createDataPartition(data$result, p = 0.60,list=FALSE)
training<- data[trainIndex,]
testing_temp<- data[-trainIndex,]

## Now split the testing into actual testing and CV
testIndex<- createDataPartition(testing_temp$result, p = 0.50,list=FALSE)
testing<- testing_temp[testIndex,]
CV<- testing_temp[-testIndex,]

########
## We will be using the randomforest package to do the learning, since it is usually the most accurate, but 
## also because we are interested in being able to interpret the results, and we can pick out single trees to
## get a good idea of what is happening and why. We must first find the best values to use for settings. That
## is to say, how many trees to build, and on minimum, how big should a node be. The default is 5, but with so 
## much data, we overfit grossly if we don¨t set it.
#######

# We set the different values we want to test.
nodeSize<-c(10,20,40,100,200,400)
numberOfTrees<-c(100,200,400,600,800,1000)

results<-data.frame(nodeSize=integer(),
                    numberOfTrees=integer(),
                    accuracy=double())

# It´s not a small dataset though so this takes some time, but worth it.
for(i in nodeSize){
    for(d in numberOfTrees){
        print(paste("Nodesize:",i," Trees:",d))
        rfFit<-randomForest(result~.,data=training,nodesize=i, ntree=d)
        rfPredictions<-predict(rfFit,newdata=CV)
        rfMatrix<-confusionMatrix(rfPredictions,CV$result)    
        
        tempVec<-c(i,d,rfMatrix$overall[1])
        
        results<-rbind(results,tempVec)
    }
}

colnames(results)<-c("nodeSize","treeNumber","accuracy")

# Plot the results
plot_ly(results, x = nodeSize, y = treeNumber, z = accuracy, type = "scatter3d", mode = "markers")

# Find the best results, it's 100 nodesize and 400 treesS
bestResult <- results[which(results$accuracy==max(results$accuracy)),]

# Do the final random forest with the best results obtained from the CV set.
rfFit <- randomForest(result~.,data=training,nodesize=bestResult[1,1], ntree=bestResult[1,2])
rfPredictions <- predict(rfFit,newdata=training)

# This gives ~70% accuracy on the training dataset
rfMatrix <- confusionMatrix(rfPredictions,training$result)

# ANd ~ 53% on the CV dataset
rfPredictions <- predict(rfFit,newdata=cv)
rfMatrix <- confusionMatrix(rfPredictions,cv$result)
rfMatrix

