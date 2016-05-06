## I want to try a logistical regression using GLM, with a regulirization term to down grade terms that are not important.
source(file="interactionTerms.R")
library(glmnet)
library(caret)

data <- readRDS("learningDatamart.RDS")

dataScaled <- as.data.frame(scale(data[,1:23]))

withInteractions<-interactionTerms(dataScaled, 2, 2)

withInteractions$result <- as.factor(ifelse(data$result == "Radiant Win", 1,0))

# Set a seed so that the training, testing and CV datasets always containt he same matches.
set.seed(322)

## Split into training and testing 60/40
trainIndex<- createDataPartition(withInteractions$result, p = 0.60,list=FALSE)
training<- withInteractions[trainIndex,]
testing_temp<- withInteractions[-trainIndex,]

## Now split the testing into actual testing and CV
testIndex<- createDataPartition(testing_temp$result, p = 0.50,list=FALSE)
testing<- testing_temp[testIndex,]
CV<- testing_temp[-testIndex,]


## Need to create a mtrix for it to fit the format that GLMnet needs
tempGLMtraining<-as.matrix(training[,1:254])
tempGLMTesting<-as.matrix(testing[,1:254])
tempGLMcrossValidation<-as.matrix(CV[,1:254])

## Away Win
y <- as.numeric(training$result)-1
cvFit<-cv.glmnet(tempGLMtraining,y,family="binomial")
glmTraining<-as.data.frame(predict(cvFit, tempGLMtraining, s= "lambda.min", type="response"))
colnames(glmTraining)<-c("prob")

glmTraining$result <- ifelse(glmTraining$prob > 0.5, 1, 0)

glmMatrix <- confusionMatrix(glmTraining$result,training$result)
