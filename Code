if (!require("caret")) {
  install.packages("caret")
  library("caret")
}
if (!require("mice")) {
  install.packages("mice")
  library("mice")
}
if (!require("FNN")) {
  install.packages("FNN")
  library("FNN")
}
if (!require("DMwR")) {
  install.packages("DMwR")
  library("DMwR")
}
if (!require("e1071")) {
  install.packages("e1071")
  library("e1071")
}
if (!require("outliers")) {
  install.packages("outliers")
  library("outliers")
}
if (!require("Metrics")) {
  install.packages("Metrics")
  library("Metrics")
}
if (!require("leaps")) {
  install.packages("leaps")
  library("leaps")
}
if (!require("randomForest")) {install.packages('randomForest')
  library(randomForest)
}
if (!require("rpart")) {
  install.packages("rpart")
  library("rpart")
}
if (!require("rpart.plot")) {
  install.packages("rpart.plot")
  library("rpart.plot")
}
if (!require("psych")) {
  install.packages("psych")
  library("psych")
}
#Hierarchical Clustering
########################################################
myData <- read.csv("FinalDataFramingham.csv", stringsAsFactors = TRUE)
myDataCopy <- myData
#Replace cigsPerDay NAs with 20. Average of cigsPerDay was 18, 1 pack is 20 so we used 20 to reflect typical use by pack
myDataCopy$cigsPerDay[is.na(myDataCopy$cigsPerDay)] <- 20
anyNA(myDataCopy$cigsPerDay)
#Split data randomly into a training set and a test set
########################################################
trainSetSize <- floor(0.6 * nrow(myDataCopy))   
RNGkind(sample.kind = "Rejection")
set.seed(12345)                       
trainInd <- sample(seq_len(nrow(myData)), size = trainSetSize) 
myDataTrain <- myDataCopy[trainInd, ]               
myDataTest <- myDataCopy[-trainInd, ] 
########################################################
myDataScaled<-scale(myDataCopy[,1:14])
k=3
distNorm <- dist(myDataScaled, method="euclidean")
#create hierarchical clusters based on specific linkage method
hcSingle <- hclust(distNorm, method="single")
hcComplete <- hclust(distNorm, method="complete")
#plot the clusters (dendrogram)
plot(hcSingle, hang = -1, ann = FALSE)
plot(hcComplete, hang = -1, ann = FALSE)
#"cutting the dendrogram"
heart <- cutree(hcComplete,k=3)
heart
myDataClusteredH <- data.frame(myData,heart)
row.names(myDataScaled) <- paste(heart, ":", row.names(myDataScaled), sep ="")
#create a heatmap to describe clusters
heatmap(myDataScaled, Colv = NA, hclustfun = hclust, col = rev(paste("gray",1:99,sep="")))
#return the centroid of each cluster
aggregate(myDataClusteredH, by=list(heart), mean)
#display the full summary statistics for each cluster
describeBy(myDataClusteredH, myDataClusteredH$heart)


#KNN
############################################
myDataCopy <- read.csv("FinalDataFramingham.csv", stringsAsFactors = TRUE)
#Replace cigsPerDay NAs with 20. Average of cigsPerDay was 18, 1 pack is 20 so we used 20 to ensure a round number
myDataCopy$cigsPerDay[is.na(myDataCopy$cigsPerDay)] <- 20
anyNA(myDataCopy)
#Transform variables into their correct type
############################################
myDataCopy$TenYearCHD <- as.factor(myDataCopy$TenYearCHD)
########################################################
#Split data randomly into a training set and a test set
########################################################
trainSetSize <- floor(0.6 * nrow(myDataCopy))  
RNGkind(sample.kind = "Rejection")
set.seed(50)                      
trainInd <- sample(seq_len(nrow(myDataCopy)), size = trainSetSize)
myDataTrain <- myDataCopy[trainInd, ]              
myDataTest <- myDataCopy[-trainInd, ]
# Step 1: Array
myDataTrainSc <- myDataTrain
myDataTestSc <- myDataTest
myDataSc <- myDataCopy
# Step 2: Normalize
normValues <- preProcess(myDataTrain[,1:14], method = c("center", "scale"))
myDataTrainSc[,1:14] <- predict(normValues, myDataTrain[,1:14])
myDataTestSc[,1:14] <- predict(normValues, myDataTest[,1:14])
myDataSc[,1:14] <- predict(normValues, myDataCopy[,1:14])
# Step 3: Predict
predTestClass1 <- knn(train = myDataTrainSc[,1:14], test = myDataTestSc[,1:14], cl =
                        myDataTrainSc[,15], k = 1)
predTestClass3 <- knn(train = myDataTrainSc[,1:14], test = myDataTestSc[,1:14], cl =
                        myDataTrainSc[,15], k = 3)
predTestClass5 <- knn(train = myDataTrainSc[,1:14], test = myDataTestSc[,1:14], cl =
                        myDataTrainSc[,15], k = 5)
predTestClass7 <- knn(train = myDataTrainSc[,1:14], test = myDataTestSc[,1:14], cl =
                        myDataTrainSc[,15], k = 7)
# Step 4: print confusion matrix for each
actualTestClass <- myDataTestSc$TenYearCHD
#k1
confMx1 <- confusionMatrix(predTestClass1, actualTestClass, positive = "1")
confMx1
#k3
confMx3 <- confusionMatrix(predTestClass3, actualTestClass, positive = "1")
confMx3
#k5
confMx5 <- confusionMatrix(predTestClass5, actualTestClass, positive = "1")
confMx5
#k7
confMx7 <- confusionMatrix(predTestClass7, actualTestClass, positive = "1")
confMx7


#Logistic Regression
############################################
myData <- read.csv("FinalDataFramingham.csv", stringsAsFactors = TRUE)
myDataCopy <- myData
#Replace cigsPerDay NAs with 20. Average of cigsPerDay was 18, 1 pack is 20 so we used 20 to ensure a round number
myDataCopy$cigsPerDay[is.na(myDataCopy$cigsPerDay)] <- 20
anyNA(myDataCopy$cigsPerDay)
#Transform variables into their correct type
myDataCopy$male <- as.factor(myDataCopy$male)
myDataCopy$currentSmoker <- as.factor(myDataCopy$currentSmoker)
myDataCopy$BPMeds <- as.factor(myDataCopy$BPMeds)
myDataCopy$prevalentStroke <- as.factor(myDataCopy$prevalentStroke)
myDataCopy$prevalentHyp <- as.factor(myDataCopy$prevalentHyp)
myDataCopy$diabetes <- as.factor(myDataCopy$diabetes)
myDataCopy$TenYearCHD <- as.factor(myDataCopy$TenYearCHD)
#Split data randomly into a training set and a test set
trainSetSize <- floor(0.6 * nrow(myDataCopy))   
RNGkind(sample.kind = "Rejection")
set.seed(12345)                       
trainInd <- sample(seq_len(nrow(myData)), size = trainSetSize) 
myDataTrain <- myDataCopy[trainInd, ]               
myDataTest <- myDataCopy[-trainInd, ] 
#Create a simple logistic regression model using all variables
logRegrModelInc <- glm(TenYearCHD ~ ., 
                       data = myDataTrain,
                       family ="binomial")
summary(logRegrModelInc)
exp(coef(logRegrModelInc)) 
#Score the logistic regression model on the test data set
predTestScores <- predict(logRegrModelInc, type="response", newdata=myDataTest) 
#Classify test observations based on the probabilities 
#calculated from the logistic regression model
#Set cutoff value 
cutoff <- 0.5
##Initially, set all predicted class assignments to 0
predTestClass <- rep(0, length(predTestScores))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass[predTestScores >= cutoff] <- 1
#Create a confusion matrix to determine accuracy
actualTestClass <- myDataTest$TenYearCHD
actualTestClass <- as.numeric(as.character(actualTestClass))
#Simply using tables
confMx <- table(predTestClass, actualTestClass) 
confMx
confusionMatrix(as.factor(predTestClass), as.factor(actualTestClass), positive = "1")
#Create an ROC curve 
simpleROC <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}
rocData <- simpleROC(actualTestClass,predTestScores)
plot(rocData$FPR,rocData$TPR, xlab = "1 - Specificity", ylab = "Sensitivity")
#Use library pROC to plot ROC and 
#calculate area under the curve (AUC)
pROCData <- pROC::roc(myDataTest$TenYearCHD,predTestScores) 
plot(pROCData) # Gets a smoother version of the curve
#Create a lift chart
dfForLiftChart <- data.frame(predTestScores, actualTestClass) 
sortedData <- dfForLiftChart[order(-dfForLiftChart$predTestScores),] 
cumulCases <- cumsum(sortedData[,2]) 
##Plot the lift chart
plot(cumulCases, xlab = "Number of Cases", ylab = "Number of 1s Identified by Algorithm So Far", type="l", col = "blue") 
##Plot the 45 degree line
X <- c(0, length(predTestScores))
Y <- c(0, cumulCases[length(predTestScores)])
lines(X, Y, col = "red", type = "l", lty = 2)
#Lift chart using the "caret" library
li <-lift(relevel(as.factor(actualTestClass), ref="1") ~ predTestScores)
xyplot(li, plot = "gain")
pROCData[9]
#best subsets regression
ExhSearch <- regsubsets(TenYearCHD ~ ., data = myDataTrain) 
plot(ExhSearch,scale="adjr2") # displays results of exhaustive search in table format (best model for each #predictors)
summary(ExhSearch)$adjr2


#Naive Bayes
############################################
nbModel <- naiveBayes(TenYearCHD ~ male + age + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, myDataTrain)
nbModel
#Predict the probabilities and the outcomes for new observation(s) stored in test data set
predTestProb <- predict(nbModel, myDataTest, type = "raw") 
predTestClass <- predict(nbModel, myDataTest) #default cutoff is 0.5
#results
predTestProb
predTestClass
#Calculate the confusion matrix and the accuracy
actualTestClass <- myDataTest$TenYearCHD
confMx <- table(predTestClass, actualTestClass) 
confMx
confusionMatrix(as.factor(predTestClass), as.factor(actualTestClass), positive = "1")


#Classification Tree
############################################
#Transform variables into their correct type
############################################
myDataCopy$TenYearCHD <- as.numeric(as.character(myDataCopy$TenYearCHD))
#Split data into 75% training set and 25% test set
trainSetSize <- floor(0.7 * nrow(myDataCopy))   
RNGkind(sample.kind = "Rejection")
set.seed(50)                       
trainInd <- sample(seq_len(nrow(myDataCopy)), size = trainSetSize) 
myDataCopyTrain <- myDataCopy[trainInd, ]               
myDataCopyTest <- myDataCopy[-trainInd, ]
#Creating classTree1
classTree1 <- rpart(TenYearCHD ~ male + age + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, 
                    #classTree1 <- rpart(TenYearCHD ~ age + cigsPerDay + prevalentStroke + diabetes, 
                    data =myDataCopyTrain,
                    method = "class",
                    control = rpart.control(minsplit = ,cp = 0))

rpart.plot(classTree1, type=3, extra=2, fallen.leaves = FALSE,
           varlen = 4, tweak = 0.5, cex = 0.8)
classTree1 #print tree rules
printcp(classTree1) #print cp table (useful for pruning)
plotcp(classTree1) #plot graph of how xerror changes with cp
prp(classTree1, extra=1)
classTree1
#classTree2 Pruned
classTree2 <- prune(classTree1, cp=.00549828)
rpart.plot(classTree2, type=3, extra=2, fallen.leaves = FALSE,
           varlen = 6, tweak = 1.1, cex = 0.9)

printcp(classTree2)
printcp(classTree1)
plotcp(classTree2)
prp(classTree2, extra=1)
classTree2
#predict
predTestClass <- predict(classTree2, newdata = myDataCopyTest, type="class")
confMx <- table(actual = myDataCopyTest$TenYearCHD,
                predicted = predTestClass)
confMx
confusionMatrix(as.factor(predTestClass), as.factor(myDataCopyTest$TenYearCHD), positive = "1")
