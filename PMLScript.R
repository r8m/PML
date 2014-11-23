library(caret)
#load  data for model and submit data for 20 cases.
if (!file.exists("pml-training.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")
}
if (!file.exists("pml-testing.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")
}
pmlData <- read.csv("pml-training.csv", sep = ",", na.strings = c("NA",""))
submitData <- read.csv("pml-testing.csv", sep = ",", na.strings = c("NA",""))

#remove useless features from data  contain timestamps,  string names, boolean variables. Also in order to simplify the input for our algorithm we reduce number of features by removing all columns containing at least one NA. We also switch `classe` definitive column to be first
useless<- grep("timestamp|X|user_name|new_window", names(pmlData))
pmlData <- pmlData[,-useless]
nonNA <- colSums(is.na(pmlData))==0
pmlData <- pmlData[, nonNA]
pmlData<-pmlData[, c(ncol(pmlData), 1:ncol(pmlData)-1)]


inTrain <- createDataPartition(y = pmlData[,1], p = 0.6, list=FALSE)
#training data set
training <- pmlData[inTrain,]

testcv <- pmlData[-inTrain,]
inTrain <- createDataPartition(y = testcv[,1],  p =  0.5, list=FALSE)

#cross validation data set
cv <- testcv[inTrain,]

#test data set
test<- testcv[-inTrain,]

#reduce the number of features with Dimensionality reduction procedure:
preproc <- preProcess(pmlData[,-1], method='pca', thresh=0.99)

training.pca <- predict(preproc, training[,-1])     
cv.pca <- predict(preproc, cv[,-1])    
test.pca <- predict(preproc, test[,-1])    

# Train Random Forest (RF)
trainControl <- trainControl(method = "cv", number = 10)
modelFitRF <- train(training$classe ~., data=training.pca, method='rf', trControl = trainControl)

# Train support vector machine (SVM)
modelFitSVM <- train(training$classe ~., data=training.pca, method='svmRadial')

# Train Generalized Boosted Regression Models (GBM)
modelFitGBM <- train(training$classe ~., data=training.pca, method="gbm", verbose=FALSE)

# Train atent Dirichlet allocation (LDA)
modelFitLDA <- train(training$classe ~., data=training.pca, method="lda")

# Cross Validation test
predictionRF <- predict(modelFitRF,  cv.pca)
predictionSVM <- predict(modelFitSVM,  cv.pca)
predictionGBM <- predict(modelFitGBM,  cv.pca)
predictionLDA <- predict(modelFitLDA,  cv.pca)

confusionMatrix(predictionRF, cv$classe)$overall["Accuracy"]
confusionMatrix(predictionSVM, cv$classe)$overall["Accuracy"]
confusionMatrix(predictionGBM, cv$classe)$overall["Accuracy"]
confusionMatrix(predictionLDA, cv$classe)$overall["Accuracy"]

testPrediction <- predict(modelFitRF, testpca)
confusionMatrix(testPrediction, test$classe)

#This sript creates 20 txt files for second part of assigment
submitPrediction <- predict(modelFitRF, submit[,-"classes"])
pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                    col.names = FALSE)
    }
}
pml_write_files(submitPrediction)