# ----------------------------------------
# Function to write individual vector 
# elements to a file
#-----------------------------------------
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# ----------------------------------------
# Function to perform exploratory work
# on the prediction algorithm 
# -----------------------------------------
courseProject <- function(){
  # Load necessary libraries
  library(ggplot2)
  library(caret)

  # Load training data from file
  trainData <- read.csv("pml-training.csv", sep = ",", header = TRUE, na.strings = "NA")
  
  # Load the testing data for grading
  gradeData <- read.csv("pml-testing.csv", sep = ",", header = TRUE, na.strings = "NA")
  
  # Select only aggregated data as covariates
  avgCovs <- grep("^avg",colnames(trainData))
  varCovs <- grep("^var",colnames(trainData))
  minCovs <- grep("^min",colnames(trainData))
  maxCovs <- grep("^max",colnames(trainData))
  skeCovs <- grep("^skewness",colnames(trainData))
  kurCovs <- grep("^kurtosis",colnames(trainData))
  stdCovs <- grep("^stddev",colnames(trainData))
  ampCovs <- grep("^amplitude",colnames(trainData))
  
  aggCovs <- c(avgCovs,varCovs,minCovs,maxCovs,skeCovs,kurCovs,stdCovs,ampCovs)
  aggData <- trainData[,-aggCovs]
  aggData <- aggData[,-c(1:7)]
  
  gradeData <- gradeData[,-aggCovs]
  gradeData <- gradeData[,-c(1:7)]
  
  # Select only complete cases of the data (i.e. no NAs)
  aggData <- aggData[complete.cases(aggData),]

  # Pick 75% of the data as a training set
  inTrain <- createDataPartition(y=aggData$classe, p=0.75, list=FALSE)
  training <- aggData[inTrain,]
  testing <- aggData[-inTrain,]

  # Apply a random forest to the classification problem
  if(file.exists("randm_forest_model.rda")){
    load("randm_forest_model.rda")
  }
  else{
    ctrl <- trainControl(method="cv",number=5)
    modFit <- train(classe ~ ., data=training, method="rf", trControl = ctrl, allowParallel=TRUE)
    save(modFit, file = "randm_forest_model.rda")
  }
  
  # Apply model on testing data
  #testPred <- predict(modFit, testing)
  #confusionMatrix(testPred, testing$classe)
  
  # Apply the model on the grading data
  gradePred <- predict(modFit, gradeData)
  
  # Save predictions on grading data to file
  pml_write_files(gradePred)
}