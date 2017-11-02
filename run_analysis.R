library(dplyr)

cleanData <- function() {
  
    # Read features vector
  features <- read.table("features.txt")
  
  # Read test and train data
  setwd("test")
  stest <- read.table("subject_test.txt")
  ytest <- read.table("y_test.txt")
  xtest <- read.table("x_test.txt")
  setwd("..")
  setwd("train")
  strain <- read.table("subject_train.txt")
  ytrain <- read.table("y_train.txt")
  xtrain <- read.table("x_train.txt")
  setwd("..")
  
  # Combine data
  sall <- rbind(strain, stest)
  yall <- rbind(ytrain, ytest)
  xall <- rbind(xtrain, xtest)
  
  # Update variables names of relevant names
  names(sall) <- c("subject")
  names(yall) <- c("activity")
  names(xall) <- features[[2]]
  
  # Filter features for means and standard deviations
  meanstd <- grep("mean\\(\\)|std\\(\\)", features[[2]], value = T)
  xall <- xall[,meanstd]
  
  # Combine all data sets
  dsall <- cbind(sall,yall,xall)
  
  # Update activity with factor names
  dsall$activity <- factor(dsall$activity, dsall$activity[[1]], dsall$activity[[2]])
  
  # Calculate mean of each variable for each activity and subject
  resultDS <- group_by(dsall,subject,activity) %>% summarize_all(funs(mean))
  
  # Write resulting dataset to a text file
  write.table(resultDS, file = "resultDS.txt", row.names = F)
}
