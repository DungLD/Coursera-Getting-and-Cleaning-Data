
# 1. create one dataset 
    # read data and merge train and test data
    dataPath =  "UCI HAR Dataset/"  
    activityTrain <- read.table(file.path(dataPath, "train", "Y_train.txt"), header = F)
    activityTest <- read.table(file.path(dataPath, "test", "Y_test.txt"), header = F)
    activity <- rbind(activityTrain, activityTest)  
    
    subjectTrain <- read.table(file.path(dataPath, "train", "subject_train.txt"), header = F)
    subjectTest <- read.table(file.path(dataPath, "test", "subject_test.txt"), header = F)
    subject <- rbind(subjectTrain, subjectTest)
    
    featuresTrain <- read.table(file.path(dataPath, "test", "X_test.txt"), header = F)
    featuresTest <- read.table(file.path(dataPath, "train", "X_train.txt"), header = F)
    features <- rbind(featuresTrain, featuresTest)
    
    # read variables
    names(subject) <- c("subject")
    names(activity) <- c("activity")
    featuresNames <- read.table(file.path(dataPath, "features.txt"), header = F)
    names(features) <- featuresNames[,2]
    
    #merging data
    subject_activity <- cbind(subject, activity)
    Data <- cbind(features, subject_activity)
  
#2. Extracts only measurements on the mean and standard deviation for each measurement
    mean_and_std_featuresNames <- featuresNames[,2][grep("-(mean|std)\\(\\)", featuresNames[,2])] 
    selectedNames <- c(as.character(mean_and_std_featuresNames), "subject", "activity")
    Data <- subset(Data, select=selectedNames)
    
#3  Uses descriptive activity names to name the activities in the dataset
    activityLabels <- read.table(file.path(dataPath, "activity_labels.txt"), header = F) 
    Data$activity <- factor(Data$activity, levels = activityLabels$V1, labels = activityLabels$V2)
    
#4 Appropriately labels the data set with descriptive variable names
    names(Data) <- gsub("^t", "time", names(Data))
    names(Data) <- gsub("^f", "frequency", names(Data))
    names(Data) <- gsub("Acc", "Accelerometer", names(Data))
    names(Data) <- gsub("Gyro", "Gyroscope", names(Data))
    names(Data) <- gsub("Mag", "Magnitude", names(Data))
    names(Data) <- gsub("BodyBody", "Body", names(Data))
#5 Creates a second, independently tidy dataset and output it
    oData <- aggregate(.~subject + activity, Data, mean)
    oData <- oData[order(oData$subject, oData$activity),]
    write.table(oData, file = "tidydata.txt", row.name = F)
