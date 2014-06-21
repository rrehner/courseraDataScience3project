run_analysis <- function(directory) {
    # 1 Merges the training and the test sets to create one data set.
    features <- read.table(paste(directory, "features.txt", sep="/"))
    labels <- gsub("[^A-Za-z0-9]",".",features$V2)
    
    testSubject <- read.table(paste(directory, "test/subject_test.txt", sep="/"))
    names(testSubject) <- c("subject")
    testY <- read.table(paste(directory, "test/y_test.txt", sep="/"))
    names(testY) <- c("activity")
    testX <- read.table(paste(directory, "test/X_test.txt", sep="/"))
    names(testX) <- labels
    test <- cbind(testSubject,testY,testX)
    
    trainSubject <- read.table(paste(directory, "train/subject_train.txt", sep="/"))
    names(trainSubject) <- c("subject")
    trainY <- read.table(paste(directory, "train/y_train.txt", sep="/"))
    names(trainY) <- c("activity")
    trainX <- read.table(paste(directory, "train/X_train.txt", sep="/"))
    names(trainX) <- labels
    train <- cbind(trainSubject,trainY,trainX)
    
    all <- rbind(test,train)
    
    # 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
    extract <- all[,grep("subject|activity|mean\\.|std\\.", names(all))]
    
    # 3 Uses descriptive activity names to name the activities in the data set
    act <- read.table(paste(directory, "activity_labels.txt", sep="/"))
    extract$activity <- factor(extract$activity, levels = act$V1, labels = gsub("_", " ", tolower(act$V2)))
    
    # 4 Appropriately labels the data set with descriptive variable names. 
    names(extract) <- gsub("\\.\\.|\\.\\.\\.","\\.",names(extract))
    
    # 5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
    tidy <- aggregate(. ~ subject + activity, data=extract, mean)
    tidy
}


