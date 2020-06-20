##Libraries Used
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)

##Read Supporting Metadata
# read features
featureNames <- read.table("C:/Users/94710/Desktop/coursera/UCI HAR Dataset/features.txt")
# read activity labels
activityLabels <- read.table("C:/Users/94710/Desktop/coursera/UCI HAR Dataset/activity_labels.txt", header = FALSE)

##Format training and test data sets
##Read training data
subjectTrain <- read.table("C:/Users/94710/Desktop/coursera/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("C:/Users/94710/Desktop/coursera/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("C:/Users/94710/Desktop/coursera/UCI HAR Dataset/train/X_train.txt", header = FALSE)

##Read test data
subjectTest <- read.table("C:/Users/94710/Desktop/coursera/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("C:/Users/94710/Desktop/coursera/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("C:/Users/94710/Desktop/coursera/UCI HAR Dataset/test/X_test.txt", header = FALSE)

##Merge the training and the test sets to create one data set
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

##Naming the columns
colnames(features) <- t(featureNames[2])

##Part1 - Merge the data
# assign column names
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

##Part2 - Extracts only the measurements on the mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

##Part3 - Uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

##Part4 - Appropriately labels the data set with descriptive variable names
names(extractedData)

# expand abbreviations and clean up names
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)

##Part5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
write.table(tidyData, "C:/Users/94710/Desktop/Tidy.txt", row.names = FALSE,sep=" ")
