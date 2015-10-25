setwd("/Users/janicecanedo/Desktop/Coursera/UCI HAR Datasety/")
library(data.table)
require(reshape2)



#reads activities and features
activityLabels <- read.table("activity_labels.txt", header=FALSE)
features <- read.table("features.txt")[,2]

#reads test data
subjectTest <- read.table("test/subject_test.txt", header = FALSE)
xTest <- read.table("test/X_test.txt", header=FALSE)
yTest <- read.table("test/y_test.txt", header=FALSE)

#reads training data
subjectTrain <- read.table("train/subject_train.txt", header=FALSE)
xTrain <- read.table("train/X_train.txt", header=FALSE)
yTrain <- read.table("train/y_train.txt", header=FALSE)

#binds test and training data
subject <- rbind(subjectTest, subjectTrain)
x <- rbind(xTest, xTrain)
y <- rbind(yTest, yTrain)

#sets column names
colnames(x) <- features
colnames(y) <- "Activity"
colnames(subject) <- "Subject"

#Merges datasets
completedSet <- cbind(x,y,subject)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
columnsWithMean <- grep(".*Mean.*|.*Std.*", names(completedSet), ignore.case=TRUE)

#gets required Columns
requiredColumns <- c(columnsWithMean, 562, 563)

#Uses the required Columns to created the extracted data
extracted <- completedSet[,requiredColumns]

#set column information to character for comparison
extracted$Activity <- as.factor(extracted$Activity)

#Uses for loop to add activty name
for (i in 1:6){
  extracted$Activity[extracted$Activity == i] <- as.character(activityLabels[i,2])
}

#set column information back to a factor
extracted$Activity <- as.factor(extracted$Activity)

names(extracted)


#We want to make names more readable

names(extracted)<-gsub("Acc", "Accelerometer", names(extracted))
names(extracted)<-gsub("Gyro", "Gyroscope", names(extracted))
names(extracted)<-gsub("BodyBody", "Body", names(extracted))
names(extracted)<-gsub("Mag", "Magnitude", names(extracted))
names(extracted)<-gsub("^t", "Time", names(extracted))
names(extracted)<-gsub("^f", "Frequency", names(extracted))
names(extracted)<-gsub("tBody", "TimeBody", names(extracted))
names(extracted)<-gsub("-mean()", "Mean", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("-std()", "STD", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("-freq()", "Frequency", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("angle", "Angle", names(extracted))
names(extracted)<-gsub("gravity", "Gravity", names(extracted))


#view new names

names(extracted)

extracted$Subject <- as.factor(extracted$Subject)
extracted <- data.table(extracted)


tidyData <- aggregate(. ~Subject + Activity, extracted, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)