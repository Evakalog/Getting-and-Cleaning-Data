#read libraries
library(data.table)
library(dplyr)

#read data
feature <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
features_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
features_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Merges the training and the test sets to create one data set.
subject <- rbind(subject_train, subject_test)
activity <- rbind(activity_train, activity_test)
features <- rbind(features_train, features_test)
colnames(features) <- t(feature[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
complete_data <- cbind(features,activity,subject)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
columns_meanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
required_col <- c(columns_meanSTD, 562, 563)
dim(complete_data)
extracted_data <- complete_data[,required_col]
dim(extracted_data)

#Uses descriptive activity names to name the activities in the data set
extracted_data$Activity <- as.character(extracted_data$Activity)
for (i in 1:6){
  extracted_data$Activity[extracted_data$Activity == i] <- as.character(activity_labels[i,2])
}
extracted_data$Activity <- as.factor(extracted_data$Activity)

#Appropriately labels the data set with descriptive variable names. 
names(extracted_data)
names(extracted_data)<-gsub("Acc", "Accelerometer", names(extracted_data))
names(extracted_data)<-gsub("Gyro", "Gyroscope", names(extracted_data))
names(extracted_data)<-gsub("BodyBody", "Body", names(extracted_data))
names(extracted_data)<-gsub("Mag", "Magnitude", names(extracted_data))
names(extracted_data)<-gsub("^t", "Time", names(extracted_data))
names(extracted_data)<-gsub("^f", "Frequency", names(extracted_data))
names(extracted_data)<-gsub("tBody", "TimeBody", names(extracted_data))
names(extracted_data)<-gsub("-mean()", "Mean", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-std()", "STD", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-freq()", "Frequency", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("angle", "Angle", names(extracted_data))
names(extracted_data)<-gsub("gravity", "Gravity", names(extracted_data))
names(extracted_data)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
extracted_data$Subject <- as.factor(extracted_data$Subject)
extracted_data <- data.table(extracted_data)
tidy <- aggregate(. ~Subject + Activity, extracted_data, mean)
tidy <- tidyData[order(tidy$Subject,tidy$Activity),]
write.table(tidy, file = "Tidy_data.txt", row.names = FALSE)
