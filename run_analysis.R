##Week 4 Course Project 

# Load required library
library(dplyr)

# Download the dataset
if(!file.exists("./getcleandata")) {
  dir.create("./getcleandata")
}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./getcleandata/projectdataset.zip")

# Unzip the dataset
unzip(zipfile = "./getcleandata/projectdataset.zip", exdir = "./getcleandata")

##Read in Training data
y_train <- read.table("C:\\Users\\LGordonRogers\\OneDrive - Union of Concerned Scientists\\ProgrammingAssignment2\\UCI HAR Dataset\\train\\y_train.txt")
x_train <- read.table("C:\\Users\\LGordonRogers\\OneDrive - Union of Concerned Scientists\\ProgrammingAssignment2\\UCI HAR Dataset\\train\\X_train.txt")
subject_train <- read.table("C:\\Users\\LGordonRogers\\OneDrive - Union of Concerned Scientists\\ProgrammingAssignment2\\UCI HAR Dataset\\train\\subject_train.txt")

##Read in Testing data
x_test <- read.table("C:\\Users\\LGordonRogers\\OneDrive - Union of Concerned Scientists\\ProgrammingAssignment2\\UCI HAR Dataset\\test\\X_test.txt")
y_test <- read.table("C:\\Users\\LGordonRogers\\OneDrive - Union of Concerned Scientists\\ProgrammingAssignment2\\UCI HAR Dataset\\test\\y_test.txt")
subject_tests <- read.table("C:\\Users\\LGordonRogers\\OneDrive - Union of Concerned Scientists\\ProgrammingAssignment2\\UCI HAR Dataset\\test\\subject_test.txt")

##Read in features 
features <- read.table("C:\\Users\\LGordonRogers\\OneDrive - Union of Concerned Scientists\\ProgrammingAssignment2\\UCI HAR Dataset\\features.txt")

##Read in activity levels
activity_levels <- read.table("C:\\Users\\LGordonRogers\\OneDrive - Union of Concerned Scientists\\ProgrammingAssignment2\\UCI HAR Dataset\\activity_labels.txt")
colnames(activity_levels) <- c("activityID", "activitytype") 

##Setting variable names
colnames(x_train) <- features[, 2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(x_test) <- features[, 2]
colnames(y_test) <- "activityID"
colnames(subject_tests) <- "subjectID"

##Merge training and test datasets 
train <- cbind(x_train, y_train, subject_train)
test <- cbind(x_test, y_test, subject_tests)
alldata <- rbind(test, train) 

  
##Extract only the measurements on the mean and standard deviation for each measurement
meanstd <- grepl("activityID|subjectID|mean\\(\\)|std\\(\\)", colnames(alldata)) 
meanstddata <- alldata[,meanstd] 


##Use descriptive activity names to name the activities in the dataset
actnames <- merge(meanstddata, activity_levels, by="activityID", all.x=TRUE) 


##Label with descriptive variable names
colnames(actnames) <- gsub("^t", "time", colnames(actnames)) 
colnames(actnames) <- gsub("^f", "frequency", colnames(actnames)) 
colnames(actnames) <- gsub("Acc", "Accelermeter", colnames(actnames)) 
colnames(actnames) <- gsub("Gyro", "Gyroscope", colnames(actnames))
colnames(actnames) <- gsub("Mag", "Magnitude", colnames(actnames)) 
colnames(actnames) <- gsub("BodyBody", "Body", colnames(actnames)) 


##Create a second, independent tidy data set with the average of each variable for each activity and subject
tidy <- actnames %>%
  group_by(subjectID, activityID, activitytype) %>%
  summarize_all(mean) 

