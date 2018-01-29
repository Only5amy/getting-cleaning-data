#Getting and Cleaning Data Course Project

#Step 1: Merge the training and test sets to create one data set.

#loading all the data
activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

subjecttrain <-read.table("./UCI HAR Dataset/train/subject_train.txt")
Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")

subjecttest <-read.table("./UCI HAR Dataset/test/subject_test.txt")
Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")

#Assign column names to training data

colnames(activitylabels)<-c("activityid","activityname")
colnames(Ytrain) <- "activityid"
colnames(Xtrain) <- features[,2]
colnames(subjecttrain) <- "subject"

#Bind training data
trainingdata <- cbind(subjecttrain, Ytrain, Xtrain)

#Assign column names to test data 
colnames(Ytest) <- "activityid"
colnames(Xtest) <- features[,2]
colnames(subjecttest) <- "subject"

#Bind test data
testdata <- cbind(subjecttest,Ytest,Xtest)

#Now combine both the test and training data
AllData <- rbind(testdata,trainingdata)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

meanstd <-AllData[,grepl("subject|activityid|mean|std",colnames(AllData))] #need subject and activityid columns for later steps

#Step 3: Uses descriptive activity names to name the activities in the data set

library(dplyr)
meanstd <- merge(meanstd,activitylabels, by.x="activityid", by.y="activityid", all=TRUE)

#Step 4: Appropriately labels the data set with descriptive variable names.

colnames(meanstd) <- gsub("^t", "Time",colnames(meanstd))
colnames(meanstd) <- gsub("^f", "Freq", colnames(meanstd))
colnames(meanstd) <- gsub("Acc", "Acceleration", colnames(meanstd))
colnames(meanstd) <- gsub("BodyBody", "Body", colnames(meanstd))
colnames(meanstd) <- gsub("\\(|\\)", "", colnames(meanstd))

#Step 5: From step 4, creates a second independent tidy data set with average of each variable for each activity and each subject.
library(reshape)
datavariables = setdiff(colnames(meanstd), c("subject","activityid","activityname")) 
tidydata = melt(meanstd, id = c("subject","activityid","activityname"), measure.vars = datavariables)

# Apply mean function to dataset using dcast function
library(reshape2)
tidyaveragedata = dcast(tidydata, subject + activityname ~ variable, mean)

write.table(tidyaveragedata, file = "./UCI HAR Dataset/tidydata.txt",row.name=FALSE)

