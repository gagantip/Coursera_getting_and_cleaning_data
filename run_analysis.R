library(dplyr)

#reading training data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subj_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

#reading test data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subj_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#reading activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#reading features 
features <- read.table("./UCI HAR Dataset/features.txt")

#Step 1: Merging training and test data into a single dataset
x_total <- rbind(x_train,x_test)
y_total <- rbind(y_train,y_test)
subj_total <- rbind(subj_train,subj_test)

#Step 2: Extracting the measurements wirh mean and std deviation on each measurement
req_features <- features[(grep("mean\\(\\)|std\\(\\)", features[,2])),]
x_total <- x_total[,req_features[,1]]

#Step 3: Using descriptive names to describe activities
colnames(y_total) <- "activity"
y_total$activitylabel <- factor(y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- y_total[,-1]

#Step 4: Labeling the dataset with descriptive names
colnames(x_total) <- features[req_features[,1],2]


#Step 5; Creating an independent tidy dataset with average of each variable for each activity and each subject,
#using the data from step 4 
colnames(subj_total) <- "subject"
total <- cbind(x_total, activitylabel, subj_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)