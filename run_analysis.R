## Coursera Getting and Cleaning Data Course Project

# data for the project: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# Script does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
#    measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.

# Load data sets
features      = read.table('./features.txt') 
activity_type = read.table('./activity_labels.txt') 
subject_train = read.table('./train/subject_train.txt') 
x_train       = read.table('./train/x_train.txt')
y_train       = read.table('./train/y_train.txt')
subject_test  = read.table('./test/subject_test.txt')
x_test        = read.table('./test/x_test.txt')
y_test        = read.table('./test/y_test.txt')

# Assign column names
colnames(activity_type)  = c('activityId','activityType')
colnames(subject_train)  = "subjectId"
colnames(x_train)        = features[,2]
colnames(y_train)        = "activityId"
colnames(subject_test)   = "subjectId"
colnames(x_test)         = features[,2]
colnames(y_test)         = "activityId"

# Prepare complete train and test data sets
data_train <- cbind(x_train, y_train, subject_train)
data_test  <- cbind(x_test, y_test, subject_test)

# Merge data sets
data_compl <- rbind(data_train, data_test)

# Find features: avg and st_dev, keep activity and subject Ids
srch_features <- grep("((mean|std)\\(\\))|(activity|subject)", colnames(data_compl))

# Subset needed columns
data_subset <- data_compl[, srch_features]

# Assign activity names
data_subset = merge(data_subset, activity_type,by='activityId',all.x=TRUE);

# Appropriately labels the data set with descriptive variable names. 
names(data_subset) <- gsub("\\()","", names(data_subset))
names(data_subset) <- gsub("-std","StdDev", names(data_subset))
names(data_subset) <- gsub("-mean","Mean", names(data_subset))
names(data_subset) <- gsub("^t", "time", names(data_subset))
names(data_subset) <- gsub("^f", "frequency", names(data_subset))
names(data_subset) <- gsub("Acc", "Accelerometer", names(data_subset))
names(data_subset) <- gsub("Gyro", "Gyroscope", names(data_subset))
names(data_subset) <- gsub("Mag", "Magnitude", names(data_subset))
names(data_subset) <- gsub("BodyBody", "Body", names(data_subset))

# Create new tidy data set with the average of each variable 
#for each activity and each subject
data_tidy <- aggregate(. ~subjectId + activityType, data_subset, mean)
data_tidy <- data_tidy[order(data_tidy$subjectId,data_tidy$activityType),]
write.table(data_tidy, file = "tidydata.txt",row.name=FALSE)
