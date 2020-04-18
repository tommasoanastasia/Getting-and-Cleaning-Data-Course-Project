#Author: Tommaso Anastasia

library(dplyr)

# Download raw data 
if (!file.exists("./UCI HAR Dataset")) { Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                                         download.file(Url, "project_data.zip", method = "curl")
                                         unzip("project_data.zip")

                                         if(file.exists("project_data.zip")) { file.remove("project_data.zip") }
}


# 1. Merging the training and the test sets to create one data set

# raw train data
X_train         <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train         <- read.table("./UCI HAR Dataset/train/y_train.txt")
sub_train       <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# raw test data
X_test          <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test          <- read.table("./UCI HAR Dataset/test/y_test.txt")
sub_test        <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Creating one dataset for train data
merged_train <- cbind(X_train, y_train, sub_train)

# Creating one dataset for test data
merged_test  <- cbind(X_test, y_test, sub_test)

# Merging train and test data
merged_train_test <- rbind(merged_train, merged_test)

# set features names
features                    <- read.table("./UCI HAR Dataset/features.txt")
featuresDesired             <- features[,2]
colnames(merged_train_test) <- c(as.character(featuresDesired), 'activity', 'subject')

# 2. Extracting the measurements on the mean and standard deviation for each measurement 
meanstdcolonly <- grep('mean\\(|std\\(', names(merged_train_test), value = TRUE)

# 3. Uses descriptive activity names to name the activities in the data set
meanstdcolandactandsub <- c(meanstdcolonly, 'activity', 'subject')

# Select only required data (activity, subject, and mean and std measurements)
meanstddataonly <- merged_train_test[, meanstdcolandactandsub]

activities <- read.table('./UCI HAR Dataset/activity_labels.txt')
meanstddataonly$activity <- factor(meanstddataonly$activity, labels = tolower(activities[, 2]))

# 4. Appropriately labels the data set with descriptive variable names
vnames <- names(meanstddataonly)

vnames <- sapply(vnames, tolower)
vnames <- gsub(pattern = "-"           , replacement = ""            , x = vnames)
vnames <- sub(pattern = "\\(\\)"       , replacement = ""            , x = vnames)
vnames <- sub(pattern = "^t"           , replacement = "time"        , x = vnames)
vnames <- sub(pattern = "acc"          , replacement = "acceleration", x = vnames)
vnames <- sub(pattern = "f"            , replacement = "frequency"   , x = vnames)
vnames <- sub(pattern = "gyro"         , replacement = "gyroscope"   , x = vnames)
vnames <- sub(pattern = "mag"          , replacement = "magnitude"   , x = vnames)
vnames <- sub(pattern = "mean"         , replacement = ".mean"       , x = vnames)
vnames <- sub(pattern = "std"          , replacement = ".std"        , x = vnames)
vnames <- sub(pattern = "x$"           , replacement = ".x"          , x = vnames)
vnames <- sub(pattern = "y$"           , replacement = ".y"          , x = vnames)
vnames <- sub(pattern = "z$"           , replacement = ".z"          , x = vnames)
vnames <- sub(pattern = "activit.y"    , replacement = "activity"    , x = vnames)

names(meanstddataonly) <- vnames
View(meanstddataonly)


## 5. Creates a second, independent tidy data set with the average of each variable 
##    for each activity and each subject

final_dataset <- meanstddataonly %>% group_by(subject, activity)

sum_mean <- final_dataset %>% summarise_all(.funs = mean)

View(sum_mean)


# submitting a tidy data set
write.table(x = sum_mean, file = "table.txt", row.names = FALSE)
