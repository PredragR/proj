run_analysis <- function(){
  # Course projct - Getting and Cleaning Data  
  # This script does the following:
  # 1. Merges the training and the test sets to create one data set.
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  # 3. Uses descriptive activity names to name the activities in the data set
  # 4. Appropriately labels the data set with descriptive activity names.
  # 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  # assumptions: following packages are installed tidyr,dplyr,data.table, reshape2
  library("tidyr")
  library("dplyr")
  library("data.table")  
  library("reshape2")
  
  # read test data
  x_test <- read.csv("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
  y_test <- read.csv("./UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
  subject_test <- read.csv("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  # read train data
  x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  # read features, use them for column names
  features <- read.csv("./UCI HAR Dataset/features.txt", sep="", header = FALSE)[,2]
  names(x_test) = features
  names(x_train) = features
  # get only features which contain words "maean" and "std"
  means_and_stds <- grep("mean|std", features)
  x_test = x_test[,means_and_stds]
  x_train = x_train[,means_and_stds]
  
  # link activity labels with subjects and test data
  activities <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  names(y_test) = c("Activity_ID")
  names(activities) = c("Activity_ID","Activity")
  y_test <- full_join(y_test,activities,by="Activity_ID")
  names(subject_test) = "Subject"
  test_data <- bind_cols(list(subject_test,y_test,x_test))
  
  # link activity labels with subjects and train data
  names(y_train) = c("Activity_ID")
  y_train <- full_join(y_train,activities,by="Activity_ID")
  names(subject_train) = "Subject"
  train_data <- bind_cols(list(subject_train,y_train,x_train))
  
  # combine two datasets 
  data_combined = bind_rows(test_data, train_data)
  
  # make sure data adhares to tidy data criteria
  data_melted = melt(data_combined, 
                     id.vars = c("Subject", "Activity_ID", "Activity"), 
                     measure.vars = setdiff(colnames(data_combined), c("Subject", "Activity_ID", "Activity")))
 
  # calculate means, put data into tidy set
  data_tidied   = dcast(data_melted, Subject + Activity ~ variable, mean)
  
  # output tidy data 
  write.table(data_tidied, file = "./tidy_data.txt",row.name=FALSE)
  "adf"
  
}