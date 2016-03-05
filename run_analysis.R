## COURSERA DATA SCIENCE SPECIALIZATION
## GETTING AND CLEANING DATA
## DATA COURSE PROJECT
## CARLOS ORELLANA, FEB 2016

setwd("C:/UCI HAR Dataset");

#LOAD DIMENSIONS
  features <- read.table("features.txt", header = FALSE)[,2];
  # 3. Uses descriptive activity names to name the activities in the data set
  features <- gsub("^f","frequency",features);
  features <- gsub("^t","time",features);
  features <- gsub("[\\(\\)]","",features);
  features <- gsub("-mean","Mean",features);
  features <- gsub("-std","Std",features);
  features <- gsub("-max","Max",features);
  features <- gsub("-min","Min",features);
  features <- gsub("-kurtosis","Kurtosis",features);
  features <- gsub("-skewness","Skewness",features);
  features <- gsub("-entropy","Entropy",features);
  features <- gsub("-bands","Bands",features);
  features <- gsub("-iqr","Iqr",features);
  features <- gsub("-","",features);
  features <- gsub(",","_",features);

  activity_labels <- read.table("activity_labels.txt", header = FALSE);
  subject_test <- read.table("test/subject_test.txt", header = FALSE);
  subject_train <- read.table("train/subject_train.txt", header = FALSE);


#LOAD FACTS
  x_test <- read.table("test/X_test.txt", header = FALSE);
  y_test <- read.table("test/y_test.txt", header = FALSE);
  x_train <- read.table("train/X_train.txt", header = FALSE);
  y_train <- read.table("train/y_train.txt", header = FALSE);

#SET COLUMN NAMES
  # 4. Appropriately labels the data set with descriptive variable names.
  colnames(activity_labels) <- c("activityID", "activityType");
  colnames(subject_test) <- "subjectID";
  colnames(subject_train) <- "subjectID";
  colnames(x_test) <- features;
  colnames(x_train) <- features;
  colnames(y_test) <- "activityID";
  colnames(y_train) <- "activityID";


#FINAL DATA SET 1
  # 1. Merges the training and the test sets to create one data set.
  data_1 <- rbind(data.frame(y_test, subject_test, x_test),data.frame(y_train, subject_train, x_train));
  data_1;


#FINAL DATA SET 2
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  cols <- colnames(data_1)
  bool_cols <- (grepl("activity..",cols) | grepl("subjectID",cols) | (grepl("Mean..",cols) & !grepl("MeanFreq..",cols)) | grepl("Std..",cols));
  data_2 <- data_1[bool_cols == TRUE];
  data_2;

#FINAL DATA SET 3
  # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  data_3 <- data_2[,colnames(data_2) != "activityType"];
  data_3 <- aggregate(data_3[,colnames(data_3) != c("activityID','subjectID")],by=list(activityID=data_3$activityID, subjectID = data_3$subjectID),mean);
  data_3;
  write.table(data_3, './finalData.txt',row.names=FALSE,sep='|');
