library(dplyr);

## Loads dataset from a directory specified by file_path
## Returns the combined data table
openDataSet <- function(file_path) {
  xname <- sprintf("%s/X_%s.txt", file_path, file_path);
  yname <- sprintf("%s/y_%s.txt", file_path, file_path);
  subject_name <- sprintf("%s/subject_%s.txt", file_path, file_path);
  xdata <- read.table(xname);
  ydata <- read.table(yname);
  subject_data <- read.table(subject_name);
  cbind(subject_data, xdata, ydata);
}

## Does a row-wise combine of two datasets
## Used for combining test and train datasets
mergeDataSets <- function(dataset1, dataset2) {
  rbind(dataset1, dataset2);
}

## Selects only the columns of the dataset specifying Activity and Subject,
## and the mean and standard deviation of each value
extractMeanAndStd <- function(dataset) {
  select(dataset, matches("Subject"), contains("mean()"),
                  contains("std()"), matches("Activity"))
}

## Renames variables using features found in features.txt, and subject and activity for respective parts of dataset
renameVariables <- function(dataset) {
  feature_names <- read.table("features.txt");
  feature_names <- as.character(feature_names[,"V2"]);
  names(dataset)[1] <- "Subject";
  names(dataset)[2:(length(feature_names) + 1)] <- feature_names;
  names(dataset)[length(dataset)] <- "Activity";
  dataset[,!duplicated(names(dataset))];
}

## Renames the activities based on the activity labels stored in activity_labels.txt
renameActivities <- function(dataset) {
  activity_names <- read.table("activity_labels.txt");
  dataset$Activity <- as.factor(dataset$Activity);
  levels(dataset$Activity) <- levels(activity_names$V2);
  dataset;
}

## Substitutes verbose names for cryptic names
## Converts shortened names into full words
## Uses camelCase
addVerboseNames <- function(dataset) {
  processNames <- names(dataset);
  processNames <- gsub("^f", "frequency", processNames);
  processNames <- gsub("^t", "time", processNames);
  processNames <- gsub("Acc", "Acceleration", processNames);
  processNames <- gsub("Mag", "Magnitude", processNames);
  processNames <- gsub("Gyro", "Gyroscope", processNames);
  processNames <- gsub("std\\(\\)", "StandardDeviation", processNames);
  processNames <- gsub("mean\\(\\)", "Mean", processNames);
  processNames <- gsub("-", "", processNames);
  names(dataset) <- processNames;
  dataset;
}

## Calculates the average for each subject across each activity
calcAverages <- function(dataset) {
  avgDataset <-dataset %>%
    group_by(Subject, Activity) %>%
    summarise_each(funs(mean));
  
  processNames <- names(avgDataset);
  processNames[3:length(processNames)] = paste(processNames[3:length(processNames)],
                                               "Average", sep="");
  names(avgDataset) <- processNames;
  avgDataset;
}

## Writes data to file specified by fileName
write_dataset <- function(dataset, fileName) {
  write.table(dataset, fileName, row.name = FALSE);
}

## Function to perform data analysis
main <- function() {
  train_set <- openDataSet("train");
  test_set <- openDataSet("test");
  merged_set <- mergeDataSets(train_set, test_set);
  named_data <- renameVariables(merged_set);
  extracted_data <- extractMeanAndStd(named_data);
  renamed_data <- renameActivities(extracted_data);
  renamed_data <- addVerboseNames(renamed_data);
  averaged_data <- calcAverages(renamed_data);
  write_dataset(averaged_data, "tidy_data.txt");
}

## On sourcing the file, run the data analysis
main();