library(plyr)
run_analysis <- function () {
  
  ## Part 1 - Merge test and train datasets
  features <- read.table('UCI_HAR_Dataset/features.txt')
  
  x_test <- read.table('UCI_HAR_Dataset/test/X_test.txt')
  y_test <- read.table('UCI_HAR_Dataset/test/y_test.txt')
  subject_test <- read.table('UCI_HAR_Dataset/test/subject_test.txt')
  y_train <- read.table('UCI_HAR_Dataset/train/y_train.txt')
  x_train <- read.table('UCI_HAR_Dataset/train/X_train.txt')
  subject_train <- read.table('UCI_HAR_Dataset/train/subject_train.txt')
  
  colnames(x_test) <- features$V2
  colnames(x_train) <- features$V2
  colnames(y_test) <- c('Activity')
  colnames(y_train) <- c('Activity')
  colnames(subject_test) <- c('Subject')
  colnames(subject_train) <- c('Subject')
  full_test <- cbind(y_test, subject_test, x_test)
  full_train <- cbind(y_train, subject_train, x_train)
  combined <- rbind(full_test, full_train)
  
  ## Part 2 - Extract only measurements on mean and std dev
  ## Part 4 - Appropriately label data set with descriptive variable names
  combined_mean_std <- combined[,c(1, 2, grep("mean|std",colnames(full_test)))]
  
  ## Part 3 - Convert activities from numbers to names in data set
  activity_labels <- read.table('UCI_HAR_Dataset/activity_labels.txt')
  combined_mean_std$Activity <- mapvalues(combined_mean_std$Activity, 
                                from=as.character(activity_labels$V1),
                                to=as.character(activity_labels$V2))
  
  # Part 5 - Average for each subject and activity
  unique_subj_act <- unique(combined_mean_std[,1:2])
  unique_subj_act <- unique_subj_act[order(unique_subj_act[,2], unique_subj_act[,1]),]
  all_averages <- data.frame()
  for (row in 1:nrow(unique_subj_act)) {
    average_precalc_df <- combined_mean_std[combined_mean_std$Activity==unique_subj_act[row,"Activity"] & combined_mean_std$Subject==unique_subj_act[row,"Subject"],c(3:ncol(combined_mean_std))]
    average_post_calc <- colMeans(average_precalc_df)
    all_averages <- rbind(all_averages, as.data.frame(t(average_post_calc)))
  }
  avg_for_subj_and_act <- cbind(unique_subj_act, all_averages)
  colnames(avg_for_subj_and_act) <- colnames(combined_mean_std)
  write.table(avg_for_subj_and_act, row.names = FALSE, file="./average_of_all_measures_with_mean_or_std.txt")
}
