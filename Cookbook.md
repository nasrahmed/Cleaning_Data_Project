Cleaning_Data_Project
=====================

Here we are working on the data we downloaded from the project of the course Getting and Cleaning Data (On coursera)
from the following link https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The data:
=====================
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.


Raw data:
=====================
Train and Test data that shows the 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

Processed Data:
=====================
1-Making one big Dataset that will include both Train and Test Data.
2-Using descriptive activity names to name the activities in the data set (this is found in the Y files (train and test) and was mapped to variables (y_train,y_test))
3-Appropriately labelling the data set with descriptive variable names (this is found in the feature file and was mapped to variable feature)
4-Extracts only the measurements on the mean and standard deviation for each measurement
5-Last is making a new Data set like the one we did in step 4 but it calculates the average of each variable for each activity and each subject.

Variables:
=====================
1-feature: raw data of feature file
2-activity_labels: raw data of activity label file
3-x_train: raw data of x_train file
4-y_train: raw data of y_train file
5-subject_train: raw data of subject_train file
6-x_test: raw data of x_test file
7-y_test: raw data of y_test file
8-subject_test: raw data of subject_test file.

9-label_y_train: merging y_train and activity_label
10-label_y_test: merging y_test and activity_label

11-x_train_with_y: like x_train but adding to it the column (activity label based on y_train)-
12-train_data_set: The new Data set for train, include x_train_with_y added to it 2 columns	(subject number and dataset_type),
		the dataset_type is for identifying the type of this dataset (Train or Test)
13-x_test_with_y: like x_test but adding to it the column (activity label based on y_test)
14-test_data_set:The new Data set for test, include x_test_with_y added to it 2 columns	(subject number and dataset_type),
		the dataset_type is for identifying the type of this dataset (Train or Test)
15-full_data_set: is the full data set the combined Train and Test dataset together
16-mean_and_std_cols: holds the column number that matches mean() or std()
17-mean_and_std_data_set: is like the full dataset but with only the columns ( mean_and_std_cols, subject_number, activity_label,dataset_type)
18-new_ds: the results dataset, which contains dataset with calculating of average of each variable for each activity and each subject