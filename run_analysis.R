run_analysis <- function(directory) {
    #Here the function assumes that it will take a directory of the files related to the project of the course Getting and Cleaning Data
    #for example: As we downloaded the files and extract it to folder "UCI HAR Dataset", so just run the function like this 
    #result_ds<-run_analysis("./UCI HAR Dataset")
    
    #Now we will load all the files we have downloaded into R
    
    #Loading Feature and Activity Labels
    feature<-read.table(paste(directory,"/features.txt",sep=""),header = FALSE)
    activity_labels<-read.table(paste(directory,"/activity_labels.txt",sep=""),header = FALSE)
    
    #Loading all the train files
    x_train<-read.table(paste(directory,"/train/x_train.txt",sep=""),header = FALSE)
    y_train<-read.table(paste(directory,"/train/y_train.txt",sep=""),header = FALSE)
    subject_train<-read.table(paste(directory,"/train/subject_train.txt",sep=""),header = FALSE)
    
    #Loading all the test files
    x_test<-read.table(paste(directory,"/test/x_test.txt",sep=""),header = FALSE)
    y_test<-read.table(paste(directory,"/test/y_test.txt",sep=""),header = FALSE)
    subject_test<-read.table(paste(directory,"/test/subject_test.txt",sep=""),header = FALSE)
    
    #Here will merge Y and activity_label according to Train variables and Test variables (each one alone)
    label_y_train=merge(y_train,activity_labels,"V1",all=FALSE,sort=FALSE)
    label_y_test=merge(y_test,activity_labels,"V1",all=FALSE,sort=FALSE)
    
    #For the Train data, we will add the Activity Labels, then change the field name to be "activity_label"
    #Then add the subjects numbers to the Train data, then change the field name to "subject_number"
    names(x_train)<-as.character(feature$V2)
    x_train_with_y<-cbind(x_train,label_y_train$V2)
    names(x_train_with_y)[names(x_train_with_y)=="label_y_train$V2"]<-"activity_label"
    
    train_data_set<-cbind(x_train_with_y,subject_train$V1)
    names(train_data_set)[names(train_data_set)=="subject_train$V1"]<-"subject_number"
    #adding extra field called dataset_type and assign it with "Train" value
    train_data_set$dataset_type<-"Train"
    
    #Repeat what we have made above but for Test Data
    #For the Test data, we will add the Activity Labels, then change the field name to be "activity_label"
    #Then add the subjects numbers to the Test data, then change the field name to "subject_number"
    names(x_test)<-as.character(feature$V2)
    x_test_with_y<-cbind(x_test,label_y_test$V2)
    names(x_test_with_y)[names(x_test_with_y)=="label_y_test$V2"]<-"activity_label"
    
    test_data_set<-cbind(x_test_with_y,subject_test$V1)
    names(test_data_set)[names(test_data_set)=="subject_test$V1"]<-"subject_number"
    #adding extra field called dataset_type and assign it with "Test" value
    test_data_set$dataset_type<-"Test"
    
    #Now its time to combine both data sets together to form one dataset
    #and by the field dataset_type we can distinguish the datasets we combined.
    full_data_set<-rbind(train_data_set,test_data_set)
    
    #Here we will make a new dataset that will contain only the mean and std fields along with "subject_number","dataset_type","activity_label"
    mean_and_std_cols<-c(grep("mean\\(\\)", names(full_data_set)),grep("std\\(\\)", names(full_data_set)))
    mean_and_std_data_set<-full_data_set[,c(mean_and_std_cols,562,563,564)]
    
    #Here we will calculate average of each variable for each activity and each subject.
    library("data.table")
    dataset_DT <- data.table(mean_and_std_data_set)
    dataset_DT[,lapply(.SD,ave),by=list(activity_label,subject_number,dataset_type)]
    new_ds<-unique(dataset_DT[,lapply(.SD,ave),by=list(activity_label,subject_number,dataset_type)])
    
    #return results
    new_ds
}