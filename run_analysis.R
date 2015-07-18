# data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# store data in working direcotry


# read in Testing data
#subject_test.txt
#X_test.txt
#y_test.txt
#x_test is the data
#sub test tells us which subject (row label)
#y_test tells us which activity (row label)
#feat is the measurement (V2 is col label)


sub_test <- read.table("subject_test.txt", header=FALSE, col.names=c("Subject"))
#- Each row identifies the subject who performed the activity for each window sample. 
#Its range is from 1 to 30. 

feat <- read.table("features.txt",  header=FALSE)
feat_name <- feat$V2

x_test <- read.table("X_test.txt", header=FALSE,  col.names=feat_name )
y_test <- read.table("y_test.txt",  sep="\t", col.names=c("activ") , header=FALSE)


data2 <- cbind(sub_test , y_test, x_test)


#add the labels 
labels <- read.table("activity_labels.txt", header=FALSE, col.names =c("V1", "Activity"))
label_data = merge(data2, labels, by.x="activ", by.y="V1", all.x=TRUE)


# read in traning
#subject_train
#X_train.txt
#y_train.txt

sub_train <- read.table("subject_train.txt", header=FALSE, col.names=c("Subject"))
 
x_train <- read.table("X_train.txt", header=FALSE,  col.names=feat_name )
y_train <- read.table("y_train.txt",  sep="\t", col.names=c("activ") , header=FALSE)


data2_2 <- cbind(sub_train , y_train, x_train)




#Merges the training and the test sets to create one data set.
data_full <- rbind(data2_2, data2)


#Appropriately labels the data set with descriptive variable names. 
data_full2 <- merge(data_full, labels, by.x="activ", by.y="V1", all.x=TRUE)



#Extracts only the measurements on the mean and standard deviation for each measurement. 

data_sm <- data_full2 %>% select(Subject , Activity , tBodyAcc.mean...X , tBodyAcc.mean...Y , tBodyAcc.mean...Z , tBodyAcc.std...X , 
tBodyAcc.std...Y , tBodyAcc.std...Z , tGravityAcc.mean...X , tGravityAcc.mean...Y , 
tGravityAcc.mean...Z , tGravityAcc.std...X , tGravityAcc.std...Y , tGravityAcc.std...Z , 
tBodyAccJerk.mean...X , tBodyAccJerk.mean...Y , tBodyAccJerk.mean...Z , tBodyAccJerk.std...X , 
tBodyAccJerk.std...Y , tBodyAccJerk.std...Z ,tBodyGyro.mean...X , tBodyGyro.mean...Y , 
tBodyGyro.mean...Z , tBodyGyro.std...X , tBodyGyro.std...Y , tBodyGyro.std...Z , 
tBodyGyroJerk.mean...X , tBodyGyroJerk.mean...Y , tBodyGyroJerk.mean...Z , tBodyGyroJerk.std...X , 
tBodyGyroJerk.std...Y , tBodyGyroJerk.std...Z ,tBodyAccMag.mean.. , tBodyAccMag.std.. , 
tGravityAccMag.mean.. , tGravityAccMag.std.. , tBodyAccJerkMag.mean.. ,tBodyAccJerkMag.std.. , 
tBodyGyroMag.mean.. , tBodyGyroMag.std.. , tBodyGyroJerkMag.mean.. , tBodyGyroJerkMag.std.. , 
fBodyAcc.mean...X , fBodyAcc.mean...Y , fBodyAcc.mean...Z , fBodyAcc.std...X , fBodyAcc.std...Y , 
fBodyAcc.std...Z , fBodyAcc.meanFreq...X , fBodyAcc.meanFreq...Y , fBodyAcc.meanFreq...Z , 
fBodyAccJerk.mean...X , fBodyAccJerk.mean...Y , fBodyAccJerk.mean...Z , fBodyAccJerk.std...X , 
fBodyAccJerk.std...Y , fBodyAccJerk.std...Z , fBodyGyro.mean...X , fBodyGyro.mean...Y , 
fBodyGyro.mean...Z , fBodyGyro.std...X , fBodyGyro.std...Y , fBodyGyro.std...Z , 
fBodyAccMag.mean.. , fBodyAccMag.std.. , fBodyBodyAccJerkMag.mean.., fBodyBodyAccJerkMag.std.., 
fBodyBodyGyroMag.mean.., fBodyBodyGyroMag.std.. , fBodyBodyGyroJerkMag.mean.., fBodyBodyGyroJerkMag.std..,
angle.tBodyAccMean.gravity. , angle.X.gravityMean. , angle.tBodyAccJerkMean..gravityMean. ,
angle.Y.gravityMean. , angle.tBodyGyroMean.gravityMean. , angle.Z.gravityMean. , angle.tBodyGyroJerkMean.gravityMean.)


#Uses descriptive activity names to name the activities in the data set
# used feat_name for the names



#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(reshape2)
dataMelt <- melt(data_sm ,id=c("Subject" , "Activity"),measure.vars=c("tBodyAcc.mean...X", "tBodyAcc.mean...Y", "tBodyAcc.mean...Z", "tBodyAcc.std...X", "tBodyAcc.std...Y", "tBodyAcc.std...Z", "tGravityAcc.mean...X", "tGravityAcc.mean...Y", 
"tGravityAcc.mean...Z", "tGravityAcc.std...X", "tGravityAcc.std...Y", "tGravityAcc.std...Z", "tBodyAccJerk.mean...X", "tBodyAccJerk.mean...Y", 
"tBodyAccJerk.mean...Z", "tBodyAccJerk.std...X", "tBodyAccJerk.std...Y", "tBodyAccJerk.std...Z", "tBodyGyro.mean...X", "tBodyGyro.mean...Y", 
"tBodyGyro.mean...Z", "tBodyGyro.std...X", "tBodyGyro.std...Y", "tBodyGyro.std...Z", "tBodyGyroJerk.mean...X", "tBodyGyroJerk.mean...Y", 
"tBodyGyroJerk.mean...Z", "tBodyGyroJerk.std...X", "tBodyGyroJerk.std...Y", "tBodyGyroJerk.std...Z", "tBodyAccMag.mean..", "tBodyAccMag.std..", 
"tGravityAccMag.mean..", "tGravityAccMag.std..", "tBodyAccJerkMag.mean..", "tBodyAccJerkMag.std..", "tBodyGyroMag.mean..", "tBodyGyroMag.std..", 
"tBodyGyroJerkMag.mean..", "tBodyGyroJerkMag.std..", "fBodyAcc.mean...X", "fBodyAcc.mean...Y", "fBodyAcc.mean...Z", "fBodyAcc.std...X", "fBodyAcc.std...Y", 
"fBodyAcc.std...Z", "fBodyAcc.meanFreq...X", "fBodyAcc.meanFreq...Y", "fBodyAcc.meanFreq...Z", "fBodyAccJerk.mean...X", "fBodyAccJerk.mean...Y", 
"fBodyAccJerk.mean...Z", "fBodyAccJerk.std...X", "fBodyAccJerk.std...Y", "fBodyAccJerk.std...Z", "fBodyGyro.mean...X", "fBodyGyro.mean...Y", 
"fBodyGyro.mean...Z", "fBodyGyro.std...X", "fBodyGyro.std...Y", "fBodyGyro.std...Z", "fBodyAccMag.mean..", "fBodyAccMag.std..", "fBodyBodyAccJerkMag.mean..", 
"fBodyBodyAccJerkMag.std..", "fBodyBodyGyroMag.mean..", "fBodyBodyGyroMag.std..", "fBodyBodyGyroJerkMag.mean..", "fBodyBodyGyroJerkMag.std..", "angle.tBodyAccMean.gravity.", 
"angle.X.gravityMean.", "angle.tBodyAccJerkMean..gravityMean.", "angle.Y.gravityMean.", "angle.tBodyGyroMean.gravityMean.", "angle.Z.gravityMean.", 
"angle.tBodyGyroJerkMean.gravityMean."))


library(plyr)
data_order <- arrange(dataMelt , Subject, Activity, variable )

data_sum <- ddply(data_order, .( Subject, Activity, variable), summarize,  mean = (mean(value)))


#output the data
#txt file created with write.table() using row.name=FALSE
write.table(data_sum, file="tidy_proj.txt",row.name=FALSE )

