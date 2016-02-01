# Course 3A January 2016 
# course assignment

Xtrain <- read.table("./Coursera/X_train.txt")
dim(Xtrain)

Xtrain <- Xtrain[, c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254,
266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543) ]
dim(Xtrain)

Xtest <- read.table("./Coursera/X_test.txt")
dim(Xtest)

Xtest <- Xtest[, c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254,
266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543) ]
dim(Xtest)

Ytrain <- read.table("./Coursera/y_train.txt")
dim(Ytrain)
table(Ytrain)

Ytest <- read.table("./Coursera/y_test.txt")
dim(Ytest)
table(Ytest)

sub_train <- read.table("./Coursera/subject_train.txt")
dim(sub_train)
table(sub_train)

sub_test <- read.table("./Coursera/subject_test.txt")
dim(sub_test)
table(sub_test)

train <- cbind(Ytrain, sub_train, Xtrain)
dim(train)

test <- cbind(Ytest, sub_test, Xtest)
dim(test)

# This next section does not get used 
# these variables do not have separate labels 
# although description exists in features_info.txt

bodyaccx_test <- read.table("./Coursera/body_acc_x_test.txt")
dim(bodyaccx_test)

bodyaccy_test <- read.table("./Coursera/body_acc_y_test.txt")
dim(bodyaccy_test)

bodyaccz_test <- read.table("./Coursera/body_acc_z_test.txt")
dim(bodyaccz_test)

bodygyrox_test <- read.table("./Coursera/body_gyro_x_test.txt")
dim(bodygyrox_test)

bodygyroy_test <- read.table("./Coursera/body_gyro_y_test.txt")
dim(bodygyroy_test)

bodygyroz_test <- read.table("./Coursera/body_gyro_z_test.txt")
dim(bodygyroz_test)

totaccx_test <- read.table("./Coursera/total_acc_x_test.txt")
dim(totaccx_test)

totaccy_test <- read.table("./Coursera/total_acc_y_test.txt")
dim(totaccy_test)

totaccz_test <- read.table("./Coursera/total_acc_z_test.txt")
dim(totaccz_test)

bodyaccx_train <- read.table("./Coursera/body_acc_x_train.txt")
dim(bodyaccx_train)

bodyaccy_train <- read.table("./Coursera/body_acc_y_train.txt")
dim(bodyaccy_train)

bodyaccz_train <- read.table("./Coursera/body_acc_z_train.txt")
dim(bodyaccz_train)

bodygyrox_train <- read.table("./Coursera/body_gyro_x_train.txt")
dim(bodygyrox_train)

bodygyroy_train <- read.table("./Coursera/body_gyro_y_train.txt")
dim(bodygyroy_train)

bodygyroz_train <- read.table("./Coursera/body_gyro_z_train.txt")
dim(bodygyroz_train)

totaccx_train <- read.table("./Coursera/total_acc_x_train.txt")
dim(totaccx_train)

totaccy_train <- read.table("./Coursera/total_acc_y_train.txt")
dim(totaccy_train)

totaccz_train <- read.table("./Coursera/total_acc_z_train.txt")
dim(totaccz_train)

# end of section that does not get used

total <- rbind(train, test)
dim(total)

features <- read.table("./Coursera/features.txt",stringsAsFactors=FALSE)
str(features)
dim(features)
names(features) <- c("featureno", "feature")

activity_subject <- append("activity", "subject")
activity_subject
total_features <- features[c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254,
266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543),2]
total_features
names(total) <- append(activity_subject, total_features)

activities <- read.table("./Coursera/activity_labels.txt",stringsAsFactors=FALSE)
dim(activities)
activities

class(total$activity)
total$activity <- factor(total$activity)
class(total$activity)
table(total$activity)

levels(total$activity) <- c("1"="walking", "2"="walking upstairs", "3"="walking downstairs", "4"="sitting", "5"="standing", "6"="laying")
table(total$activity)

names(total)
table(total$activity)
table(total$subject)

# this ends steps 1-4 
# now create second independent tidy data set with average for each parameter by activity, subject

table(total$activity, total$subject)

library(reshape2)
length(names(total))

var.out <- names(total)[!names(total) %in% c("activity", "subject")]
mean_total <- melt(total, id=c("activity", "subject"), measure.vars=var.out)
dim(mean_total)

#mean_total[which(mean_total$activity=="walking" & mean_total$subject==1 & 
#head(mean_total, 200)
#total[which(total$activity=="laying" & total$subject==30), c(1,2,3)]


# use plyr package
library(plyr)
finaldata <- ddply(mean_total, .(activity, subject, variable), summarize, average = mean(value))
tail(finaldata,66)
dim(finaldata)
finaldata

# use dcast
# newData <- dcast(mean_total, .(activity, subject) ~ variable, mean) 
# dcast does not address the issue as it sets up all the variables as sideways columns
# not what we want

# write finaldata file to a table to upload

#library(data.table)

#write.table(finaldata, "./Coursera/wearableactivity.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)