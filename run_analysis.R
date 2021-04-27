library(stringr)
library(dplyr)
library(data.table)

#read measurement,labels and subject data for test & train data
test.data <- read.table("test/X_test.txt")
train.data <- read.table("train/X_train.txt")
test.labels <- read.table("test/y_test.txt")
train.labels <- read.table("train/y_train.txt")
test.subject <- read.table("test/subject_test.txt")
train.subject <- read.table("train/subject_train.txt")

#merge the measurement,labels and subject data (train + test)
data <- rbind(train.data,test.data)
label <- rbind(train.labels,test.labels)
subject <- rbind(train.subject,test.subject)

# Create names for the table columns from features.txt file
cnames.data <- read.table("features.txt")
cnames.data <- make.names(str_replace_all(cnames.data[,2],"[()]",""))
names(data) <- cnames.data
names(label) <- "activity"
names(subject) <- "Subject.ID"

#merge all 3 tables to create master data
master.data <- cbind(subject,label,data)

#Extract measurements on mean and std deviation for each measurment
#included meanFreq. To exclude it grep on "mean." instead
extracted.cols <- grep(pattern = "mean|std",names(data))
submeasure <- data[,extracted.cols]
subdata <- cbind(subject,label,submeasure)

#Replacing descriptive activity name to name the activities
activity.name <- read.table("activity_labels.txt")
subdata <- mutate(subdata,activity = activity.name[activity,2])

#Independent data set with average of each variable for each activity & subject
#Calculation of column means for each SubjectID+activity combination
temp <- subdata %>% 
      mutate(activity = paste(Subject.ID,activity)) %>%
      subset(select = 2:81) 
temp <- split(temp[-1],temp$activity) 
temp <- sapply(temp, colMeans,na.rm=TRUE) %>%
                  t

#Reformating data with Subject.ID and activity separated and cleaning rownames
avg.data <- temp %>% rownames %>%
                  strsplit(" ") %>%
                  as.data.frame() %>%
                  t %>%
                  cbind(temp) %>%
                  as.data.frame
names(avg.data)[1:2] <- c("Subject.ID","activity")
row.names(avg.data) <- NULL

#Cleaning up the enviroment and leaving only the required output
rm(activity.name,data,label,master.data,subject,
   submeasure,temp,test.data,test.labels,test.subject,
   train.data,train.labels,train.subject)

#Write data into files inside Output_data folder
write.csv(avg.data,"Output_data/avg_data.csv",row.names = FALSE)
write.csv(subdata,"Output_data/Mean_Std_Data.csv",row.names = FALSE)
