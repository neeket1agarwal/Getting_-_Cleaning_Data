#Load library if missing
if (!require("dplyr", character.only = TRUE)) {
      install.packages("dplyr", dependencies = TRUE)
      library("dplyr", character.only = TRUE)
}
if (!require("stringr", character.only = TRUE)) {
      install.packages("stringr", dependencies = TRUE)
      library("stringr", character.only = TRUE)
}

#read measurement,labels and subject data for test & train data
test.data <- read.table("test/X_test.txt")
train.data <- read.table("train/X_train.txt")
test.label <- read.table("test/y_test.txt")
train.label <- read.table("train/y_train.txt")
test.subject <- read.table("test/subject_test.txt")
train.subject <- read.table("train/subject_train.txt")
cnames.data <- read.table("features.txt")
activity.name <- read.table("activity_labels.txt")

#merge the measurement,labels and subject data (train + test)
data <- rbind(train.data,test.data)
label <- rbind(train.label,test.label)
subject <- rbind(train.subject,test.subject)

# Create names for the table columns from features.txt file
cnames.data <- make.names(str_replace_all(cnames.data[,2],"[()]",""))
names(data) <- cnames.data
names(label) <- "activity"
names(subject) <- "Subject.ID"

#merge all 3 tables to create master data
master.data <- cbind(subject,label,data)

#Extract measurements on mean and std deviation for each measurment
extracted.cols <- grep(pattern = "([Mm]ean|std)",names(data))
submeasure <- data[,extracted.cols]
#Remove MeanFreq related columns. To keep it exclude the next 2 commands
removed.cols <- grep(pattern = "(meanFreq)",names(submeasure))
submeasure <- submeasure[,-removed.cols]
subdata <- cbind(subject,label,submeasure)

#Replacing descriptive activity name to name the activities
subdata <- mutate(subdata,activity = activity.name[activity,2])

#Independent data set with average of each variable for each activity & subject
avg.data <- subdata %>%
      group_by(Subject.ID, activity) %>%
      summarise_all(mean)

#Write data into files inside Output_data folder
write.table(avg.data,"Output_data/avg_data.txt",row.names = FALSE)
write.table(subdata,"Output_data/Mean_Std_Data.txt",row.names = FALSE)
