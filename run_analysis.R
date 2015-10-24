if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
library(plyr)
if("reshape" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape")}
library(rehape)

if(!file.exists("./gcd project")){dir.create("./gcd project")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile = "./gcd project/data.zip")
unzip("./gcd project/data.zip")

setwd("./UCI Har Dataset")


activity_labels <- read.table("C:/Users/Enrique/Downloads/UCI HAR Dataset/activity_labels.txt", quote="\"", comment.char="")
features <- read.table("C:/Users/Enrique/Downloads/UCI HAR Dataset/features.txt", quote="\"", comment.char="")


subject_tests <- read.fwf("./test/subject_test.txt",widths = c(3))
X_test <- read.table("C:/Users/Enrique/Downloads/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
y_test <- read.table("C:/Users/Enrique/Downloads/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")

subject_train <- read.fwf("./train/subject_train.txt",widths = c(3))
X_train <- read.table("C:/Users/Enrique/Downloads/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
y_train <- read.table("C:/Users/Enrique/Downloads/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")



step1 <- rbind(X_test,X_train)

X_mean_std_cols <- c(grep("-mean()",features$V2,fixed = TRUE),grep("-std()",features$V2,fixed = TRUE))
step2 <- step1[,X_mean_std_cols] 

step3.1 <- rbind(y_test,y_train)
step3.2 <- merge(step3.1,activity_labels,by="V1")
names(step3.2) <- c("code_activity","activity")

names(step2) <- features[X_mean_std_cols,]$V2

step4.1 <- data.frame(subjects=c(subject_tests$V1,subject_train$V1))
step4.2 <- cbind(step4.1,step3.2,step2)

step5 <- melt(step4.2,id.vars = c("subjects","code_activity","activity"))
step5.1 <- ddply(step5,.(subjects,activity,variable),summarize,average_value=mean(value))

step5.2 <- cast(step5.1,subjects + activity ~ variable,value = "average_value")
