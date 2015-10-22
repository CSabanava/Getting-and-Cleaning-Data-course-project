if(!file.exists("./gcd project")){dir.create("./gcd project")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile = "./gcd project/data.zip")
unzip("./gcd project/data.zip")

setwd("./UCI Har Dataset")

subject_tests <- read.fwf("./test/subject_test.txt",widths = c(3))
X_test <- read.fwf("./test/X_test.txt",widths = c(15))
y_test <- read.fwf("./test/y_test.txt",widths = c(3))
body_acc_x_test <- read.fwf("./test/Inertial Signals/body_acc_x_test.txt",widths = c(16))
body_acc_y_test <- read.fwf("./test/Inertial Signals/body_acc_y_test.txt",widths = c(16))
body_acc_z_test <- read.fwf("./test/Inertial Signals/body_acc_z_test.txt",widths = c(16))
body_gyro_x_test <- read.fwf("./test/Inertial Signals/body_acc_x_test.txt",widths = c(16))
body_gyro_y_test <- read.fwf("./test/Inertial Signals/body_acc_y_test.txt",widths = c(16))
body_gyro_z_test <- read.fwf("./test/Inertial Signals/body_acc_z_test.txt",widths = c(16))
total_acc_x_test <- read.fwf("./test/Inertial Signals/total_acc_x_test.txt",widths = c(16))
total_acc_y_test <- read.fwf("./test/Inertial Signals/total_acc_y_test.txt",widths = c(16))
total_acc_z_test <- read.fwf("./test/Inertial Signals/total_acc_z_test.txt",widths = c(16))

test <- data.frame(subject_tests,X_test,y_test,body_acc_x_test,body_acc_y_test,
                   body_acc_z_test,body_gyro_x_test,body_gyro_y_test,body_gyro_z_test,
                   total_acc_x_test,total_acc_y_test,total_acc_z_test)
names(test) <- c("subject","X_test","y_test","body_acc_x_test","body_acc_y_test",
                 "body_acc_z_test","body_gyro_x_test","body_gyro_y_test","body_gyro_z_test",
                 "total_acc_x_test","total_acc_y_test","total_acc_z_test")

subject_train <- read.fwf("./train/subject_train.txt",widths = c(3))
X_train <- read.fwf("./train/X_train.txt",widths = c(15))
y_train <- read.fwf("./train/y_train.txt",widths = c(3))
body_acc_x_train <- read.fwf("./train/Inertial Signals/body_acc_x_train.txt",widths = c(16))
body_acc_y_train <- read.fwf("./train/Inertial Signals/body_acc_y_train.txt",widths = c(16))
body_acc_z_train <- read.fwf("./train/Inertial Signals/body_acc_z_train.txt",widths = c(16))
body_gyro_x_train <- read.fwf("./train/Inertial Signals/body_acc_x_train.txt",widths = c(16))
body_gyro_y_train <- read.fwf("./train/Inertial Signals/body_acc_y_train.txt",widths = c(16))
body_gyro_z_train <- read.fwf("./train/Inertial Signals/body_acc_z_train.txt",widths = c(16))
total_acc_x_train <- read.fwf("./train/Inertial Signals/total_acc_x_train.txt",widths = c(16))
total_acc_y_train <- read.fwf("./train/Inertial Signals/total_acc_y_train.txt",widths = c(16))
total_acc_z_train <- read.fwf("./train/Inertial Signals/total_acc_z_train.txt",widths = c(16))

train <- data.frame(subject_train,X_train,y_train,body_acc_x_train,body_acc_y_train,
                    body_acc_z_train,body_gyro_x_train,body_gyro_y_train,body_gyro_z_train,
                    total_acc_x_train,total_acc_y_train,total_acc_z_train)
names(train) <- c("subject","X_train","y_train","body_acc_x_train","body_acc_y_train",
                  "body_acc_z_train","body_gyro_x_train","body_gyro_y_train","body_gyro_z_train",
                  "total_acc_x_train","total_acc_y_train","total_acc_z_train")

if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
library(plyr)
step1 <- join(test,train)

step1.1 <- melt(step1,id.vars = 2:23,measure.vars = "subject")
step1.2 <- dcast(step1.1,step1.1[,2]~variable,mean)

means <- data.frame()
stds <- data.frame()
for (i in 2:23){
  x <- step1[step1$subject == i,]
  mean <- colMeans(x,na.rm = TRUE)
  std <- apply(x,2,sd)
  means <- rbind(means,mean)
  stds <- rbind(stds,std)
}
m_s <- data.frame()
m_s <- rbind(m_s,means);m_s <- cbind(m_s,stds[2:23])
names(m_s) <- c("subject","X_test_mean","y_test_mean","body_acc_x_test_mean","body_acc_y_test_mean",
                "body_acc_z_test_mean","body_gyro_x_test_mean","body_gyro_y_test_mean","body_gyro_z_test_mean",
                "total_acc_x_test_mean","total_acc_y_test_mean","total_acc_z_test_mean","X_train_mean","y_train_mean","body_acc_x_train_mean","body_acc_y_train_mean",
                "body_acc_z_train_mean","body_gyro_x_train_mean","body_gyro_y_train_mean","body_gyro_z_train_mean",
                "total_acc_x_train_mean","total_acc_y_train_mean","total_acc_z_train_mean","X_test_sd","y_test_sd","body_acc_x_test_sd","body_acc_y_test_sd",
                "body_acc_z_test_sd","body_gyro_x_test_sd","body_gyro_y_test_sd","body_gyro_z_test_sd",
                "total_acc_x_test_sd","total_acc_y_test_sd","total_acc_z_test_sd","X_train_sd","y_train_sd","body_acc_x_train_sd","body_acc_y_train_sd",
                "body_acc_z_train_sd","body_gyro_x_train_sd","body_gyro_y_train_sd","body_gyro_z_train_sd",
                "total_acc_x_train_sd","total_acc_y_train_sd","total_acc_z_train_sd")

