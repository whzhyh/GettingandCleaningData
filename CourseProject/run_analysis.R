setwd("~/data")

# Step 1. Merges the training and the test sets to create one data set.
# Read tain and test data
trainData <- read.table("./train/X_train.txt")
trainLabel <- read.table("./train/y_train.txt")
trainSubject <- read.table("./train/subject_train.txt")

testData <- read.table("./test/X_test.txt")
testLabel <- read.table("./test/y_test.txt")
testSubject <- read.table("./test/subject_test.txt")

# Merge rows
data <- rbind(trainData, testData)
subject <- rbind(trainSubject, testSubject)
names(subject) <- "subject"
label <- rbind(trainLabel, testLabel)
names(label) <- "activityNum"

# Merge columns
subject <- cbind(subject, label)
data <- cbind(subject, data)

# Set key.
library(data.table)
dt <- data.table(data)
setkey(dt, subject, activityNum)

# Step 2. Extracts only the measurements on the mean and standard
# deviation for each measurement.
features <- data.table(read.table("./features.txt"))
setnames(features, names(features), c("featureNum", "featureName"))

features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]
features$featureCode <- features[, paste0("V", featureNum)]

select <- c(key(dt), features$featureCode)
dt <- dt[, select, with = FALSE]

# Step 3. Uses descriptive activity names to name the activities in the
# data set
activityLabels <- data.table(read.table("./activity_labels.txt"))
setnames(activityLabels, names(activityLabels), c("activityNum", "activityName"))

# Step 4. Appropriately labels the data set with descriptive variable names.
dt <- merge(dt, activityLabels, by = "activityNum", all.x = TRUE)
setkey(dt, subject, activityNum, activityName)
library(reshape2)
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
dt <- merge(dt, features[, list(featureNum, featureCode, featureName)],
            by = "featureCode", all.x = TRUE)

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

grepthis <- function(regex) {
    grepl(regex, dt$feature)
}

## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))


# Step5. Creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument,
       featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]

write.table(dtTidy, file = "tidyDataSet.txt", quote = FALSE, sep = "\t", row.name = FALSE)
