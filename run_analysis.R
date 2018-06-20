# Read training and testing data sets
trainSet <- read.table("./UCI HAR Dataset/train/X_train.txt", sep="")
testSet <- read.table("./UCI HAR Dataset/test/X_test.txt", sep="")

# Join the training and testing data sets
completeSet <- rbind(trainSet, testSet)

# Read features names for the measurements 
features <- read.table("./UCI HAR Dataset/features.txt")
colNames <- as.vector(features$V2)

# Extract measurements on the mean and standard deviation for each measurement
cMean <- grep("mean", colNames )
cStddev <- grep("std", colNames )
vCol <- c(cMeans, cStddev)
oCol <- order(vCol)
vCol <- vCol[oCol]
colNames <- colNames[vCol]
nCol <- length(colNames)

# Extract selected columns to a new data set
selectSet <- completeSet[vCol]
nRow <- nrow(selectSet)

# Name variables columns
colnames(selectSet) <- colNames

# Read activity names
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
activityLabels <- as.vector(activityLabels$V2)

# Read activities performed by the subjects
activityTrainSet <- read.table("./UCI HAR Dataset/train/y_train.txt")
activityTestSet <- read.table("./UCI HAR Dataset/test/y_test.txt")

# Join the training and testing activities
activitySet <- rbind(activityTrainSet, activityTestSet)
activitySet <- as.vector(activitySet$V1)

# Name activities
activityNameSet <- activityLabels[activitySet]

# Read subjects who performed the measured activities
subjectTrainSet <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subjectTestSet <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Join the training and testing subjects
subjectSet <- rbind(subjectTrainSet, subjectTestSet)
subjectSet <- as.vector(subjectSet$V1)

# Create the new data table
nActivity <- length(activityLabels)
vSubject <- unique(subjectSet)
nSubject <- length(vSubject)
avgSet <- data.frame(matrix(NA, nrow=nActivity*nSubject, ncol=nCol+2))
vNames <- c("Subject", "Activity")
vNames <- c(vNames, colNames)
colnames(avgSet) <- vNames

# Compute the average of each variable for each activity and each subject
ind <- 1
for (indS in 1:nSubject) {
        for (indA in 1:nActivity) {
                avgSet$Subject[ind] <- indS
                avgSet$Activity[ind] <- activityLabels[indA]
                vInd <- c(activitySet==indA & subjectSet==indS)
                for (indC in 1:nCol) {
                        avgSet[ind, indC+2] <- mean(selectSet[vInd, indC])
                }
                ind <- ind+1
        }
}

# Write average data set
write.csv(avgSet, "avgSet.csv", row.names=FALSE)