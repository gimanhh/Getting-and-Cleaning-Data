# Set working directory
setwd("C:/Users/gilma/Desktop/Data Science/Getting and Cleaning Data/HomeWork/UCI HAR Dataset")

# Load data
# Activity Labels
actLabels<-read.table("activity_labels.txt",col.names=c("ID","Activity"))

#Feature Labels
featLabels<-read.table("features.txt",col.names=c("Column","FeatureName"))

# Training and Test subject numbers
# Each row specifies an observation, subject enumerated in an ID number (from 1 to 30)

subjTrainNum<-read.table("./train/subject_train.txt",col.names=c("NumSubject"))
subjTestNum<-read.table("./test/subject_test.txt",col.names=c("NumSubject"))

# Extract activity IDs per observation
# With subjTrainNum and subjTestNum, for each subject, they performed a particular action enumerated between 1 - 6.

subjTrainAct<-read.table("./train/y_train.txt",col.names="ID")                                  
subjTestAct<-read.table("./test/y_test.txt",col.names="ID")

# Extract the observations per subject and activity
# With subjTrainAct and subjTestAct, for each kind of action the particular subject is doing

subjTrainFeat<-read.table("./train/X_train.txt")
subjTestFeat<-read.table("./test/X_test.txt")

# Create a data frame for Training and Test,
# and assign each column the actions names

actions<-as.character(featLabels[,2])

subjTrainFeat<-data.frame(subjTrainFeat)
subjTestFeat<-data.frame(subjTestFeat)

names(subjTrainFeat)<-actions
names(subjTestFeat)<-actions

# Look at the features and convert to a character vectors
# Type ?grep and see details for syntaxis code.
# The result is a numeric vector with each location of -mean() or -std()
Means<-grep("mean\\(\\)$",actions)
Stds<-grep("std\\(\\)$",actions)

# Join the result in a large vector of locations
locations<-c(Means,Stds)

# Create the vectors of the number of subject and names of the activities
activity<-as.factor(actLabels[,2])
subjTrain<-as.numeric(subjTrainAct[[1]])
subjTest<-as.numeric(subjTestAct[[1]])

# Uses descriptive activity names to name the activities in the data set
subjTrainFinal<-data.frame(Activity=activity[subjTrain])
subjTestFinal<-data.frame(Activity=activity[subjTest])

# Appropriately labels the data set with descriptive variable names.
Training<-cbind(subjTrainNum,subjTrainAct,subjTrainFinal,subjTrainFeat)
Test<-cbind(subjTestNum,subjTestAct,subjTestFinal,subjTestFeat)

# Now extract the columns that have mean() and std() only
# note that selecting the first three columns to select mean() and std()
# must add locations + 3, to coincide with the displacement
TrainingFinal<-Training[,c(1,2,3,locations+3)]
TestFinal<-Test[,c(1,2,3,locations+3)]

# Join Training and Test in a single dataset
Dataset<-rbind(TrainingFinal,TestFinal)

# Then sort based on subject number
Dataset<-Dataset[order(Dataset$NumSubject),] 


# Step 5: From the data set in step 4, creates a second, independent    
# tidy data set with the average of each variable for each activity and 
# each subject.                                                         


# Obtain total number of subjects
numSubj<-max(Dataset$NumSubject)

# Obtain total number of actions
numAct<-max(Dataset$ID)

# Create the tidy data frame
TidyData<-data.frame()

for(i in 1:numSubj) # 1 - 30
{
   for(j in 1:numAct) # 1 - 6
   {
      # Obtain all data for subject i and action j
      TidyData<-Dataset[Dataset$NumSubject==i & Dataset$ID==j,]

      # Select the cualitative variables
      cuali<-TidyData[1,1:3]

      # Select the rest of variables
      rest<-TidyData[,c(-1,-2,-3)]
      
      # Place the mean of each subject for each activity
      finalFrame<-rbind(TidyData, data.frame(c(cuali, colMeans(rest))))
   }
}

# Set the names of the Data Frame
names(TidyData)<-names(Dataset)

# Write finalFrame to file txt
write.table(TidyData, file="UCI-HAR Dataset.txt", row.name=FALSE)

# Clean your environment removing all sub-data
rm(subjTrainNum, subjTrainFinal, subjTrainFeat, subjTrainAct,
   subjTestNum, subjTestFinal, subjTestFeat, subjTestAct,
   featLabels, actLabels, TrainingFinal, Training,
   TestFinal, Test)

# End
