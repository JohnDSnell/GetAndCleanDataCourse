##
##This function 
##      Merges the training and the test sets to create one data set.
##      Extracts only the measurements on the mean and standard deviation for each measurement. 
##      Uses descriptive activity names to name the activities in the data set
##      Appropriately labels the data set with descriptive variable names. 
##      From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## 
library(dplyr)
run_analysis<-function (path)
{
        #read activities map
        activityLabels<-read.table(paste(path,"\\activity_labels.txt",sep=""))
        
        #build extraction map by searching features for the index of all features containing either "mean" or "std"
        features<-read.table(paste(path,"\\features.txt",sep=""))
        map<-sapply(features, function(x) grep("-mean()|-std()",x))
        
        #create a vector of columnnames to go with extracted cols
        mapNames<-c("subject","activity",as.character(features$V2[map$V2]))
        #remove function brackets from columnnames
        mapNames<-sub("\\(\\)", "", mapNames)
        
        #final map for reading from merged dataset 
        datamap<-c(1,2,map$V2+2)
        mergedData<-mergeSets(path, activityLabels)

        extractedData<-extractRequiredColumns(mergedData,datamap)
        #set column names
        colnames(extractedData)<-mapNames
        
        outputSet<-determineOutputSet(extractedData)
        #write the final cleaned, merged and summarised data to a file
        opfilename<-paste("courseassignmentCleanData",as.character.Date(Sys.time()),sep="")
        write.table(outputSet,file="courseassignmentCleanData.txt",row.names = F)
        outputSet
}

## Loads the training and testing datasets separately
## Then merges the training and the testing sets to create one data set.
##Parameter:
##              path    -       the path to the directory where the test sets are stored
mergeSets<-function (path,activityLabels)
{
        trainData<-loadData("train",path,activityLabels)
        #print(trainData[,2])
        testData<-loadData("test",path,activityLabels)
        
        print(paste("dim traindata:",dim(trainData)))
        print(paste("dim testdata:",dim(testData)))
        mergedData<-rbind(trainData,testData)
}

##this function loads the information contained in the three text files specified by the dataset parameter: 
##      subject (1 column),Y (1 column),X (561 columns)
##combines them together into a single dataframe (563 columns) and returns the combined data
##Parameters:
##              dataset - identifies which dataset is required "train" or "test"
##              path    - identifies the path to the directory containing the required dataset
loadData<-function(dataset, path, activityLabels)
{
        extender<-".txt"
        extendedPath<-paste(path,"\\",dataset,"\\",sep="")
        
        #construct filename for the subject file for dataset and read in subject data
        filename<-paste(extendedPath,"subject_",dataset,extender,sep="")
        #print(filename)
        subjectData<-read.table(filename)
        
        #construct filename for the activity file for dataset and read in activity data
        filename<-paste(extendedPath,"y_",dataset,extender,sep="")
        #print(filename)
        YData<-read.table(filename)
        
        #map activity number into activity label - the x in the function is the activity number vector
        #as encoded in the Y file
        Activities<-sapply(YData,function(x){activityLabels$V2[x]})
        #construct filename for the measured data file for dataset and read in measurements data
        filename<-paste(extendedPath,"x_",dataset,extender,sep="")
        #print(filename)
        XData<-read.table(filename)
        
        #combine the subject, activity and measurements into one dataframe
        loadedData<-cbind(subjectData,Activities,XData)
}


##Uses the map to identify appropriate columns in the merged data, extracts those columns and binds them into 
##a new dataframe which returned a the extracted dataset. Note this somewaht 'knife and fork' code was written 
##in Week 2 prior to the dplyr package being introduced.  
##Parameters:
##              map - a vector of column numbers for the required columns
extractRequiredColumns<-function(mergeData,map)
{
        extract <-as.null()
        for (i in map)
        {
                #get required col from merged data
                col <- mergeData[i]
                
                if (is.null(extract))
                {
                        #no extract dataframe as yet so create it around the extracted column
                        extract <- data.frame(col)
                }
                else
                {
                        #extract dataframe has been created, so just add colum to dataframe
                        extract <- cbind(extract,col) 
                }
        }

        #return extracted columns
        extract
        
}

##This function uses the more elegant dplyr functionality of grouping a dataframe by specific and then performing a summarising function on the grouped data
##In this specific case the group is intially by subject and then by activity for each subject and the summary is of the means of the results 
##for each variable on a per subject per activity basis. The dplyr functions are chained for efficiency of reading.
##Parameters:
##              inset - the cleaned and merged data from the training and testing sets of acclerometer data
##Output:       a new dataframe containing the summarised data on a per subject per activity basis.
determineOutputSet<-function(inset)
{
        output<-inset%>%group_by(subject,activity)%>%summarise_each(funs(mean))
}
