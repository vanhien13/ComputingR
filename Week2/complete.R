complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
	
for (count in id) {
        ## Assert get data frame as ID
        data <- getmonitor(count, directory,TRUE)

        ## count the number of complete cases
        
        nobsNum<-nrow(subset(data,sulfate!="NA" & nitrate!="NA"))
    }

    ## Assert return value is a data frame with TWO (2) columns
    data.frame(id = id, nobs = nobsNum)

	return(data.frame)
}
