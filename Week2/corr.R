corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files <- list.files( path = directory )
  #filenames <- list.files("temp", pattern="*.csv", full.names=TRUE)

  correlation <- c()
  
  for(i in 1:length(files)){
    data <- read.csv( paste(directory, files[i], sep=""))
    data <- data[complete.cases(data),]
    if ( nrow(data) > threshold ) {
      correlation <- c(correlation, cor(data$sulfate, data$nitrate) ) # append correlations
    }
  }
  
  return( correlation )
}

output <- corr("specdata/", threshold = 3)

#filenames <- list.files("C:/R_Week2/specdata/specdata/", pattern="*.csv", full.names=TRUE)
#ldf <- lapply(filenames, read.csv)
#res <- lapply(ldf, summary)
#names(res) <- substr(filenames, 6, 30)
#setwd("target_dir/")
 
#file_list <- list.files()
 
#for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
#  if (!exists("dataset")){
#    dataset <- read.table(file, header=TRUE, sep="\t")
#  }
   
  # if the merged dataset does exist, append to it
#  if (exists("dataset")){
#    temp_dataset <-read.table(file, header=TRUE, sep="\t")
#    dataset<-rbind(dataset, temp_dataset)
#    rm(temp_dataset)
#  }
 
#}
