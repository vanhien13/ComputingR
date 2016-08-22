corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  file_list <- list.files(path = directory)
  correl <- c()
  for(i in 1:length(file_list)){
    data <- read.csv(paste(directory, file_list[i], sep=""))
    data[complete.cases(data),]
    if(nrow(data)>threshold){
      correl <- c(correl, cor(data$sulfate, data$nitrate)) # append correlations
    }
  }
  return(correl)  
}
