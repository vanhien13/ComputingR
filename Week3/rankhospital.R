rankhospital <- function(state, outcome, num = "best" ){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!state %in% data$State) {
    stop("invalid state")
  }
  data <- data[data$State==state,]
  if (outcome == 'heart attack') {
    cause <- as.numeric(data[,11])
  } else if (outcome == 'heart failure') {
    cause <- as.numeric(data[,17])
  } else if (outcome == 'pneumonia') {
    cause <- as.numeric(data[,23])
  } else {
    stop("invalid outcome")
  }
  
  o <- rank(cause, na.last=NA)
  
  if (num=="best") {
    ranking <- 1
  } else if (num =="worst") {
    ranking <- length(o)
  } else if (num <= length(o) ) {
    ranking <- num
  } else {
    stop("wrong order")
  }
  
  return(data$Hospital.Name[order(cause, data$Hospital.Name)[ranking]])
}
