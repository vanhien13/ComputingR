best <- function(state, outcome){
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcomes <-c("heart attack", "heart failure", "pneumonia")
  data <- read.csv(file="outcome-of-care-measures.csv", colClasses = "character")
  data[,11]<-as.numeric(data[,11])
  data[,23]<-as.numeric(data[,23])
  data[,17]<-as.numeric(data[,17])
  if(!state %in% data$State){
    stop("Invalid State")
  }else if(!outcome %in% outcomes){
    stop("Invalid Outcome!")
  }else{
      subset<-data[data$State == state, ]
      if(outcome == "heart attack"){
      heart_attack <- subset[,11]
      lowest<-min(heart_attack, na.rm=T)
      index<-which(heart_attack==lowest)
      hos_name<-subset[,2][index]    
    }
      else if(outcome == "heart failure"){
      heart_failure <- subset[,23]
      lowest<-min(heart_failure, na.rm=T)
      index<-which(heart_failure==lowest)
      hos_name<-subset[,2][index]
    }
      else if(outcome == "pneumonia"){
      pneumonia <- subset[,17]
      lowest<-min(pneumonia, na.rm=T)
      index<-which(pneumonia==lowest)
      hos_name<-subset[,2][index]
    }      
  }
  return(hos_name)    
}
