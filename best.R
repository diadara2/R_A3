best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE,
                   na.strings=c("Not Available"))
  if (!any(state == data[[7]])){                
    stop("invalid state") 
  }
  
  exp<- c("heart attack"  ,"heart failure" , "pneumonia")
  if (!any(outcome == exp)){                
    stop("invalid outcome") 
  }
 
  library(dplyr)
      
  if(outcome==exp[1]){
    data$rate <- as.numeric(data[,11])
    data <- arrange(data, rate)
    dh <- data[data$State == state,]
    dh$Hospital.Name[1]    
}

else if(outcome==exp[2]){
  data$rate <- as.numeric(data[,17])
  data <- arrange(data, rate)
  dh <- data[data$State == state,]
  dh$Hospital.Name[1]    
}

else if(outcome==exp[3]){
  data$rate <- as.numeric(data[,23])
  data <- arrange(data, rate)
  dh <- data[data$State == state,]
  dh$Hospital.Name[1]    
}
}
  
