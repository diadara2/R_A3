rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  data <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE,
                   na.strings=c("Not Available"))
  
  exp<- c("heart attack"  ,"heart failure" , "pneumonia")
  if (!any(outcome == exp)){                
    stop("invalid outcome") 
  }
  
  
  library(dplyr)
  
  STATE <- unique(as.character(data$State))
  
  df<-vector()
  
      
    for(i in 1:length(STATE)){
      if(num== "best"){
        
        if(outcome==exp[1]){
          data$rate <- as.numeric(data[,11])
          data <- arrange(data, rate)
          dh <- data[data$State == STATE[i],]
          df<- c(df,dh$Hospital.Name[1]) }
        
        else if(outcome==exp[2]){
          data$rate <- as.numeric(data[,17])
          data <- arrange(data, rate)
          dh <- data[data$State == STATE[i],]
          df<-c(df,dh$Hospital.Name[1]) }
        
        else if(outcome==exp[3]){
          data$rate <- as.numeric(data[,23])
          data <- arrange(data, rate)
          dh <- data[data$State == STATE[i],]
          df<-c(df,dh$Hospital.Name[1]) } 
      }
      
      else if(is.numeric(num)){
        
        if(outcome==exp[1]){
          data$rate <- as.numeric(data[,11])
          data <- arrange(data,rate, st )
          
          dh <- data[data$State == STATE[i],]
          df<- c(df,dh$Hospital.Name[num]) }
        
        else if(outcome==exp[2]){
          data$rate <- as.numeric(data[,17])
          data <- arrange(data,rate)
          data <- data[data$State == STATE[i],]
          data$rate <- as.numeric(data[,17])
          v <- vector()
          v<- data$rate
          if(v[1] < v[2]){ 
            df<-c(df,data$Hospital.Name[num+1])}
          
          else {df<-c(df,data$Hospital.Name[num]) }
        }
        else if(outcome==exp[3]){
          data$rate <- as.numeric(data[,23])
          data <- arrange(data,rate, st )
          dh <- data[data$State == STATE[i],]
          df<-c(df,dh$Hospital.Name[num]) } 
      }
      
      else if(num== "worst"){
        
        if(outcome==exp[1]){
          data$rate <- as.numeric(data[,11])
          data <- arrange(data, desc(rate))
          dh <- data[data$State == STATE[i],]
          df<-c(df,dh$Hospital.Name[1]) }
        
        else if(outcome==exp[2]){
          data$rate <- as.numeric(data[,17])
          data <- arrange(data, desc(rate))
          dh <- data[data$State == STATE[i],]
          df<-c(df,dh$Hospital.Name[1]) }
        
        else if(outcome==exp[3]){
          data$rate <- as.numeric(data[,23])
          data <- arrange(data, desc(rate))
          dh <- data[data$State == STATE[i],]
          df<-c(df,dh$Hospital.Name[1]) } 
      }
    }
    
  d<- data.frame(hospital = df, state = STATE)
  arrange(d, state)
  
  
  
}