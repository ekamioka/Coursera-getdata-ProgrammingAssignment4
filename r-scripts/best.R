best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character" )
        outcomes <- c("heart attack","heart failure","pneumonia")
        
        ## Check that state and outcome are valid
        if(!state %in% data$State) stop("invalid state")
        if(!outcome %in% outcomes) stop("invalid outcome")
        
        substate <- data[which(data$State == state), c(2,7,11,17,23)]
        substate[, 3] <- suppressWarnings(as.numeric(substate[, 3]))
        substate[, 4] <- suppressWarnings(as.numeric(substate[, 4]))
        substate[, 5] <- suppressWarnings(as.numeric(substate[, 5]))
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if(state %in% substate$State){
                if(outcome == "heart attack"){
                        
                        min <- min(substate[,3], na.rm = TRUE)
                        ret <- sort(substate[which(substate[,3] == min),1])
                }
                
                else if(outcome == "heart failure"){
                        min <- min(substate[,4], na.rm = TRUE)
                        ret <- sort(substate[which(substate[,4] == min),1])
                }
                
                else if(outcome == "pneumonia"){
                        min <- min(substate[,5], na.rm = TRUE)
                        ret <- sort(substate[which(substate[,5] == min),1])
                }
        }
        
        ret[1]
        
}