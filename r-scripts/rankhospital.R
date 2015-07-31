rankhospital <- function(state, outcome, num = "best") {
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
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(state %in% substate$State){
                if(outcome == "heart attack"){
                        ret <- substate[which(!is.na(substate[,3])), c(1,3)]
                        ret <- ret[order(ret$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                }
                
                else if(outcome == "heart failure"){
                        ret <- substate[which(!is.na(substate[,4])), c(1,4)]
                        ret <- ret[order(ret$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                }
                
                else if(outcome == "pneumonia"){
                        ret <- substate[which(!is.na(substate[,5])), c(1,5)]
                        ret <- ret[order(ret$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                }
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if(num == "best"){
                ret[1,1]
        }
        else if(num == "worst"){
                ret[nrow(ret),1]
        }
        else if(num > nrow(ret)){
                return(NA)
        }
        else{
                ret[num,1]
        }
}