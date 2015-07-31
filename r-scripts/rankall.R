rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character" )
        outcomes <- c("heart attack","heart failure","pneumonia")
        
        ## Check that outcome is valid
        if(!outcome %in% outcomes) stop("invalid outcome")
        
        substate <- data[, c(2,7,11,17,23)]
        substate[, 3] <- suppressWarnings(as.numeric(substate[, 3]))
        substate[, 4] <- suppressWarnings(as.numeric(substate[, 4]))
        substate[, 5] <- suppressWarnings(as.numeric(substate[, 5]))
        
        sub <- subset(substate, select = State)
        count <- as.data.frame(table(sub))
        states <- count[,1]
        rank <- data.frame(hospital = character(), state = character(), stringsAsFactors=FALSE)
        
        
        ## For each state, find the hospital of the given rank
        
        for(state in states){
                if(outcome == "heart attack"){
                        ret <- substate[which(substate$State == state), c(1:3)]
                        ret <- ret[order(c(ret$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),ret$Hospital.Name),]
                        numm <- numindex(ret,num)
                        if(is.na(ret[numm, 2])){
                                rank[state,] <- c(ret[numm,1],state)
                        }
                        else{
                                rank[state,] <- ret[numm,c(1,2)]
                        }
                }
                
                else if(outcome == "heart failure"){
                        ret <- substate[which(substate$State == state), c(1,2,4)]
                        ret <- ret[order(c(ret$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),ret$Hospital.Name),]
                        numm <- numindex(ret, num)
                        
                        if(is.na(ret[numm, 2])){
                                rank[state,] <- c(ret[numm,1],state)
                        }
                        else{
                                rank[state,] <- ret[numm,c(1,2)]
                        }
                }
                
                else if(outcome == "pneumonia"){
                        ret <- substate[which(substate$State == state), c(1,2,5)]
                        ret <- ret[order(c(ret$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),ret$Hospital.Name),]
                        numm <- numindex(ret, num)
                        if(is.na(ret[numm, 2])){
                                rank[state,] <- c(ret[numm,1],state)
                        }
                        else{
                                rank[state,] <- ret[numm,c(1,2)]
                        }
                }
        }       
               
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        rank
}

numindex <- function(ret, num){
        ## deciding the index and 
        if(num == "best"){
                numm <- 1
                return(numm)
        }
        else if(num == "worst"){
                numm <- nrow(ret)
                return(numm)
        }
        else if(num > 54){
                return(NA)
        }
        else{
                numm <- num
                return(numm)}
}