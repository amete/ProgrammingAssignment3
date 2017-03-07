rankhospital <- function(state, outcome, num = "best") {
    # Read in the data
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character")
    # Make sure the user inputs are correct
    if(!(state %in% data$State)) {
        stop("invalid state")
    }
    outcomes = c("heart attack",
                 "heart failure",
                 "pneumonia")
    index_outcome <- 0L
    if(!(outcome %in% outcomes)) {
        stop("invalid outcome")
    } else if(outcome==outcomes[1]) {
        index_outcome <-  11L
    } else if(outcome==outcomes[2]) {
        index_outcome <- 17L
    } else if(outcome==outcomes[3]) {
        index_outcome <- 23L
    }
    # Find and return the name of the hospital that
    # has the best (i.e. lowest) 30-day mortality 
    # for the specified outcome in that state.
    selected <- subset(data, data$State == state & data[,index_outcome]!="Not Available" , drop = FALSE)
    selected <- selected[ , c(2, index_outcome) ]       # Only store the part that we care about
                                                        # After this, 1st column is the name and 2nd the rate
    selected[,2] <- sapply(selected[,2], as.numeric)    # Convert to numeric
    names(selected) <- c("Hospital.Name","Rate")        # Add nicer names
    # Sort data.frame a la http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns
    sorted <- selected[ order(selected[,2], selected[,1]), ]
    sorted$Rank <- c(1:nrow(sorted)) # Not really needed
    # Return the result based on the user input
    result <- NA
    if(is.character(num)) {
        if(num=="best") { 
            result <- sorted[1,1]
        }
        else if(num=="worst") { 
            result <- sorted[nrow(sorted),1]
        }
    } else if(is.numeric(num)) {
        if(num<=nrow(sorted)) {
            result <- sorted[num,1]
        }
    }
    return(result)
}