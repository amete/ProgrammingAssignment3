rankall <- function(outcome, num = "best") {
    # Read in the data
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character")
    # Make sure the user inputs are correct
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
    # Return the result based on the user input
    result_index <- -1L
    if(is.character(num)) {
        if(num=="best") { 
            result_index <- 1L
        }
        else if(num=="worst") { 
            result_index <- 0L
        }
    } else if(is.numeric(num)) {
        result_index <- num
    }    
    # Find and return the name of the hospital that
    # has the best (i.e. lowest) 30-day mortality 
    # for the specified outcome
    selected <- subset(data, data[,index_outcome]!="Not Available" , drop = FALSE)
    selected <- selected[ , c(2, 7, index_outcome) ]    # Only store the part that we care about
                                                        # 1: Hospital name, 2: State, 3:Rate
    selected[,3] <- sapply(selected[,3], as.numeric)    # Convert to numeric
    sorted <- selected[ order(selected[,2], selected[,3], selected[,1]), ]  # Sort
    splitted <- split(sorted,sorted$State)              # Split by state
    # Build the resulting data frame and return
    hnames <- character()
    states <- character()
    for(i in seq_along(splitted)) {
        # Get data frame for the current state
        current <- splitted[[i]]
        states <- c(states,current[1,2]) # All states are the same
        if(result_index == -1L || result_index>nrow(current)) {
            hnames <- c(hnames,NA)
        } else if(result_index == 0L) {
            hnames <- c(hnames,current[nrow(current),1])
        } else {
            hnames <- c(hnames,current[result_index,1])
        } 
    }
    result <- cbind.data.frame(hnames,states)
    names(result) <- c("hospital","state")
    return(result)
}