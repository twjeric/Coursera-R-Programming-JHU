rankall <- function(outcome, num = "best") {
    ##Read data
    my_file <- read.csv("outcome-of-care-measures.csv")
    
    ##Check that outcome is valid
    if (outcome!="heart attack" & outcome!="heart failure" & outcome!="pneumonia") stop("invalid outcome")
    
    ##Select outcome and hospital names, then clean the data
    colnum <- if(outcome=="heart attack") 11
              else if (outcome=="heart failure") 17
              else 23
    my_file <- my_file[my_file[[colnum]]!="Not Available" ,]
    my_file[[colnum]]<- as.numeric(as.character(my_file[[colnum]]))
    my_file <- my_file[ ,c(2,7,colnum)]
    my_file[[1]] <- as.character(my_file[[1]])
    states <- levels(my_file[[2]])
    my_file[[2]] <- as.character(my_file[[2]])
    
    ##Process num
    if (num=="best") num <- 1
    if (num=="worst") num <- -1
    
    ##For each state, find the hospital of the given rank
    result <- data.frame(NA, states, row.names = states)
    for (state in states) {
        tmp_file <- my_file[my_file[[2]]==state,]
        tmp_file <- tmp_file[order(tmp_file[[3]],tmp_file[[1]]),]
        if (num < 0) result[result[[2]]==state,][1] = tmp_file[[1]][nrow(tmp_file)]
        else if (num <= nrow(tmp_file)) result[result[[2]]==state,][1] = tmp_file[[1]][num]
    }
    
    ##return a data frame with the hospital names and the state names
    colnames(result) <- c("hospital", "state")
    result
    
}