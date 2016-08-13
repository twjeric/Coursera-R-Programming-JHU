rankhospital <- function(state, outcome, num = "best") {
    ##Read data
    my_file <- read.csv("outcome-of-care-measures.csv")
    
    ##Check that state and outcome are valid
    if (nrow(my_file[my_file["State"]==state,])==0) stop("invalid state")
    if (outcome!="heart attack" & outcome!="heart failure" & outcome!="pneumonia") stop("invalid outcome")
    
    ##Select outcome and hospital names, then clean the data
    colnum <- if(outcome=="heart attack") 11
              else if (outcome=="heart failure") 17
              else 23
    my_file <- my_file[my_file[["State"]]==state & my_file[[colnum]]!="Not Available" ,]
    my_file[[colnum]]<- as.numeric(as.character(my_file[[colnum]]))
    my_file <- my_file[ ,c(2,colnum)]
    my_file[[1]] <- as.character(my_file[[1]])
    
    ##Process num
    if (num=="best") num <- 1
    if (num=="worst") num <- -1
    
    ##Order the hospital
    my_file <- my_file[order(my_file[[2]],my_file[[1]]),]
    
    ##Return result
    if (num > nrow(my_file)) result <- NA
    else if (num < 0) result <- my_file[[1]][nrow(my_file)]
    else result <- my_file[[1]][num]
    result
    
}