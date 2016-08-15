complete <- function(directory, id = 1:332) {
    getdir <- function(directory, id) {
        if (id<10) paste(directory, "/00", id, ".csv", sep="")
        else if (id<100) paste(directory, "/0", id, ".csv", sep="")
        else paste(directory, "/", id, ".csv", sep="")
    }
    
    x <- data.frame(numeric(), numeric())
    for (i in id) {
        dir <- getdir(directory, i)
        my_file <- read.csv(dir)
        num <- nrow(my_file[!is.na(my_file[,2]) & !is.na(my_file[,3]),])
        x <- rbind(x, c(i, num))
    }
    
    colnames(x) <- c("id", "nobs")
    x
}