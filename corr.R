corr <- function(directory, threshold = 0) {
    getdir <- function(directory, id) {
        if (id<10) paste(directory, "/00", id, ".csv", sep="")
        else if (id<100) paste(directory, "/0", id, ".csv", sep="")
        else paste(directory, "/", id, ".csv", sep="")
    }
    
    nobs <- complete(directory)
    result <- numeric()
    for (i in 1:332) {
        if (nobs[[2]][i]>threshold) {
            file <- read.csv(getdir(directory,i))
            file <- file[!is.na(file[["sulfate"]]) & !is.na(file[["nitrate"]]), ]
            result <- c(result, cor(file[["sulfate"]],file[["nitrate"]]))
        }
    }
    result
}