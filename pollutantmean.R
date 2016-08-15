pollutantmean <- function(directory, pollutant, id=1:332) {
    getdir <- function(directory, id) {
        if (id<10) paste(directory, "/00", id, ".csv", sep="")
        else if (id<100) paste(directory, "/0", id, ".csv", sep="")
        else paste(directory, "/", id, ".csv", sep="")
    }
    
    total <- 0
    num <- 0
    for (i in id) {
        dir <- getdir(directory, i)
        my_file <- read.csv(dir)
        total <- total + sum(my_file[pollutant], na.rm = TRUE)
        num <- num + sum(!is.na(my_file[pollutant]))
    }
    
    total / num
}