
pollutantmean1 <- function(directory, pollutant, id = 1:332) {
    
    m <- c()
    
    for (i in id) {
        # read csv data file identified in filelist by 'i'
        path <- file.path(directory, paste(sprintf("%03d", as.numeric(i)), ".csv", sep=""))
    
        # read data file
        data <- read.csv(path)
    
        # identify missing data
        b <- is.na(data[[pollutant]])
        
        # store pollutant data in vector
        m <- c(m, data[[pollutant]][!b])
    }
    
    # return mean of pollutant, rounded to 3 digits
    return (round(mean(m), digits=3))
}