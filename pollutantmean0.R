
pollutantmean0 <- function(directory, pollutant, id = 1:332) {
  
    files_list <- list.files(directory, full.names=TRUE)
       
    dat <- data.frame()
       
    for (i in id) {
        dat <- rbind(dat, read.csv(files_list[i]))
    }
       
    return(print(mean(dat[, pollutant], na.rm = TRUE), digits=4))

}