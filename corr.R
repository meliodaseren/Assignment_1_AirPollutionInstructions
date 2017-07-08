corr <- function(directory, threshold = 0) {
       df <- complete(directory)
       ids <- df[df["nobs"] > threshold, ]$id
       corrr <- numeric()
       for (i in ids) {
              newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                       ".csv", sep = ""))
              dff = newRead[complete.cases(newRead), ]
              corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
       }
       return(corrr)
}

corr1 <- function(directory, threshold = 0) {
       files <- list.files(directory)
       cr <- c() 
       for(f in 1 : length(files)) {
              data <- read.csv(paste(directory, "/", files[f], sep=""))
              data <- data[complete.cases(data), ]
              if (nrow(data) > threshold) {
                     cr <- c(cr, cor(data$sulfate, data$nitrate))
              }
       }
       return(cr)
}

corr2 <- function(directory, threshold = 0) {
       tcorr <- function(fname) {
              data <- read.csv(file.path(directory, fname))
              nobs <- sum(complete.cases(data))
              if (nobs > threshold) {
                     return (cor(data$nitrate, data$sulfate, use = "complete.obs"))
              }
       }
       tcorrs <- sapply(list.files(directory), tcorr)
       tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)])
       return (tcorrs)
}