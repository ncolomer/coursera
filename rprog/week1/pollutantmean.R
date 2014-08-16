pollutantmean <- function(directory, pollutant, id = 1:332) {  
  # Initialize an empty data frame
  dat <- data.frame()
  # Read matching CSV into the data frame
  for (i in id) {
    file <- sprintf(paste(directory, "%03s.csv", sep = "/"), i)
    dat <- rbind(dat, read.csv(file, header=T))
  }
  # Compute pollutant mean
  mean(dat[, pollutant], na.rm=T)
}