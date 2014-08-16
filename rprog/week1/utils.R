files <- list.files(directory, pattern = "*.csv", full.names=T)

merge <- function(files, ...) {  
  # Initialize an empty data frame
  dat <- data.frame()
  # Append all CSV into the data frame
  for (i in 1:length(files)) dat <- rbind(dat, read.csv(files[i], ...))
  # Return the populated data frame
  dat
}