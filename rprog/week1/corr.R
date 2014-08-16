corr <- function(directory, threshold = 0) {
  # Retrieve the matching monitor ids
  complete <- complete(directory)
  ids <- complete[complete$nobs > threshold, "id"]
  # Initialize the vector of correlation
  cors <- c()
  # Iterate over the monitor ids
  for (i in ids) {
    # Retrieve the file path
    file <- sprintf(paste(directory, "%03s.csv", sep = "/"), i)
    # Read the file
    csv <- read.csv(file, header=T)
    # Compute and append correlation
    cors <- c(cors, cor(csv$sulfate, csv$nitrate, use = "complete.obs"))
  }
  cors
}