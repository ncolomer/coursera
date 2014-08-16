complete <- function(directory, id = 1:332) {
  # Initialize an empty matrix
  dat <- matrix(ncol=2, nrow=0, dimnames=list(c(), c("id", "nobs")))
  # Read matching CSV into the matrix
  for (i in id) {
    # Retrieve the file path
    file <- sprintf(paste(directory, "%03s.csv", sep = "/"), i)
    # Read the file
    csv <- read.csv(file, header=T)
    # Append rows to dat
    nrow <- nrow(csv[!is.na(csv$sulfate) & !is.na(csv$nitrate),])
    dat <- rbind(dat, c(i, nrow))
  }
  data.frame(dat)
}