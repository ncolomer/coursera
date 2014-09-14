best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df[, 11] <- as.numeric(df[, 11]) ## heart attack = 11
  df[, 17] <- as.numeric(df[, 17]) ## heart failure = 17
  df[, 23] <- as.numeric(df[, 23]) ## pneumonia = 23
  
  ## Check that state and outcome are valid
  if (!(state %in% unique(df$State))) stop("invalid state")
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% outcomes)) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate 
  col <- c(11, 17, 23)[match(outcome, outcomes)]
  df[which.min(df[df$State == state, col]), 2]
}