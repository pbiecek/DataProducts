library(dplyr)

source("config.R")

get_marks_dataset <- function() {
  table <- read.csv(MARKS_CSV_PATH)
  data <- tbl_df(table)
}

data <- get_marks_dataset()

