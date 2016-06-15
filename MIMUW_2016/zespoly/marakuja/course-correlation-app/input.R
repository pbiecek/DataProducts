library(dplyr)

get_grades_dataset <- function(path) {
  table <- read.csv(path)
  data <- tbl_df(table)
}
