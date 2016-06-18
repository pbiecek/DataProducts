library(dplyr)

get_courses_vector <- function(){
  table <- read.csv("courses.csv")
  i <- sapply(table, is.factor)
  table[i] <- lapply(table[i], as.character)
  table$courses_vector
}

courses_vector <- get_courses_vector()
