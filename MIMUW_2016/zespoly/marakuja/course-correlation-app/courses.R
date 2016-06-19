source("config.R")

library(dplyr)

get_courses_vector <- function(){
  table <- read.csv(COURSES_LIST_PATH)
  i <- sapply(table, is.factor)
  table[i] <- lapply(table[i], as.character)
  table$courses_vector
}

courses_vector <- get_courses_vector()
