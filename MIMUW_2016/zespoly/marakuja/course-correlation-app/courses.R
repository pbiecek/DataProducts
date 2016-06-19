source("config.R")

library(dplyr)

get_courses_vector <- function(){
  table <- read.csv(COURSES_LIST_PATH)
  i <- sapply(table, is.factor)
  table[i] <- lapply(table[i], as.character)
  
  table <- rbind(filter(table,
                 courses_vector!="Statystyczna analiza danych II (wspólne z 1000-718SAD)"),
                 "Statystyczna analiza danych II")
  
  table <- rbind(filter(table,
           courses_vector!="Statystyczna analiza danych (wspólne z 1000-714SAD)"),
           "Statystyczna analiza danych")
  table <- rbind("Języki, automaty i obliczenia",
                 filter(table,
                 courses_vector!="Języki, automaty i obliczenia"))
  table$courses_vector
}

courses_vector <- get_courses_vector()
