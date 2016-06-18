#!/usr/bin/env Rscript

source("config.R")
library(dplyr)
courses_vector = system2(
    command = "python",
    args = "listcourses.py",
    stdout = TRUE
    )
df <- data.frame(courses_vector)
write.csv(df, "courses.csv", row.names = FALSE)
