#!/usr/bin/env Rscript

source("config.R")
source ("utils.R")
source ("input.R")
library(dplyr)

get_courses_list <- function() {
  courses_vector = system2(
      command = "python",
      args = "listcourses.py",
      stdout = TRUE
      )
  df <- data.frame(courses_vector)
  df
}

save_courses_list <- function(data, file) {
  write.csv(data, file, row.names = FALSE)
}

#' Skrypt służący do pobierania danych o ocenach z USOSa.
#'
#' Wymagania:
#'    install.packages("JDBC")
#'
#' Użycie:
#'    Ustalić wartości poniższych parametrów.
#'    Rscript import_data.R

get_data_from_DataBase <- function() {

  driverClass <- "oracle.jdbc.OracleDriver"
  classPath <- "ojdbc6.jar"
  host <- "usos.edu.pl"
  port <- 1521
  serviceName <- "USOS"
  user <- "user"
  password <- "password"
  outputFile <- GRADES_CSV_PATH

  library(RJDBC)

  codePrefixMIM <- "1000-"

  driver <- JDBC(driverClass=driverClass,
                 classPath=classPath,
                 identifier.quote=" ")

  url <- paste("jdbc:oracle:thin:@//", host, ":", port, "/", serviceName,
               sep="")

  connection <- dbConnect(driver, url, user, password)

  pattern <- paste("'", codePrefixMIM, "%'",
                 sep="")

  query <- paste("SELECT kod, prz_nazwa, numer_terminu, ocena_liczbowa, osoba FROM rstat_oceny ",
                 "WHERE ocena_liczbowa is not null ",
                 "AND kod LIKE ", pattern,
                 sep="")

  data <- dbGetQuery(connection, query)
  data
}
#write.csv(data, outputFile)

get_first_grade_for_courses <- function(grades_dataset) {
  grades_dataset %>%
    filter(!is.na(OCENA_LICZBOWA)) %>%
    filter(!is.na(NUMER_TERMINU)) %>%
    group_by(OSOBA, KOD) %>%
    mutate(pierwszy_termin = min(NUMER_TERMINU)) %>%
    filter(NUMER_TERMINU %in% pierwszy_termin) %>%
    mutate(max_ocena = max(OCENA_LICZBOWA)) %>%
    filter(OCENA_LICZBOWA == max_ocena) %>%
    distinct(OCENA_LICZBOWA) -> dataset
  dataset[c("PRZ_NAZWA", "OCENA_LICZBOWA", "OSOBA")]
}

get_filtered_dataset <- function(data) {
  processed_dataset <- data
  first_grade <- get_first_grade_for_courses(processed_dataset)
  first_grade
}

save_filtered_dataset <- function(data, file) {
  write.csv(data, file)
}

#Code to be executed
grades_file <- PROCESSED_GRADES_CSV_PATH
courses_file <- COURSES_LIST_PATH

get_courses_list() %>%
  save_courses_list(courses_file)

#get_data_from_DataBase() %>% 
#  get_filtered_dataset() %>%
#  save_filtered_dataset(grades_file)
