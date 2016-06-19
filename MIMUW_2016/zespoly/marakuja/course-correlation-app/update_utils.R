source ("utils.R")
source ("input.R")
library(dplyr)
library(RJDBC)

get_courses_list <- function() {
  courses_vector = system2(
    command = "python",
    args = "listcourses/listcourses.py",
    stdout = TRUE
  )
  df <- data.frame(courses_vector)
  df
}

save_courses_list <- function(data, file) {
  write.csv(data, file, row.names = FALSE)
}

get_driver <- function(driverClass, classPath) {
  driver <- JDBC(driverClass=driverClass,
                 classPath=classPath,
                 identifier.quote=" ")
  driver
}

get_url <- function(host, port, service_name) {
  url <- paste("jdbc:oracle:thin:@//", host, ":", port, "/", service_name,
               sep="")
  url
}

create_connection <- function(driver, url, user, password) {
  connection <- dbConnect(driver, url, user, password)
  connection
}

create_query <- function(code_prefix_MIM) {
  pattern <- paste("'", code_prefix_MIM, "%'",
                   sep="")
  
  query <- paste("SELECT kod, prz_nazwa, numer_terminu, ocena_liczbowa, osoba FROM rstat_oceny ",
                 "WHERE ocena_liczbowa is not null ",
                 "AND kod LIKE ", pattern,
                 sep="")
  query
}

get_data_from_database <- function(connection, query) {
  data <- dbGetQuery(connection, query)
  data
}

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
