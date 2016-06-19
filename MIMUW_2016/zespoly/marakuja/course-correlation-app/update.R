#!/usr/bin/env Rscript
#' Skrypt służący do pobierania danych o ocenach z USOSa.
#'
#' Wymagania:
#'    install.packages("JDBC")
#'
#' Użycie:
#'    Ustalić wartości poniższych parametrów.
#'    Rscript update.R

source("update_utils.R")
source("config.R")

get_courses_list() %>%
  save_courses_list(COURSES_LIST_PATH)

url <- get_url(HOST, PORT, SERVICE_NAME)
driver <- get_driver(DRIVER_CLASS, DRIVER_CLASS_PATH)
connection <- create_connection(driver, url, USER, PASSWORD)
query <- create_query(CODE_PREFIX)

get_data_from_database(connection, query) %>% 
  get_filtered_dataset() %>%
  save_filtered_dataset(PROCESSED_GRADES_CSV_PATH)
