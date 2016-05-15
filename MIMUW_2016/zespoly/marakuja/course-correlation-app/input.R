library(dplyr)

source("config.R")

get_marks_dataset <- function() {
  table <- read.csv(MARKS_CSV_PATH)
  data <- tbl_df(table)
}

data <- get_marks_dataset()

#' Returns dataframe of subjects and their codes.
get_subjects_data <- function() {
  subjects <- data %>% distinct(KOD) %>% select(PRZ_NAZWA, KOD)
}

#' Returns vector of subjects' names retrieved from dataset.
get_subjects_names <- function() {
  subjects <- get_subjects_data() %>% select(PRZ_NAZWA)
  c(t(subjects))
}

#' Returns vector of subjects' names retrieved from dataset.
get_subjects_codes <- function() {
  subjects <- get_subjects_data() %>% select(KOD)
  c(t(subjects))
}

#' Returns list of subjects: codes with names
get_subjects_dictionary <- function() {
  subjects <- get_subjects_data()
  kody <- subjects %>% select(KOD)
  przedmioty <- subjects %>% select(PRZ_NAZWA)
  kody <- c(t(kody))
  przedmioty <- c(t(przedmioty))
  codes_dictionary <- as.list(kody)
  names(codes_dictionary) <- przedmioty
  codes_dictionary
}

map_subject_name_to_code <- function(name) {
  courses_mapping = get_subjects_dictionary()
  courses_mapping[[name]]
}


get_last_grade_for_course <- function(data, course) {
  data %>%
    filter(!is.na(OCENA_LICZBOWA)) %>%
    filter(!is.na(NUMER_TERMINU)) %>%
    filter(KOD %in% course) %>%
    group_by(OSOBA) %>%
    mutate(ostatni_termin = max(NUMER_TERMINU)) %>%
    filter(NUMER_TERMINU %in% ostatni_termin) %>%
    mutate(max_ocena = max(OCENA_LICZBOWA)) %>%
    filter(OCENA_LICZBOWA == max_ocena) %>%
    distinct(OCENA_LICZBOWA)
}

filter_passed <- function(data) {
  data %>%
    filter(OCENA_LICZBOWA > 2)
}

filter_failed <- function(data) {
  data %>%
    filter(OCENA_LICZBOWA == 2)
}

#' @return (``dataB`` mark, number of students from ``dataA`` who got this grade) dataframe
courses_summary <- function(dataA, dataB) {
  dataB %>%
    inner_join(dataA, by="OSOBA") %>%
    group_by(OCENA_LICZBOWA.x) %>%
    summarise(liczba_studentow=n()) %>%
    select(ocena_przedmiot_B = OCENA_LICZBOWA.x, liczba_studentow)
}

count_A_by_mark_B <- function(courseA, courseB, filterA) {
  dataA <- filterA(get_last_grade_for_course(data, courseA))
  dataB <- get_last_grade_for_course(data, courseB)
  courses_summary(dataA, dataB)
}

count_A_by_mark_B_all <- function(course) {
  all_data <- data
  course_data <- get_last_grade_for_course(all_data, course)
  course_data %>%
    group_by(OCENA_LICZBOWA) %>%
    summarise(liczba_studentow=n()) %>%
    select(ocena = OCENA_LICZBOWA, liczba_studentow)
}

count_A_by_mark_B_passed <- function(courseA, courseB) {
  count_A_by_mark_B(courseA, courseB, filter_passed)
}

count_A_by_mark_B_failed <- function(courseA, courseB) {
  count_A_by_mark_B(courseA, courseB, filter_failed)
}

count_A_by_mark_B_attending <- function(courseA, courseB) {
  count_A_by_mark_B(courseA, courseB, identity)
}

count_A_by_mark_B_not_attending <- function(courseA, courseB) {
  all <- count_A_by_mark_B_all(courseB)
  attending <- count_A_by_mark_B_attending(courseA, courseB)
  all %>%
    full_join(attending, by=c("ocena" = "ocena_przedmiot_B")) %>%
    mutate(liczba_studentow.x = replace(liczba_studentow.x, is.na(liczba_studentow.x), 0)) %>%
    mutate(liczba_studentow.y = replace(liczba_studentow.y, is.na(liczba_studentow.y), 0)) %>%
    mutate(liczba_studentow = liczba_studentow.x - liczba_studentow.y) %>%
    select(ocena, liczba_studentow)
}

emptydf <- data.frame(ocena_przedmiot_B = c(2.0, 3.0, 3.5, 4.0, 4.5, 5.0), liczba_studentow = c(0, 0, 0, 0, 0, 0))
df_for_plot <- function(df) {
  colnames(df) = c("ocena_przedmiot_B", "liczba_studentow")
  df <- bind_rows(df, emptydf)
  df %>%
    group_by(ocena_przedmiot_B) %>%
    summarise(liczba_studentow = sum(liczba_studentow)) %>%
    arrange(-row_number()) -> df_new
  maks <- sum(df_new[2])
  df_new[2] = cumsum(df_new[2]) / maks
  arrange(df_new, -row_number())
}

data_for_plot <- function(failed, passed, not_attending) {
  failed_plot <- df_for_plot(failed)
  failed_plot$typ = "Nie zdał"

  passed_plot <- df_for_plot(passed)
  passed_plot$typ = "Zdał"

  not_attending_plot <- df_for_plot(not_attending)
  not_attending_plot$typ = "Nie uczestniczył"

  union(union(failed_plot, passed_plot), not_attending_plot)
}
