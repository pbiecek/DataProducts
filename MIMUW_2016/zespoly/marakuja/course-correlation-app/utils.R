source("input.R")

get_first_grade_for_courses <- function() {
  data %>%
    filter(!is.na(OCENA_LICZBOWA)) %>%
    filter(!is.na(NUMER_TERMINU)) %>%
    group_by(OSOBA, KOD) %>%
    mutate(pierwszy_termin = min(NUMER_TERMINU)) %>%
    filter(NUMER_TERMINU %in% pierwszy_termin) %>%
    mutate(max_ocena = max(OCENA_LICZBOWA)) %>%
    filter(OCENA_LICZBOWA == max_ocena) %>%
    distinct(OCENA_LICZBOWA)
}

first_grades_for_courses <- get_first_grade_for_courses()

get_first_grade_for_course <- function(data, course) {
  first_grades_for_courses %>% ungroup() %>%
    filter(KOD %in% course)
}

percent_grade <- function(course, min_grade) {
  grades <- get_first_grade_for_course(data, course)
  all <- count(grades)
  filtered <- count(grades %>% filter(OCENA_LICZBOWA >= min_grade))
  round(filtered / all * 100, 2)
}
