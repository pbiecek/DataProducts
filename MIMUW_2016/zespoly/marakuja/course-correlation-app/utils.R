get_first_grade_for_course <- function(first_grades_for_courses, course) {
  first_grades_for_courses %>% ungroup() %>%
    filter(PRZ_NAZWA %in% course)
}

percent_grade <- function(first_grades_for_courses, course, min_grade) {
  grades <- get_first_grade_for_course(first_grades_for_courses, course)
  all <- count(grades)
  filtered <- count(grades %>% filter(OCENA_LICZBOWA >= min_grade))
  round(filtered / all * 100, 2)
}
