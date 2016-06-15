percent_from_fraction <- function(fraction) {
  round(fraction * 100, 2)
}

get_first_grade_for_course <- function(first_grades_for_courses, course) {
  first_grades_for_courses %>% ungroup() %>%
    filter(PRZ_NAZWA %in% course)
}

percent_of_students_got_min_grade <- function(first_grades_for_courses, course, min_grade) {
  grades <- get_first_grade_for_course(first_grades_for_courses, course)
  all <- count(grades)
  filtered <- count(grades %>% filter(OCENA_LICZBOWA >= min_grade))
  percent_from_fraction(filtered / all)
}
