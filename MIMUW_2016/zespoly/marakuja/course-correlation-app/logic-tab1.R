source("courses.R")
source("utils.R")

summarise_data <- function(first_grades_for_courses, courseB, min_common, filterA, min_grade_B, courseB_grades) {
  courses <- courses_vector
  courses <- courses[courses != courseB]
  
  result_data <- data.frame(course=character(0), percent=numeric(0), students_min_grade=numeric(0),
                          all_students=numeric(0), stringsAsFactors=FALSE)

  grades_split <- split(first_grades_for_courses, as.factor(first_grades_for_courses$PRZ_NAZWA))
  
  for (grades_course in grades_split) {
    course <- as.character(grades_course[["PRZ_NAZWA"]][1])
    if (!course %in% courses) {
        next
    }
    dataA <- filterA(grades_course)
    joined <- courseB_grades %>% inner_join(dataA, by="OSOBA")
    all <- count(joined)
    filtered <- count(joined %>% filter(OCENA_LICZBOWA.x >= min_grade_B))
    if (filtered >= min_common) {
      percent <- percent_from_fraction(filtered / all)
      result_data <- rbind(result_data, data.frame(course=course, percent=percent,
                                               students_min_grade=filtered, all_students=all))
    }
  }
  result_data
}

sort_courses_passed <- function(first_grades_for_courses, courseB, min_common, min_grade_B, courseB_grades) {
  data <- summarise_data(first_grades_for_courses, courseB, min_common, filter_passed, min_grade_B, courseB_grades)
  names(data) <- c("Przedmiot A",
                   "Procent studentów, którzy uzyskali co najmniej wybraną ocenę",
                   "Liczba studentów, którzy zdali A, a z B uzyskali co najmniej wybraną ocenę",
                   "Liczba studentów, którzy zdali A")
  data %>% arrange(desc(`Procent studentów, którzy uzyskali co najmniej wybraną ocenę`))
}

sort_courses_failed <- function(first_grades_for_courses, courseB, min_common, min_grade_B, courseB_grades) {
  data <- summarise_data(first_grades_for_courses, courseB, min_common, filter_failed, min_grade_B, courseB_grades)
  names(data) <- c("Przedmiot A",
                   "Procent studentów, którzy uzyskali co najmniej wybraną ocenę",
                   "Liczba studentów, którzy nie zdali A, a z B uzyskali co najmniej wybraną ocenę",
                   "Liczba studentów, którzy nie zdali A")
  data %>% arrange(`Procent studentów, którzy uzyskali co najmniej wybraną ocenę`)
}
