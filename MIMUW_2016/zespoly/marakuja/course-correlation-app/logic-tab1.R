source("courses.R")
source("utils.R")

get_subjects_codes_mock <- function() {
  courses_vector
}

summarise_data <- function(first_grades_for_courses, courseB, min_common, filterA, min_grade_B, courseB_grades) {
  subjects <- get_subjects_codes_mock()
  subjects <- subjects[subjects != courseB]
  
  rate_data <- data.frame(subject=character(0), percent=numeric(0), students_min_grade=numeric(0),
                          all_students=numeric(0), stringsAsFactors=FALSE)

  grades_split <- split(first_grades_for_courses, as.factor(first_grades_for_courses$PRZ_NAZWA))
  
  for (grades_subject in grades_split) {
    subject <- as.character(grades_subject[[1]][1])
    if (!subject %in% subjects) {
        next
    }
    dataA <- filterA(grades_subject)
    joined <- courseB_grades %>% inner_join(dataA, by="OSOBA")
    all <- count(joined)
    filtered <- count(joined %>% filter(OCENA_LICZBOWA.x >= min_grade_B))
    if (filtered >= min_common) {
      percent <- round(filtered/all * 100, 2)
      rate_data <- rbind(rate_data, data.frame(subject=subject, percent=percent,
                                               students_min_grade=filtered, all_students=all))
    }
  }
  rate_data
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
