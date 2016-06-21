source("utils.R")

filter_passed <- function(data) {
  data %>%
    filter(OCENA_LICZBOWA > 2)
}

filter_failed <- function(data) {
  data %>%
    filter(OCENA_LICZBOWA == 2)
}

courses_summary <- function(dataA, dataB) {
  dataB %>%
    inner_join(dataA, by="OSOBA") %>%
    group_by(OCENA_LICZBOWA.x) %>%
    summarise(liczba_studentow=n()) %>%
    select(ocena_przedmiot_B = OCENA_LICZBOWA.x, liczba_studentow)
}

count_A_by_grade_B <- function(first_grades_for_courses, courseA, courseB, filterA) {
  dataA <- filterA(get_first_grade_for_course(first_grades_for_courses, courseA))
  dataB <- get_first_grade_for_course(first_grades_for_courses, courseB)
  courses_summary(dataA, dataB)
}

count_A_by_grade_B_all <- function(first_grades_for_courses, course) {
  course_data <- get_first_grade_for_course(first_grades_for_courses, course)
  course_data %>%
    group_by(OCENA_LICZBOWA) %>%
    summarise(liczba_studentow=n()) %>%
    select(ocena = OCENA_LICZBOWA, liczba_studentow)
}

count_A_by_grade_B_passed <- function(first_grades_for_courses, courseA, courseB) {
  count_A_by_grade_B(first_grades_for_courses, courseA, courseB, filter_passed)
}

count_A_by_grade_B_failed <- function(first_grades_for_courses, courseA, courseB) {
  count_A_by_grade_B(first_grades_for_courses, courseA, courseB, filter_failed)
}

emptydf <- data.frame(ocena_przedmiot_B = c(2.0, 3.0, 3.5, 4.0, 4.5, 5.0),
                      liczba_studentow = c(0, 0, 0, 0, 0, 0))
plot_percent_min_grade <- function(df, error) {
  colnames(df) = c("ocena_przedmiot_B", "liczba_studentow")
  df <- bind_rows(df, emptydf)
  df %>%
    group_by(ocena_przedmiot_B) %>%
    summarise(liczba_studentow = sum(liczba_studentow)) -> df_summarised
  arrange(df_summarised, -row_number()) -> df_summarised
  all_students <- sum(df_summarised$liczba_studentow)
  df_summarised$liczba_studentow = cumsum(df_summarised$liczba_studentow)
  df_summarised <- arrange(df_summarised, -row_number())
  if (error) {
    df_summarised = add_error(df_summarised)
  }
  if (all_students == 0) {
    all_students = 1
  }
  df_summarised$procent_studentow = df_summarised$liczba_studentow / all_students
  df_summarised$id = c(1:6)
  #print(df_summarised)
  df_summarised
}

add_error <- function(df) {
  all_students <- (df$liczba_studentow)[1]
  min_error <- numeric(6)
  max_error <- numeric(6)
  if(all_students != 0) {
    min_error[1] = 1
    max_error[1] = 1
  
    i <- 1
    for(students_with_mark in df$liczba_studentow) {
      if (i != 1) {
        error_range <- prop.test(students_with_mark, all_students)
        min_error[i] = error_range$conf.int[1]
        max_error[i] = error_range$conf.int[2]
      }
      i <- i + 1
    }
  }
  df$min_err = min_error
  df$max_err = max_error
  df
}

pointsTwoCourses <- function(first_grades_for_courses, course_a, course_b) {
  all <- count_A_by_grade_B_all(first_grades_for_courses, course_b)
  all_plot <- plot_percent_min_grade(all, TRUE)
  all_plot$warunek = "brak"
  
  plot_failed <- plot_percent_min_grade(count_A_by_grade_B_failed(first_grades_for_courses,
                                                                  course_a, course_b),
                                        TRUE)
  plot_failed$warunek = "nie zdał przedmiotu A"
  
  plot_passed <- plot_percent_min_grade(count_A_by_grade_B_passed(first_grades_for_courses,
                                                                  course_a, course_b),
                                        TRUE)
  plot_passed$warunek = "zdał przedmiot A"
  
  plot <- union(all_plot, union(plot_failed, plot_passed))
}

twoCoursesChart <- function(points_two_courses, course_a, course_b) {
  data <- points_two_courses
  data$procent_studentow = percent_from_fraction(data$procent_studentow)
  data$min_err = percent_from_fraction(data$min_err)
  data$max_err = percent_from_fraction(data$max_err)

  plot <- ggplot(data, aes(x = id, y = procent_studentow, color = warunek)) +
    geom_line(size = 2) + ylim(0,100) + 
    scale_x_continuous(breaks = c(1,2,3,4,5,6),labels = c("2", "3", "3.5", "4", "4.5", "5")) + 
    geom_errorbar(aes(ymax = max_err, ymin = min_err, width = 0.12)) +
    xlab(paste("ocena z przedmiotu", course_b)) +
    ylab("")
  
  plot
}

twoCoursesTable <- function(points_two_courses, course_a, course_b) {
  data <- points_two_courses
  data$procent_studentow = percent_from_fraction(data$procent_studentow)
  data$min_err = percent_from_fraction(data$min_err)
  data$max_err = percent_from_fraction(data$max_err)
  data <- select(data, ocena_przedmiot_B, warunek, liczba_studentow, procent_studentow)
  
  filter(data, warunek == "brak") %>% arrange(-ocena_przedmiot_B) -> data_no_condition
  filter(data, warunek == "nie zdał przedmiotu A") %>% arrange(-ocena_przedmiot_B) -> data_failed
  filter(data, warunek == "zdał przedmiot A") %>% arrange(-ocena_przedmiot_B) -> data_passed
  rownames(data_no_condition) = data_no_condition$ocena_przedmiot_B
  data_no_condition <- select(data_no_condition, procent_studentow)
  data_no_condition$procent_studentow = data_no_condition$procent_studentow
  data_no_condition$p2 = data_failed$procent_studentow
  data_no_condition$p3 = data_passed$procent_studentow
  
  colnames(data_no_condition) = c(
    paste('spośród wszystkich uczestników przedmiotu "', course_b, '"', sep=""),
    paste('spośród studentów, którzy nie zdali przedmiotu "', course_a, '"', sep=""),
    paste('spośród studentów, którzy zdali przedmiot "', course_a, '"', sep="")
  )
  
  data_no_condition
}

createSummary <- function(points_two_courses, course_a, course_b) {
  points_two_courses %>% filter(ocena_przedmiot_B == 2) -> data
  data %>% filter(warunek == "brak") -> data_no_condition
  data %>% filter(warunek == "nie zdał przedmiotu A") -> data_failed
  data %>% filter(warunek == "zdał przedmiot A") -> data_passed
  table <- matrix(c(data_no_condition$liczba_studentow, data_failed$liczba_studentow, data_passed$liczba_studentow), ncol = 3)
  colnames(table) = c(
    "wszystkich",
    paste('którzy nie zdali przedmiotu "', course_a, '"', sep=""),
    paste('którzy zdali przedmiot "', course_a, '"', sep="")
  )
  table
}
