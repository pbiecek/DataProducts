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

count_A_by_mark_B <- function(first_grades_for_courses, courseA, courseB, filterA) {
  dataA <- filterA(get_first_grade_for_course(first_grades_for_courses, courseA))
  dataB <- get_first_grade_for_course(first_grades_for_courses, courseB)
  courses_summary(dataA, dataB)
}

count_A_by_mark_B_all <- function(first_grades_for_courses, course) {
  course_data <- get_first_grade_for_course(first_grades_for_courses, course)
  course_data %>%
    group_by(OCENA_LICZBOWA) %>%
    summarise(liczba_studentow=n()) %>%
    select(ocena = OCENA_LICZBOWA, liczba_studentow)
}

count_A_by_mark_B_passed <- function(first_grades_for_courses, courseA, courseB) {
  count_A_by_mark_B(first_grades_for_courses, courseA, courseB, filter_passed)
}

count_A_by_mark_B_failed <- function(first_grades_for_courses, courseA, courseB) {
  count_A_by_mark_B(first_grades_for_courses, courseA, courseB, filter_failed)
}

emptydf <- data.frame(ocena_przedmiot_B = c(2.0, 3.0, 3.5, 4.0, 4.5, 5.0), liczba_studentow = c(0, 0, 0, 0, 0, 0))
df_for_plot <- function(df, error) {
  colnames(df) = c("ocena_przedmiot_B", "liczba_studentow")
  df <- bind_rows(df, emptydf)
  df %>%
    group_by(ocena_przedmiot_B) %>%
    summarise(liczba_studentow = sum(liczba_studentow)) -> df_new
  x = df_new$liczba_studentow  
  arrange(df_new, -row_number()) -> df_new
  maks <- sum(df_new$liczba_studentow)
  df_new$liczba_studentow = cumsum(df_new$liczba_studentow)
  df_new <- arrange(df_new, -row_number())
  if (error) {
    df_new = add_error(df_new)
  }
  if (maks == 0) {
    maks = 1
  }
  y = df_new$liczba_studentow
  df_new$liczba_studentow = df_new$liczba_studentow / maks
  df_new$liczba = x
  df_new$conajmniej = y
  df_new$id = c(1:6)
  df_new
}

add_error <- function(df) {
  x <- df$liczba_studentow
  maks <- x[1]
  wyn1 <- x
  wyn2 <- x
  i <- 1
  for(a in x) {
    if (maks != 0)
    {
      b <- prop.test(a, maks)
      wyn1[i] = b$conf.int[1]
      wyn2[i] = b$conf.int[2]
    } else
    {
      wyn1[i] = 0
      wyn2[i] = 0
    }
    i <- i + 1
  }
  df$min_err = wyn1
  df$max_err = wyn2
  df[1,3] = 1
  if (maks == 0)
  {
    df[1,3] = 0
  }
  df
}

pointsTwoCourses <- function(first_grades_for_courses, course_a, course_b) {
  all <- count_A_by_mark_B_all(first_grades_for_courses, course_b)
  all_plot <- df_for_plot(all, TRUE)
  all_plot$warunek = "brak"
  
  plot_failed <- df_for_plot(count_A_by_mark_B_failed(first_grades_for_courses, course_a, course_b), TRUE)
  plot_failed$warunek = "nie zdał przedmiotu A"
  
  plot_passed <- df_for_plot(count_A_by_mark_B_passed(first_grades_for_courses, course_a, course_b), TRUE)
  plot_passed$warunek = "zdał przedmiot A"
  
  plot <- union(all_plot, union(plot_failed, plot_passed))
}

twoCoursesChart <- function(first_grades_for_courses, course_a, course_b) {
  data <- pointsTwoCourses(first_grades_for_courses, course_a, course_b)
  data$liczba_studentow = round(100 * data$liczba_studentow, 2)
  data$min_err = round(100 * data$min_err, 2)
  data$max_err = round(100 * data$max_err, 2)

  plot <- ggplot(data, aes(x = id, y = liczba_studentow, color = warunek)) +
    geom_line(size = 2) + ylim(0,100) + 
    scale_x_continuous(breaks = c(1,2,3,4,5,6),labels = c("2", "3", "3.5", "4", "4.5", "5")) + 
    geom_errorbar(aes(ymax = max_err, ymin = min_err, width = 0.12)) +
    xlab(paste("ocena z przedmiotu", course_b)) +
    ylab("")
  
  plot
}

twoCoursesTable <- function(first_grades_for_courses, course_a, course_b) {
  data <- pointsTwoCourses(first_grades_for_courses, course_a, course_b)
  data$liczba_studentow = round(data$liczba_studentow * 100, 2)
  data$min_err = round(data$min_err * 100, 2)
  data$max_err = round(data$max_err * 100, 2)
  data <- select(data, ocena_przedmiot_B, warunek, conajmniej, liczba_studentow)
  
  filter(data, warunek == "brak") %>% arrange(-ocena_przedmiot_B) -> data1
  filter(data, warunek == "nie zdał przedmiotu A") %>% arrange(-ocena_przedmiot_B) -> data2
  filter(data, warunek == "zdał przedmiot A") %>% arrange(-ocena_przedmiot_B) -> data3
  rownames(data1) = data1$ocena_przedmiot_B
  data1 <- select(data1, liczba_studentow)
  data1$liczba_studentow = data1$liczba_studentow
  data1$p2 = data2$liczba_studentow
  data1$p3 = data3$liczba_studentow
  
  colnames(data1) = c(
    paste('spośród wszystkich uczestników przedmiotu "', course_b, '"', sep=""),
    paste('spośród studentów, którzy nie zdali przedmiotu "', course_a, '"', sep=""),
    paste('spośród studentów, którzy zdali przedmiot "', course_a, '"', sep="")
  )
  
  data1
}

createSummary <- function(first_grades_for_courses, course_a, course_b) {
  pointsTwoCourses(first_grades_for_courses, course_a, course_b) %>%
    filter(ocena_przedmiot_B == 2) -> data
  data %>% filter(warunek == "brak") -> data1
  data %>% filter(warunek == "nie zdał przedmiotu A") -> data2
  data %>% filter(warunek == "zdał przedmiot A") -> data3
  table <- matrix(c(data1$conajmniej, data2$conajmniej, data3$conajmniej), ncol = 3)
  colnames(table) = c(
    "wszystkich",
    paste('którzy nie zdali przedmiotu "', course_a, '"', sep=""),
    paste('którzy zdali przedmiot "', course_a, '"', sep="")
  )
  table
}
