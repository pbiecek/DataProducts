source("input.R")
source("courses.R")

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
  y = df_new$liczba_studentow
  df_new$liczba_studentow = df_new$liczba_studentow / maks
  df_new$liczba = x
  df_new$conajmniej = y
  df_new
}

add_error <- function(df) {
  x <- df$liczba_studentow
  maks <- x[1]
  wyn1 <- x
  wyn2 <- x
  i <- 1
  for(a in x) {
    b <- prop.test(a, maks)
    wyn1[i] = b$conf.int[1]
    wyn2[i] = b$conf.int[2]
    i <- i + 1
  }
  df$min_err = wyn1
  df$max_err = wyn2
  df[1,3] = 1
  df
}

get_subjects_codes_mock <- function() {
  courses_vector
}

courses_summary_joined <- function(data) {
  data %>%
    group_by(OCENA_LICZBOWA.x) %>%
    summarise(liczba_studentow=n()) %>%
    select(ocena_przedmiot_B = OCENA_LICZBOWA.x, liczba_studentow)}

compute_rate <- function(data) {
  summary <- courses_summary_joined(data)
  series_a <- df_for_plot(summary, FALSE)$liczba_studentow
  sum(series_a)
}

summarise_data <- function(courseB, min_common, filterA, min_grade_B) {
  subjects <- get_subjects_codes_mock()
  subjects <- subjects[subjects != courseB]
  
  rate_data <- data.frame(subject=character(0), percent=numeric(0), students_min_grade=numeric(0),
                          all_students=numeric(0), rate=numeric(0), stringsAsFactors=FALSE)
  
  for (subject in subjects) {
    dataA <- filterA(get_last_grade_for_course(data, subject))
    dataB <- get_last_grade_for_course(data, courseB)
    joined <- dataB %>% inner_join(dataA, by="OSOBA")
    all <- count(joined)
    filtered <- count(joined %>% filter(OCENA_LICZBOWA.x >= min_grade_B))
    rate <- compute_rate(joined)
    if (all >= min_common) {
      rate_data <- rbind(rate_data, data.frame(subject=subject, percent=filtered/all,
                                               students_min_grade=filtered, all_students=all, rate=rate))
    }
  }
  rate_data
}

sort_courses_passed <- function(courseB, min_common, min_grade_B) {
  data <- summarise_data(courseB, min_common, filter_passed, min_grade_B)
  names(data) <- c("Przedmiot A", "Procent studentów, którzy uzyskali co najmniej wybraną ocenę", "Studenci, którzy zdali A, a z B uzyskali co najmniej wybraną ocenę", "Studenci, którzy zdali A", "Wskaźnik")
  data %>% arrange(desc(`Procent studentów, którzy uzyskali co najmniej wybraną ocenę`))
}

sort_courses_failed <- function(courseB, min_common, min_grade_B) {
  data <- summarise_data(courseB, min_common, filter_failed, min_grade_B)
  names(data) <- c("Przedmiot A", "Procent studentów, którzy uzyskali co najmniej wybraną ocenę", "Studenci, którzy nie zdali A, a z B uzyskali co najmniej wybraną ocenę", "Studenci, którzy nie zdali A", "Wskaźnik")
  data %>% arrange(desc(`Procent studentów, którzy uzyskali co najmniej wybraną ocenę`))
}

plot_for_data <- function(input_course, computed_courses, row_func, p_or_f) {
  all <- count_A_by_mark_B_all(input_course)

  subselected_courses = union(head(computed_courses, 1), tail(computed_courses, 1))

  all_plot <- df_for_plot(all, TRUE)
  all_plot$warunek = "brak"

  plot <- all_plot
  for (course_a in subselected_courses) {
      chosen_plot <- df_for_plot(row_func(course_a, input_course), TRUE)
      chosen_plot$warunek = paste(p_or_f, course_a)

      plot <- union(plot, chosen_plot)
  }

  plot
}

pointsTwoCourses <- function(course_a, course_b) {
  all <- count_A_by_mark_B_all(course_b)
  all_plot <- df_for_plot(all, TRUE)
  all_plot$warunek = "brak"

  plot_failed <- df_for_plot(count_A_by_mark_B_failed(course_a, course_b), TRUE)
  plot_failed$warunek = "nie zdał przedmiotu A"

  plot_passed <- df_for_plot(count_A_by_mark_B_passed(course_a, course_b), TRUE)
  plot_passed$warunek = "zdał przedmiot A"

  # plot_passed <- df_for_plot(count_A_by_mark_B_not_attending(course_a, course_b))
  # plot_failed$warunek = "zdał przedmiot A"
  plot <- union(all_plot, union(plot_failed, plot_passed))
}

twoCoursesChart <- function(course_a, course_b) {
  data <- pointsTwoCourses(course_a, course_b)

  plot <- ggplot(data, aes(x = ocena_przedmiot_B, y = liczba_studentow, color = warunek)) +
    geom_line(size = 2) + ylim(0,1) +
    geom_errorbar(aes(ymax = max_err, ymin = min_err, width = 0.12)) +
    ylab("p-stwo uzyskania przynajmniej podanej oceny") +
    xlab("ocena z przedmiotu B")

  plot
}

twoCoursesTable <- function(course_a, course_b) {
  data <- pointsTwoCourses(course_a, course_b)
  data$liczba_studentow = round(data$liczba_studentow * 100, 2)
  data$min_err = round(data$min_err * 100, 2)
  data$max_err = round(data$max_err * 100, 2)
  data <- select(data, ocena_przedmiot_B, warunek, conajmniej, liczba_studentow)

  filter(data, warunek == "brak") %>% arrange(-ocena_przedmiot_B) -> data1
  filter(data, warunek == "nie zdał przedmiotu A") %>% arrange(-ocena_przedmiot_B) -> data2
  filter(data, warunek == "zdał przedmiot A") %>% arrange(-ocena_przedmiot_B) -> data3
  rownames(data1) = data1$ocena_przedmiot_B
  data1 <- select(data1, liczba_studentow)
  data1$liczba_studentow = paste(data1$liczba_studentow, "%")
  #data1$ls2 = data2$conajmniej
  data1$p2 = paste(data2$liczba_studentow, "%")
  #data1$ls3 = data3$conajmniej
  data1$p3 = paste(data3$liczba_studentow, "%")
  
  colnames(data1) = c(#"Ocena nie mniejsza niż",
                     #"Liczba studentów (wszyscy)",
                     "Prawdopodobieństwo",
                     #"Liczba studentow (nie zdali A)",
                     "Prawdopodobieństwo jeżeli nie zdałeś przedmiotu A",
                     #"Liczba studentow (zdali A)",
                     "Prawdopodobieństwo jeżeli zdałeś przedmiotu A)")

  data1
}
