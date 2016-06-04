source("input.R")
source("courses.R")

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
  series_a <- df_for_plot(summary)$liczba_studentow
  sum(series_a)
}

sort_courses <- function(courseB, min_common, filterA) {
  subjects <- get_subjects_codes_mock()
  subjects <- subjects[subjects != courseB]
  
  rate_data <- data.frame(subject=character(0), rate=numeric(0), student_count=numeric(0), stringsAsFactors=FALSE)
  
  for (subject in subjects) {
    dataA <- filterA(get_last_grade_for_course(data, subject))
    dataB <- get_last_grade_for_course(data, courseB)
    joined <- dataB %>% inner_join(dataA, by="OSOBA")
    common <- count(joined)
    if (common >= min_common) {
      rate <- compute_rate(joined)
      rate_data <- rbind(rate_data, data.frame(subject=subject, rate=rate, student_count=common))
    }
  }
  names(rate_data) <- c("subject", "rate", "student count")
  rate_data
}

sort_courses_passed <- function(courseB, min_common) {
  data <- sort_courses(courseB, min_common, filter_passed)
  data %>% arrange(desc(rate))
}

sort_courses_failed <- function(courseB, min_common) {
  data <- sort_courses(courseB, min_common, filter_failed)
  data %>% arrange(rate)
}
