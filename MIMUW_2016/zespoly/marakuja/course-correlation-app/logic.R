source("input.R")

PLOTS_NUMBER = 1

get_subjects_codes_mock <- function() {
  c("1000-214bWWW", "1000-224bJNP2", "1000-214bJAO", "1000-213bASD")
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
  
  rate_data <- data.frame(subject=character(0), rate=numeric(0), stringsAsFactors=FALSE)
  
  for (subject in subjects) {
    dataA <- filterA(get_last_grade_for_course(data, subject))
    dataB <- get_last_grade_for_course(data, courseB)
    joined <- dataB %>% inner_join(dataA, by="OSOBA")
    if (count(joined) >= min_common) {
      rate <- compute_rate(joined)
      rate_data <- rbind(rate_data, c(subject, rate))
    }
  }
  names(rate_data) <- c("subject", "rate")
  rate_data
}

sort_courses_passed <- function(courseB, min_common) {
  data <- sort_courses(courseB, min_common, filter_passed)
  data %>% arrange(desc(rate)) %>% head(PLOTS_NUMBER) %>% select(subject)
}

sort_courses_failed <- function(courseB, min_common) {
  data <- sort_courses(courseB, min_common, filter_failed)
  data %>% arrange(rate) %>% head(PLOTS_NUMBER) %>% select(subject)
}