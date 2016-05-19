source("input.R")

PLOTS_NUMBER = 1

get_subjects_codes_mock <- function() {
  c("1000-214bWWW", "1000-224bJNP2", "1000-214bJAO", "1000-213bASD")
}

correlation_mock <- function(fun, courseA, courseB) {
  dist_a <- fun(courseA, courseB)
  series_a <- df_for_plot(dist_a)$liczba_studentow

  sum(series_a)
}

correlation_mock_passed <- function(subject, courseB) {
  correlation_mock(count_A_by_mark_B_passed, subject, courseB)
}

correlation_mock_failed <- function(subject, courseB) {
  -correlation_mock(count_A_by_mark_B_failed, subject, courseB)
}

sort_courses_by_correlation <- function(fun, courseB) {
  subjects <- tbl_df(data.frame(sub = get_subjects_codes_mock())) %>%
    subset(sub != courseB) %>%
    rowwise() %>% mutate(corr = fun(sub, courseB)) %>%
    arrange(desc(corr)) %>% head(PLOTS_NUMBER) %>% select(sub)
}

sort_courses_passed_by_correlation <- function(courseB) {
  sort_courses_by_correlation(correlation_mock_passed, courseB)
}

sort_courses_failed_by_correlation <- function(courseB) {
  sort_courses_by_correlation(correlation_mock_failed, courseB)
}