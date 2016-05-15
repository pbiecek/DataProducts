source("input.R")

PLOTS_NUMBER = 1

get_subjects_codes_mock <- function() {
  c("1000-214bWWW", "1000-224bJNP2", "1000-214bJAO", "1000-213bASD")
}

unify_grades_df <- function(df, join_by) {
  emptydf <- data.frame(ocena = c(2.0, 3.0, 3.5, 4.0, 4.5, 5.0))
  df <- emptydf %>% left_join(df, by=c("ocena" = join_by))
  df[is.na(df)] <- 0
  df
}

correlation_mock <- function(fun, courseA, courseB) {
  courseB_grades <- unify_grades_df(count_A_by_mark_B_all(courseB), "ocena")
  courseA_grades <- unify_grades_df(fun(courseA, courseB), "ocena_przedmiot_B")
  cor(courseB_grades$liczba_studentow, courseA_grades$liczba_studentow)
}

correlation_mock_passed <- function(subject, courseB) {
  correlation_mock(count_A_by_mark_B_passed, subject, courseB)
}

correlation_mock_failed <- function(subject, courseB) {
  correlation_mock(count_A_by_mark_B_failed, subject, courseB)
}

sort_courses_by_correlation <- function(fun, courseB) {
  subjects <- tbl_df(data.frame(sub = get_subjects_codes_mock())) %>%
    rowwise() %>% mutate(corr = fun(sub, courseB)) %>%
    arrange(desc(corr)) %>% head(PLOTS_NUMBER) %>% select(sub)
}

sort_courses_passed_by_correlation <- function(courseB) {
  sort_courses_by_correlation(correlation_mock_passed, courseB)
}

sort_courses_failed_by_correlation <- function(courseB) {
  sort_courses_by_correlation(correlation_mock_failed, courseB)
}