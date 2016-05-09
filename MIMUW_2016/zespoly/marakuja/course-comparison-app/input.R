library(sqldf)
library(ggplot2)

path <- "../report-course-comparison/oceny.csv"

usos_dataset <- function(){
  tbl_df(read.csv.sql(path,
                      "select * from file where OCENA_WARTOSC between 2 and 6"))
}

filter_subjects <- function(dataset, group){
  dataset %>%
    filter(PRZ_KOD %in% group) %>%
    group_by(OS_ID)
}

get_comparison_points <- function(dataset, group1, group2){
  grades1 <- filter_subjects(dataset, group1) %>%
    summarise(group1_mean = mean(OCENA_WARTOSC, na.rm=TRUE))
  grades2 <- filter_subjects(dataset, group2) %>%
    summarise(group2_mean = mean(OCENA_WARTOSC, na.rm=TRUE))
  results <- merge(grades1, grades2)
}

get_course_names <- function(dataset) {
  unique(dataset$PRZ_KOD)
}

usos_dump <- usos_dataset()