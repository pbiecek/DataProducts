library(ggplot2)
library(RJDBC)

source("config.R")

get_connection <- function() {
    driver <- JDBC("oracle.jdbc.OracleDriver", classPath=classPath, " ")
    url <- paste("jdbc:oracle:thin:@//", host, ":", port, "/", service_name, sep="")
    dbConnect(driver, url, user, password)
}

vector_to_string <- function(vector) {
    paste(shQuote(vector, type = "sh"), collapse = ',')
}

avg_grades_group_statement <- function(group) {
    group_string <- vector_to_string(group)
    paste("select osoba, avg(ocena_liczbowa) srednia from rstat_oceny
        where kod in (", eval(group_string), ")
        having count(distinct kod) = ", length(group), " group by osoba"
    )
}

get_comparison_points <- function(connection, group1, group2){
    avg_grades_groups_stmt <- paste(
        "select g1.osoba, g1.srednia as avg_group_1, g2.srednia as avg_group_2
        from (", avg_grades_group_statement(group1), ") g1
        inner join (", avg_grades_group_statement(group2), ") g2
        on g1.osoba = g2.osoba"
    )
    dbGetQuery(connection, avg_grades_groups_stmt)
}

get_course_names <- function(connection) {
    select_courses_statement <- "select distinct kod from rstat_oceny"
    result_set <- dbGetQuery(connection, select_courses_statement)
    c(t(result_set))
}

connection <- get_connection()
