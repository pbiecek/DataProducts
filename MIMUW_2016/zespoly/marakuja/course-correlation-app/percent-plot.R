source("utils.R")
library(stringr)

process_dataset <- function(data, direction) {
  bar_num <- min(3, count(data)[[1]]/2)
  data %>% head(bar_num) %>% mutate(type=direction) -> highest
  data %>% tail(bar_num) %>% mutate(type=-direction) -> lowest
  rbind(highest, lowest)
}

barPercentPlot <- function(first_grades_for_courses, data, direction, course, min_grade) {
  dataset <- process_dataset(data, direction)
  percent_no_condition <- percent_of_students_got_min_grade(first_grades_for_courses, course, min_grade)
  ggplot(dataset, aes(x = reorder(`Przedmiot A`, direction * `Procent studentów, którzy uzyskali co najmniej wybraną ocenę`),
                     y = `Procent studentów, którzy uzyskali co najmniej wybraną ocenę`,
                     fill=factor(type))) +
    geom_bar(stat = "identity", width=.5) +
    geom_text(aes(label=`Procent studentów, którzy uzyskali co najmniej wybraną ocenę`),
              position=position_dodge(width=0.9), vjust=-0.25) +
    xlab('Przedmiot A') +
    ylab('') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_fill_manual(values = c("deepskyblue2", "hotpink2"),
                      name=paste("Czołówka przedmiotów z procentem osób \npowyżej oceny X"),
                      labels=c("Najwyższym", "Najniższym")) +
    geom_hline(aes(yintercept=percent_no_condition, colour=Threshold), linetype="dashed",
      data.frame(y=percent_no_condition,
                 Threshold = "Procent studentów uczestniczących w przedmiocie B,\nktórzy otrzymali co najmniej wybraną ocenę")) +
    scale_y_continuous(breaks = sort(c(seq(from = 0,
                                           to = max(dataset$`Procent studentów, którzy uzyskali co najmniej wybraną ocenę`,
                                                    percent_no_condition[[1]]),
                                           by=10),
                                       percent_no_condition[[1]]))) +
    scale_colour_manual(
      values = c(`Procent studentów uczestniczących w przedmiocie B,\nktórzy otrzymali co najmniej wybraną ocenę` = "black")) +
    guides(colour=guide_legend(title=NULL))
}
