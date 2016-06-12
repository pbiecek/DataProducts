source("utils.R")
library(stringr)

formatPlot <- function(dataFunc) {
  ggplot(dataFunc(), aes(x = ocena_przedmiot_B, y = liczba_studentow, color = warunek)) +
    geom_line(size = 2) + ylim(0,1) +
    geom_errorbar(aes(ymax = max_err, ymin = min_err, width = 0.12)) +
    xlab("ocena z wybranego przedmiotu")
}

barPercentPlot <- function(data, direction, course, min_grade) {
  bar_num <- min(3, count(data)[[1]]/2)
  data %>% head(bar_num) %>% mutate(type=direction) -> highest
  data %>% tail(bar_num) %>% mutate(type=-direction) -> lowest
  chosen <- rbind(highest, lowest)
  line_value <- percent_grade(course, min_grade)
  ggplot(chosen, aes(x = reorder(`Przedmiot A`, direction * `Procent studentów, którzy uzyskali co najmniej wybraną ocenę`),
                     y = `Procent studentów, którzy uzyskali co najmniej wybraną ocenę`,
                     fill=factor(type))) +
    geom_bar(stat = "identity", width=.5) +
    geom_text(aes(label=`Procent studentów, którzy uzyskali co najmniej wybraną ocenę`), position=position_dodge(width=0.9), vjust=-0.25) +
    xlab('Przedmiot A') +
    ylab('') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_fill_manual(values = c("deepskyblue2", "hotpink2"),
                      name=paste("Czołówka przedmiotów z procentem osób \npowyżej oceny X"),
                      labels=c("Najwyższym", "Najniższym")) +
    geom_hline(aes(yintercept=line_value, colour=Threshold),
               linetype="dashed",
               data.frame(y=line_value,
                          Threshold = "Procent studentów uczestniczących w przedmiocie B,\nktórzy otrzymali co najmniej wybraną ocenę")) +
    scale_y_continuous(breaks = sort(c(seq(from = 0,
                                           to = max(chosen$`Procent studentów, którzy uzyskali co najmniej wybraną ocenę`, line_value[[1]]),
                                           by=10),
                                       line_value[[1]]))) +
    scale_colour_manual(values = c(`Procent studentów uczestniczących w przedmiocie B,\nktórzy otrzymali co najmniej wybraną ocenę` = "black")) +
    guides(colour=guide_legend(title=NULL))
}