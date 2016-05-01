source("input.R")

courseA = '1000-214bJAO'
courseB = '1000-214bSIK'

count_A_by_mark_B_all(courseB)
count_A_by_mark_B_failed(courseA, courseB)
passed <- count_A_by_mark_B_passed(courseA, courseB)
count_A_by_mark_B_attending(courseA, courseB)
count_A_by_mark_B_not_attending(courseA, courseB)

ggplot(df_for_plot(passed), aes(x = ocena_przedmiot_B, y = liczba_studentow)) + geom_step()
