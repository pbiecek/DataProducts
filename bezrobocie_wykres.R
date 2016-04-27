# XROK - dane z danego roku
# x - dane dla konkretnej gminy

library(ggplot2)

XROK <- subset(bezrobocie, Rok == 2010)
x <- subset(XROK, Kod == 240503)

base <- ggplot(XROK, aes(Wartosc)) +
  geom_area(aes(y = ..count..), stat = "bin", binwidth = 5, colour = "lightblue", fill = "lightblue")

pl <- base +
  geom_vline(xintercept = x$Wartosc, linetype = "dashed", color = "#D55E00", size = 0.6) +
  labs(x = "Liczba bezrobotnych na 1000 osób w wieku produkcyjnym", y = "Liczba gmin") +
  geom_text(aes(x$Wartosc, mean(range(ggplot_build(base)$data[[1]]$count)), label = x$Wartosc), colour = "black", angle = 90, vjust = 1, nudge_x = 1)
