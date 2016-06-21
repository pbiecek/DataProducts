library(shiny)

# Sciezki do danych dotyczacych ankiet, ktore uzytkownik powinien podmienic na swoje wlasne
ankiety_path <- "/home/marcin/jnp2/proj/data/ankiety_clean.txt"
#ankiety_path <- "/home/magda/Pulpit/JNP II/prez3/server/ankiety_clean.txt"
#ankiety_path <- "~/Desktop/UW/JNP2/ankiety_clean.txt"

# Sciezki do danych dotyczacych ocen, ktore uzytkownik powinien podmienic na swoje wlasne
oceny_path <- "/home/marcin/jnp2/proj/data/oceny.csv"
#oceny_path <- "/home/magda/Pulpit/JNP II/prez3/server/oceny.csv"
#oceny_path <- "~/Desktop/UW/JNP2/oceny.csv"

oceny_data    <- read.csv(oceny_path , header=TRUE)
ankiety_data  <- read.csv(ankiety_path, header=TRUE, sep=";")

cycle     <- levels(oceny_data$CYKL_DYD)
sub_names <- levels(oceny_data$PRZ_NAZWA)
questions <- levels(ankiety_data$TRESC_PYTANIA)

# Glowny Render aplikacji Shiny
shinyUI(fluidPage(
  tags$head(tags$style(HTML(".well {
                            background-color: #dd9999!important;
                            width: 200px;
                            }")),
            tags$script(src = "message-handler.js")
  ),
  titlePanel("Porównanie ocen z przedmiotów i wyników ankiet"),
  # Cala aplikacja jest zawarta w jednym wierszu
  fluidRow(
    # Ten wiersz jest podzielony na dwa paski - jeden o szerokosci 3 kolumn - do wyboru parametrow wyswietlania
    column(3,
           selectInput("sub_name", 
                       label = "Wybierz przedmiot",
                       choices = sub_names,
                       selected = "Indywidualny projekt programistyczny"),
           uiOutput("choose_dyd_cycle"),
           uiOutput("select_all"),
           uiOutput("unselect_all"),
           checkboxInput("show_together",
                         label = "Czy pokazać na jednym wykresie, czy na osobnych?",
                         value = FALSE),
           selectInput("questions", 
                       label = "Wybierz treść pytania",
                       choices = questions,
                       selected = NULL)
    ),
    # a drugi o szerokosci 9 kolumn - do wyswietlania wizualizacji
    column(9,
           "Wybierz sposób prezentacji danych",
           tabsetPanel(
             tabPanel("Oceny-Wykres", 
                      plotOutput("plot_grades")
             ),
             tabPanel("Ankiety-Wykres",
                      plotOutput("plot_questionnaire"),
                      dataTableOutput("summary_questionnaire")
             ),
             tabPanel("Oceny-Tabela",
                      dataTableOutput("table_grades")
             ),
             tabPanel("Ankiety-Tabela",
                      dataTableOutput("table_questionnaire")
                      )
           )
    )
  )
  ))