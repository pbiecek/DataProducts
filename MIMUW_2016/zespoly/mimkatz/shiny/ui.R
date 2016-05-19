library(shiny)

ankiety_path <- "/home/marcin/jnp2/proj/data/ankiety_clean.txt"
#ankiety_path <- "/home/magda/Pulpit/JNP II/prez3/server/ankiety_clean.txt"
#ankiety_path <- "~/Desktop/UW/JNP2/ankiety_clean.txt"

oceny_path <- "/home/marcin/jnp2/proj/data/oceny.csv"
#oceny_path <- "/home/magda/Pulpit/JNP II/prez3/server/oceny.csv"
#oceny_path <- "~/Desktop/UW/JNP2/oceny.csv"

oceny_data    <- read.csv(oceny_path , header=TRUE)
ankiety_data  <- read.csv(ankiety_path, header=TRUE, sep=";")

cycle     <- levels(oceny_data$CYKL_DYD)
sub_names <- levels(oceny_data$PRZ_NAZWA)
questions <- levels(ankiety_data$TRESC_PYTANIA)

shinyUI(fluidPage(
  tags$head(tags$style(HTML(".well {
                            background-color: #dd9999!important;
                            width: 200px;
                            }")),
            tags$script(src = "message-handler.js")
  ),
  titlePanel("Analiza danych z USOSem"),
  fluidRow(
    column(3,
           "Wybor przedmiotu do analizy",
           selectInput("sub_name", 
                       label = "Przedmiot",
                       choices = sub_names,
                       selected = "Indywidualny projekt programistyczny"),
           uiOutput("choose_dyd_cycle"),
           uiOutput("select_all"),
           uiOutput("unselect_all"),
           checkboxInput("show_together",
                         label = "Czy pokazac na jednym wykresie, czy na osobnych?",
                         value = FALSE),
           selectInput("questions", 
                       label = "Tresci pytan",
                       choices = questions,
                       selected = NULL)
    ),
    column(9,
           "Wybor wynikowego wykresu",
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
