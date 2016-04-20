library(shiny)


shinyUI(fluidPage(
  titlePanel("Szkoły najlepiej przygotowujące na wybrany kierunek studiów"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "region",
                  label = "Szkoły: Wybierz region",
                  choices = c("Warszawa"),
                  selected = "Warszawa"),
      selectInput(inputId = "wybranaUczelnia", 
                  label = "Wybierz uczelnię",
                  choices = c("UW", "PW", "UJ", "UKSW"),
                  selected = "UW"),
      selectInput(inputId = "wybranyKierunek", 
                  label = "Wybierz kierunek",
                  choices = c("Matematyka", "Informatyka"),
                  selected = "Informatyka"),
      checkboxInput(inputId = "prog",
                    label = "Czy zaznaczyć próg?",
                    value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        # tabPanel("Ranking",
        #   plotOutput("wykresSlupkowy"),
        #   verbatimTextOutput("ranking")
        # ),
        tabPanel("Rozkład wyników",
          plotOutput("wykresSrzypcowy")
        )
      )
    )
  )
))
