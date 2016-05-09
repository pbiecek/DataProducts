library(shiny)


shinyUI(fluidPage(
  tabsetPanel(
    tabPanel(
      "Najlepsze szkoly",
      titlePanel("Szkoly najlepiej przygotowujace na wybrany kierunek studiow"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "region",
                      label = "Szkoly: Wybierz region",
                      choices = c("Warszawa"),
                      selected = "Warszawa"),
          selectInput(inputId = "wybranaUczelnia", 
                      label = "Wybierz uczelnie",
                      choices = c("UW", "PW", "UJ", "UKSW"),
                      selected = "UW"),
          selectInput(inputId = "wybranyKierunek", 
                      label = "Wybierz kierunek",
                      choices = c("Matematyka", "Informatyka"),
                      selected = "Informatyka"),
          sliderInput("ileSzkol", label = "Ile szkol?", min = 2, max = 10, value = 5),
          checkboxInput(inputId = "prog",
                        label = "Czy zaznaczyc prog?",
                        value = TRUE)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Ranking",
              htmlOutput("tytul"),
              plotOutput("wykresSlupkowy"),
              dataTableOutput("ranking")
            ),
            tabPanel("Rozklad wynikow",
              htmlOutput("tytulDwa"),
              plotOutput("wykresSrzypcowy")
            )
          )
        )
      )
    ),
    tabPanel(
      "Profil szkoly",
      titlePanel("Profil wybranych szkol"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "region2",
                      label = "Szkoly: Wybierz region",
                      choices = c("Warszawa"),
                      selected = "Warszawa"),
          htmlOutput("wyborSzkoly"),
          htmlOutput("wyborPrzedmiotow")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Tendencje wynikow z matur",
              htmlOutput("tytul2"),
              dataTableOutput("ranking2"),
              plotOutput("wykresCA2")
            )
          )
        )
    )
  )
)))
