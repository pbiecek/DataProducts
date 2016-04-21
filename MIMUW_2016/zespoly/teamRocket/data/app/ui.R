library(shiny)


shinyUI(fluidPage(
  tabsetPanel(
    tabPanel(
      "Najlepsze szkoły",
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
          sliderInput("ileSzkol", label = "Ile szkół?", min = 2, max = 10, value = 5),
          checkboxInput(inputId = "prog",
                        label = "Czy zaznaczyć próg?",
                        value = TRUE)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Ranking",
                     verbatimTextOutput("wykresSlupkowy"),
              verbatimTextOutput("ranking")
            ),
            tabPanel("Rozkład wyników",
              plotOutput("wykresSrzypcowy")
            )
          )
        )
      )
    ),
    tabPanel(
      "Profil szkoły",
      titlePanel("Profil wybranych szkół"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "region2",
                      label = "Szkoły: Wybierz region",
                      choices = c("Warszawa"),
                      selected = "Warszawa"),
          checkboxGroupInput("przedmioty", label = "Szkoły", choices = names(data.wawa)[2:15])
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Rankingg"
            )
          )
        )
    )
  )
)))
