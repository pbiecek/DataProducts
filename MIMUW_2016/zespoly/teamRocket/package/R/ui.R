#' UI aplikacji shiny
maturiser.ui <- shiny::shinyUI(shiny::navbarPage(
  "Maturiser",
  shiny::tabPanel("Statystyki szkoły",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::htmlOutput("gmina"),
        shiny::htmlOutput("szkola")
      ),
      shiny::mainPanel(
        shiny::htmlOutput("wyznacznik"),
        shiny::htmlOutput("instrukcja"),
        shiny::plotOutput("wykres")
      )
    )
  ),
  
  shiny::tabPanel("Porównaj szkoły",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::htmlOutput("gmina1"),
            shiny::htmlOutput("szkola1"),
            width = 12
          ),
          shiny::mainPanel(width = 0)
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::htmlOutput("gmina2"),
            shiny::htmlOutput("szkola2"),
            width = 12
          ),
          shiny::mainPanel(width = 0)
        )
      ),
      shiny::mainPanel(
        shiny::htmlOutput("wyznacznik2"),
        shiny::htmlOutput("instrukcja2"),
        shiny::plotOutput("wykres2")
      )
    )
  )
))
