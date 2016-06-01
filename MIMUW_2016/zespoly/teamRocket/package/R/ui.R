# library(shiny)

#' UI aplikacji shiny
maturiser.ui <- shiny::shinyUI(shiny::fluidPage(
  shiny::titlePanel("Wyniki matur"),
  shiny::sidebarLayout(
      shiny::sidebarPanel(
          shiny::htmlOutput("gmina"),
          shiny::htmlOutput("szkola")
      ),
      shiny::mainPanel(
          shiny::htmlOutput("wyznacznik"),
          shiny::htmlOutput("instrukcja"),
          shiny::plotOutput("wykres")
          #htmlOutput("wykresy")
      )
    )
  )
)
