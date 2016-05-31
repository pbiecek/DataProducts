library(shiny)

#' UI aplikacji shiny
maturiser.ui <- shinyUI(fluidPage(
  titlePanel("Statystyki szkoły"),
  sidebarLayout(
      sidebarPanel(
          htmlOutput("gmina"),
          htmlOutput("szkola")
      ),
      mainPanel(
          htmlOutput("wykresy")
      )
    )
  )
)
