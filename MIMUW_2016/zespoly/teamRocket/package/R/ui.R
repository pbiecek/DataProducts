library(shiny)

#' UI aplikacji shiny
maturiser.ui <- shinyUI(fluidPage(
  titlePanel("Wyniki matur"),
  sidebarLayout(
      sidebarPanel(
          htmlOutput("gmina"),
          htmlOutput("szkola")
      ),
      mainPanel(
          htmlOutput("wyznacznik"),
          plotOutput("wykres")
          #htmlOutput("wykresy")
      )
    )
  )
)
