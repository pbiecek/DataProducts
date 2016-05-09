## UI for school info selection


library(shiny)

shinyUI(fluidPage(
  titlePanel(""),

  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, uiOutput('wybierz_wojewodztwo'))
      ),
      
      fluidRow(
        column(12, uiOutput('wybierz_powiat'))
      ),
      
      fluidRow(
        column(12, uiOutput('wybierz_gmine'))
      ),
      
      fluidRow(
        column(12, uiOutput('wybierz_rodzaj'))
      ),
      
      fluidRow(
        column(12, uiOutput('wybierz_szkole'))
        ),
      
      fluidRow(
        column(12, uiOutput('wybierz_czesc_egzaminu'))
      ),
      
      fluidRow(
        column(12, uiOutput('wybierz_lata'))
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Mapy", plotOutput('mapa')), 
        tabPanel("Tabela", dataTableOutput('wyniki_egzaminu'))
      )#,
      # 
      # fluidRow(
      #   column(12, uiOutput('wyniki_egzaminu'))
      # ),
      # 
      # fluidRow(
      #   column(12, plotOutput('mapa'))
      # )
    ))))
