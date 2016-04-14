library(shiny)

data <- read.csv("/home/marcin/jnp2/proj/data/oceny.csv", header=TRUE)
#data <- read.csv("~/Desktop/usos/oceny.csv", header=TRUE)

cycle <- levels(data$CYKL_DYD)
sub_names <- levels(data$PRZ_NAZWA)

shinyUI(fluidPage(
  tags$head(tags$style(HTML("
                            .well {
                            background-color: #dd9999!important;
                            width: 200px;
                            }
                            "))),
  titlePanel("Analiza danych z USOSem"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sub_name", 
                  label = "Przedmiot",
                  choices = sub_names,
                  #selected = "Algebra I (potok *)"),
                  #selected = "Algorytmy i struktury danych"),
                  selected = "Indywidualny projekt programistyczny"),
      checkboxGroupInput("dyd_cycle", 
                         label = "Cykl dydaktyczny",
                         choices = cycle,
                         selected = c("2012L", "2013L", "2014L"),
                         inline = TRUE),
      checkboxInput("show_together",
                    label = "Czy pokazać na jednym wykresie, czy na osobnych?",
                    value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Wykres", 
                 plotOutput("trend_2"))
      )
    )
  )
  ))
