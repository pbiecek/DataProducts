
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

prep_choices = function(vec) setNames(as.list(unique(vec)), unique(vec))

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Proof of concept"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("rok", label = "Rok:", min = 1990, max=2015, value=1990, sep=""),
    textInput("twoj_text", "Wpisz swój tekst:"),
    selectInput("typ_szkoly", "Typ szkoły:",
              choices = prep_choices(szkoly$typ_szkoly)),
    selectInput("egzamin", label = "Egzamin:",
                  choices = prep_choices(
                  paste(typy_testow$rodzaj_egzaminu,
                        typy_testow$czesc_egzaminu,
                        typy_testow$rok, sep=" ")
              )),
    selectInput("wojewodztwo", label = "Województwo:",
                choices = prep_choices(regiony$wojewodztwo)),
    selectInput("powiat", label = "Powiat:", 
                choices = prep_choices(regiony$powiat),
                selected = 1),
    selectInput("gmina", label = "Gmina:", 
                choices = prep_choices(regiony$gmina),
                selected = 1),
    selectInput("szkola", label = "Szkoła:",
                choices = prep_choices(szkoly$nazwa_szkoly))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("kryteriaPlot")
  )
))
