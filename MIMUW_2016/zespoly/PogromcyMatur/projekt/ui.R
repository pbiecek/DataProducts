
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

prep_choices = function(vec) setNames(as.list(unique(vec)), unique(vec))

shinyUI(pageWithSidebar(
  
  headerPanel("Trzecia iteracja"),
  
  sidebarPanel(
    selectInput("egzamin", label = "Egzamin:",
                choices = prep_choices(
                  paste(typy_testow$rodzaj_egzaminu,
                        typy_testow$czesc_egzaminu,
                        typy_testow$rok, sep=" ")
                  )),
    
    checkboxInput("wykresy_woj", "Wykres zależności dla województw?", 
                                 value = FALSE),
    checkboxInput("wykresy_plec", "Wykres zależności dla płci?", 
                  value = FALSE),
    checkboxInput("wykresy_wies", "Wykres zależności przy podziale wieś/miasto?", 
                  value = FALSE),
    checkboxInput("wykresy_szkola", "Wykres zależności dla danych szkół?", 
                  value = FALSE),
    actionButton("gen", "Generuj")
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.wykresy_plec == true",
        plotOutput("plec_plot")
      ),
      conditionalPanel(
        condition = "input.wykresy_plec == true",
        plotOutput("woj_plot")
      ),
      conditionalPanel(
        condition = "input.wykresy_plec == true",
        plotOutput("wies_plot")
      ),
      conditionalPanel(
        condition = "input.wykresy_ == true",
        plotOutput("szkola_plot")
      )
  )
))