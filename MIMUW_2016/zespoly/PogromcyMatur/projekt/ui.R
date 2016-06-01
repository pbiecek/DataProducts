
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(shinyBS)

interface_help <- tags$html(
  tags$div("Korzystasz z narzędzia służącego do wizualizacji zależności pomiędzy wynikami 
           osiągniętymi przez uczniów na poprzednim szczeblu edukacji, a wynikami egzaminu."),
  tags$div("Na początku możesz wybrać:"),
  tags$ul(
    tags$li("Egzamin którym jesteś zainteresowany."),
    tags$li("Arkusz egzaminu który ma zostać rozpatrzony. ten sam egzamin może posiadać różne arkusze,
            na przykład wersje dla dyslektyków. Sprawdzić który z nich cię interesuje możesz np. na 
            stronie", tags$a(href="http://zpd.ibe.edu.pl/doku.php?id=bazatestypytania", "arkusze") ,
            "lub wybierając arkusz i wchodząc w link do jego treści."),
    tags$li("Poprzedni egzamin - czyli ten, z którym będziemy porównywać wyniki."),
    tags$li("Jak bardzo szczegółowy ma być wykres. Pytania egzaminacyjne podzielone są na:" 
            , tags$ul(
                tags$li("Kryteria ocen - najbardziej atomiczne cząstki oceny. Może to być np. pojedyncze
                        pytanie zamknięte, lub ocena za interpunkcję w wypracowaniu."),
                tags$li("Pytania - jedno pytanie składa się z jednego lub większej liczby kryterium oceny.
                        Odpowiadają rzeczywistym pytaniom występującym na arkuszach."),
                tags$li("Wiązki pytań - pytania są czasami, choć nie zawsze, podzielone na tematyczne grupy
                        nazywane wiązkami pytań.")
              )
            )
  )
)

modalHelp <- function(name, header = "Help", content = "Pomoc niedostępna.") {
  tags$html(actionButton(name, "Help"),
            bsModal(paste(c("window",name), collapse="_"), header, name, tags$div(content)))
}

shinyUI(pageWithSidebar(
  
  headerPanel("Czwarta, finalna iteracja"),
    
  sidebarPanel(
      uiOutput("egz_wybor"),
      uiOutput("arkusz_wybor"),
      uiOutput("egz_wybor_poprz"),
      selectInput("poziom", label = "Grupuj po:", list(
        "wiązkach pytań" = "wia",
        "pytaniach" = "pyt",
        "kryteriach oceny" = "kry"
        )
      ),
      modalHelp("help_but", "Interfejs użytkownika", interface_help)
    ),
  mainPanel(
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Wpływ wcześniejszych etapów edukacji",
        value = "poprzednie",
        plotOutput("poprz_plot", click="poprz_click"),
        splitLayout(
          style = "border: 1px solid silver;",
          modalHelp("help_but_main", "Wykresy"),
          htmlOutput("link_do_arkusza"),
          htmlOutput("link_do_klucza")
        ),
        plotOutput("poprz_plot_jedno"),
        htmlOutput("arkusze_zawierajace")
      )
    ))
))