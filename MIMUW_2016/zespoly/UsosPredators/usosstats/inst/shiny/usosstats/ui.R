library(shiny)

shinyUI(navbarPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.rawgit.com/twbs/bootstrap/v4-dev/dist/css/bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "base.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
    tags$script(src = "https://cdn.rawgit.com/twbs/bootstrap/v4-dev/dist/js/bootstrap.js")
  ),

  tabPanel("Główna",
    tags$div(class = "header",
      tags$p("Szukasz najlepszych ćwiczeniowców i wykładowców?"),
      tags$p("A może chiałbyś zobaczyć jakie oceny z przedmiotów dostawali Twoi poprzednicy?"),
      tags$p(class="highlighted", "Jeżeli tak, to nasza aplikacja jest dla Ciebie idealna!")
    ),

    tags$div(class="cards",
      tags$div(class="card",
        tags$div(class="card__image",
          tags$img(src = "wykladowcy.png")
        ),
        tags$div(class="card__headline", "Wykładowcy"),
        tags$div(class="card__description", "Zobacz, jak wypadali wykładowcy i ćwiczeniowcy w ankietach i jakie wyniki zdobywali ich podopieczni!")
      ),
      tags$div(class="card",
        tags$div(class="card__image",
          tags$img(src = "tendecje.png")
        ),
        tags$div(class="card__headline", "Tendencje"),
        tags$div(class="card__description", "Porównaj przedmioty i dowiedź się, jakie miały tendecje w ocenach i zdawalności!")
      ),
      tags$div(class="card",
        tags$div(class="card__image",
          tags$img(src = "przedmioty.png")
        ),
        tags$div(class="card__headline", "Przedmioty"),
        tags$div(class="card__description", "Sprawdź, jakie oceny zdobywali Twoi poprzednicy z przedmiotów na przestrzeni lat!")
      )
    )
  ),


  tabPanel("Ankiety",
    sidebarLayout(
      sidebarPanel(
        tags$label(class="control-label", "Wybierz przedmiot:"),
        htmlOutput("surveySubject"),

        tags$label(class="control-label", "Wybierz typ zajęć:"),
        htmlOutput("surveySubjectType"),

        tags$label(class="control-label", "Wybierz prowadzącego:"),
        htmlOutput("surveyLecturer"),
        
        tags$label(class="control-label", "Wybierz pytanie:"),
        htmlOutput("surveyQuestion"),

        tags$label(class="checkbox"),
        htmlOutput("surveyPercentage")

      ),
      mainPanel(
        plotOutput("surveyPlot"),
        plotOutput("surveyPlotAdditional")
      )
    )
  ),

  tabPanel("Tendencje",
     sidebarLayout(
       sidebarPanel(
         tags$label(class="control-label", "Wybierz przedmioty:"),
         htmlOutput("trendSubjectsGroup"),

         tags$label(class="control-label", "Wybierz miarę:"),
         htmlOutput("trendEstimators"),

         tags$label(class="checkbox"),
         htmlOutput("yearsPercentage"),

         tags$label(class="control-label", "Wybierz przedział lat:"),
         htmlOutput("yearsTrendSlider"), 
         
         tags$label(class="checkbox"),
         htmlOutput("trendingsFirstTerm"), 
         
         tags$label(class="checkbox"),
         htmlOutput("trendingsSecondTerm")
       ),
       mainPanel(
         plotOutput("trendPlot")
       )
     )
  ),

  tabPanel("Przedmiot: Lata",
    sidebarLayout(
      sidebarPanel(
        tags$label(class="control-label", "Wybierz przedmiot:"),
        htmlOutput("yearsSubject"),

        tags$label(class="control-label", "Wybierz przedział lat:"),
        htmlOutput("yearsSlider"),
        
        tags$label(class="checkbox"),
        htmlOutput("firstTerm"), 
        
        tags$label(class="checkbox"),
        htmlOutput("secondTerm"),
        
        tags$label(class="checkbox"),
        htmlOutput("subjectYearsPercentage")
      ),
      mainPanel(
        plotOutput("yearsPlot")
      )
    )
  )

))
