library(shiny)

shinyUI(navbarPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.rawgit.com/twbs/bootstrap/v4-dev/dist/css/bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "base.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
    tags$script(src = "https://cdn.rawgit.com/twbs/bootstrap/v4-dev/dist/js/bootstrap.js")
  ),

  tabPanel("Wyniki egzaminów względem wykładowcy",
    sidebarLayout(
      sidebarPanel(
        tags$label(class="control-label", "Wybierz przedmiot:"),
        htmlOutput("lecturerSubject")
      ),
      mainPanel(
        plotOutput("lecturerPlot")
      )
    )
  ),

  tabPanel("Ankiety",
    sidebarLayout(
      sidebarPanel(
        tags$label(class="control-label", "Wybierz przedmiot:"),
        htmlOutput("surveySubject"),

        tags$label(class="control-label", "Wybierz prowadzącego:"),
        htmlOutput("surveyLecturer")

        #tags$label(class="control-label", "Wybierz lata:"),
        #htmlOutput("surveyYears")
      ),
      mainPanel(
        plotOutput("surveyPlot")
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

         tags$label(class="control-label", "Wybierz przedział lat:"),
         htmlOutput("yearsTrendSlider")
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
        htmlOutput("yearsSlider")
      ),
      mainPanel(
        plotOutput("yearsPlot")
      )
    )
  )

))
