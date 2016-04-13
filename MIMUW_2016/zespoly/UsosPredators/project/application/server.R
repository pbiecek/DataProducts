library(ggplot2)
library(dplyr)
library(googleVis)
library(shiny)

oceny <- tbl_df(read.csv("../../data/oceny.txt", sep=";", dec=","))
database <- read.csv("../../data/database.txt", sep=";", dec=",")

subjectNames <- c("Matematyka dyskretna",
                  "Analiza matematyczna inf. I",
                  "Analiza matematyczna inf. II",
                  "Algorytmy i struktury danych")


# UI
plotTitleUI <- element_text(size=27, margin=margin(b=20, unit="pt"))
axisTextUI <- element_text(size=14)
axisTitleXUI <- element_text(size=18, margin=margin(t=15, unit="pt"))
axisTitleYUI <- element_text(size=18, margin=margin(r=20, unit="pt"))
legendTitleUI <- element_text(size=18, margin=margin(b=400,t=500, unit="pt"))
legendTextUI <- element_text(size=16, margin=margin(b=20, unit="pt"))



shinyServer(function(input, output) {
  
  #FIRST TAB
  output$firstSubject <- renderUI({
    selectInput("firstSubject", "", sort(subjectNames))
  })

  output$secondSubject <- renderUI({
    selectInput("secondSubject", "", sort(setdiff(subjectNames, input$firstSubject)))
  })

  firstSubjectReactive <- reactive({
    dplyr:::filter(database, NAZWA == input$firstSubject) %>% dplyr:::select(OS_ID, OCENA_WARTOSC)
  })

  secondSubjectReactive <- reactive({
    dplyr:::filter(database, NAZWA == input$secondSubject) %>% dplyr:::select(OS_ID, OCENA_WARTOSC)
  })

  output$comparisonPlot = renderPlot({
    firstSubject <- firstSubjectReactive()
    secondSubject <- secondSubjectReactive()

    analysis <- merge(firstSubject, secondSubject, by = "OS_ID")

    library(stringr)
    analysis <- mutate(analysis, a1 = str_replace(OCENA_WARTOSC.x, ",", "."))
    analysis <- mutate(analysis, a2 = str_replace(OCENA_WARTOSC.y, ",", "."))
    analysis <- mutate(analysis, a1num = as.numeric(as.character(a1)))
    analysis <- mutate(analysis, a2num = as.numeric(as.character(a2)))
    analysis <- dplyr:::filter(analysis, a1num != "NA")
    analysis <- dplyr:::filter(analysis, a2num != "NA")

    analysis <- mutate(analysis, Roznica = a1num - a2num)

    groupAnalysis = dplyr:::group_by(analysis, Roznica) %>% summarise(Count = n())

    (ggplot(groupAnalysis, aes(x=Roznica, y=Count))
     + geom_point(size = groupAnalysis$Count*0.15)
     + scale_x_continuous(limits = c(-2.5, 2.5))
     + scale_y_continuous(limits = c(0, 250))
     + ggtitle("Różnica w wynikach")
     + xlab("Różnica") + ylab("Ilość osób")
     + theme(plot.title=plotTitleUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI)
    )
  })


  # SECOND TAB
  output$lecturerSubject <- renderUI({
    selectInput("lecturerSubject", "", sort(subjectNames))
  })

  selectedLecturerSubject <- reactive({
    data <- dplyr:::filter(database, NAZWA == input$lecturerSubject)
    data[grepl("*WYK$", data$TZAJ_KOD_1),]
  })

  output$lecturerPlot = renderPlot({
    lecturerData <- selectedLecturerSubject()

    lecturerData <- dplyr:::group_by(lecturerData, PRAC_ID_1, OCENA_WARTOSC) %>%
      filter(OCENA_WARTOSC != "") %>%
      tally %>%
      group_by(PRAC_ID_1) %>%
      mutate(PCT=n/sum(n))

    ggplot(data=lecturerData, aes(x=OCENA_WARTOSC, y=PCT, colour = as.factor(PRAC_ID_1))) +
      geom_line(aes(group = PRAC_ID_1), size=2) + geom_point() +
      scale_y_continuous(labels = scales::percent) +
      ggtitle("Porównanie wyników wykładowców") +
      xlab("Ocena") + ylab("Procent osób uzyskująca dany wynik") +
      labs(colour = "ID Wykładowcy") +
      theme(plot.title=plotTitleUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI,
            legend.title=legendTitleUI, legend.text=legendTextUI, legend.title.align=0.5,
            legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line"))
  })

  #THIRD TAB

  output$trendSubjectsGroup <- renderUI({
    selectInput("trendSubjectsGroup", "", sort(subjectNames), multiple = TRUE)
  })

  output$yearsTrendSlider <- renderUI({
    sliderInput("yearsTrendSlider", "", min=2000, max=2015, value=c(2004,2015), sep="", step=1)
  })


  selectedYearsTrend <- reactive({
    trendSubjects <- input$trendSubjectsGroup
    data <- dplyr:::filter(oceny, PRZ_NAZWA %in% trendSubjects)
    data$CDYD_KOD <- gsub("Z", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- gsub("L", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- as.numeric(data$CDYD_KOD)
    data
  })

  chosenYearsTrendSlider <- reactive({
    sliderTrend <- input$yearsTrendSlider

    data <- selectedYearsTrend()
    data <- dplyr:::filter(data, CDYD_KOD >= sliderTrend[1] & CDYD_KOD <= sliderTrend[2])
    data
  })


  output$trendPlot = renderPlot({
    yearsTrendData <- chosenYearsTrendSlider() %>%
      filter(OCENA != "") %>% filter(OCENA != "NK") %>%
      dplyr:::group_by(CDYD_KOD, PRZ_NAZWA) %>%
      summarise(srednia=mean(as.numeric(OCENA)))

    ggplot(data=yearsTrendData, aes(x=CDYD_KOD, y=srednia, colour = as.factor(PRZ_NAZWA))) +
      geom_line(aes(group = PRZ_NAZWA), size=2) + geom_point() +
      ggtitle("Średnia ocen przedmiotów w kolejnych latach") +
      xlab("Lata") + ylab("Średnia ocena") +
      labs(colour = "Przedmiot") +
      theme(plot.title=plotTitleUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI,
            legend.title=legendTitleUI, legend.text=legendTextUI, legend.title.align=0.5,
            legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line"))
  })

  # FOURTH TAB
  output$yearsSubject <- renderUI({
    selectInput("yearsSubject", "", sort(subjectNames))
  })

  selectedYearsSubject <- reactive({
    data <- dplyr:::filter(oceny, PRZ_NAZWA == input$yearsSubject)
    data$CDYD_KOD <- gsub("Z", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- gsub("L", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- as.numeric(data$CDYD_KOD)
    data
  })

  output$yearsSlider <- renderUI({
    sliderInput("yearsSlider", "", min=2000, max=2015, value=c(2012,2015), sep="", step=1)
  })

  chosenYearsSlider <- reactive({
    slider <- input$yearsSlider

    data <- selectedYearsSubject()
    data <- dplyr:::filter(data, CDYD_KOD >= slider[1] & CDYD_KOD <= slider[2])
    data
  })

  output$yearsPlot = renderPlot({
    yearsData <- chosenYearsSlider()

    yearsData <- dplyr:::group_by(yearsData, CDYD_KOD, OCENA) %>%
      filter(OCENA != "") %>% filter(OCENA != "NK") %>%
      tally %>%
      group_by(CDYD_KOD) %>%
      mutate(PCT=n/sum(n))

    ggplot(data=yearsData, aes(x=OCENA, y=PCT, colour = as.factor(CDYD_KOD))) +
      geom_line(aes(group = CDYD_KOD), size=2) + geom_point() +
      scale_y_continuous(labels = scales::percent) +
      ggtitle("Porównanie wyników przedmiotu w kolejnych latach") +
      xlab("Ocena") + ylab("Procent osób uzyskująca dany wynik") +
      labs(colour = "Rok") +
      theme(plot.title=plotTitleUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI,
            legend.title=legendTitleUI, legend.text=legendTextUI, legend.title.align=0.5,
            legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line"))
  })
  
  # FIFTH TAB
  output$histSubject <- renderUI({
    selectInput("histSubject", "", sort(subjectNames))
  })
  
  selectedHistSubject <- reactive({
    data <- dplyr:::filter(oceny, PRZ_NAZWA == input$histSubject)
  })
  
  output$histPlot <- renderGvis({
    data <- selectedHistSubject()
    data <- dplyr:::mutate(data, ROK = substr(CDYD_KOD, 0, 4))
    data <- dplyr:::mutate(data, ROKNUM = as.numeric(as.character(ROK)))
    data <- dplyr:::select(data, ROKNUM, OCENA)
    data <- dplyr:::group_by(data, ROKNUM, OCENA) %>% dplyr:::summarise(ILE = n())
    data <- filter(data, OCENA != 'NK')
    gvisMotionChart(data, idvar="OCENA", timevar="ROKNUM")
  })
  
})
