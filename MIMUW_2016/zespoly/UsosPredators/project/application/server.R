library(ggplot2)
library(dplyr)
library(shiny)
library(scales)

oceny <- tbl_df(read.csv("../../data/oceny.txt", sep=";", dec=","))
database <- read.csv("../../data/database.txt", sep=";", dec=",")

subjectNames <- c("Matematyka dyskretna",
                  "Analiza matematyczna inf. I",
                  "Analiza matematyczna inf. II",
                  "Algorytmy i struktury danych",
                  "Programowanie obiektowe",
                  "Systemy operacyjne",
                  "Indywidualny projekt programistyczny")

estimators <- c("Średnia", "Mediana", "Wariancja", "Zaliczenie")


# UI
plotTitleUI <- element_text(size=27, margin=margin(b=20, unit="pt"))
plotMarginUI <- unit(c(1,1,1,1), "cm")
axisTextUI <- element_text(size=14)
axisTitleXUI <- element_text(size=18, margin=margin(t=15, unit="pt"))
axisTitleYUI <- element_text(size=18, margin=margin(r=20, unit="pt"))
legendTitleUI <- element_text(size=18, margin=margin(b=400,t=500, unit="pt"))
legendTextUI <- element_text(size=16, margin=margin(b=20, unit="pt"))



shinyServer(function(input, output) {
  
  # FIRST TAB
  output$lecturerSubject <- renderUI({
    selectInput("lecturerSubject", "", sort(subjectNames))
  })

  selectedSubject <- reactive({
    data <- dplyr:::filter(database, NAZWA == input$lecturerSubject)
  })
  
  lecturers <- reactive({
    data <- selectedSubject()
    temp <- data[grepl("*WYK$", data$TZAJ_KOD_1),]
    temp <- dplyr:::select(temp, PRAC_ID_1)
    dplyr:::group_by(temp, PRAC_ID_1) %>% summarise()
  })

  output$lecturerPlot = renderPlot({
    lecturerData <- selectedSubject()
    lecturers <- lecturers()
    lv <- c(t(lecturers))
    
    lecturerData <- dplyr:::filter(lecturerData, OCENA_WARTOSC != "", PRAC_ID_1 %in% lv) %>% dplyr:::select(PRAC_ID_1, OCENA_WARTOSC)
    
    dFMatrix <- matrix(nrow = 0, ncol = nrow(lecturers))

    grades = c('2', '3', '3,5', '4', '4,5', '5')
    
    for (i in grades) {
      temp <- c()
      for (w in lecturers$PRAC_ID_1) {
        temp <- c(temp, nrow(dplyr:::filter(lecturerData, PRAC_ID_1 == w, OCENA_WARTOSC == i)))
      }
      dFMatrix <- rbind(dFMatrix, temp)
    }
    
    rownames(dFMatrix) <- grades
    colnames(dFMatrix) <- c(t(lecturers))
    
    df <- as.data.frame(dFMatrix)
    df$Oceny <- row.names(df)

    library(reshape2)
    df.molten <- melt(df, value.name="Rozkład", variable.name="Wykładowcy", na.rm=TRUE)
    library(ggplot2)
    
    plot <- ggplot(data = df.molten, aes(x=Wykładowcy, fill=Oceny)) + 
      geom_bar(data = subset(df.molten, Oceny %in% c("2")),
               aes(y = -Rozkład), position="stack", stat="identity") +
      geom_bar(data = subset(df.molten, !Oceny %in% c("2")), 
               aes(y = Rozkład), position="stack", stat="identity") + 
      scale_y_continuous(breaks = NULL) +
      geom_hline(aes(yintercept = 0)) +
      labs(x = "Id wykładowcy", y = "Rozkład ocen") + 
      ggtitle("Wyniki egzaminów względem wykładowcy") +
      theme(plot.title=plotTitleUI, plot.margin=plotMarginUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI,
            legend.title=legendTitleUI,
            legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line"))
    
    for (i in 1:(ncol(df)-1)) {
      plot <- plot +
        annotate("text", x = i, y = (sum(df[,i]) - df[1,i]) + 2, label = sum(df[,i]), size = 10)
    }
    
    plot
  })

  # SECOND TAB
  output$trendSubjectsGroup <- renderUI({
    selectInput("trendSubjectsGroup", "", sort(subjectNames), multiple = TRUE)
  })

  output$trendEstimators <- renderUI({
    selectInput("trendEstimators", "", estimators)
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

  selectedEstimator <- reactive({
    selectedEstimator <- input$trendEstimators
    selectedEstimator
  })

  chosenYearsTrendSlider <- reactive({
    sliderTrend <- input$yearsTrendSlider

    data <- selectedYearsTrend()
    data <- dplyr:::filter(data, CDYD_KOD >= sliderTrend[1] & CDYD_KOD <= sliderTrend[2])
    data
  })


  output$trendPlot = renderPlot({
    estimator <- selectedEstimator()

    yearsTrendData <- chosenYearsTrendSlider() %>%
      filter(OCENA != "") %>%
      dplyr:::group_by(CDYD_KOD, PRZ_NAZWA)

    if (estimator == "Zaliczenie") {
      pos <- position_dodge(width=0.9)
      yearsTrendData <- yearsTrendData %>% summarise(count=n(), niezal=sum(ifelse(as.numeric(OCENA) > 2, 0, 1)))
      pl <- ggplot(data=yearsTrendData, aes(x=CDYD_KOD, weight=niezal, y=count, ymin=niezal, ymax=niezal, fill=as.factor(PRZ_NAZWA))) +
        geom_bar(aes(group = PRZ_NAZWA), size=2, position=pos, stat="identity") +
        geom_errorbar(aes(y=niezal), linetype="dashed", position=pos) +
        ggtitle("Zaliczenia przedmiotów w kolejnych latach") +
        xlab("Lata") + ylab("Ilość studentów") +
        labs(fill = "Przedmiot") +
        theme(plot.title=plotTitleUI, plot.margin=plotMarginUI,
              axis.text=axisTextUI,
              axis.title.x=axisTitleXUI,
              axis.title.y=axisTitleYUI,
              legend.position="top",
              legend.title=legendTitleUI, legend.text=legendTextUI, legend.title.align=0.5,
              legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line")) +
        scale_x_continuous(breaks=seq(2000, 2015, 1))
      pl
    } else {
      if (estimator == "Średnia") {
        yearsTrendData <- yearsTrendData %>% summarise(srednia=mean(as.numeric(OCENA)))
      } else if (estimator == "Mediana") {
        yearsTrendData <- yearsTrendData %>% summarise(srednia=median(as.numeric(OCENA)))
      } else if (estimator == "Wariancja") {
        yearsTrendData <- yearsTrendData %>% summarise(srednia=var(as.numeric(OCENA)))
      }

      ggplot(data=yearsTrendData, aes(x=CDYD_KOD, y=srednia, colour = as.factor(PRZ_NAZWA))) +
        geom_line(aes(group = PRZ_NAZWA), size=2) + geom_point() +
        ggtitle(paste(estimator, "ocen przedmiotów w kolejnych latach")) +
        xlab("Lata") + ylab(paste(estimator, "ocen")) +
        labs(colour = "Przedmiot") +
        theme(plot.title=plotTitleUI, plot.margin=plotMarginUI,
              axis.text=axisTextUI,
              axis.title.x=axisTitleXUI,
              axis.title.y=axisTitleYUI,
              legend.position="top",
              legend.title=legendTitleUI, legend.text=legendTextUI, legend.title.align=0.5,
              legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line")) +
        scale_x_continuous(breaks=seq(2000, 2015, 1))
    }
  })

  # THIRD TAB
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
      dplyr:::select(CDYD_KOD, OCENA)
      #tally %>%
      #group_by(CDYD_KOD) %>%
      #summarise()
      #mutate(PCT=n/sum(n))
    
    years <- dplyr:::group_by(yearsData, CDYD_KOD) %>% summarise()
    
    dFMatrix <- matrix(nrow = 0, ncol = nrow(years))
    
    grades = c('2', '3', '3,5', '4', '4,5', '5')
    
    for (i in grades) {
      temp <- c()
      for (w in years$CDYD_KOD) {
        #print(nrow(dplyr:::filter(yearsData, CDYD_KOD == w, OCENA == i)))
        temp <- c(temp, nrow(dplyr:::filter(yearsData, CDYD_KOD == w, OCENA == i)))
      }
      dFMatrix <- rbind(dFMatrix, temp)
    }
    
    rownames(dFMatrix) <- grades
    colnames(dFMatrix) <- c(t(years))
    
    df <- as.data.frame(dFMatrix)
    df$Oceny <- row.names(df)
    
    
    
    library(reshape2)
    df.molten <- melt(df, value.name="Rozkład", variable.name="Lata", na.rm=TRUE)
    library(ggplot2)
    
    print(df.molten)
    
    plot <- ggplot(data = df.molten, aes(x=Lata, fill=Oceny)) + 
      geom_bar(data = subset(df.molten, Oceny %in% c('2')),
               aes(y = -Rozkład), position="stack", stat="identity") +
      geom_bar(data = subset(df.molten, !Oceny %in% c('2')), 
               aes(y = Rozkład), position="stack", stat="identity") + 
      scale_y_continuous(breaks = NULL) +
      geom_hline(aes(yintercept = 0)) +
      labs(x = "Lata", y = "Rozkład ocen") + 
      ggtitle("Wyniki egzaminów względem lat") +
      theme(plot.title=plotTitleUI, plot.margin=plotMarginUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI,
            legend.title=legendTitleUI,
            legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line"))
    
    for (i in 1:(ncol(df)-1)) {
      plot <- plot +
        annotate("text", x = i, y = (sum(df[,i]) - df[1,i]) + 2, label = sum(df[,i]), size = 10)
    }
    
    plot
    
  })

})
