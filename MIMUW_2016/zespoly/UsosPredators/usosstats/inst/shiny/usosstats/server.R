library(ggplot2)
library(dplyr)
library(shiny)
library(scales)

#oceny <- tbl_df(read.csv("../../data/oceny.txt", sep=";", dec=","))
#ankiety <- tbl_df(read.csv("../../data/ankiety.txt", sep=";", dec=","))


subjectNames <- c(
  "Algorytmy i struktury danych", "1000-213bASD",
  "Matematyka dyskretna", "1000-212bMD",
  "Analiza matematyczna inf. I", "1000-211bAM1",
  "Analiza matematyczna inf. II", "1000-212bAM2",
  "Indywidualny projekt programistyczny", "1000-222bIPP",
  "Systemy operacyjne", "1000-213bSOP",
  "Bazy danych", "1000-213bBAD",
  "Geometria z algebrą liniową", "1000-211bGAL",
  "Inżynieria oprogramowania", "1000-214bIOP",
  "Języki, automaty i obliczenia", "1000-214bJAO",
  "Rachunek prawdopodobieństwa i statystyka", "1000-213bRPS",
  "Sieci komputerowe", "1000-214bSIK",
  "Aplikacje WWW", "1000-214bWWW"
)


subjectNames <- matrix(data=subjectNames, ncol = 2, byrow = TRUE)
colnames(subjectNames) <- list("NAZWA", "KOD")
subjectNames <- as.data.frame(subjectNames)


estimators <- c("Średnia", "Zaliczenie")


# UI
plotTitleUI <- element_text(size=27, margin=margin(b=20, unit="pt"))
plotMarginUI <- unit(c(1,1,1,1), "cm")
axisTextUI <- element_text(size=14)
axisTitleXUI <- element_text(size=18, margin=margin(t=15, unit="pt"))
axisTitleYUI <- element_text(size=18, margin=margin(r=20, unit="pt"))
legendTitleUI <- element_text(size=18, margin=margin(b=400,t=500, unit="pt"))
legendTextUI <- element_text(size=16, margin=margin(b=20, unit="pt"))



shinyServer(function(input, output) {
  # SURVEYS TAB
  output$surveySubject <- renderUI({
    selectInput("surveySubject", "", sort(as.matrix(subjectNames$NAZWA)))
  })

  selectedSurveySubject <- reactive({
    subjectCode <- dplyr:::filter(subjectNames, NAZWA == input$surveySubject)
    data <- dplyr:::filter(ankiety, KOD == as.vector(subjectCode$KOD))
    data
  })

  output$surveySubjectType <- renderUI({
    data <- selectedSurveySubject()
    type <- dplyr:::group_by(data, ZAJ_KOD)
    type <- dplyr:::distinct(type, ZAJ_KOD)

    selectInput("surveySubjectType", "", as.matrix(data$ZAJ_KOD))
  })

  selectedSurveySubjectType <- reactive({
    data <- selectedSurveySubject()
    data <- dplyr:::filter(data, ZAJ_KOD == input$surveySubjectType)
    data
  })

  output$surveyLecturer <- renderUI({
    data <- selectedSurveySubjectType()
    data <- dplyr:::group_by(data, PROWADZACY)
    data <- dplyr:::distinct(data, PROWADZACY)

    selectInput("surveyLecturer", "", sort(as.matrix(data$PROWADZACY)))
  })

  output$surveyQuestion <- renderUI({
    data <- selectedSurveySubjectType()
    data <- dplyr:::filter(data, PROWADZACY == input$surveyLecturer)
    data <- dplyr:::group_by(data, TRESC_PYTANIA)
    data <- dplyr:::distinct(data, TRESC_PYTANIA)

    selectInput("surveyQuestion", "", sort(as.matrix(data$TRESC_PYTANIA)))
  })

  output$surveyPercentage <- renderUI({
    checkboxInput("surveyPercentage", "Pokaż w postaci procentów")
  })

  showPercentage <- reactive({
    input$surveyPercentage
  })

  selectedSurveyLecturer <- reactive({
    data <- selectedSurveySubject()
    data <- dplyr:::filter(data, PROWADZACY == input$surveyLecturer)
    data <- dplyr:::group_by(data, PROWADZACY)
    data
  })

  #output$surveyYears <- renderUI({
    #selectInput("surveyYears", "", sort(c(seq(2000, 2015, 1))), multiple = TRUE)
  #})

  selectedSurveyYears <- reactive({
    data <- selectedSurveyLecturer()

    data$CDYD_KOD <- gsub("Z", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- gsub("L", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- as.numeric(data$CDYD_KOD)

    years <- input$surveyYears
    data <- dplyr:::filter(data, CDYD_KOD %in% years)

    data
  })

  output$surveyPlot = renderPlot({
    data <- selectedSurveyLecturer()
    data <- dplyr:::filter(data, TRESC_PYTANIA == input$surveyQuestion)
    data <- dplyr:::select(data, CDYD_KOD, WARTOSC_ODPOWIEDZI, LICZBA_ODPOWIEDZI)
    data <- data %>% ungroup %>% group_by(CDYD_KOD) %>% mutate(SUMA = sum(LICZBA_ODPOWIEDZI))
    data <- dplyr:::mutate(data, PROCENT = LICZBA_ODPOWIEDZI * 100/SUMA)


    #oceny_ankiet <- as.vector(dplyr:::group_by(data, WARTOSC_ODPOWIEDZI))

    data$CDYD_KOD <- gsub("Z", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- gsub("L", "", as.character(data$CDYD_KOD))

    wszystkie_oceny <- dplyr:::group_by(ankiety, WARTOSC_ODPOWIEDZI)
    wszystkie_oceny <- dplyr:::summarise(wszystkie_oceny)

    library(RColorBrewer)
    myColors <- brewer.pal(7,"Set1")
    names(myColors) <- levels(factor(sort(as.matrix(as.data.frame(wszystkie_oceny)))))
    colScale <- scale_fill_manual(name = "Wartość odpowiedzi",values = myColors)

    if (!showPercentage()) {
      ylab_title = "Liczba odpowiedzi"
      plot = ggplot(ankiety, aes(CDYD_KOD, LICZBA_ODPOWIEDZI, width = 0.5))
    } else {
      ylab_title = "Procent odpowiedzi"
      plot = ggplot(ankiety, aes(CDYD_KOD, PROCENT, width = 0.5))
    }

    q <- plot +
      geom_bar(aes(fill = factor(sort(WARTOSC_ODPOWIEDZI))), position = "stack", stat="identity") +
      ggtitle(gsub('(.{1,40})(\\s|$)', '\\1\n', input$surveyQuestion)) +
      xlab("Lata") +
      ylab(ylab_title) +
      guides(fill = guide_legend(reverse=TRUE)) +
      theme(plot.title=plotTitleUI, plot.margin=plotMarginUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI,
            legend.title=legendTitleUI,
            legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line")) +
      scale_y_continuous(labels = function (x) floor(x))

    q %+% data + colScale

  })

  selectedMarksSubject <- reactive({
    data <- dplyr:::filter(oceny, PRZ_NAZWA == input$surveySubject)
    data
  })

  output$surveyPlotAdditional = renderPlot({
    surveys <- selectedSurveyLecturer()
    surveys <- dplyr:::filter(surveys, TRESC_PYTANIA == input$surveyQuestion)
    surveys <- dplyr:::mutate(surveys, CDYD_KOD = gsub("Z", "", as.character(CDYD_KOD)))
    surveys <- dplyr:::mutate(surveys, CDYD_KOD = gsub("L", "", as.character(CDYD_KOD)))
    years <- dplyr:::group_by(surveys, CDYD_KOD) %>% summarise()
    v = as.vector(years$CDYD_KOD)
    v_int = strtoi(v)

    marks = selectedMarksSubject()
    marks <- dplyr:::mutate(marks, CDYD_KOD = gsub("Z", "", as.character(CDYD_KOD)))
    marks <- dplyr:::mutate(marks, CDYD_KOD = gsub("L", "", as.character(CDYD_KOD)))
    marks <- dplyr:::filter(marks, CDYD_KOD %in% v_int)
    marks <- dplyr:::group_by(marks, CDYD_KOD) %>% summarise(COUNT = n())

    ggplot(data = marks, aes(x=CDYD_KOD, y = COUNT, width = 0.4)) + geom_bar(stat="identity") +
      ggtitle("Ilość ocen z wybranego przedmiotu") +
      xlab("Lata") +
      ylab("Ilość ocen") +
      theme(plot.title=plotTitleUI, plot.margin=plotMarginUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI,
            legend.title=legendTitleUI)
  })


  # TRENDINGS TAB
  output$trendingsFirstTerm <- renderUI({
    checkboxInput("trendingsFirstTerm", "Pokaż oceny z pierwszego terminu")
  })
  
  output$trendingsSecondTerm <- renderUI({
    checkboxInput("trendingsSecondTerm", "Pokaż oceny z drugiego terminu")
  })
  
  showTrendingsFirstTerm <- reactive({
    input$trendingsFirstTerm
  })
  
  showTrendingsSecondTerm <- reactive({
    input$trendingsSecondTerm
  })
  
  output$trendSubjectsGroup <- renderUI({
    selectInput("trendSubjectsGroup", "", sort(as.matrix(subjectNames$NAZWA)), multiple = TRUE)
  })

  output$trendEstimators <- renderUI({
    selectInput("trendEstimators", "", estimators)
  })

  output$yearsTrendSlider <- renderUI({
    sliderInput("yearsTrendSlider", "", min=2000, max=2015, value=c(2004,2015), sep="", step=1)
  })

  selectedYearsTrend <- reactive({
    subjectCode <- dplyr:::filter(subjectNames, NAZWA %in% input$trendSubjectsGroup)

    data <- dplyr:::filter(oceny, KOD %in% subjectCode$KOD)
    data$CDYD_KOD <- gsub("Z", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- gsub("L", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- as.numeric(data$CDYD_KOD)
    data
  })

  selectedEstimator <- reactive({
    selectedEstimator <- input$trendEstimators
    selectedEstimator
  })

  output$yearsPercentage <- renderUI({
    if (selectedEstimator() == "Zaliczenie")
      checkboxInput("yearsPercentage", "Pokaż w postaci procentów")
  })

  showYearsPercentage <- reactive({
    input$yearsPercentage
  })

  chosenYearsTrendSlider <- reactive({
    sliderTrend <- input$yearsTrendSlider

    data <- selectedYearsTrend()
    data <- dplyr:::filter(data, CDYD_KOD >= sliderTrend[1] & CDYD_KOD <= sliderTrend[2])
    
    terms = c()
    if (showTrendingsFirstTerm()) {
      terms = c(terms, 1)
    }
    if (showTrendingsSecondTerm()) {
      terms = c(terms, 2)
    }
    data <- dplyr:::filter(data, NUMER_TERMINU %in% terms)
    
    data
  })


  output$trendPlot = renderPlot({
    estimator <- selectedEstimator()

    yearsTrendData <- chosenYearsTrendSlider() %>%
      filter(OCENA != "") %>%
      dplyr:::group_by(CDYD_KOD, PRZ_NAZWA)

    if (estimator == "Zaliczenie") {
      pos <- position_dodge(width=0.9)
      if (!showYearsPercentage()) {
        ylab_title = "Ilość studentów"
        yearsTrendData <- yearsTrendData %>% summarise(count=n(), niezal=sum(ifelse(as.numeric(as.character(OCENA)) > 2, 0, 1), na.rm = TRUE))
      } else {
        ylab_title = "Procent studentów"
        yearsTrendData <- yearsTrendData %>% summarise(count=100, niezal=sum(ifelse(as.numeric(as.character(OCENA)) > 2, 0, 1) / n() * 100, na.rm = TRUE))
      }

      pl <- ggplot(data=yearsTrendData, aes(x=CDYD_KOD, weight=niezal, y=count, ymin=niezal, ymax=niezal, fill=as.factor(PRZ_NAZWA))) +
        geom_bar(aes(group = PRZ_NAZWA), size=2, position=pos, stat="identity") +
        geom_errorbar(aes(y=niezal), linetype="dashed", position=pos) +
        ggtitle("Zaliczenia przedmiotów w kolejnych latach") +
        xlab("Lata") + ylab(ylab_title) +
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
      min.mean.sd.max <- function(x) {
        r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
        r
      }

      ggplot(data=yearsTrendData, aes(x=CDYD_KOD, y=as.numeric(as.character(OCENA)), colour=as.factor(PRZ_NAZWA))) +
        stat_summary(fun.data=min.mean.sd.max, geom="boxplot", position=position_dodge(width=0.9)) +
        ggtitle("Srednia ocen przedmiotów w kolejnych latach") +
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
  output$firstTerm <- renderUI({
    checkboxInput("firstTerm", "Pokaż oceny z pierwszego terminu")
  })

  output$secondTerm <- renderUI({
    checkboxInput("secondTerm", "Pokaż oceny z drugiego terminu")
  })

  showFirstTerm <- reactive({
    input$firstTerm
  })

  showSecondTerm <- reactive({
    input$secondTerm
  })

  output$yearsSubject <- renderUI({
    selectInput("yearsSubject", "", sort(as.matrix(subjectNames$NAZWA)))
  })

  selectedYearsSubject <- reactive({
    subjectCode <- dplyr:::filter(subjectNames, NAZWA == input$yearsSubject)

    data <- dplyr:::filter(oceny, KOD == as.vector(subjectCode$KOD))
    data$CDYD_KOD <- gsub("Z", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- gsub("L", "", as.character(data$CDYD_KOD))
    data$CDYD_KOD <- as.numeric(data$CDYD_KOD)
    data
  })

  output$yearsSlider <- renderUI({
    sliderInput("yearsSlider", "", min=2000, max=2015, value=c(2012,2015), sep="", step=1)
  })

  output$subjectYearsPercentage <- renderUI({
    checkboxInput("subjectYearsPercentage", "Pokaż w postaci procentów")
  })

  showSubjectPercentage <- reactive({
    input$subjectYearsPercentage
  })

  chosenYearsSlider <- reactive({
    slider <- input$yearsSlider

    data <- selectedYearsSubject()
    data <- dplyr:::filter(data, CDYD_KOD >= slider[1] & CDYD_KOD <= slider[2])

    terms = c()
    if (showFirstTerm()) {
      terms = c(terms, 1)
    }
    if (showSecondTerm()) {
      terms = c(terms, 2)
    }

    data <- dplyr:::filter(data, NUMER_TERMINU %in% terms)

    data
  })

  output$yearsPlot = renderPlot({
    yearsData <- chosenYearsSlider()

    yearsData <- dplyr:::group_by(yearsData, CDYD_KOD, OCENA) %>%
      filter(OCENA != "") %>% filter(OCENA != "NK") %>%
      dplyr:::select(CDYD_KOD, OCENA)

    years <- dplyr:::group_by(yearsData, CDYD_KOD) %>% summarise()


    dFMatrix <- matrix(nrow = 0, ncol = nrow(years))

    grades = c('2', '3', '3,5', '4', '4,5', '5')

    if (showSubjectPercentage()) {
      for (i in grades) {
        temp <- c()
        for (w in years$CDYD_KOD) {
          #print(nrow(dplyr:::filter(yearsData, CDYD_KOD == w, OCENA == i)))
          temp <- c(temp, nrow(dplyr:::filter(yearsData, CDYD_KOD == w, OCENA == i))*100/nrow(dplyr:::filter(yearsData, CDYD_KOD == w, OCENA %in% grades)))
        }
        dFMatrix <- rbind(dFMatrix, temp)
      }
    } else {
      for (i in grades) {
        temp <- c()
        for (w in years$CDYD_KOD) {
          #print(nrow(dplyr:::filter(yearsData, CDYD_KOD == w, OCENA == i)))
          temp <- c(temp, nrow(dplyr:::filter(yearsData, CDYD_KOD == w, OCENA == i)))
        }
        dFMatrix <- rbind(dFMatrix, temp)
      }
    }

    rownames(dFMatrix) <- grades
    colnames(dFMatrix) <- c(t(years))

    df <- as.data.frame(dFMatrix)
    df$Oceny <- row.names(df)

    library(reshape2)
    df.molten <- melt(df, value.name="Rozkład", variable.name="Lata", na.rm=TRUE)
    library(ggplot2)


    plot <- ggplot(data = df.molten, aes(x=Lata, fill=Oceny)) +
      scale_fill_brewer(palette = "Set1") +
      geom_hline(aes(yintercept = 0)) +
      ggtitle("Wyniki egzaminów względem lat") +
      theme(plot.title=plotTitleUI, plot.margin=plotMarginUI,
            axis.text=axisTextUI,
            axis.title.x=axisTitleXUI,
            axis.title.y=axisTitleYUI,
            legend.title=legendTitleUI,
            legend.key.width=unit(3,"line"), legend.key.height=unit(2,"line")) +
      guides(fill = guide_legend(reverse=TRUE))

    if (!showSubjectPercentage()) {
      # stwórz skale Likerta
      plot <- plot +
        geom_bar(data = subset(df.molten, Oceny %in% c('2')),
                 aes(y = -Rozkład), position="stack", stat="identity") +
        geom_bar(data = subset(df.molten, !Oceny %in% c('2')),
                 aes(y = Rozkład), position="stack", stat="identity") +
        labs(x = "Lata", y = "Rozkład ocen")

      # dopisz ilość ocen

      slider <- input$yearsSlider
      font_size = 10
      if (slider[2] - slider[1] > 9) {
        font_size = 8
      }

      for (i in 1:(ncol(df)-1)) {
        plot <- plot +
          annotate("text", x = i, y = (sum(df[,i]) - df[1,i]) + 10, label = sum(df[,i]), size = font_size)
      }
    } else {
      # zwykłe słupki stosowe
      plot <- plot +
        geom_bar(data = df.molten,
                 aes(y = Rozkład), position="stack", stat="identity") +
      labs(x = "Lata", y = "Procentowy rozkład ocen")
    }

    plot

  })

})
