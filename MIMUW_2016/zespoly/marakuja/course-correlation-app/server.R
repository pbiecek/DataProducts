library(ggplot2)

source("logic.R")

row_click_callback <- "function(table) {
    table.on('click.dt', 'tr', function() {
      tabs = $('.tabbable .nav.nav-tabs li a');
      Shiny.onInputChange('przedmiot_a', table.row(this).data()[0]);
      Shiny.onInputChange('przedmiot_b', $('#przedmiot').val());
      $(tabs[1]).click();
    });
}"

shinyServer(function(input, output, session) {
  
  positive_subject <- reactive ({
    validate(
      need(input$`min-common`, "Wybierz minimalną liczbę wspólnych uczestników")
    )
    course <- input$przedmiot
    sorted <- sort_courses_passed(course, input$`min-common`, input$`min-grade`)
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    sorted
  })
  
  negative_subject <- reactive ({
    course <- input$przedmiot
    validate(
      need(input$`min-common`, "Wybierz minimalną liczbę wspólnych uczestników")
    )
    sorted <- sort_courses_failed(course, input$`min-common`, input$`min-grade`)
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    sorted
  })
  
  points_positive <- reactive ({
    input_course <- input$przedmiot
    computed_courses <- positive_subject()[[1]]
    plot_for_data(input_course, computed_courses, count_A_by_mark_B_passed, "zdał")
  })
  
  points_negative <- reactive ({
    course <- input$przedmiot
    computed_courses <- negative_subject()[[1]]
    plot_for_data(course, computed_courses, count_A_by_mark_B_failed, "nie zdał")
  })

  output$headerPositive <- renderText({
    paste("Związek oceny ze zdaniem innego przedmiotu")
  })

  output$headerNegative <- renderText({
    paste("Związek oceny z niezdaniem innego przedmiotu")
  })

  output$headerTwoCourses <- renderText({
    paste("Związek oceny z", input$przedmiot_b, "ze zdaniem lub niezdaniem",
          input$przedmiot_a)
  })

  output$corDiagramTwoCourses <- renderPlot(
    twoCoursesChart(input$przedmiot_a, input$przedmiot_b)
  )

  output$legendTwoCoursesDiagram <- renderText(
    paste("Procent studentów, którzy uzyskali przynajmniej daną ocenę z",
          input$przedmiot_b)
  )
  
  output$countSummary = renderTable(
    createSummary(input$przedmiot_a, input$przedmiot_b),
    display = c("d", "d", "d", "d"), include.rownames = FALSE
  )
  
  output$tableTwoCourses = renderTable(twoCoursesTable(input$przedmiot_a, input$przedmiot_b))

  output$legendCountSummary = renderText(
    paste("Liczba studentów, którzy uczestniczyli w", input$przedmiot_b),
  )

  output$legendTwoCourses = renderText(
    paste("Procent studentów, którzy uzyskali przynajmniej daną ocenę z",
          input$przedmiot_b)
  )
  
  formatPlot <- function(dataFunc) {
    ggplot(dataFunc(), aes(x = ocena_przedmiot_B, y = liczba_studentow, color = warunek)) +
      geom_line(size = 2) + ylim(0,1) +
      geom_errorbar(aes(ymax = max_err, ymin = min_err, width = 0.12)) +
      xlab("ocena z wybranego przedmiotu")
  }

  percent_grade <- function(course, min_grade) {
    grades <- get_last_grade_for_course(data, course)
    all <- count(grades)
    filtered <- count(grades %>% filter(OCENA_LICZBOWA >= min_grade))
    round(filtered / all * 100, 2)
  }

  barPercentPlot <- function(data, direction) {
    bar_num <- min(3, count(data)[[1]]/2)
    data %>% head(bar_num) %>% mutate(type=direction) -> highest
    data %>% tail(bar_num) %>% mutate(type=-direction) -> lowest
    chosen <- rbind(highest, lowest)
    line_value <- percent_grade(input$przedmiot, input$`min-grade`)
    ggplot(chosen, aes(x = reorder(`Przedmiot A`, direction * `Procent studentów, którzy uzyskali co najmniej wybraną ocenę`),
                       y = `Procent studentów, którzy uzyskali co najmniej wybraną ocenę`,
                       fill=factor(type))) +
      geom_bar(stat = "identity", width=.5) +
      xlab('Przedmiot A') +
      scale_fill_discrete(name=paste("Czołówka przedmiotów z procentem osób \npowyżej oceny graniczej"),
                          labels=c("Najwyższym", "Najniższym")) +
      geom_hline(aes(yintercept=line_value, colour=Threshold),
                 linetype="dashed",
                 data.frame(y=line_value,
                            Threshold = "Procent studentów uczestniczących w przedmiocie B,\nktórzy otrzymali co najmniej wybraną ocenę")) +
      scale_colour_manual(values = c(`Procent studentów uczestniczących w przedmiocie B,\nktórzy otrzymali co najmniej wybraną ocenę` = "black")) +
      guides(colour=guide_legend(title=NULL))
  }

  output$corDiagramPositive = renderPlot(barPercentPlot(positive_subject(), -1) +
    ylab(paste('Procent studentów, którzy uzyskali co najmniej wybraną ocenę \nz przedmiotu B wśród studentów, którzy zaliczyli przedmiot A')))
  output$corDiagramNegative = renderPlot(barPercentPlot(negative_subject(), 1) +
    ylab(paste('Procent studentów, którzy uzyskali co najmniej wybraną ocenę \nz przedmiotu B wśród studentów, którzy nie zaliczyli przedmiotu A')))

  tab2_przedmiot_a = reactive({input$przedmiot_a})
  tab2_przedmiot_b = reactive({input$przedmiot_b})
  observe({updateSelectInput(session, "przedmiot_a", selected = tab2_przedmiot_a())})
  observe({updateSelectInput(session, "przedmiot_b", selected = tab2_przedmiot_b())})

  output$tableNegative = renderDataTable({negative_subject()}, options = list(pageLength = 10),
                                         callback = row_click_callback)
  output$tablePositive = renderDataTable({positive_subject()}, options = list(pageLength = 10),
                                         callback = row_click_callback)
})
