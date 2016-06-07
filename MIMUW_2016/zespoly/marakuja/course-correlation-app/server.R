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
    sorted <- sort_courses_passed(course, input$`min-common`)
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    names(sorted) = c("Przedmiot", "Suma*", "Liczba studentów")
    sorted
  })
  
  negative_subject <- reactive ({
    course <- input$przedmiot
    validate(
      need(input$`min-common`, "Wybierz minimalną liczbę wspólnych uczestników")
    )
    sorted <- sort_courses_failed(course, input$`min-common`)
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    names(sorted) = c("Przedmiot", "Suma*", "Liczba studentów")
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
    paste("Związek oceny z przedmiotu B ze zdaniem lub niezdaniem przedmiotu A")
  })

  output$corDiagramTwoCourses <- renderPlot(
    twoCoursesChart(input$przedmiot_a, input$przedmiot_b)
  )
  
  output$tableTwoCourses = renderDataTable(twoCoursesTable(input$przedmiot_a, input$przedmiot_b))

  output$legendTwoCourses = renderText({
    paste("Kolumny w tabeli przedstawiają ilu studentów zdobyło co najmniej daną ocenę")
  })
  
  formatPlot <- function(dataFunc) {
    ggplot(dataFunc(), aes(x = ocena_przedmiot_B, y = liczba_studentow, color = warunek)) +
      geom_line(size = 2) + ylim(0,1) +
      geom_errorbar(aes(ymax = max_err, ymin = min_err, width = 0.12)) +
      ylab("p-stwo uzyskania przynajmniej podanej oceny") +
      xlab("ocena z wybranego przedmiotu")
  }

  output$corDiagramPositive = renderPlot(formatPlot(points_positive))
  output$corDiagramNegative = renderPlot(formatPlot(points_negative))

  tab2_przedmiot_a = reactive({input$przedmiot_a})
  tab2_przedmiot_b = reactive({input$przedmiot_b})
  observe({updateSelectInput(session, "przedmiot_a", selected = tab2_przedmiot_a())})
  observe({updateSelectInput(session, "przedmiot_b", selected = tab2_przedmiot_b())})

  output$tableNegative = renderDataTable({negative_subject()}, options = list(pageLength = 10),
                                         callback = row_click_callback)
  output$tablePositive = renderDataTable({positive_subject()}, options = list(pageLength = 10),
                                         callback = row_click_callback)
})
