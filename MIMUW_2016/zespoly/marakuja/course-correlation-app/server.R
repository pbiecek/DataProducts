library(ggplot2)

source("logic.R")


shinyServer(function(input, output) {
  
  positive_subject <- reactive ({
    validate(
      need(input$`min-common`, "Wybierz minimalną liczbę wspólnych uczestników")
    )
    course <- input$przedmiot
    sorted <- sort_courses_passed(course, input$`min-common`)
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
    sorted <- sort_courses_failed(course, input$`min-common`)
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    sorted
  })
  
  points_positive <- reactive ({
    input_course <- input$przedmiot
    computed_courses <- positive_subject()[[1]]
    plot_for_data(input_course, computed_courses, count_A_by_mark_B_passed, "passed")
  })
  
  points_negative <- reactive ({
    course <- input$przedmiot
    computed_courses <- negative_subject()[[1]]
    plot_for_data(course, computed_courses, count_A_by_mark_B_failed, "failed")
  })
  
  output$headerPositive <- renderText({
    paste("Jeżeli zdałeś ", positive_subject()[[1,1]], "przedmiot ",
          input$przedmiot, " jest dla Ciebie")
  })

  output$headerNegative <- renderText({
    paste("Jeżeli nie zdałeś ", negative_subject()[[1,1]],
          "lepiej nie wybieraj przedmiotu ", input$przedmiot)
  })

  output$corDiagramPositive = renderPlot(ggplot(points_positive(), aes(x = ocena_przedmiot_B, y = liczba_studentow, color = typ)) + geom_line() + ylim(0,1) + ylab("p-stwo uzyskania oceny >= niż"))
  output$corDiagramNegative = renderPlot(ggplot(points_negative(), aes(x = ocena_przedmiot_B, y = liczba_studentow, color = typ)) + geom_line() + ylim(0,1) + ylab("p-stwo uzyskania oceny >= niż"))

  output$tableNegative = renderDataTable(negative_subject())
  output$tablePositive = renderDataTable(positive_subject())
})
