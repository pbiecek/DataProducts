library(ggplot2)

source("input.R")
source("logic.R")

shinyServer(function(input, output) {
  
  positive_subject <- reactive ({
    course <- input$przedmiot
    sorted <- sort_courses_passed_by_correlation(course)
    # działa tylko przy N = 1
    c(t(sorted))
  })
  
  negative_subject <- reactive ({
    course <- input$przedmiot
    sorted <- sort_courses_failed_by_correlation(course)
    # działa tylko przy N = 1
    c(t(sorted))
  })
  
  plot_for_data <- function(computed_course, data, p_or_f) {
    input_course <- input$przedmiot
    all <- count_A_by_mark_B_all(input_course)
    plot <- data_for_plot(data, all, p_or_f)
    plot$header <- computed_course
    plot
  }
  
  points_positive <- reactive ({
    input_course <- input$przedmiot
    computed_course <- positive_subject()
    plot_for_data(computed_course, count_A_by_mark_B_passed(computed_course, input_course), "passed")
  })
  
  points_negative <- reactive ({
    course <- input$przedmiot
    computed_course <- negative_subject()
    plot_for_data(computed_course, count_A_by_mark_B_failed(computed_course, course), "failed")
  })
  
  output$headerPositive <- renderText({
    paste("Jeżeli zdałeś ", positive_subject(), "przedmiot ",
          input$przedmiot, " jest dla Ciebie")
  })

  output$headerNegative <- renderText({
    paste("Jeżeli nie zdałeś ", negative_subject(),
          "lepiej nie wybieraj przedmiotu ", input$przedmiot)
  })

  output$corDiagramPositive = renderPlot(
    ggplot(points_positive(), aes(x = ocena_przedmiot_B, y = liczba_studentow, color = typ)) + geom_line() + geom_errorbar(aes(ymax = max_err, ymin = min_err)) + ylim(0,1) + ylab("p-stwo uzyskania oceny >= niż")
  )

  output$corDiagramNegative = renderPlot(
    ggplot(points_negative(), aes(x = ocena_przedmiot_B, y = liczba_studentow, color = typ)) + geom_line() +geom_errorbar(aes(ymax = max_err, ymin = min_err)) + ylim(0,1) + ylab("p-stwo uzyskania oceny >= niż")
  )
})
