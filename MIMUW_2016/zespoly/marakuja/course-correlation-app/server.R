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
  
  plot_for_data <- function(computed_course, data) {
    input_course <- input$przedmiot
    plot <- df_for_plot(data)
    plot$header <- computed_course
    plot
  }
  
  points_positive <- reactive ({
    input_course <- input$przedmiot
    computed_course <- positive_subject()
    plot_for_data(computed_course, count_A_by_mark_B_passed(computed_course, input_course))
  })
  
  points_negative <- reactive ({
    course <- input$przedmiot
    computed_course <- negative_subject()
    plot_for_data(computed_course, count_A_by_mark_B_failed(computed_course, course))
  })
  
  output$corDiagramPositive = renderPlot(
    ggplot(points_positive(), aes(x = ocena_przedmiot_B - 0.25, y = liczba_studentow)) +
      geom_step() + ggtitle(positive_subject())
  )
  
  output$corDiagramNegative = renderPlot(
    ggplot(points_negative(), aes(x = ocena_przedmiot_B - 0.25, y = liczba_studentow)) +
      geom_step() + ggtitle(negative_subject())
  )
})
