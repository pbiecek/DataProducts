library(ggplot2)

source("input.R")


shinyServer(function(input, output) {
  points <- reactive({
    courseA <- input$przedmiot1
    courseB <- input$przedmiot2
    
    passed = count_A_by_mark_B_passed(courseA, courseB)
    not_attending = count_A_by_mark_B_not_attending(courseA, courseB)
    failed = count_A_by_mark_B_failed(courseA, courseB)
    data_for_plot(failed, passed, not_attending)
  })
  
  output$corDiagram = renderPlot(
    ggplot(points(), aes(x = ocena_przedmiot_B - 0.25, y = liczba_studentow, colour = typ)) + geom_step() + facet_wrap(~typ, ncol=1)
  )
  
  output$corTable = renderDataTable(points())
})
