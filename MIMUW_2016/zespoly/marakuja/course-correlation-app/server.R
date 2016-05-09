library(ggplot2)

source("input.R")

shinyServer(function(input, output) {
  points <- reactive({
    courseA = input$przedmiot1
    courseB = input$przedmiot2
    count_A_by_mark_B_passed(courseA, courseB)
  })
  
  output$corDiagram = renderPlot(
    ggplot(df_for_plot(points()), aes(x = ocena_przedmiot_B, y = liczba_studentow)) + geom_step()
  )
  
  output$corTable = renderDataTable(points())
})
