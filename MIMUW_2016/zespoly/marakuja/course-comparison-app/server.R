library(PogromcyDanych)
library(ggplot2)

source("input.R")

shinyServer(function(input, output) {
  points <- reactive({
    group1 = input$przedmiot1
    group2 = input$przedmiot2
    get_comparison_points(usos_dump, group1, group2)
  })
  
  output$corDiagram = renderPlot(
    ggplot(points(), aes(x=group1_mean, y=group2_mean)) + stat_bin_2d(bins=6)
  )
  
  output$corTable = renderDataTable(points())

})
