library(ggplot2)

source("input.R")

shinyServer(function(input, output) {
  points <- reactive({
    group1 = input$przedmiot1
    group2 = input$przedmiot2
    get_comparison_points(connection, group1, group2)
  })
  
  output$corDiagram = renderPlot(
    ggplot(points(), aes(x=AVG_GROUP_1, y=AVG_GROUP_2)) + stat_bin2d(bins=6) + ggtitle("Zależności ocen grup przedmiotów") + xlab("Grupa 1.") + ylab("Grupa 2.") + scale_fill_gradient(low="#eeeeee", high="#000000", guide = "legend") + theme(panel.background = element_rect(fill = "#ffffff"))
  )
  
  output$corTable = renderDataTable(points())
})
