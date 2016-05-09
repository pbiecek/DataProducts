library(ggplot2)
library(plyr)
library(shiny)

shinyServer(function(input, output) {
  
  data <- read.csv("/home/marcin/jnp2/proj/data/oceny.csv", header=TRUE)
  #data <- read.csv("~/Desktop/usos/oceny.csv", header=TRUE)
  
  output$trend_2 <-  renderPlot({
    
    #sub_code <- input$sub_code
    sub_name <- input$sub_name 
    show_together <- input$show_together
    dyd_cycle <- input$dyd_cycle
    plot_title <- paste("Rozkład ocen z przedmiotu ", sub_name)
    print(sub_name)
    print(dyd_cycle)
    
    if(is.null(dyd_cycle)) {
      #nic nie robi
    } else {
      plot_data <- data[data$PRZ_NAZWA==sub_name,]
      plot_data <- plot_data[grepl(paste(dyd_cycle,collapse="|"), plot_data$CYKL_DYD),]
      #print(plot_data)
      if(!empty(plot_data)) {
        if(show_together) {
          plot <- ggplot(data=plot_data, aes(OCENA, fill=CYKL_DYD)) + 
            geom_bar() + 
            scale_fill_brewer(palette="PuRd") +
            xlab("Ocena") + 
            ylab("Liczba osob") +
            labs(title=plot_title) +
            theme(legend.title = element_blank())
        } else {
          plot <- ggplot(data=plot_data, aes(OCENA, fill = I("maroon"))) + 
            geom_bar() + 
            geom_text(aes( label = format(..count.., digits=2, drop0trailing=TRUE),
                           y= ..count.. ), stat= "count", vjust = -.5) +
            facet_wrap(~ CYKL_DYD) +
            xlab("Ocena") + 
            ylab("Liczba osob") +
            labs(title=plot_title)
        }
        plot  
      }
    }
  })
  
})
