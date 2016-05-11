
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(ZPD)
library(ggplot2)
library(scales)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

rysuj_wykres_plec_old <- function() {
  wyniki_dobre = head(filter(wyniki_po_plci, plec != 0),20)
  return (ggplot(wyniki_dobre, aes(x=id_kryterium, y = wynik)) + 
            geom_bar(stat = "identity", aes(fill=plec), position = "dodge") +
            scale_fill_discrete(name="Płeć", breaks=c("k","m"), labels=c("Kobieta", "Mężczyzna")))
}

rysuj_wykres_plec <- function() {
  # TODO
  #kry = names(dane)
  #kry = kry[grepl('^k_', kry)]
  #d <- (uczniowie %>% inner_join(dane))[c("plec", kry)]
  #d <- d %>% group_by(plec) %>% summarize_each(funs(mean))
  #d <- d %>% gather()
  #View(d)
  #return(plot(rnorm(100), rnorm(100)))
}

rysuj_histogram_calosci <- function() {
  kry = names(dane)
  kry = kry[grepl('^k_', kry)]
  tmp <- dane[, kry]
  do_wykresu <- rowSums(tmp, na.rm=TRUE)
  res <- qplot(do_wykresu,
               geom="histogram",
               binwidth = 0.5,  
               main = paste0("Histogram of overall results."), 
               xlab = "Ilość punktów",
               fill=I("blue"),
               col=I("red"))
  return(res)
}

ustaw_dane <- function(wybrano) {
  data_env = new.env()
  load(paste0("../../teamRocket/raw_data/ZPD_", wybrano, ".dat"), data_env)
  nowe_dane = data_env[[ls(data_env)]]
  dane <<- nowe_dane
  cat(paste0("Loaded data set: ", wybrano, "\n"))
  gc()
}

shinyServer(function(input, output) {
  observeEvent(input$gen, {
    if(input$wykresy_plec)
      output$plec_plot <- renderPlot(rysuj_wykres_plec())
  })
  observeEvent(input$egzamin, {
    ustaw_dane(input$egzamin)
    output$histogram_plot <- renderPlot(rysuj_histogram_calosci())
  })
})
