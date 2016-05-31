library(ggplot2)
library(shiny)


#matury <- data.frame(pl.p=c("23", "73", "69"), pl.r = c("0", "64", "0"))
#wyznaczniki <- c(c("a", "A"), c("b", "B"), c("c", "C"))


#'Serwer aplikacji shiny
maturiser.server <- function(matury, wyznaczniki){
  ilosc.wyznacznikow <- length(wyznaczniki)
  shinyServer(function(input, output) {
  output$gmina <- renderUI({
    gminy <- c("Kraków", "Puławy")
    selectInput(inputId = "gmina",
                label = "Wybierz gminę",
                choices = gminy,
                selected = "Gmina1")
  })
  
  output$szkola <- renderUI({
    szkoly <- c("Szkoła1", "Szkoła2")
    selectInput(inputId = "szkola", 
                label = "Wybierz szkołę",
                choices = szkoly,
                selected = "Szkoła1")
  })
  
  
  output$wykresy <- renderUI({
    lista.wykresow <- lapply(1:ilosc.wyznacznikow, function(i) {
      nazwa.wykresu <- paste("wykres", i, sep="")
      plotOutput(nazwa.wykresu)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, lista.wykresow)
  })
  
  # Iterujemy po liście wyznaczników, generując wykres dla każdego z nich
  for(i in 1:ilosc.wyznacznikow) {
    local({
      nowe.i <- i
      nazwa.wykresu <- paste("wykres", nowe.i, sep="")
      
      output[[nazwa.wykresu]] <- renderPlot({
        # tutaj będzie wywołanie generuj.histogram()
        ggplot(matury, aes(x=pl.p, y=pl.r)) + geom_bar(stat='identity', fill='blue', width=0.5)
      })
    })
  }
})}