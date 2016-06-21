library(shiny)
library(ggplot2)
library(plyr)
library(xtable)
library(reshape2)

# Funkcja usuwa puste wiersze tabeli (zawierajace same zera)
remove_empty_rows = function(table) {
  col = ncol(table)
  to_keep = rep(TRUE, nrow(table))
  for (i in 1:nrow(table)) {
    if (all(table[i, ] == rep.int(0, col))) {
      to_keep[i] = FALSE
    }
  }
  table <- table[to_keep,, drop=FALSE]
  table <- cbind(rownames(table), table)
  return (table)
}

shinyServer(function(input, output, session) {
  
  ankiety_path <- "/home/marcin/jnp2/proj/data/ankiety_clean.txt"
  #ankiety_path <- "/home/magda/Pulpit/JNP II/prez3/server/ankiety_clean.txt"
  #ankiety_path <- "~/Desktop/UW/JNP2/ankiety_clean.txt"
  
  oceny_path <- "/home/marcin/jnp2/proj/data/oceny.csv"
  #oceny_path <- "/home/magda/Pulpit/JNP II/prez3/server/oceny.csv"
  #oceny_path <- "~/Desktop/UW/JNP2/oceny.csv"
  
  oceny_data <- read.csv(oceny_path, header=TRUE)
  ankiety_data <- read.csv(ankiety_path, header=TRUE, sep=";")
  
  # Tworzenie menu wyboru cyklu dydaktycznego.
  output$choose_dyd_cycle <- renderUI({
    if(is.null(input$sub_name)) {
      return()
    }
    dyd_cycle_data <- unique(as.character(oceny_data[oceny_data$PRZ_NAZWA==input$sub_name,]$CYKL_DYD))
    checkboxGroupInput("dyd_cycle", 
                       label = "Wybierz cykl/e dydaktyczny/e",
                       choices = dyd_cycle_data,
                       selected = NULL,
                       inline = TRUE)
  })
  
  # Tworzenie przycisku "Zaznacz wszystkie cykle"
  output$select_all <- renderUI({
    if(is.null(input$sub_name)) {
      return()
    }
    actionButton("select_all", "Zaznacz wszystkie cykle")
  })
  
  # Tworzenie przycisku "Odznacz wszystkie cykle"
  output$unselect_all <- renderUI({
    if(is.null(input$sub_name)) {
      return()
    }
    actionButton("unselect_all", "Odznacz wszystkie cykle")
  })
  
  # Funkcja oczekujaca i reagujaca na klikniecie przycisku "Zaznacz wszystkie cykle"
  observeEvent(input$select_all, {
    if (input$select_all > 0) {
      dyd_cycle_data <- unique(as.character(oceny_data[oceny_data$PRZ_NAZWA==input$sub_name,]$CYKL_DYD))
      updateCheckboxGroupInput(session = session, 
                               inputId = "dyd_cycle", 
                               choices = dyd_cycle_data, 
                               selected = dyd_cycle_data, 
                               inline = TRUE)
    }
  })
  
  # Funkcja oczekujaca i reagujaca na klikniecie przycisku "Odznacz wszystkie cykle"
  observeEvent(input$unselect_all, {
    if (input$unselect_all > 0) {
      dyd_cycle_data <- unique(as.character(oceny_data[oceny_data$PRZ_NAZWA==input$sub_name,]$CYKL_DYD))
      updateCheckboxGroupInput(session = session, 
                               inputId = "dyd_cycle", 
                               choices = dyd_cycle_data, 
                               selected = NULL, 
                               inline = TRUE)
    }
  })  
  
  # Tworzenie wykresu ocen.
  output$plot_grades <-  renderPlot({
    
    # Pobieranie danych z UI.
    sub_name <- input$sub_name 
    show_together <- input$show_together
    dyd_cycle <- input$dyd_cycle
    plot_title <- paste("Rozkład ocen z przedmiotu ", sub_name)
    
    if(is.null(dyd_cycle)) {
      #nic nie robi
    } else {
      # Filtrowanie danych.
      plot_data <- oceny_data[oceny_data$PRZ_NAZWA==sub_name,]
      plot_data <- plot_data[grepl(paste(dyd_cycle,collapse="|"), plot_data$CYKL_DYD),]
      
      if(!empty(plot_data)) {
        if(show_together) { # jesli dane maja pojawic sie na jednym wykresie
          degree <- c("2", "3", "3,5", "4", "4,5", "5", "5!", "NK")
          
          plot_data <- as.data.frame(table(plot_data$OCENA, plot_data$CYKL_DYD))
          colnames(plot_data) <- c("OCENA", "CYKL_DYD", "COUNT")
          plot_data <- plot_data[grepl(paste(dyd_cycle, collapse="|"), plot_data$CYKL_DYD),]
          plot_data <- plot_data[grepl(paste(degree, collapse="|"), plot_data$OCENA),]
          
          plot <- ggplot(plot_data,aes(x = OCENA,y = COUNT,fill = CYKL_DYD)) + 
            geom_bar(colour = "black",position = "dodge",stat = "identity") +
            geom_text(aes(label = COUNT), position = position_dodge(width = .8), vjust = -0.5) +
            scale_fill_brewer(palette="PuRd") +
            xlab("Ocena") + 
            ylab("Liczba osób") +
            labs(title=plot_title) +
            theme(legend.title = element_blank())
        } else { # jesli maja sie pojawic na osobnych wykresach
          
          tmp_data <- as.data.frame(table(plot_data$CYKL_DYD))
          colnames(tmp_data) <- c("CYKL_DYD", "COUNT")
          tmp_data <- tmp_data[grepl(paste(dyd_cycle,collapse="|"), tmp_data$CYKL_DYD),]
          
          plot <- ggplot(data=plot_data, aes(as.factor(OCENA), fill = I("maroon"))) + 
            geom_bar(colour = "black") + 
            geom_text(aes( label = format(..count.., digits=2, drop0trailing=TRUE),
                           y= ..count.. ), stat= "count", vjust = -.5) +
            annotate("text", x=7, y=80, label= tmp_data$COUNT, color="maroon", size=6) +
            facet_wrap(~ CYKL_DYD) +
            xlab("Ocena") + 
            ylab("Liczba osob") +
            labs(title=plot_title)
        }
        plot  
      }
    }
  })
  
  # Tworzenie tabeli z ocenami.
  output$table_grades <- renderDataTable({
    
    # Pobieranie danych z UI.
    sub_name <- input$sub_name
    dyd_cycle <- input$dyd_cycle
    
    if(is.null(dyd_cycle)) {
      #nic nie robi
    } else {
      data <- oceny_data[oceny_data$PRZ_NAZWA==sub_name,]
      data <- data[grepl(paste(dyd_cycle,collapse="|"), data$CYKL_DYD),]
      if(!empty(data)) {
        degree <- c("2", "3", "3,5", "4", "4,5", "5", "5!", "NK")
        data <- as.data.frame(table(data$OCENA, data$CYKL_DYD))
        colnames(data) <- c("OCENA", "CYKL_DYD", "COUNT")
        data <- data[grepl(paste(dyd_cycle, collapse="|"), data$CYKL_DYD),]
        data <- data[grepl(paste(degree, collapse="|"), data$OCENA),]
        table <- xtabs(COUNT ~ CYKL_DYD + OCENA, data)
        table <- table[, 1:8]
        table <- remove_empty_rows(table)
        colnames(table) <- c("Cykl dydaktyczny", "2", "3", "3,5", "4", "4,5", "5", "5!", "NK")
      }
    }
    table
  }, options=list(orderClasses=TRUE, pageLength=15, searching = FALSE))
  
  # Tworzenie tabeli pod wykresem wynikow ankiet.
  output$summary_questionnaire <- renderDataTable({
    if(is.null(input$questions) || is.null(input$dyd_cycle))
      return()
    
    # Pobieranie danych z UI.
    sub_name <- input$sub_name
    questions <- input$questions
    dyd_cycle <- input$dyd_cycle
    
    data <- oceny_data[oceny_data$PRZ_NAZWA==sub_name,]
    data <- data[grepl(paste(dyd_cycle,collapse="|"), data$CYKL_DYD),]
    
    l_data <- data[grepl("2|3$", data$OCENA),]
    q_data <- ankiety_data[ankiety_data$PRZ_NAZWA == sub_name,]
    
    data2 <- matrix(, length(questions)*length(dyd_cycle), 4)
    for(j in 1:length(dyd_cycle)){
      q_temp <- q_data[q_data$CDYD_KOD == dyd_cycle[j],]
      for(i in 1:length(questions)) {
        temp <- q_temp[q_temp$TRESC_PYTANIA== questions[i],]
        q <- sum(as.numeric(as.vector(temp$LICZBA_ODPOWIEDZI)))
        d <- table(data$CYKL_DYD)[dyd_cycle[j]]
        l <- table(l_data$CYKL_DYD)[dyd_cycle[j]]
        data2[(j-1)*length(questions) + i,] <- c(d, l, q, dyd_cycle[j])
      }
    }
    
    data3 <- as.data.frame(data2)
    colnames(data3) <- c("Liczba ocen", "Liczba ocen 2 i 3", "Liczba ankiet", "Cykl dydaktyczny")
    data3 <- data3[, c(4, 1, 2, 3)]
    data3
  }, options = list(searching = FALSE))
  
  # Tworzenie wykresu wynikow ankiet.
  output$plot_questionnaire <-  renderPlot({
    if(is.null(input$questions) || is.null(input$dyd_cycle))
      return()
    
    questions <- input$questions
    sub_name <- input$sub_name
    dyd_cycle <- input$dyd_cycle
    
    sub_data <- ankiety_data[ankiety_data$PRZ_NAZWA == sub_name,]
    
    data2 <- matrix(, length(questions)*length(dyd_cycle), 4)
    total <- c()
    
    # Funkcja pomocnicza liczaca mode.
    mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    for(j in 1:length(dyd_cycle)){
      sub_temp <- sub_data[sub_data$CDYD_KOD == dyd_cycle[j],]
      
      for(i in 1:length(questions)) {
        temp <- sub_temp[sub_temp$TRESC_PYTANIA== questions[i],]
        
        value <- as.numeric(as.vector(temp$WARTOSC_ODPOWIEDZI))
        count <- as.numeric(as.vector(temp$LICZBA_ODPOWIEDZI))
        
        value_sum <- sum(value*count)
        total_count <- sum(count)
        total <- c(total, total_count)
        
        if(total_count == 0) {
          data2[(j-1)*length(questions) + i,] <- c(0, 0, 0, dyd_cycle[j])
        } else {
          
          med <- c()
          for(k in 1:length(count)){
            med <- c(med, rep(value[k], count[k]))
          }
          
          data2[(j-1)*length(questions) + i,] <- c( 
            round(value_sum/total_count, 3), 
            median(med),
            mode(med),
            dyd_cycle[j])  
        }  
      }
    }
    
    data2 <- as.data.frame(data2)
    colnames(data2) <- c("SREDNIA", "MEDIANA", "MODA", "CDYD_KOD")
    
    data3 <- melt(data2, id="CDYD_KOD")
    plot2 <- ggplot(data=data3, aes(x=CDYD_KOD, 
                                    y=as.numeric(as.vector(value)), 
                                    colour=variable,
                                    group=variable#,
                    #                shape=variable
                    )) +
      geom_line() +
      scale_color_manual(name="Statystyka:",values=c("#CC6666", "#9999CC", "#CC9999"))+
      geom_point(size=3) +
      ylab("Ocena pytania") +
      xlab("Cykl dydaktyczny") +
      labs(title=questions) +
      ylim(0, 10)
    
    plot2
  })
  
  # Tworzenie tabeli z szczegolowymi wynikami ankiet.
  output$table_questionnaire <- renderDataTable({
    if(is.null(input$questions) || is.null(input$dyd_cycle))
      return()
    questions <- input$questions
    sub_name <- ankiety_data[ankiety_data$PRZ_NAZWA == input$sub_name,]
    dyd_cycle <- input$dyd_cycle
    sub_name <- sub_name[grepl(paste(dyd_cycle,collapse="|"), sub_name$CDYD_KOD),]
    sub_name <- sub_name[sub_name$TRESC_PYTANIA == questions[1], ]
    
    # Usuwanie zbednych kolumn.
    sub_name$PRZ_KOD <- NULL
    sub_name$TRESC_PYTANIA <- NULL
    sub_name$PRZ_NAZWA <- NULL
    sub_name <- sub_name[, c(2, 3, 4, 5, 6)]
    sub_name$KOD_ZAJ <- NULL
    sub_name$CDYD_KOD <- NULL
    
    if (nrow(sub_name) > 0) {
      sub_name <- aggregate(. ~ PROWADZACY + WARTOSC_ODPOWIEDZI, data = sub_name, FUN=sum)
      table <- xtabs(LICZBA_ODPOWIEDZI ~ PROWADZACY + WARTOSC_ODPOWIEDZI, sub_name)
      table <- remove_empty_rows(table)
      colnames(table)[1] <- "Prowadzący"
      table
    } else {
      NULL
    }
  }, options=list(orderClasses=TRUE, pageLength=15))
})