library(ggplot2)

source("logic-tab1.R")
source("logic-tab2.R")
source("plots.R")

row_click_callback <- "function(table) {
    table.on('click.dt', 'tr', function() {
      tabs = $('.tabbable .nav.nav-tabs li a');
      Shiny.onInputChange('przedmiot_a', table.row(this).data()[0]);
      Shiny.onInputChange('przedmiot_b', $('#przedmiot').val());
      $(tabs[1]).click();
    });
}"

shinyServer(function(input, output, session) {

  first_grades_for_input_course <- reactive ({
    get_first_grade_for_course(data, input$przedmiot)
  })

  positive_subject <- reactive ({
    validate(
      need(input$`min-common`, "Wybierz minimalną liczbę wspólnych uczestników")
    )
    course <- input$przedmiot
    sorted <- sort_courses_passed(course, input$`min-common`, input$`min-grade`, first_grades_for_input_course)
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    sorted
  })

  negative_subject <- reactive ({
    course <- input$przedmiot
    validate(
      need(input$`min-common`, "Wybierz minimalną liczbę wspólnych uczestników")
    )
    sorted <- sort_courses_failed(course, input$`min-common`, input$`min-grade`, first_grades_for_input_course)
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    sorted
  })

  tab2_przedmiot_a = reactive({input$przedmiot_a})
  tab2_przedmiot_b = reactive({input$przedmiot_b})
  observe({updateSelectInput(session, "przedmiot_a", selected = tab2_przedmiot_a())})
  observe({updateSelectInput(session, "przedmiot_b", selected = tab2_przedmiot_b())})

  output$headerNegative <- renderText({
    paste("Związek oceny z niezdaniem innego przedmiotu")
  })

  output$descriptionNegative = renderText(
    paste('Procent studentów, którzy uzyskali co najmniej wybraną ocenę z przedmoiotu "',
          input$przedmiot,
          '" wśród studentów, którzy nie zaliczyli przedmiotu A',
          sep="")
  )

  output$corDiagramNegative = renderPlot(barPercentPlot(negative_subject(), 1, input$przedmiot, input$`min-grade`))

  output$tableNegative = renderDataTable({negative_subject()}, options = list(pageLength = 10),
                                         callback = row_click_callback)

  output$headerPositive <- renderText({
    paste("Związek oceny ze zdaniem innego przedmiotu")
  })

  output$descriptionPositive = renderText(
    paste('Procent studentów, którzy uzyskali co najmniej wybraną ocenę z przedmiotu "',
          input$przedmiot,
          '" wśród studentów, którzy zaliczyli przedmiot A',
          sep="")
  )

  output$corDiagramPositive = renderPlot(barPercentPlot(positive_subject(), -1, input$przedmiot, input$`min-grade`))

  output$tablePositive = renderDataTable({positive_subject()}, options = list(pageLength = 10),
                                         callback = row_click_callback)


  output$headerTwoCourses <- renderText({
    paste('Związek oceny z przedmiotu "', input$przedmiot_b, '" ze zdaniem lub niezdaniem przedmiotu "',
          input$przedmiot_a, '"', sep="")
  })

  output$legendCountSummary = renderText(
    paste('Liczba studentów, którzy uczestniczyli w przedmiocie "', input$przedmiot_b, '"', sep=""),
  )

  output$countSummary = renderTable(
    createSummary(input$przedmiot_a, input$przedmiot_b),
    display = c("d", "d", "d", "d"), include.rownames = FALSE
  )

  output$legendTwoCoursesDiagram <- renderText(
    paste('Procent studentów, którzy uzyskali przynajmniej daną ocenę z przedmiotu "',
          input$przedmiot_b, '"', sep="")
  )

  output$corDiagramTwoCourses <- renderPlot(
    twoCoursesChart(input$przedmiot_a, input$przedmiot_b)
  )

  output$legendTwoCourses = renderText(
    paste('Procent studentów, którzy uzyskali przynajmniej daną ocenę z przedmiotu "',
          input$przedmiot_b, '"', sep="")
  )

  output$tableTwoCourses = renderTable(twoCoursesTable(input$przedmiot_a, input$przedmiot_b))
})
