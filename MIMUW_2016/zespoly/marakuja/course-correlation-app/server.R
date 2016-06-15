library(ggplot2)

source("config.R")
source("input.R")
source("utils.R")
source("logic-tab1.R")
source("logic-tab2.R")
source("plots.R")

row_click_callback <- "function(table) {
    table.on('click.dt', 'tr', function() {
      tabs = $('.tabbable .nav.nav-tabs li a');
      Shiny.onInputChange('course_a', table.row(this).data()[0]);
      Shiny.onInputChange('course_b', $('#selected_course').val());
      $(tabs[1]).click();
    });
}"

shinyServer(function(input, output, session) {

  first_grades_for_courses <- get_grades_dataset(PROCESSED_GRADES_CSV_PATH)

  first_grades_for_input_course <- reactive ({
    get_first_grade_for_course(first_grades_for_courses, input$selected_course)
  })

  sorted_courses_condition_passed <- reactive ({
    validate(
      need(input$`min-common-students`, "Wybierz minimalną liczbę wspólnych uczestników")
    )
    course <- input$selected_course
    sorted <- sort_courses_passed(first_grades_for_courses, course, input$`min-common-students`, input$`min-grade`,
				  first_grades_for_input_course())
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    sorted
  })

  sorted_courses_condition_failed <- reactive ({
    course <- input$selected_course
    validate(
      need(input$`min-common-students`, "Wybierz minimalną liczbę wspólnych uczestników")
    )
    sorted <- sort_courses_failed(first_grades_for_courses, course, input$`min-common-students`, input$`min-grade`,
				  first_grades_for_input_course())
    validate(
      need(nrow(sorted) > 0, "Brak pasujących przedmiotów")
    )
    sorted
  })

  points_two_courses <- reactive ({
    pointsTwoCourses(first_grades_for_courses, input$course_a, input$course_b)
  })

  tab2_course_a = reactive({input$course_a})
  tab2_course_b = reactive({input$course_b})
  observe({updateSelectInput(session, "course_a", selected = tab2_course_a())})
  observe({updateSelectInput(session, "course_b", selected = tab2_course_b())})

  output$headerFailed <- renderText({
    paste("Związek oceny z niezdaniem innego przedmiotu")
  })

  output$descriptionFailed = renderText(
    paste('Procent studentów, którzy uzyskali co najmniej wybraną ocenę z przedmiotu "',
          input$selected_course,
          '" wśród studentów, którzy nie zaliczyli przedmiotu A i jednocześnie uczestniczyli w przedmiocie "',
          input$selected_course, '"', sep="")
  )

  output$corDiagramFailed = renderPlot(
    barPercentPlot(first_grades_for_courses, sorted_courses_condition_failed(), 1, input$selected_course,
                   input$`min-grade`)
  )

  output$tableFailed = renderDataTable({sorted_courses_condition_failed()}, options = list(pageLength = 10),
                                         callback = row_click_callback)

  output$headerPassed <- renderText({
    paste("Związek oceny ze zdaniem innego przedmiotu")
  })

  output$descriptionPassed = renderText(
    paste('Procent studentów, którzy uzyskali co najmniej wybraną ocenę z przedmiotu "',
          input$selected_course,
          '" wśród studentów, którzy zaliczyli przedmiot A i jednocześnie uczestniczyli w przedmiocie "',
          input$selected_course, '"', sep="")
  )

  output$corDiagramPassed = renderPlot(
    barPercentPlot(first_grades_for_courses, sorted_courses_condition_passed(), -1, input$selected_course,
                   input$`min-grade`)
  )

  output$tablePassed = renderDataTable({sorted_courses_condition_passed()}, options = list(pageLength = 10),
                                         callback = row_click_callback)


  observeEvent(input$`change-btn`, {
    course_a <- input$course_a
    course_b <- input$course_b
    updateSelectInput(session, "course_a", selected=course_b)
    updateSelectInput(session, "course_b", selected=course_a)
  })

  output$headerTwoCourses <- renderText({
    paste('Związek oceny z przedmiotu "', input$course_b, '" ze zdaniem lub niezdaniem przedmiotu "',
          input$course_a, '"', sep="")
  })

  output$legendCountSummary = renderText(
    paste('Liczba studentów, którzy uczestniczyli w przedmiocie "', input$course_b, '"', sep=""),
  )

  output$countSummary = renderTable(
    createSummary(points_two_courses(), input$course_a, input$course_b),
    display = c("d", "d", "d", "d"), include.rownames = FALSE
  )

  output$legendTwoCoursesDiagram <- renderText(
    paste('Procent studentów, którzy uzyskali przynajmniej daną ocenę z przedmiotu "',
          input$course_b, '"', sep="")
  )

  output$corDiagramTwoCourses <- renderPlot(
    twoCoursesChart(points_two_courses(), input$course_a, input$course_b)
  )

  output$legendTwoCourses = renderText(
    paste('Procent studentów, którzy uzyskali przynajmniej daną ocenę z przedmiotu "',
          input$course_b, '"', sep="")
  )

  output$tableTwoCourses = renderTable(twoCoursesTable(points_two_courses(), input$course_a, input$course_b))
})
