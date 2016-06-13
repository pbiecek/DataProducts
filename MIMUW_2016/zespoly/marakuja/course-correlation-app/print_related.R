# Command line equivalent of the app

source("input.R")
source("config.R")
source("courses.R")
source("logic-tab1.R")
source("logic-tab2.R")

course <- courses_vector[1]
marks_dataset <- get_marks_dataset(MARKS_CSV_PATH)
first_grades_for_courses <- get_first_grade_for_courses(marks_dataset)
first_grades_for_input_course <- get_first_grade_for_course(first_grades_for_courses, course)

print(course)
print("correlation - passed")
sort_courses_passed(first_grades_for_courses, course, 1, 3.5, first_grades_for_input_course)
print("correlation - failed")
sort_courses_failed(first_grades_for_courses, course, 1, 3.5, first_grades_for_input_course)

