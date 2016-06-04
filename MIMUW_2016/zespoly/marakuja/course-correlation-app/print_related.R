# Command line equivalent of the app

source("logic.R")

course <- courses_vector[1]
print(course)
print("correlation - passed")
sort_courses(course, 1, filter_passed)
print("correlation - failed")
sort_courses(course, 1, filter_failed)

