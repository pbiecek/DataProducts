# Command line equivalent of the app

source("courses.R")
source("logic-tab1.R")
source("logic-tab2.R")

course <- courses_vector[1]
print(course)
print("correlation - passed")
summarise_data(course, 1, filter_passed, 2)
print("correlation - failed")
summarise_data(course, 1, filter_failed, 2)

