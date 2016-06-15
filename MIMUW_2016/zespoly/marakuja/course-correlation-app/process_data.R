source ("config.R")
source ("utils.R")
source ("input.R")


get_first_grade_for_courses <- function(marks_dataset) {
  marks_dataset %>%
    filter(!is.na(OCENA_LICZBOWA)) %>%
    filter(!is.na(NUMER_TERMINU)) %>%
    group_by(OSOBA, KOD) %>%
    mutate(pierwszy_termin = min(NUMER_TERMINU)) %>%
    filter(NUMER_TERMINU %in% pierwszy_termin) %>%
    mutate(max_ocena = max(OCENA_LICZBOWA)) %>%
    filter(OCENA_LICZBOWA == max_ocena) %>%
    distinct(OCENA_LICZBOWA) -> dataset
    dataset[c("PRZ_NAZWA", "OCENA_LICZBOWA", "OSOBA")]
}

input_file <- MARKS_CSV_PATH
output_file <- PROCESSED_GRADES_CSV_PATH
marks_dataset <- get_marks_dataset(input_file)
first_grade <- get_first_grade_for_courses(marks_dataset)
write.csv(first_grade, output_file)
