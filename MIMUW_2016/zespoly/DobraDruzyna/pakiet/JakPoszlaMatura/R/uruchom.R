#' @export
uruchom <- function() {
  

  
  appDir <- system.file("shiny", "JakPoszlaMatura", package = "JakPoszlaMatura")
  if (appDir == "") {
    stop("Nie znaleziono aplikacji. Spróbuj zainstalować pakiet ponownie.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}