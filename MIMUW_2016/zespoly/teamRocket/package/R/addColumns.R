# library(dplyr)

#' Add single new column to table
#' @param table table to which new column should be added
#' @param nameFunct pair (newColumnName, newColumnValue)
#' @return table with added new column
addSingle <- function(table, nameFunct){
  dplyr::mutate_(table, .dots=setNames(nameFunct[2], nameFunct[1]))
}

#' Add list of new columns to a table
#' @param table table to which new column should be added
#' @param funcList list containing pairs of new column name and new column lazy-evaluable value
#' @return table with added new columns
#' @usage
#' x <- dplyr::data_frame(a=1:5, b=a+7)
#' l <- list(
#'   c("col_1", "a + b"),
#'   c("col_2_name", ~pmax(a*a, a*b - a*a))
#' )
#' y <- addList(x, l)
addList <- function(table, funcList){
  Reduce(addSingle, funcList, table)
}


#' example use:
#' load("ZPD_iter3.dat")
#' l = list(
#'  c("suma_mat", "p_mat + r_mat"),
#'  c("srednia_mat", "mean(c(p_mat, r_mat), na.rm=TRUE)")
#' dd = addList(data.all.pl, l)