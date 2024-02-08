
#' Get Variable Names of Object
#'
#' This function retrieves the variable names of a given object, such as column names for a data frame or matrix,
#' or the name of the object itself for other types of objects.
#'
#' @param x The object for which variable names are to be retrieved.
#'
#' @return A character vector containing the variable names. For data frames and matrices,
#' it returns the column names. For other objects, it returns the name of the object.
#'
#' @examples
#' data <- data.frame(Name = c("John", "Alice"), Age = c(25, 30))
#' get_variables_names(data)
#'
#' matrix_data <- matrix(1:12, ncol = 3, dimnames = list(NULL, c("A", "B", "C")))
#' get_variables_names(matrix_data)
#'
#' single_variable <- c(1, 2, 3, 4, 5)
#' get_variables_names(single_variable)
#'
#' @export
#'
#' @keywords variable names data frame matrix object
#'
#' @family data
#'
#' @rdname get_variables_names
#' @aliases get_variables_names
#' @name get_variables_names

#' @usage get_variables_names(x)
get_variables_names = function(x)
{
  if(is.null(x)) varname = NULL
  else if(is.data.frame(x) | is.matrix(x)) varname = colnames(x)
  else varname = deparse(substitute(x))
  return(varname)
}

