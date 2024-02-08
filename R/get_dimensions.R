#' Get Dimensions of Object
#'
#' This function retrieves the dimensions of a given object, such as the number of rows and columns
#' for a data frame or matrix, or the length for other types of objects.
#'
#' @param object The object for which dimensions are to be retrieved.
#'
#' @return A list containing the dimensions of the object. For data frames and matrices,
#' it returns a list with 'row' and 'col' elements. For other objects, it returns the length.
#'
#' @examples
#' data <- matrix(1:12, ncol = 3)
#' get_dimensions(data)
#'
#' vector <- c(1, 2, 3, 4, 5)
#' get_dimensions(vector)
#'
#' empty <- NULL
#' get_dimensions(empty)
#'
#' @export
#'
#' @keywords dimensions matrix data frame length
#'
#' @family data
#'
#' @rdname get_dimensions
#' @aliases get_dimensions
#' @name get_dimensions

#' @usage get_dimensions(object)
get_dimensions = function(object)
{
  if(is.null(object)) return(NULL)
  else if(is.data.frame(object) | is.matrix(object)) return(list(row = nrow(object),col = ncol(object)))
  else return(length(object))
}
