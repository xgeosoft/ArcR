#' Frequency Table for StatVector
#'
#' This generic function computes frequency tables for objects of class StatVector.
#' It can generate frequency tables for one or two variables.
#'
#' @param x A vector object representing the main variable.
#' @param y An optional second vector object for cross-tabulation.
#' @param type A character vector indicating the type of frequencies to compute: "absoluts" or "relatives" (default).
#' @param var.names Optional character vector of length 1 (for one variable) or 2 (for two variables), naming the variables.
#' @param digits Number of digits to round the output values (default is 3).
#' @param sum_of_relative_frequency The base total for percentage calculations (default is 100).
#'
#' @return A matrix with frequency and/or percentage information.
#' @importFrom stats addmargins
#' @export
setGeneric("table_r", function(x, y = NULL, type = c("absoluts", "relatives"),
                               var.names = NULL, digits = 3, sum_of_relative_frequency = 100) {
  standardGeneric("table_r")
})

#' @describeIn table_r Method for single vector object
#' @export
setMethod("table_r", signature(x = "vector", y = "missing"),
          function(x, y, type = c("absoluts", "relatives"),
                   var.names = NULL, digits = 3, sum_of_relative_frequency = 100) {
            tryCatch(
              {
                valeurs <- x@valeurs
                freq <- table(valeurs)
                percent <- prop.table(freq) * sum_of_relative_frequency
                result <- rbind(Frequency = freq, Percent = percent)
                result <- round(result, digits)

                if (is.null(var.names)) {
                  colnames(result) <- names(freq)
                } else if (is.character(var.names) && length(var.names) == 1) {
                  colnames(result) <- paste(var.names, names(freq), sep = " :: ")
                } else {
                  stop("var.names must be a character vector of length 1.")
                }
                return(result)
              },
              error = function(e) {
                message("Error in table_r check vector structure : ", e$message)
                return(NA_real_)
              })
          })

#' @describeIn table_r Method for two vector objects
#' @export
setMethod("table_r", signature(x = "vector", y = "vector"),
          function(x, y, type = c("absoluts", "relatives"),
                   var.names = NULL, digits = 3, sum_of_relative_frequency = 100) {
            tryCatch(
              {
                type <- match.arg(type)

                vx <- x@valeurs
                vy <- y@valeurs
                tab <- table(vx, vy)

                if (type == "relatives") {
                  tab <- prop.table(tab) * sum_of_relative_frequency
                }

                result <- addmargins(tab)
                result <- round(result, digits)

                if (!is.null(var.names)) {
                  if (is.character(var.names) && length(var.names) == 2) {
                    rownames(result) <- paste(var.names[2], rownames(result), sep = " :: ")
                    colnames(result) <- paste(var.names[1], colnames(result), sep = " :: ")
                  } else {
                    stop("var.names must be a character vector of length 2.")
                  }
                }

                return(result)
              },
              error = function(e) {
                message("Error in table_r check vector structure : ", e$message)
                return(NA_real_)
              })
          })

#' @examples
#' x <- c("A", "B", "A", "C", "A")
#' y <- c("X", "Y", "X", "Y", "X")
#'
#' # Frequency table for one variable
#' table_r(x, var.names = "Lettre")
#'
#' # Contingency table for two variables
#' table_r(x, y, type = "relatives", var.names = c("Lettre", "Groupe"))
