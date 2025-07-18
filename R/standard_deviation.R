#' Calculate Standard Deviation (S4)
#'
#' Computes the standard deviation of a numeric object.
#'
#' @param x A \code{numeric} object.
#' @param na.rm Logical, whether to remove NA values.
#' @param for.sample Logical, whether to calculate sample standard_deviation (default: TRUE).
#'
#' @return A numeric value: the standard deviation.
#' @importFrom stats var sd
#' @export
#' @examples
#' v <- c(1, 2, 3, 4, 5)
#' standard_deviation(v)
#' standard_deviation(v, for.sample = FALSE)
setGeneric("standard_deviation", function(x, na.rm = FALSE, for.sample = TRUE) {
  standardGeneric("standard_deviation")
})

#' @describeIn standard_deviation Method for numeric
setMethod("standard_deviation", "numeric", function(x, na.rm = FALSE, for.sample = TRUE) {
  tryCatch(
    {
      if (for.sample) {
        return(sd(x, na.rm = na.rm))
      } else {
        return(sqrt(var(x, na.rm = na.rm)))
      }
    },
    error = function(e) {
      message("Error in standard deviation calculation : ", e$message)
      return(NA_real_)
    })
})




