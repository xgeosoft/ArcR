#' Coefficient of Variation (S4)
#'
#' Computes the coefficient of variation for a \code{numeric} object,
#' which is the ratio of the standard deviation to the mean.
#'
#' @param x A \code{numeric} object.
#' @param na.rm Logical, whether to remove missing values.
#' @param for.sample Logical, whether to use sample standard deviation (default) or population.
#'
#' @return A numeric value representing the coefficient of variation.
#' @export
#' @examples
#' vec <- c(10, 15, 20, 25, 30)
#' CV(vec)
#' CV(vec, for.sample = FALSE)
setGeneric("CV", function(x, na.rm = FALSE, for.sample = TRUE) {
  standardGeneric("CV")
})

#' @describeIn CV Method for numeric
setMethod("CV", "numeric", function(x, na.rm = FALSE, for.sample = TRUE) {
  tryCatch(
    {
      sd_val <- standard_deviation(x, na.rm = na.rm, for.sample = for.sample)
      mean_val <- mean(x, na.rm = na.rm)
      return(sd_val / mean_val)
    },
  error = function(e) {
    message("Error in coefficient of variation calculation : ", e$message)
    return(NA_real_)
  })
})


