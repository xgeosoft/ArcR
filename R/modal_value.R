#' Modal Value (S4)
#'
#' Computes the mode (most frequent value) of a \code{vector} object.
#'
#' @param x A \code{vector} object.
#' @param na.rm Logical, whether to remove missing values before computation. Defaults to \code{TRUE}.
#'
#' @return The most frequent (modal) value in the vector.
#' @export
#'
#' @examples
#' v <- c(1, 2, 2, 3, 3, 3, 4)
#' modal_value(v)
setGeneric("modal_value", function(x, na.rm = TRUE) {
  standardGeneric("modal_value")
})

#' @describeIn modal_value Method
setMethod("modal_value", "vector", function(x, na.rm = TRUE) {
  tryCatch(
    {
      if (na.rm) x <- x[!is.na(x)]
      freq <- table(x)
      return(as.numeric(names(freq)[which.max(freq)]))
    },
  error = function(e) {
    message("Error in modal value calculation : ", e$message)
    return(NA_real_)
  })
})
