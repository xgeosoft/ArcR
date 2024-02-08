#' Convert Numeric Values to Discrete Ranges
#'
#' This function converts numeric values to discrete ranges based on specified parameters.
#'
#' @param x A numeric vector to be converted to ranges.
#' @param amplitude The width of each range.
#' @param nb.cluster The number of clusters (ranges) to create.
#' @param start.by The starting value for the first range.
#' @param include.lower Logical, indicating whether the lower bound of the range should be inclusive.
#' @param include.upper Logical, indicating whether the upper bound of the range should be inclusive.
#' @param digits The number of digits to round the values to.
#'
#' @return A factor representing the discrete ranges for each element in the input vector.
#'
#' @examples
#' data <- c(1, 3, 5, 7, 9, 11, 13, 15)
#' numeric_to_range(data, amplitude = 3, nb.cluster = 4)
#'
#' @export
#'
#' @keywords numeric ranges discrete conversion
#'
#' @family data
#'
#' @rdname numeric_to_range
#' @aliases numeric_to_range
#' @name numeric_to_range
#' @usage numeric_to_range(x, amplitude = NULL, nb.cluster = NULL,
#'  start.by = NULL, include.lower = TRUE, include.upper = FALSE, digits = 3)
numeric_to_range = function(x,amplitude = NULL,nb.cluster = NULL,start.by = NULL,include.lower = TRUE,include.upper = FALSE,digits = 3)
{
  i = 1; max_val = max(x,na.rm = TRUE); output = rep(NA,length(x))

  if(is.null(start.by))
  {
    start.by = min(x,na.rm = TRUE)
  } else if(!is.numeric(start.by)) {
    stop("\nThe argument start.by must be a numeric value.\n")
  }

  if(is.null(nb.cluster))
  {
    nb.cluster = 5
  } else if(nb.cluster <= 0) {
    stop("\nThe argument nb.cluster must be a positive value not equal to zero.\n")
  }

  if(is.null(amplitude))
  {
    amplitude = (max_val-start.by)/nb.cluster
  }
  else if(amplitude <= 0)
  {
    stop("\nThe argument amplitude must be a positive value not equal to zero.\n")
  }

  while(start.by<max_val)
  {
    start.by = round(start.by,digits)
    upper.value = round(start.by + amplitude,digits)
    if(start.by + amplitude >= max_val)
    {
      include.upper = TRUE
    }

    if(include.lower & include.upper)
    {
      output[x>=start.by & x <=upper.value] = paste("[",start.by,",",upper.value,"]",sep = "")
    } else if(include.lower & !include.upper) {
      output[x>=start.by & x <upper.value] = paste("[",start.by,",",upper.value,")",sep = "")
    } else if(!include.lower & include.upper) {
      output[x>start.by & x <=upper.value] = paste("(",start.by,",",upper.value,"]",sep = "")
    } else if(!include.lower & !include.upper) {
      output[x>start.by & x <upper.value] = paste("(",start.by,",",upper.value,")",sep = "")
    }
    start.by = start.by + amplitude
  }
  cat("\nNote: The amplitude value is",amplitude,"\n")
  return(as.factor(output))
}

