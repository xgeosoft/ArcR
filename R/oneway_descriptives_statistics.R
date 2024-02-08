#' Calculate Standard Deviation
#'
#' This function calculates the standard deviation of a given numeric vector.
#'
#' @param x A numeric vector.
#' @param na.rm Logical, indicating whether missing values should be removed.
#' @param for.sample Logical, indicating whether to calculate the sample standard deviation.
#'
#' @return The standard deviation of the input vector.
#' @importFrom stats var sd
#'
#' @examples
#' # Calculate standard deviation for the entire population
#' standard_deviation(c(1, 2, 3, 4, 5))
#'
#' # Calculate sample standard deviation
#' standard_deviation(c(1, 2, 3, 4, 5), for.sample = TRUE)
#'
#' @seealso \code{\link{var}}, \code{\link{sd}}
#'
#' @export
standard_deviation <- function(x,na.rm = FALSE,for.sample = TRUE) {
  if(for.sample) return(sd(x,na.rm = na.rm))
  else return(sqrt(var(x,na.rm = na.rm)))
}





#' Coefficient of Variation
#'
#' This function calculates the coefficient of variation, a measure of relative variability,
#' for a given numeric vector.
#'
#' @param x A numeric vector for which the coefficient of variation is calculated.
#' @param na.rm Logical, indicating whether missing values should be removed from the data.
#' @param for.sample Logical, indicating whether the calculation should be based on the sample standard deviation (TRUE) or population standard deviation (FALSE).
#'
#' @return The coefficient of variation.
#'
#' @details The coefficient of variation is the ratio of the standard deviation to the mean,
#' providing a normalized measure of dispersion.
#'
#' @examples
#' data <- c(23, 21, 18, 30, 25, 28, 20, 16, 24, 22)
#' Coefficient_Variation(data)
#'
#' @export
#'
#' @seealso \code{\link{mean}}, \code{\link{sd}}
#'
#' @keywords coefficient variation standard deviation mean
#'
#' @family statistics
#'
#' @rdname Coefficient_Variation
#' @aliases Coefficient_Variation
#' @name Coefficient_Variation
#' @usage Coefficient_Variation(x, na.rm = FALSE, for.sample = TRUE)
Coefficient_Variation = function(x,na.rm = FALSE,for.sample = TRUE)
{
  return(standard_deviation(x,
                            for.sample = for.sample,
                            na.rm = na.rm)/mean(x,na.rm = na.rm))
}

#' Custom Frequency Table
#'
#' This function generates a custom frequency table for one or two variables in a data frame or a combination of vectors.
#' If only one variable is provided, it generates a frequency table with counts and percentages.
#' If two variables are provided, it generates a contingency table with absolute or relative frequencies.
#'
#' @param x A vector or data frame column for which to generate the frequency table. If two variables are provided, this represents the rows of the contingency table.
#' @param y A vector or data frame column. If provided, it represents the columns of the contingency table.
#' @param type A character vector indicating the type of frequencies to compute. Possible values are "absoluts" for absolute frequencies and "relatives" for relative frequencies (default is c("absoluts", "relatives")).
#' @param var.names A character vector of length 2 indicating the names for the rows and columns of the contingency table (optional).
#' @param digits Number of digits to round the output table values (default is 3).
#' @param sum_of_relative_frequency The sum of relative frequency for percent calculation (default is 100).
#'
#' @return A custom frequency table with counts, percentages, and sum of relative frequencies.
#'
#' @examples
#' data_vector <- c("A", "B", "A", "C", "B", "A")
#' table_r(data_vector, var.names = "Category")
#'
#' @export
#'
#' @keywords frequency table percent data frame vector
#'
#' @family data
#'
#' @rdname table_r
#' @aliases table_r
#' @name table_r
table_r = function(x,y = NULL,type = c("absoluts","relatives"),var.names = NULL,digits = 3,sum_of_relative_frequency = 100)
{
  if(is.null(y)){
    output = table(x)
    output = rbind(output,prop.table(output)*sum_of_relative_frequency)
    output = addmargins(output)[-3,]
    row.names(output) = c("Frequency","Percent")

    if(is.null(var.names)) var.names = deparse(substitute(x))
    else if(is.character(var.names)) colnames(output) = paste(var.names,colnames(output), sep = " :: ")
    else stop("\nThe argument var.names must be a character that indicate the name of the variable.\n")

  } else {
    output = t(table(x,y))
    if(type[1] == "absoluts"){

    } else if(type[1] == "relatives"){
      output = prop.table(output) * sum_of_relative_frequency
    } else stop("Invalid type! choose absoluts or relatives.")

    output = addmargins(output)
    if(is.null(var.names)) var.names = deparse(substitute(x))
    else if(is.character(var.names)){
      if(length(var.names) == 2){
        rownames(output) = paste(var.names[2],rownames(output), sep = " :: ")
        colnames(output) = paste(var.names[1],colnames(output), sep = " :: ")
      }
    } else stop("\nThe argument var.names must be a character that indicate the name of the variable.\n")

  }
  return(t(round(output,digits = digits)))
}







#' Calculate the modal value of a variable.
#'
#' This function takes a vector \code{x} and computes the modal value,
#' which is the most frequently occurring value in the dataset.
#'
#' @param x A vector or column from a data frame.
#' @param na.rm Logical, indicating whether to remove missing values before calculation.
#'   Defaults to \code{TRUE}.
#'
#' @return The modal value of the input vector.
#'
#' @examples
#' modal_value(c(1, 2, 2, 3, 3, 3, 4)) # Returns 3
#'
#' @export
#'
#' @keywords frequency mode
#'
#' @seealso \code{\link{table}}, \code{\link{sort}}
#'
#' @family statistics
#' @family data manipulation
#' @family univariate analysis
#' @family descriptive statistics
#'
#'
#' @author Your Name
#' @examples
#' modal_value(c(1, 2, 2, 3, 3, 3, 4))
modal_value = function(x,na.rm = TRUE)
{
  return(names(sort(table(x),decreasing = TRUE))[1])
}




