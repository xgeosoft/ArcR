#' Complete Disjunctive Table
#'
#' This function creates a complete disjunctive table (CDT) from a vector, factor, or data frame.
#'
#' @param x A vector, factor, or data frame for which to create the complete disjunctive table.
#' @param add.current.df Logical, indicating whether to add the CDT columns to the input data frame.
#' @param return.numeric Logical, indicating whether the CDT columns should be returned as numeric or factor.
#'
#' @return A data frame representing the complete disjunctive table.
#'
#' @examples
#' # Example with a data frame
#' data <- data.frame(A = c("High", "Low", "Medium"), B = c("Red", "Green", "Blue"))
#' complete_disjunctive_table(data, add.current.df = TRUE)
#'
#' # Example with a vector
#' vector_data <- c("Male", "Female", "Male", "Female")
#' complete_disjunctive_table(vector_data)
#'
#' @export
#'
#' @keywords disjunctive table complete categorical conversion
#'
#' @family data
#'
#' @rdname complete_disjunctive_table
#' @aliases complete_disjunctive_table
#' @name complete_disjunctive_table
#' @usage complete_disjunctive_table(x, add.current.df = FALSE, return.numeric = TRUE)
complete_disjunctive_table = function(x,add.current.df = FALSE,return.numeric = TRUE)
{
  if(is.data.frame(x) | is.matrix(x))
  {
    outputcdt = data.frame(ID = 1:nrow(x))
    varname = colnames(x)
    cdt_varname = NULL
    for(i in 1:ncol(x))
    {
      current_var = x[,i]
      unique_values = unique(current_var)

      for(j in 1:length(unique_values))
      {
        current_cdt_column = rep(0,length(current_var))
        current_level = unique_values[j]
        cdt_varname = c(cdt_varname,paste(varname[i],"==",current_level))
        current_cdt_column[current_var == current_level] = 1

        if(!return.numeric) current_cdt_column = as.factor(current_cdt_column)

        outputcdt = cbind(outputcdt,as.data.frame(current_cdt_column))
      }
    }
    colnames(outputcdt) = c("ID",cdt_varname)
    if(add.current.df) outputcdt = cbind(x,outputcdt)

  } else if(is.vector(x) | is.factor(x)) {
    outputcdt = data.frame(ID = 1:length(x))
    varname = deparse(substitute(x))
    cdt_varname = NULL

    current_var = x
    unique_values = unique(current_var)

    for(j in 1:length(unique_values))
    {
      current_cdt_column = rep(0,length(current_var))
      current_level = unique_values[j]
      cdt_varname = c(cdt_varname,paste(varname,"==",current_level))
      current_cdt_column[current_var == current_level] = 1

      if(!return.numeric) current_cdt_column = as.factor(current_cdt_column)

      outputcdt = cbind(outputcdt,as.data.frame(current_cdt_column))
    }
    colnames(outputcdt) = c("ID",cdt_varname)
    if(add.current.df) outputcdt = cbind(x,outputcdt)

  } else {
    stop("\nThe argument x must be a vector or a data frame or a matrix.\n")
  }
  return(outputcdt[,-1])
}






#' Burt Table
#'
#' This function generates a Burt table from a given data frame.
#'
#' @param x A data frame for which to generate the Burt table.
#'
#' @return A matrix representing the Burt table.
#'
#' @examples
#' data <- data.frame(A = c("High", "Low", "Medium"),
#' B = c("Red", "Green", "Blue"))
#' burt_table(data)
#'
#' @export
#'
#' @keywords Burt table binary association matrix
#'
#' @family data
#'
#' @rdname burt_table
#' @aliases burt_table
#' @name burt_table
#' @usage burt_table(x)
burt_table = function(x)
{
  if(!is.data.frame(x)) stop("\nThe argument x must be a data frame.\n")
  else {
    cdt = as.matrix(complete_disjunctive_table(x))
    return(t(cdt)%*%cdt)
  }
}



