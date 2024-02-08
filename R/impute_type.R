#' Impute Type
#'
#' This function infers the real type of variables and imputes missing values
#' accordingly. It can handle both vectors and data frames.
#'
#' @param x A vector or a data frame with missing values to be imputed based on variable types.
#' @param min.qual The number of unique values (levels) required to convert as a factor (default is 2).
#' @param ventilation.percentage The percentage of non-NA observations required for a variable to be retained. Variables with a percentage lower than this will be deleted. The default limit is 0.02 (2%).
#'
#' @return A vector or a data frame with missing values imputed based on the inferred variable types
#' @export
#' @examples
#' # Impute missing values in a data frame
#' data_frame <- data.frame(Age = c(25, NA, 30, 22, 35),
#'                          Gender = c("Male", "Female", NA, "Male", "Female"))
#' impute_type(data_frame)
impute_type = function(x,min.qual = 2,ventilation.percentage = 0.02)
{
  if(is.vector(x)) return(impute_type_vector(x,min.qual = min.qual))
  else if(is.data.frame(x)) return(impute_type_df(x,min.qual = min.qual,ventilation.percentage = ventilation.percentage))
  else stop("No treatement avalaible for type",class(x))
}









#' Impute Type for Vector
#'
#' This function infers the real type of a vector and imputes missing values accordingly.
#'
#' @param x A vector with missing values to be imputed based on variable types.
#' @param min.qual The number of unique values (levels) required to convert as a factor.
#'
#' @return A vector with missing values imputed based on the inferred variable type
#'
#' @examples
#' data_vector <- c(1, NA, 3, 4, NA)
#' impute_type_vector(data_vector)
impute_type_vector = function(x,min.qual = 2)
{
  if(!(is.numeric(min.qual) & min.qual>=0)) stop("\nThe argument min.qual must be a positive value\n")

  if(!is.vector(x)) stop("\nThe argument x must be a vector object.\n")
  else {
    output = NA
    varname = deparse(substitute(x))
    actual_var = x

    if(is.factor(actual_var) | is.numeric(actual_var)) { final_var = actual_var }
    else if(!is.numeric(actual_var))
    {
      converted_var = as.numeric(actual_var[!is.na(actual_var)])
      if(any(is.na(converted_var)))
      {
        #Quali var
        final_var = as.factor(actual_var)
      } else {
        #Quanti var
        final_var = as.numeric(actual_var)
      }
    }

    if(is.numeric(final_var) & min.qual>=length(unique(final_var)))
    {
      #min.qual
      final_var = as.factor(final_var)
    }

    output = final_var
  }
  return(output)
}




#' Impute Type for Data Frame
#'
#' This function infers the real type of variables in a data frame and imputes missing values accordingly.
#'
#' @param x A data frame with missing values to be imputed based on variable types.
#' @param min.qual The number of unique values (levels) required to convert as a factor.
#' @param ventilation.percentage For a variable, if the percentage of non-NA observations is less than this percentage, then that variable will be deleted. The default limit is 0.02 (2%).
#'
#' @return A data frame with missing values imputed based on the inferred variable types
#'
#' @examples
#' data_frame <- data.frame(Age = c(25, NA, 30, 22, 35),
#' Gender = c("Male", "Female", NA, "Male", "Female"))
#' impute_type_df(data_frame)

impute_type_df = function(x,min.qual = 2,ventilation.percentage = 0.02)
{
  if(!(is.numeric(min.qual) & min.qual>=0)) stop("\nThe argument min.qual must be a positive value\n")

  if(!is.data.frame(x)) stop("\nThe argument x must be a data frame.\n")
  else {
    output = data.frame(ID = 1:nrow(x))
    varname = colnames(x)
    i = 1
    while(i<=ncol(x))
    {
      actual_var = unlist(x[,i])

      if(is.factor(actual_var) | is.numeric(actual_var)) { final_var = actual_var }
      else if(!is.numeric(actual_var))
      {
        converted_var = as.numeric(actual_var[!is.na(actual_var)])
        if(any(is.na(converted_var)))
        {
          #Quali var
          final_var = as.factor(actual_var)
        } else {
          #Quanti var
          final_var = as.numeric(actual_var)
        }
      }

      if(is.numeric(final_var) & min.qual>=length(unique(final_var)))
      {
        #min.qual
        final_var = as.factor(final_var)
      }

      vector_under_ventilation = NULL
      if(length(which(!(is.na(final_var))))/length(final_var)<=ventilation.percentage & i>=2){
        vector_under_ventilation = c(vector_under_ventilation,i)
      }

      output = cbind(output,as.data.frame(final_var))
      i = i + 1
    }
  }
  output = output[,-1]
  colnames(output) = varname
  if(is.numeric(vector_under_ventilation)) output = output[,-vector_under_ventilation]

  return(output)
}


