#' Split Data Frame
#'
#' This function splits a data frame based on a specified condition.
#'
#' @param x A data frame to be split.
#' @param condition A logical vector indicating the rows to include in the split.
#' @param select An optional vector specifying the columns to include in the split.
#'
#' @return A data frame representing the split result based on the condition.
#'
#' @examples
#' data <- data.frame(ID = 1:5, Age = c(25, 30, 22, 35, 28),
#'  Gender = c("Male", "Female", "Male", "Female", "Male"))
#' split_data_frame(data, condition = data$Age > 25, select = c(1,2))
#'
#' @export
#'
#' @keywords split data frame condition selection
#'
#' @family data
#'
#' @rdname split_data_frame
#' @aliases split_data_frame
#' @name split_data_frame
#' @usage split_data_frame(x, condition = NULL, select = NULL)
split_data_frame = function(x,condition = NULL,select = NULL)
{
  output = NULL
  if(!is.data.frame(x))
  {
    stop("\nThe argument x must be a data frame.\n")
  } else {
    if(is.null(condition)) condition = rep(TRUE,nrow(x))
    if(is.logical(condition))
    {
      if(is.null(select))
      {
        output = x[condition,]
      } else if(is.numeric(select)) {
        output = x[condition,select]
      } else if(!is.numeric(select)) {
        stop("\nThe select argument must be a vector that contain the index of variable into the dataframe.\n")
      }
    } else {
      stop("\nThe condition must be a logical object.\n")
    }
  }
  return(output)
}








#' Split Data Frame by Structure
#'
#' This function splits a data frame into quantitative and qualitative data based on variable types.
#'
#' @param x A data frame to be split by structure.
#'
#' @return A list containing the quantitative data, qualitative data, variable list, and structure vector.
#'
#' @examples
#' data <- data.frame(ID = 1:5, Age = c(25, 30, 22, 35, 28),
#' Gender = c("Male", "Female", "Male", "Female", "Male"))
#' split_df_by_structure(data)
#'
#' @export
#'
#' @keywords split data frame structure quantitative qualitative
#'
#' @family data
#'
#' @rdname split_df_by_structure
#' @aliases split_df_by_structure
#' @name split_df_by_structure
#' @usage split_df_by_structure(x)
split_df_by_structure = function(x)
{
  output = NULL
  dataquant = NULL
  dataqual = NULL
  default_type = c("numeric","non_numeric")
  structure_vector = NULL

  if(!is.data.frame(x)) stop("\nThe argument x must be a data frame.\n")
  else {
    varname = colnames(x)
    for(i in 1:ncol(x))
    {
      if(is.numeric(x[,i])) type = default_type[1]
      else type = default_type[2]
      structure_vector = c(structure_vector,type)
    }
    indexQuant = which(structure_vector == default_type[1])
    indexQual = which(structure_vector == default_type[2])

    if(length(indexQuant) == 1)
    {
      dataquant = as.data.frame(split_data_frame(x,select = indexQuant))
      colnames(dataquant) = varname[indexQuant]
    } else if(length(indexQuant)>1) {
      dataquant = split_data_frame(x,select = indexQuant)
    }
    if(length(indexQual) == 1)
    {
      dataqual = as.data.frame(split_data_frame(x,select = indexQual))
      colnames(dataqual) = varname[indexQual]
    } else if(length(indexQual)>1) {
      dataqual = split_data_frame(x,select = indexQual)
    }
  }
  return(list(`Quantitative data` = dataquant,`Qualitative data` = dataqual,`Varlist` = varname,`Structure` = structure_vector))
}

