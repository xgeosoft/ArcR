#' Get Variable Index in Dataframe
#'
#' This function returns the indices of specified variables in a dataframe or matrix.
#'
#' @param dataframe The dataframe or matrix from which to extract variable indices.
#' @param variable_names A character vector specifying the names of the variables whose indices are to be retrieved.
#'
#' @return A numeric vector containing the indices of the specified variables in the dataframe or matrix.
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6), z = c(7, 8, 9))
#' get_variable_indexINdataframe(data, c("x", "z")) # Returns: 1 3
#'
#' @export
#'
get_variable_indexINdataframe = function(dataframe,variable_names){
  if(is.data.frame(dataframe) | is.matrix(dataframe)){
    return(which(colnames(dataframe) %in% variable_names))
  }
}



#' Compute Distance between Vectors A and B
#'
#' This function computes the distance between two vectors A and B using either Euclidean distance or absolute difference.
#'
#' @param A Numeric vector representing the first vector.
#' @param B Numeric vector representing the second vector.
#' @param method Character vector specifying the method to compute distance. It can be either "euclidean" (default) for Euclidean distance or "absolute" for absolute difference.
#'
#' @return The distance between vectors A and B based on the specified method. If the method is not recognized or if the lengths of A and B are not equal, NULL is returned.
#'
#' @examples
#' A <- c(1, 2, 3)
#' B <- c(4, 5, 6)
#' distanceAB(A, B) # Returns Euclidean distance: 5.196152
#' distanceAB(A, B, method = "absolut") # Returns absolute difference: 9
#'
#' @export
#'
distanceAB = function(A,B,method = c("eucludian","absolut")){
  if(length(A) == length(B)){
    AminusB = A-B
    if(method[1] == "eucludian") answer = as.numeric(sqrt(t(AminusB) %*% AminusB))
    else if(method[1] == "absolut") answer = as.numeric(sum(abs(AminusB)))
    else answer = NULL
    return(answer)
  }
}






#' Correct Table Values
#'
#' This function corrects values in a table based on specified criteria.
#'
#' @param table The table to be corrected.
#' @param limit Numeric value specifying the threshold for correction.
#' @param method Character vector specifying the method for correction. It can be "less" (default) for values less than the limit, "equal" for values equal to the limit, or "more" for values greater than the limit.
#' @param replace_by Numeric vector specifying the value to replace the corrected values with. If NULL (default), the corrected values remain unchanged. If NA_real_, the corrected values are replaced by NA. If a numeric value is provided, all corrected values are replaced by this value.
#'
#' @return The corrected table with values replaced based on the specified criteria.
#'
#' @examples
#' table <- matrix(1:9, nrow = 3)
#' correcttable(table, limit = 5)
#' # Returns the table with values less than 5 unchanged and other
#' #values replaced by NA
#' correcttable(table, limit = 5, method = "equal", replace_by = 10)
#' # Returns the table with values equal to 5 replaced by 10 and other
#' #values unchanged
#'
#' @export
#'
correcttable = function(table,limit,method = c("less","equal","more"),replace_by = NA_real_){
  for(i in 1:nrow(table)){
    for(j in 1:ncol(table)){
      if(method[1] == "less"){
        cond = table[i,j]<limit
      } else if(method[1] == "equal"){
        cond = table[i,j] == limit
      } else if(method[1] == "more"){
        cond = table[i,j] > limit
      } else stop("\nInvalid method\n")
      if(cond){
        if(is.null(replace_by)) table[i,j] = table[i,j]
        else if(is.na(replace_by)) table[i,j] = NA_real_
        else table[i,j] = replace_by[1]
      }
    }
  }
  return(table)
}

