

#' Recode Variable
#'
#' This function recodes values in a variable based on specified old and new vectors.
#'
#' @param x A vector (numeric, character, or factor) to be recoded.
#' @param old_vector A vector containing the old values to be replaced.
#' @param new_vector A vector containing the corresponding new values.
#'
#' @return A vector with values recoded based on the specified mapping.
#'
#' @examples
#' data <- data.frame(Gender = c("Male", "Female", "Male", "Female", "Male"))
#' recode_var(data$Gender, old_vector = "Male", new_vector = "M")
#'
#' @export
#'
#' @keywords recode variable transformation mapping
#'
#' @family data
#'
#' @rdname recode_var
#' @aliases recode_var
#' @name recode_var
#' @usage recode_var(x, old_vector, new_vector)
recode_var = function(x,old_vector,new_vector)
{
  if(is.null(old_vector)) stop("\nNul vector not allowed.\n")
  if(is.null(new_vector)) stop("\nNul vector not allowed.\n")
  if(is.null(x)) stop("\nNul vector not allowed.\n")
  else {
    if(!(length(old_vector)==length(new_vector))) stop("\nOld and New vector should have the same length.\n")
    else {
      nameX = deparse(substitute(x))
      if(is.factor(x)) x = as.character(x)
      output = x

      for(i in 1:length(old_vector))
      {
        indexReplacement = which(x %in% old_vector[i])
        if(length(indexReplacement)>0)
        {
          output[indexReplacement] = new_vector[i]
        } else {
          cat("\n- We didn't found for variable",nameX,"level named as: ",old_vector[i],"\n")
        }
      }
    }
  }
  return(output)
}


