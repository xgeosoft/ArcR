

#' The `search_replace_character` function allows users to search and replace characters within a given object.
#' It offers flexibility in handling multiple search terms and provides options for case sensitivity.
#'
#' @param object The character object to be manipulated.
#' @param search The character(s) to search for.
#' @param replace The replacement character(s).
#' @param full_case Logical, indicating whether to perform a full case-sensitive match.
#'
#' @return A modified character object after performing the search and replace operations.
#'
#' @examples
#' \dontrun{
#' # Create an environment
#' env_arcR <- new.env()
#'
#' # Example usage of search_replace_character
#' result <- search_replace_character("Hello World", search = "Hello",
#' replace = "Hi")
#' print(result)  # Output: "Hi World"
#' }
#'
#'
#' @keywords character manipulation search replace
#'
#' @author [Your Name]
#'
#' @seealso Other functions in the arcR package.
#'
#' @references
#' [Provide any references if applicable]
#'
#' @family arcR
#'
#' @note This package is under development. Feedback and contributions are welcome.
#'
#' @examples
#' \dontrun{
#' # Additional examples or use cases
#' }
search_replace_character = function(object,search = "",replace = NULL,full_case = FALSE)
{
  object_character = if(is.numeric(object)){as.numeric(object)}else{as.character(object)}

  if(is.na(object_character)) {object_nchar = 2; object_character = "NA"}
  else object_nchar = nchar(object_character)

  if(!is.null(replace)){
    if(is.na(replace)) {replace_nchar = 2; replace = "NA"}
    else replace_nchar = nchar(replace)
  }

  report = NULL
  report_name = NULL

  if(object_nchar>0)
  {
    for(multi_search_index in 1:length(search)){

      if(is.na(search[multi_search_index])) {search_nchar = 2 ; search[multi_search_index] = "NA"      }                  #NA resolution
      else search_nchar = nchar(search[multi_search_index])

      if(full_case){
        if(object_character == search){
          report = c(report,"OK")                      # report of research
          report_name = c(report_name,search[multi_search_index])                      # report name

          if(!is.null(replace) & is.character(replace)){
            object_character = replace
          }
        }
      } else {
        i = 0
        while(i <= object_nchar){
          #cat("\n i = ",i,"\n nchar = ",object_nchar)
          if(substring(object_character,i,i+search_nchar-1) == search[multi_search_index]){
            report = paste(report,paste(i,i+search_nchar-1,sep = "-"),sep = "|")                      # report of research
            report_name = paste(report_name,search[multi_search_index],"|")                      # report name

            if(!is.null(replace) & is.character(replace)){
              object_character = paste0(substring(object_character,0,i-1),replace,substring(object_character,i+search_nchar,object_nchar))
              i = i+replace_nchar-1
              object_nchar = nchar(object_character)
            }
          }
          i = i+1
        }
      }
    }
  } else {
    stop("\nInvalid object\n")
  }

  #==================
  if(is.null(replace)){
    if(is.null(report)){
      report = rep("Not found!",length(search))
      names(report) = search
    } else {
      names(report) = report_name
    }
    return(report)
  } else {
    return(object_character)
    }
}





#' Search and Replace in Vector
#'
#' The `search_replace_vector` function applies the `search_replace_character` function
#' to each element of a vector or factor, allowing users to perform search and replace
#' operations across multiple elements simultaneously.
#'
#' @param object A vector or factor of character elements to be manipulated.
#' @param search The character(s) to search for.
#' @param replace The replacement character(s).
#' @param full_case Logical, indicating whether to perform a full case-sensitive match.
#'
#' @return A vector with elements modified after performing the search and replace operations.
#'
#' @examples
#' \dontrun{
#' # Example usage of search_replace_vector
#' vector_result <- search_replace_vector(c("apple", "banana", "orange"),
#' search = "a", replace = "X")
#' print(vector_result)  # Output: "Xpple" "bXnXnX" "orXnge"
#' }
#'
#'
#' @keywords character manipulation search replace vector
#'
#' @seealso \code{\link{search_replace_character}}
#'
#' @family arcR
#'
#' @author [Your Name]
#'
#' @note This function is part of the arcR package under development. Feedback and contributions are welcome.
#'
#' @examples
#' \dontrun{
#' # Additional examples or use cases
#' }
search_replace_vector = function(object,search = "",replace = NULL,full_case = FALSE)
{
  print("Result of your research",quote = FALSE)
  object_character = NULL
  if(is.vector(object) | is.factor(object)){
    for(i in 1:length(object)){
      object_character[i] = search_replace_character(object = object[i],search = search,replace = replace,full_case = full_case)
    }
  }
  return(object_character)
}




#' Search and Replace in Data Frame
#'
#' The `search_replace_data_frame` function applies the `search_replace_vector` function
#' to each column of a data frame, allowing users to perform search and replace
#' operations across multiple columns simultaneously.
#'
#' @param object A data frame with character columns to be manipulated.
#' @param search The character(s) to search for.
#' @param replace The replacement character(s).
#' @param full_case Logical, indicating whether to perform a full case-sensitive match.
#'
#' @return A data frame with columns modified after performing the search and replace operations.
#'
#' @examples
#' \dontrun{
#' # Example usage of search_replace_data_frame
#' data_frame_result <-
#' search_replace_data_frame(data.frame(A = c("apple", "banana", "orange"),
#'                            B = c("apple", "banana", "orange")),
#'                        search = "a", replace = "X")
#' print(data_frame_result)
#' # Output:   ID     A     B
#' #         1 Xpple Xpple
#' #         2  bXnXnX bXnXnX
#' #         3 orXnge orXnge
#' }
#'
#'
#' @keywords character manipulation search replace data frame
#'
#' @seealso \code{\link{search_replace_vector}}
#'
#' @family arcR
#'
#' @author [Your Name]
#'
#' @note This function is part of the arcR package under development. Feedback and contributions are welcome.
#'
#' @examples
#' \dontrun{
#' # Additional examples or use cases
#' }
search_replace_data_frame = function(object,search = "",replace = NULL,full_case = FALSE)
{
  print("Result of your research",quote = FALSE)
  object_character = data.frame(ID = 1:nrow(object))
  if(is.data.frame(object)){
    for(i in 1:ncol(object)){
      object_character = cbind(object_character,as.data.frame(search_replace_vector(object = as.character(object[,i]),search = search,replace = replace,full_case = full_case)))
    }

    colnames(object_character) = c("ID",colnames(object))
    object_character = impute_type(object_character)
  }

  return(object_character[,-1])
}





#' Search and Replace in Objects
#'
#' The `search_r` function provides a unified interface for performing search and replace
#' operations on different types of R objects, including vectors, factors, and data frames.
#'
#' @param object The R object (vector, factor, or data frame) to be manipulated.
#' @param values The character(s) to search for.
#' @param full_case Logical, indicating whether to perform a full case-sensitive match.
#'
#' @return The modified object after performing the search and replace operations.
#'
#' @examples
#' \dontrun{
#' # Example usage of search_r with a vector
#' vector_result <- search_r(c("apple", "banana", "orange"), values = "a", full_case = TRUE)
#' print(vector_result)  # Output: "Xpple" "bXnXnX" "orXnge"
#'
#' # Example usage of search_r with a data frame
#' data_frame_result <- search_r(data.frame(A = c("apple", "banana", "orange"),
#'                                           B = c("apple", "banana", "orange")),
#'                                values = "a", full_case = TRUE)
#' print(data_frame_result)
#' # Output:     ID     A     B
#' #           1 Xpple Xpple
#' #           2  bXnXnX bXnXnX
#' #           3 orXnge orXnge
#' }
#'
#' @export
#'
#' @keywords character manipulation search replace
#'
#' @seealso \code{\link{search_replace_vector}}, \code{\link{search_replace_data_frame}}, \code{\link{search_replace_character}}
#'
#' @family arcR
#'
#' @author [Your Name]
#'
#' @note This function is part of the arcR package under development. Feedback and contributions are welcome.
#'
#' @examples
#' \dontrun{
#' # Additional examples or use cases
#' }
search_r = function(object,values,full_case = TRUE){
  if(is.data.frame(object)) return(search_replace_data_frame(object = object,search = values,full_case = full_case))
  else if(is.vector(object) | is.factor(object)) return(search_replace_vector(object = object,search = values,full_case = full_case))
  else return(search_replace_character(object = object,search = values,full_case = full_case))
}



#' Replace Values in Objects
#'
#' The `replace_r` function offers a unified approach for replacing values in different types
#' of R objects, including vectors, factors, and data frames.
#'
#' @param object The R object (vector, factor, or data frame) to be manipulated.
#' @param values The character(s) to search for.
#' @param replace_by The replacement character(s).
#' @param full_case Logical, indicating whether to perform a full case-sensitive match.
#'
#' @return The modified object after performing the search and replace operations.
#'
#' @examples
#' \dontrun{
#' # Example usage of replace_r with a vector
#' vector_result <- replace_r(c("apple", "banana", "orange"), values = "a",
#' replace_by = "X", full_case = TRUE)
#' print(vector_result)  # Output: "Xpple" "bXnXnX" "orXnge"
#'
#' # Example usage of replace_r with a data frame
#' data_frame_result <- replace_r(data.frame(A = c("apple", "banana", "orange"),
#'                                          B = c("apple", "banana", "orange")),
#'                             values = "a", replace_by = "X", full_case = TRUE)
#' print(data_frame_result)
#' # Output:     ID     A     B
#' #           1 Xpple Xpple
#' #           2  bXnXnX bXnXnX
#' #           3 orXnge orXnge
#' }
#'
#' @export
#'
#' @keywords character manipulation search replace
#'
#' @seealso \code{\link{search_replace_vector}}, \code{\link{search_replace_data_frame}}, \code{\link{search_replace_character}}
#'
#' @family arcR
#'
#' @author [Your Name]
#'
#' @note This function is part of the arcR package under development. Feedback and contributions are welcome.
#'
#' @examples
#' \dontrun{
#' # Additional examples or use cases
#' }
replace_r = function(object,values,replace_by,full_case = TRUE){
  if(is.data.frame(object)) return(search_replace_data_frame(object = object,search = values,replace = replace_by,full_case = full_case))
  else if(is.vector(object) | is.factor(object)) return(search_replace_vector(object = object,search = values,replace = replace_by,full_case = full_case))
  else return(search_replace_character(object = object,search = values,replace = replace_by,full_case = full_case))
}



