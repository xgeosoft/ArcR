

#' Codebook for Data Frame
#'
#' This function generates a codebook for a given data frame, including variable information.
#'
#' @param x A data frame for which to generate the codebook.
#' @param show.numeric.values Logical indicating whether to show unique values for numeric variables.
#'
#' @return A codebook data frame with information about variables, their structure, and unique values.
#'
#' @examples
#' data_frame <- data.frame(Age = c(25, 30, 22, 35, 28),
#' Gender = c("Male", "Female", "Male", "Female", "Male"))
#' codebook_r(data_frame)
#'
#' @export
#'
#' @keywords codebook variable information data frame
#'
#' @family data
#'
#' @rdname codebook_r
#' @aliases codebook_r
#' @name codebook_r
#' @usage codebook_r(x, show.numeric.values = FALSE)
codebook_r = function(x,show.numeric.values = FALSE)
{
  if(!is.data.frame(x)) stop("\nThe argument x must be a data frame.\n")
  else {
    varname = colnames(x)
    namecb = c("Varname","Replace_varnames_by","Structure","Number_of_level","Levels","Remplace_levels_by")
    outputcb = data.frame(V1 = NA,V2 = NA,V3 = NA,
                          V4 = NA,V5 = NA,V6 = NA)
    for(i in 1:ncol(x))
    {
      current_rowcb = data.frame(V1 = NA,V2 = NA,V3 = NA,
                                 V4 = NA,V5 = NA,V6 = NA)
      current_var = unlist(x[,i])
      unique_values = sort(as.character(unique(current_var)))
      length_unique_values = length(unique_values)

      current_rowcb[1,1] = varname[i]
      current_rowcb[1,2] = varname[i]
      current_rowcb[1,3] = if(is.numeric(current_var)) {"numeric"} else {"non_numeric"}
      current_rowcb[1,4] = length_unique_values

      if(!show.numeric.values & is.numeric(current_var)) {length_unique_values=1;unique_values = NA}


      if(length_unique_values>= 1)
      {
        for(j in 1:length_unique_values)
        {
          if(j>=2)
          {
            current_rowcb[1,1] = NA
            current_rowcb[1,2] = NA
            current_rowcb[1,3] = NA
            current_rowcb[1,4] = NA_character_
          }
          current_rowcb[1,5] = unique_values[j]
          current_rowcb[1,6] = unique_values[j]
          outputcb = rbind(outputcb,as.data.frame(current_rowcb))
        }
      }
    }
    cat("\n===================================================================\n
    NOTE : If you want to recode, relevel or rename automatically variables\n
        of your datasets, use `codebook_editor()` function to edit your dictionary\n
        and modify only the replacement column to setting your modifications.\n
        After, store the modified ditionnary into an object and \n
        use `apply_codebook_to_df()` function to apply your new dictionnary on the datasets.
        \n===================================================================\n

        ")
    colnames(outputcb) = namecb
    return(print(outputcb[-1,],na.print = ""))
  }
}







#' Apply Codebook to Data Frame
#'
#' This function applies a modified codebook to a given data frame, recoding variables based on the codebook.
#'
#' @param data_frame The data frame to which the codebook should be applied.
#' @param new_modified_codebook The modified codebook containing information on variable recoding.
#' @param infer_var_structure Logical indicating whether to infer the variable structure after recoding.
#'
#' @return A data frame with variables recoded based on the modified codebook.
#'
#' @examples
#' data_frame <- data.frame(Age = c(25, 30, 22, 35, 28),
#' Gender = c("Male", "Female", "Male", "Female", "Male"))
#'
#' @export
#'
#' @keywords apply codebook recode data frame variable recoding
#'
#' @family data
#'
#' @rdname apply_codebook_to_df
#' @aliases apply_codebook_to_df
#' @name apply_codebook_to_df
#' @usage apply_codebook_to_df(data_frame, new_modified_codebook, infer_var_structure = TRUE)
apply_codebook_to_df = function(data_frame,new_modified_codebook,infer_var_structure = TRUE)
{
  if(is.null(data_frame) | is.null(new_modified_codebook)) stop("\nNul argument not allowed.\n")
  else {
    var_o_index = which(colnames(new_modified_codebook) %in% "Varname")
    var_r_o_index = var_o_index + 1
    number_of_level_o_index = var_o_index + 3
    level_o_index = var_o_index + 4
    level_r_o_index = var_o_index + 5

    data_varnames = colnames(data_frame)
    for(i in 1:nrow(new_modified_codebook))
    {
      index_var_finding = which(data_varnames %in% new_modified_codebook[i,var_o_index])
      if(length(index_var_finding) > 0)
      {
        number_of_levels = as.numeric(new_modified_codebook[i,number_of_level_o_index])
        data_frame[,index_var_finding] = recode_var(data_frame[,index_var_finding],
                                                    t(new_modified_codebook[i:(i+number_of_levels-1),level_o_index]),
                                                    t(new_modified_codebook[i:(i+number_of_levels-1),level_r_o_index]))
        data_varnames[index_var_finding] = new_modified_codebook[i,var_r_o_index]
      }
    }
    colnames(data_frame) = data_varnames
    if(infer_var_structure) data_frame = impute_type(data_frame)
    return(data_frame)
  }
}







#' Edit and Save Codebook Dictionary
#'
#' This function allows you to interactively edit a codebook dictionary and save the
#' modified version to a new file.
#'
#' @param x The codebook dictionary to be edited.
#' @param save_file To save the new dictionary, if TRUE.
#'
#' @return A modified codebook dictionary.
#' @importFrom utils edit
#'
#' @details This function opens an interactive editor for the input codebook dictionary (`x`),
#' allowing you to make changes. After editing, the modified dictionary is saved as a new
#' file with a name following the pattern "new_<input_filename>.rds".
#'
#' @examples
#' # Example usage:
#' my_codebook = codebook_r(iris)
#' #edited_codebook <- codebook_editor(my_codebook,save_file = FALSE)
#'
#' @export
#'
#' @seealso Use \code{\link{edit}} to open the interactive editor.
#' @seealso Use \code{\link{saveRDS}} to save R objects in a serialized format.
#'
#' @references Provide any relevant references or citations.
#'
#' @keywords codebook editing interactive saveRDS
#'
codebook_editor = function(x,save_file = TRUE)
{
  new_dico = edit(x)
  name = paste("new_",deparse(substitute(x)),".rds",sep = "")
  if(save_file)
  {
    saveRDS(new_dico,name)
    cat("\nThe modified dictionary is stored at: ",paste(getwd(),"/",name,sep = ""),"\n")
  }
  return(new_dico)
}











#' Describe Data Frame
#'
#' This function generates a summary description for each variable in a data frame.
#'
#' @param x A data frame for which to generate the variable description.
#' @param show.numeric.values Logical indicating whether to show unique values for numeric variables.
#' @param short_variable_length The maximum length for short variable names.
#'
#' @return A data frame with a summary description for each variable.
#'
#' @examples
#' data_frame <- data.frame(Age = c(25, 30, 22, 35, 28),
#' Gender = c("Male", "Female", "Male", "Female", "Male"))
#' describe_df(data_frame)
#'
#' @export
#'
#' @keywords describe summary data frame variable description
#'
#' @family data
#'
#' @rdname describe_df
#' @aliases describe_df
#' @name describe_df
#' @usage describe_df(x, show.numeric.values = FALSE, short_variable_length = 20)
describe_df = function(x,show.numeric.values = FALSE,short_variable_length = 20)
{
  if(!is.data.frame(x)) stop("\nThe argument x must be a data frame.\n")
  else {
    varname = colnames(x)
    namecb = c("Short_variable_name","Structure","Number_of_level","Levels","Value")
    outputcb = data.frame(V1 = NA,V2 = NA,V3 = NA,V4 = NA,V5 = NA)

    for(i in 1:ncol(x))
    {
      current_rowcb = data.frame(V1 = NA,V2 = NA,V3 = NA,V4 = NA,V5 = NA)
      current_var = unlist(x[,i])
      unique_values = sort(as.character(unique(current_var)))
      length_unique_values = length(unique_values)

      current_rowcb[1,1] = substring(varname[i],1,short_variable_length)
      current_rowcb[1,2] = if(is.numeric(current_var)) {"numeric"} else {"non_numeric"}
      current_rowcb[1,3] = length_unique_values
      if(!show.numeric.values & is.numeric(current_var)) {length_unique_values=1;unique_values = "..."}

      if(length_unique_values>= 1)
      {
        for(j in 1:length_unique_values)
        {
          if(j>=2)
          {
            current_rowcb[1,1] = NA_character_
            current_rowcb[1,2] = NA_character_
            current_rowcb[1,3] = NA_character_
          }
          current_rowcb[1,4] = unique_values[j]
          current_rowcb[1,5] = length(which(current_var == unique_values[j]))
          outputcb = rbind(outputcb,as.data.frame(current_rowcb))
        }
      }
    }
    colnames(outputcb) = namecb
    return(print(outputcb[-1,],na.print = "",zero.print = ""))
  }
}

