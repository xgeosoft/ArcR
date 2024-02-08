#' Perform analysis by group
#'
#' This function splits a dataset based on a grouping variable and applies a specified function to each group.
#'
#' @param dataset The input dataset to be analyzed.
#' @param grouping_variable The variable used for grouping the dataset.
#' @param FUN The function to be applied to each group. Default is 'resumedf'.
#'
#' @return A list containing the analysis results and the split data.
#'
#' @examples
#' # Example usage:
#' # result <- by_group(my_dataset, my_grouping_variable, my_custom_function)
#'
#' @export
by_group = function(dataset,grouping_variable,FUN = resumedf){
  level = as.character(unique(grouping_variable))
  data = list()
  analysis = list()
  for(i in 1:length(level)){
    current_data = split_data_frame(dataset,grouping_variable == level[i])
    data = c(data,list(current_data))
    analysis = c(analysis,list(try(FUN(current_data),silent = TRUE)))
  }
  names(data) = paste("Group of",level)
  names(analysis) = paste("Group of",level)

  return(list(Result = analysis,Data = data))
}


