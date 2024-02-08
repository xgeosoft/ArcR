

#' Calculate Correlation
#'
#' This function calculates the correlation between two vectors or a correlation matrix for a given data frame using different correlation methods.
#'
#' @param x If y is NULL, x can be either a numeric vector, matrix, or data frame. If y is provided, x is the first numeric vector.
#' @param y If provided, it is the second numeric vector for correlation. If NULL, x is treated as a matrix or data frame to calculate the correlation matrix.
#' @param method The method for calculating correlation. "auto" uses Pearson if both vectors are normally distributed; otherwise, it chooses between Spearman and Kendall. Alternatively, specify "spearman", "kendall", or "pearson".
#' @param show_all Logical indicating whether to show the used methods along with the correlation values (default is TRUE).
#'
#' @return A correlation object containing correlation coefficient, p-value, and method information for vectors, or a correlation matrix for data frames.
#'
#' @examples
#' vector_x <- c(1, 2, 3, 4, 5)
#' vector_y <- c(2, 4, 6, 8, 10)
#' correlation_r(vector_x, vector_y)
#'
#' data_frame <- data.frame(A = c(1, 2, 3, 4, 5),
#'  B = c(2, 4, 6, 8, 10), C = c(5, 4, 3, 2, 1))
#' correlation_r(data_frame)
#'
#' @export
#'
#' @keywords correlation statistical test correlation matrix
#'
#' @family statistics
#'
#' @rdname correlation_r
#' @aliases correlation_r
#' @name correlation_r
#' @usage correlation_r(x, y = NULL, method = "auto", show_all = TRUE)
correlation_r = function(x,y = NULL,method = "auto",show_all = TRUE)
{
  if(is.null(y)) {
    if(is.matrix(x) | is.data.frame(x)) return(correlation_r_df_matrix(x,method = method,show_all = show_all))
  } else {
    return(correlation_r_vector(x,y,method = method))
  }
}









#' Calculate Correlation
#'
#' This function calculates the correlation between two vectors using different methods.
#'
#' @param x The first numeric vector.
#' @param y The second numeric vector.
#' @param method The method for calculating correlation. "auto" uses Pearson if both vectors are normally distributed; otherwise, it chooses between Spearman and Kendall. Alternatively, specify "spearman", "kendall", or "pearson".
#'
#' @return A correlation object containing correlation coefficient, p-value, and method information.
#' @export
#' @examples
#' vector_x <- c(1, 2, 3, 4, 5)
#' vector_y <- c(2, 4, 6, 8, 10)
#' correlation_r_vector(vector_x, vector_y)
#'
#' @keywords correlation correlation coefficient statistical test
#'
#' @family statistics
#' @importFrom stats cor cor.test
#'
#' @rdname correlation_r_vector
#' @aliases correlation_r_vector
#' @name correlation_r_vector
#' @usage correlation_r_vector(x, y, method = "auto")
correlation_r_vector = function(x,y,method = "auto")
{
  normalX = is_normal_distribution(x)
  normalY = is_normal_distribution(y)

  if(method == "auto")
  {
    if(normalX & normalY) output = cor(x,y,method = "pearson",use = "complete.obs")
    else {
      spearman_rho = cor.test(x,y,method = "spearman",use = "complete.obs")
      kendall_rho = cor.test(x,y,method = "kendall",use = "complete.obs")
      if(is.na(spearman_rho$p.value) | is.na(kendall_rho$p.value)) { output = list(estimate = 0,p.value = 0)}
      else if(spearman_rho$p.value < kendall_rho$p.value) { output = spearman_rho}
      else {output = kendall_rho}
    }
  } else if(method %in% c("spearman","kendall","pearson")) {
    output =  cor.test(x,y,method = method,use = "complete.obs")
  }
  rm(list =c("kendall_rho","spearman_rho","normalY","normalX"))
  return(output)
}




#' Calculate Correlation Matrix
#'
#' This function calculates the correlation matrix for a given data frame using different correlation methods.
#'
#' @param x A numeric data frame.
#' @param method The method for calculating correlation. "auto" uses Pearson if both vectors are normally distributed; otherwise, it chooses between Spearman and Kendall. Alternatively, specify "spearman", "kendall", or "pearson".
#' @param show_all Logical indicating whether to show the used methods along with the correlation values (default is FALSE).
#'
#' @return A correlation matrix or a list containing the correlation matrix and the used methods if show_all is TRUE.
#'
#' @examples
#' data_frame <- data.frame(A = c(1, 2, 3, 4, 5),
#' B = c(2, 4, 6, 8, 10), C = c(5, 4, 3, 2, 1))
#' correlation_r_df_matrix(data_frame)
#'
#'
#' @keywords correlation correlation matrix statistical test
#'
#' @family statistics
#'
#' @export
#' @rdname correlation_r_df_matrix
#' @aliases correlation_r_df_matrix
#' @name correlation_r_df_matrix
#' @usage correlation_r_df_matrix(x, method = "auto", show_all = FALSE)
correlation_r_df_matrix = function(x,method = "auto",show_all = FALSE)
{
  output = matrix(NA,nrow = ncol(x),ncol = ncol(x))
  output_methods = matrix(NA,nrow = ncol(x),ncol = ncol(x))
  for(i in 1:ncol(x))
  {
    for(j in 1:ncol(x))
    {
      value = correlation_r_vector(x[,i],x[,j],method = method)
      output[i,j] = value$estimate
      output_methods[i,j] = paste(value$method,"P.value =",value$p.value)
    }
  }

  rownames(output) = colnames(x)
  colnames(output) = colnames(x)
  rownames(output_methods) = colnames(x)
  colnames(output_methods) = colnames(x)
  if(!show_all) return(output)
  else return(list(Correlation = output,Used_methods = output_methods))

}




