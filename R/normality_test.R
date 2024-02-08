

#' Check for Normal Distribution
#'
#' This function checks whether a given vector follows a normal distribution using statistical tests.
#'
#' @param x A numeric vector to test for normal distribution.
#' @param show_all Logical indicating whether to show all p-values from tests.
#' @param alpha The significance level for the tests (default is 0.05).
#'
#' @return A logical indicating whether the vector follows a normal distribution.
#' @return If show_all is TRUE, a list containing alpha, the decision, and the p-value table.
#'
#' @examples
#' data_vector <- rnorm(100)
#' is_normal_distribution(data_vector)
#'
#' @export
#'
#' @keywords normal distribution statistical test hypothesis testing
#' @importFrom stats shapiro.test ks.test
#'
#' @family statistics
#'
#' @rdname is_normal_distribution
#' @aliases is_normal_distribution
#' @name is_normal_distribution
#' @usage is_normal_distribution(x, show_all = FALSE, alpha = 0.05)
is_normal_distribution = function(x,show_all = FALSE,alpha = 0.05)
{
  p_value_matrix = matrix(1,nrow = 2,ncol = 1)
  p_value_matrix[1,1] =  if(length(x)<= 5000 & length(x)>=3){shapiro.test(x)$p.value} else {1}
  p_value_matrix[2,1] = ks.test(x,y = "pnorm")$p.value

  colnames(p_value_matrix) = "p.value"
  rownames(p_value_matrix) = c("Shapiro-Wilk-Test","Kolmogorov-Smirnov-Test")

  if(!show_all) return(min((p_value_matrix[,1])) >= alpha)
  else return(list(`Alpha` = alpha,`Decision` = min(p_value_matrix[,1]) >= alpha,`P.VALUE.TABLE` = p_value_matrix))
}
