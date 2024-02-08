
#' Chi-Square Test
#'
#' This function performs a chi-square test of independence for two categorical variables or a chi-square goodness-of-fit test for a categorical variable against expected frequencies.
#'
#' @param x If y is NULL, x can be either a table, matrix, or data frame for a chi-square goodness-of-fit test. If y is provided, x and y should be categorical vectors for a chi-square test of independence.
#' @param y If provided, it is the second categorical vector for a chi-square test of independence. If NULL, x is treated as a table or data frame for a chi-square goodness-of-fit test.
#' @param alpha The significance level for the chi-square test (default is 0.05).
#'
#' @return A list containing the result of the chi-square test, including the chi-square statistic, degrees of freedom, p-value, and the test method.
#'
#' @examples
#' # Chi-square test of independence
#' variable1 <- c("A", "B", "A", "B", "A")
#' variable2 <- c("X", "Y", "X", "Y", "Z")
#' chisq_test_r(variable1, variable2)
#'
#' # Chi-square goodness-of-fit test
#' observed_freq <- c(15, 20, 25)
#' expected_freq <- c(10, 20, 30)
#' chisq_test_r(observed_freq, expected_freq)
#'
#' @export
#'
#' @keywords chi-square statistical test independence goodness-of-fit
#'
#' @family statistics
#'
#' @rdname chisq_test_r
#' @aliases chisq_test_r
#' @name chisq_test_r

#' @usage chisq_test_r(x, y = NULL, alpha = 0.05)
chisq_test_r = function(x,y = NULL,alpha = 0.05)
{
  if(!is.null(y))
  {
    output = chisq_test_r_vector(x,y,alpha = alpha)
  } else {
    if(is.data.frame(x) | is.matrix(x))
    {
      output = chisq_test_r_df_matrix(x,alpha = alpha)
    }
  }
  return(output)
}




#' Chi-Square Test for Categorical Vectors
#'
#' This function performs a chi-square test of independence for two categorical vectors.
#'
#' @param x The first categorical vector for the chi-square test.
#' @param y The second categorical vector for the chi-square test.
#' @param alpha The significance level for the chi-square test (default is 0.05).
#' @param time An optional parameter to specify the number of times the test is performed.
#'
#' @return A list containing the result of the chi-square test, including the decision (dependence or independence), the combination of variables being tested, and the p-value.
#'
#' @importFrom stats chisq.test
#' @export
#'
#' @examples
#' variable1 <- c("A", "B", "A", "B", "A")
#' variable2 <- c("X", "Y", "X", "Y", "Z")
#' chisq_test_r_vector(variable1, variable2)
#'
#'
#' @keywords chi-square statistical test independence categorical-vectors
#'
#' @family statistics
#'
#' @rdname chisq_test_r_vector
#' @aliases chisq_test_r_vector
#' @name chisq_test_r_vector
#' @usage chisq_test_r_vector(x, y, alpha = 0.05, time = 0)
chisq_test_r_vector = function(x,y,alpha = 0.05,time = 0)
{
  test = min(get_dimensions(table(x,y))$row,get_dimensions(table(x,y))$col)>=2
  p_value = if(test){chisq.test(table(x,y))$p.value}else{0}
  p_value = if(is.na(p_value)){1}else{p_value}
  output = list(Decision = if(p_value<=alpha){paste("Dependance between variables at ",alpha*100,"%",sep = "")}else{paste("Independance between variables at ",alpha*100,"%",sep = "")},
                Between = paste(deparse(substitute(x)),"-",deparse(substitute(y))),
                p.value = p_value)
  return(output)
}





#' Chi-Square Test for Categorical Data Frame or Matrix
#'
#' This function performs a chi-square test of independence for each pair of categorical variables in a data frame or matrix.
#'
#' @param x The categorical data frame or matrix for the chi-square tests.
#' @param alpha The significance level for the chi-square tests (default is 0.05).
#'
#' @return A matrix containing the results of chi-square tests for each pair of categorical variables, including the decision (dependence or independence) and the p-value.
#'
#' @examples
#' data_matrix <- data.frame(A = c("A", "B", "A", "B", "A"), B = c("X", "Y", "X", "Y", "Z"))
#' chisq_test_r_df_matrix(data_matrix)
#' @export
#'
#' @keywords chi-square statistical test independence categorical-data categorical-matrix
#'
#' @family statistics
#'
#' @rdname chisq_test_r_df_matrix
#' @aliases chisq_test_r_df_matrix
#' @name chisq_test_r_df_matrix

#' @usage chisq_test_r_df_matrix(x, alpha = 0.05)
chisq_test_r_df_matrix = function(x,alpha = 0.05)
{
  varname = colnames(x)
  output = matrix(NA,ncol = ncol(x),nrow = ncol(x))
  for(i in 1:ncol(x))
  {
    for(j in 1:ncol(x))
    {
      test = chisq_test_r_vector(x[,i],x[,j],alpha = alpha,time = i*j-1)
      output[i,j] = paste(test$Decision,"p.value:",test$p.value)
    }
  }
  colnames(output) = varname
  rownames(output) = varname
  return(output)
}
