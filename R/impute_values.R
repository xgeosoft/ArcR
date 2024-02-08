
#' Impute Missing Values in Objects - Multiple Methods
#'
#' The `impute_NA_values` function provides a flexible approach for imputing missing values in different types
#' of R objects using multiple imputation methods, including standard approaches and econometric modeling.
#'
#' @param object The R object (vector, factor, or data frame) containing missing values to be imputed.
#' @param formula A formula specifying the dependent and independent variables for econometric modeling methods.
#' @param data The data frame containing the variables specified in the formula for econometric modeling methods.
#' @param methods A character vector indicating the imputation methods to be used (e.g., "standard", "lm", "logit").
#'
#' @return The imputed object based on the chosen imputation method(s).
#'
#' @examples
#' \dontrun{
#' # Example usage of impute_NA_values
#' vector <- c(1, NA, 3, 4, NA, 6)
#' formula <- formula(dependent_var ~ explanatory_var1 + explanatory_var2)
#' data_frame <- data.frame(dependent_var = c(1, NA, 3, 4, NA, 6),
#'                          explanatory_var1 = c(2, 3, 1, 5, 4, 2),
#'                          explanatory_var2 = c("A", "B", "A", "B", "A", "B"))
#' result <- impute_NA_values(object = vector, methods = "standard")
#' result_econometric <- impute_NA_values(object = data_frame, formula = formula,
#'  data = data_frame, methods = "lm")
#' print(result)
#' print(result_econometric)
#' }
#'
#' @export
#'
#' @keywords missing values imputation multiple methods
#'
#' @seealso \code{\link{impute_NA_vector_standard}}, \code{\link{impute_NA_by_econometry}}
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
impute_NA_values = function(object,formula,data = NULL,methods = c("standard","lm","logit","poisson","quasipoisson","Gamma")){
  if(methods[1] == "standard"){
    if(is.data.frame(object)){
      output = impute_NA_data_frame_standard(object = object)
    } else {
      output = impute_NA_vector_standard(object = object)
    }
  } else if(methods[1] %in% c("lm","logit","poisson","quasipoisson","Gamma")) {
    if(is.null(data)) stop("\n",methods[1]," require a dataset. So specify data object.\n")
    else {
      output = impute_NA_by_econometry(formula = formula,data = data,USE = methods[1])
    }
  }
  return(output)
}



#' Impute Missing Values in Vector - Standard Approach
#'
#' The `impute_NA_vector_standard` function imputes missing values in a vector based on standard approaches.
#' Users can specify conditions for imputation, such as replacing NAs with the mode for character or factor vectors,
#' and with the mean for numeric vectors.
#'
#' @param object The vector containing missing values to be imputed.
#'
#' @return A vector with missing values imputed based on standard approaches.
#'
#' @examples
#' \dontrun{
#' # Example usage of impute_NA_vector_standard
#' vector <- c(1, NA, 3, 4, NA, 6)
#' result <- impute_NA_vector_standard(vector)
#' print(result)  # Output: 1 3 3 4 3 6 (NAs imputed with mean)
#' }
#'
#'
#' @keywords missing values imputation vector standard
#'
#' @author xgeosoft corporation
#'
#' @note This function is part of the arcR package under development. Feedback and contributions are welcome.
#'
#' @examples
#' \dontrun{
#' # Additional examples or use cases
#' }
impute_NA_vector_standard = function(object){
  conditions = is.na(object)
  if(is.logical(conditions) & length(conditions) == length(object)) concerned_index = which(conditions)
  else stop("Condition concerned the object. For example. To impute values where
            object is more than zero write :  object>0 , to impute value where
            vector contain NA write: is.na(object). ...")
  object_output = as.character(object)
  if(is.factor(object) | is.character(object)) object_output[concerned_index] = modal_value(object,na.rm = TRUE)
  else if(is.numeric(object)) {object[concerned_index] = mean(object,na.rm = TRUE) ; object_output = object}

  return(object_output)
}




#' Impute Missing Values in Data Frame - Standard Approach
#'
#' The `impute_NA_data_frame_standard` function imputes missing values in each column of a data frame
#' based on standard approaches defined in `impute_NA_vector_standard`.
#'
#' @param object The data frame containing missing values to be imputed.
#'
#' @return A data frame with missing values imputed based on standard approaches.
#'
#' @examples
#' \dontrun{
#' # Example usage of impute_NA_data_frame_standard
#' data_frame <- data.frame(A = c(1, NA, 3, 4, NA, 6),
#'                    B = c("apple", "banana", NA, "orange", "apple", "orange"))
#' result <- impute_NA_data_frame_standard(data_frame)
#' print(result)
#' # Output:   ID  A      B
#' #         1  1  apple
#' #         2  3 banana
#' #         3  3 banana
#' #         4  4 orange
#' #         5  3  apple
#' #         6  6 orange
#' }
#'
#'
#' @keywords missing values imputation data frame standard
#'
#' @author [Your Name]
#'
#' @note This function is part of the arcR package under development. Feedback and contributions are welcome.
#'
#' @examples
#' \dontrun{
#' # Additional examples or use cases
#' }
impute_NA_data_frame_standard = function(object){
  if(!is.data.frame(object)) stop("\nObject must be a dataframe.\n")
  output = data.frame(ID = 1:nrow(object))
  for(i in 1:ncol(object)){
    output = cbind(output,as.data.frame(impute_NA_vector_standard(object[,i])))
  }
  colnames(output) = c("ID",colnames(object))
  return(output[,-1])
}







#' Impute Missing Values by Econometric Modeling
#'
#' The `impute_NA_by_econometry` function imputes missing values in the dependent variable of a formula
#' using various econometric models such as linear regression, logistic regression, Poisson regression,
#' quasipoisson regression, and Gamma regression.
#'
#' @param formula A formula specifying the dependent and independent variables.
#' @param data The data frame containing the variables specified in the formula.
#' @param USE A character vector indicating the type of econometric model to be used (e.g., "lm", "logit", "poisson").
#' @param signif The significance level for statistical tests and diagnostics.
#'
#' @return A list containing the imputed values and the final dataset with the imputed values added as a new column.
#'
#' @examples
#' \dontrun{
#' # Example usage of impute_NA_by_econometry
#' formula <- formula(dependent_var ~ explanatory_var1 + explanatory_var2)
#' data_frame <- data.frame(dependent_var = c(1, NA, 3, 4, NA, 6),
#'                          explanatory_var1 = c(2, 3, 1, 5, 4, 2),
#'                          explanatory_var2 = c("A", "B", "A", "B", "A", "B"))
#' result <- impute_NA_by_econometry(formula, data_frame,
#' USE = "lm", signif = 0.05)
#' print(result)
#' }
#'
#'
#' @keywords missing values imputation econometric modeling
#'
#' @seealso \code{\link{impute_NA_vector_standard}}, \code{\link{impute_NA_data_frame_standard}}
#'
#' @family arcR
#'
#' @author xgeosoft
#'
#' @note This function is part of the arcR package under development. Feedback and contributions are welcome.
#'
#' @importFrom stats lm lm.fit residuals predict glm terms
#' @examples
#' \dontrun{
#' # Additional examples or use cases
#' }
impute_NA_by_econometry = function(formula,data = NULL,USE = c("lm","logit","poisson","quasipoisson","Gamma"),signif = 0.05) {
  if(is.null(data)) stop("No data specified.")
  else {
    print("MAKE SURE THE DEPENDENT VARIABLE CONTAINE NA's VALUES")
    dependant_var_index = which(colnames(data) == as.character(formula[[2]]))
    dependant <- data[,dependant_var_index]
    concerned_index = which(is.na(dependant))
    newdata = impute_NA_data_frame_standard(data)

    if(is.numeric(dependant)) prediction = dependant
    else prediction = as.character(dependant)


    if(USE[1] == "lm"){
      model = lm(formula = formula,data = newdata[-concerned_index,])
      resid = residuals(model)
      print(summary(model))
      if(is_normal_distribution(resid,alpha = signif)){
        cat("\n---------------------------------------------------------------\n
            NOTE 1: Residuals are normals at",signif,"
            \n---------------------------------------------------------------\n")
      }
      prediction[concerned_index] = predict(model,newdata[concerned_index,],type = "response")
    }

    if(USE[1] == "logit"){
      cat("\n",USE[1],"Dependent variable must be numeric. Defined as probability 0 <= y <= 1.\n")
      model = glm(formula = formula,data = newdata[-concerned_index,],family = "binomial")
      resid = residuals(model)
      print(summary(model))
      prediction[concerned_index] = predict(model,newdata[concerned_index,],type = "response")
    }

    if(any(USE[1] == c("poisson","quasipoisson"))){
      cat("\n",USE[1],"Dependent variable must be numeric. Defined as a count.\n")
      model = glm(formula = formula,data = newdata[-concerned_index,],family = USE[1])
      resid = residuals(model)
      print(summary(model))
      prediction[concerned_index] = predict(model,newdata[concerned_index,],type = "response")
    }

    if(any(USE[1] == c("Gamma"))){
      cat("\n",USE[1],"Dependent variable must be numeric. Defined as a count.\n")
      model = glm(formula = formula,data = newdata[-concerned_index,],family = USE[1])
      resid = residuals(model)
      print(summary(model))
      prediction[concerned_index] = predict(model,newdata[concerned_index,],type = "response")
    }
  }

  final_data = cbind(data,as.data.frame(prediction))
  colnames(final_data) = c(colnames(data),paste0(as.character(formula)[2],"_predicted"))

  #----------------------summary_after operation
  explanatory_variables <- attr(terms(formula), "term.labels")
  cat("\n---------------------------------------------------------------\n
            NOTE 2: STATISTICS ON VARIABLES at",signif,"
            \n---------------------------------------------------------------\n")

  resumedf(final_data[c(colnames(data)[dependant_var_index],explanatory_variables)],alpha = signif)
  return(list(prediction = prediction,final_dataset = final_data))
}

