#' Summarize Data Frame
#'
#' This function provides a summary of both quantitative and qualitative variables in a data frame.
#'
#' @param x The data frame to be summarized.
#' @param alpha The significance level for the chi-square independence test (default is 0.05).
#' @param digits The number of digits to round the numeric values (default is 4).
#'
#' @return A list containing various summaries, including statistics for numeric variables, correlation matrix, statistics for non-numeric variables, one-way table, Burt table, and chi-square independence test.
#'
#' @examples
#' data_frame <- data.frame(Age = c(25, 30, 22, 35),
#'  Gender = c("Male", "Female", "Male", "Female"))
#' resumedf(data_frame)
#'
#' @export
#' @importFrom stats median var
#'
#' @keywords summary statistics quantitative qualitative correlation chi-square
#'
#' @family statistics
#'
#' @rdname resumedf
#' @aliases resumedf
#' @name resumedf
#' @usage resumedf(x, alpha = 0.05, digits = 4)
resumedf = function(x,alpha = 0.05,digits = 4){
  if(!is.data.frame(x)){
    stop("\nThe argument x must be a data frame.\n")
  } else {
    output = list()
    splited = split_df_by_structure(x)
    output$data$Quant = splited$`Quantitative data`
    output$data$Qual = splited$`Qualitative data`

    runQuali = is.data.frame(output$data$Qual)
    runQuanti = is.data.frame(output$data$Quant)

    if(runQuanti)
    {
      dim_data_quant = get_dimensions(output$data$Quant)
      output$Name$Quanti = get_variables_names(output$data$Quant)

      nameQuantiStatistics = c("Obs","NA's","Min","Max","Mean","Median","Var","Sd","Cv")
      output$`Statistics for numeric variable(s)` = matrix(NA,nrow = 1,ncol = length(nameQuantiStatistics))[-1,]

      for(i in 1:dim_data_quant$col)
      {
        # descriptive statistics
        statistics_vector = matrix(nrow = 1,ncol = length(nameQuantiStatistics))
        statistics_vector[1,1] = length(which(!is.na(output$data$Quant[,i])))
        statistics_vector[1,2] = length(which(is.na(output$data$Quant[,i])))
        statistics_vector[1,3] = min(output$data$Quant[,i],na.rm = TRUE)
        statistics_vector[1,4] = max(output$data$Quant[,i],na.rm = TRUE)
        statistics_vector[1,5] = mean(output$data$Quant[,i],na.rm = TRUE)
        statistics_vector[1,6] = median(output$data$Quant[,i],na.rm = TRUE)
        statistics_vector[1,7] = var(output$data$Quant[,i],na.rm = TRUE)
        statistics_vector[1,8] = standard_deviation(output$data$Quant[,i],na.rm = TRUE)
        statistics_vector[1,9] = Coefficient_Variation(output$data$Quant[,i],na.rm = TRUE)

        output$`Statistics for numeric variable(s)` = rbind(output$`Statistics for numeric variable(s)`,statistics_vector)

      }

      colnames(output$`Statistics for numeric variable(s)`) = nameQuantiStatistics
      rownames(output$`Statistics for numeric variable(s)`) = output$Name$Quanti

      #Correlation frequency
      output$Correlations = try(correlation_r(output$data$Quant),silent = TRUE)

    }

    if(runQuali)
    {
      dim_data_qual = get_dimensions(output$data$Qual)
      output$Name$Quali = get_variables_names(output$data$Qual)

      nameQualiStatistics = c("Obs","NA's","Mode","Modal_Percentage (%)")
      output$`Statistics for non numeric variable(s)` = matrix(NA_character_,nrow = 1,ncol = length(nameQualiStatistics))[-1,]
      output$`One-way table` = matrix(NA_character_,nrow = 1,ncol = 2)[-1,]
      output$`Two-way table` = list()
      namestwoway = NULL
      j=2;k=1
      for(j in 1:dim_data_qual$col)
      {
        # descriptives statistics
        statistics_vector = matrix(nrow = 1,ncol = length(nameQualiStatistics))
        current_table  = table_r(output$data$Qual[,j],var.names = output$Name$Quali[j],digits = digits)
        statistics_vector[1,1] = length(which(!is.na(output$data$Qual[,j])))
        statistics_vector[1,2] = length(which(is.na(output$data$Qual[,j])))
        statistics_vector[1,3] = modal_value(output$data$Qual[,j])
        statistics_vector[1,4] = round(sort(prop.table(table(output$data$Qual[,j]))*100,decreasing = TRUE)[1],digits = digits)

        for(k in 1:dim_data_qual$col){
          if(j>k){
            output$`Two-way table` = c(output$`Two-way table`,list(table_r(
              x = output$data$Qual[,j],
              y = output$data$Qual[,k],
              var.names = c(output$Name$Quali[j],output$Name$Quali[k]),
              type = "relatives"
            )))
            namestwoway = c(namestwoway,paste(output$Name$Quali[j],output$Name$Quali[k],sep = "*"))
          }
        }

        output$`Statistics for non numeric variable(s)` = rbind(
          output$`Statistics for non numeric variable(s)`,
          statistics_vector
        )

        output$`One-way table` = rbind(
          output$`One-way table`,
          matrix(NA,nrow = 1,ncol = ncol(current_table)),
          current_table
        )
      }

      names(output$`Two-way table`) = namestwoway
      colnames(output$`Statistics for non numeric variable(s)`) = nameQualiStatistics
      rownames(output$`Statistics for non numeric variable(s)`) = output$Name$Quali

      #Burt table
      output$`Burt_table` = try(burt_table(output$data$Qual),silent = TRUE)

      #Chisq-quare test
      output$Chisq_square = try(chisq_test_r(output$data$Qual,alpha = alpha),silent = TRUE)

    }
  }
  return(output)
}

