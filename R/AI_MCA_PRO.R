#' Multiple Correspondence Analysis (MCA) Module
#'
#' This function performs Multiple Correspondence Analysis (MCA) on categorical data with supplementary quantitative variables.
#'
#' @param data The dataframe containing the categorical and quantitative variables.
#' @param main_variable_names Character vector specifying the names of the main categorical variables.
#' @param caracterize_variable_name Character vector specifying the name of the quantitative variable to characterize the analysis.
#'
#' @return A list containing the results of the Multiple Correspondence Analysis, including eigenvalues, contributions, and inertia. The graph of the analysis is also displayed if graph = TRUE.
#' @importFrom FactoMineR MCA
#'
#' @examples
#' #data <- data.frame(var1 = c("A", "B", "A", "C"),
#' #var2 = c("X", "Y", "Y", "X"),numeric_var = c(1, 2, 3, 4))
#' #MODULE_MCA(data, c("var1", "var2"), "numeric_var")
#'
#' @export
#'
#'
MODULE_MCA = function(data,main_variable_names,caracterize_variable_name){
  if(is.data.frame(data)){
    if(!(is.character(main_variable_names) | is.character(caracterize_variable_name))){
      stop("\nNote: The variable name is not valid.\n")
    }

    data1 = impute_type(data,min.qual = 3)
    factor_type_index = which(split_df_by_structure(data1)$Structure != "numeric")
    numeric_type_index = if(length(which(split_df_by_structure(data1)$Structure == "numeric"))>0){
      which(split_df_by_structure(data1)$Structure == "numeric")
    } else {
      NULL
    }
    quali_sup_index = c(1:ncol(data1))[-get_variable_indexINdataframe(dataframe = data1,variable_names = main_variable_names)]

    return(MCA(
      data1,
      ncp = ncol(data1),
      quanti.sup = numeric_type_index,
      quali.sup = quali_sup_index,
      graph = TRUE
    ))
  }
}






#' Interpretative Analysis for Multiple Correspondence Analysis (MCA)
#'
#' This function performs an Interpretative Analysis (AI) for Multiple Correspondence Analysis (MCA) on categorical data with additional quantitative variables.
#'
#' @param data The dataframe containing categorical and quantitative variables.
#' @param main_variable_names Character vector specifying the names of the main categorical variables.
#' @param caracterize_variable_name Character vector specifying the name of the quantitative variable characterizing the analysis.
#' @param cosinus_limit Threshold value for the cosine of the quality of representation of variables on the axes. Variables with a cosine lower than this limit are not considered.
#'
#' @return A list containing the results of the Multiple Correspondence Analysis (MCA), including eigenvalues, contributions, and inertia. The graph of the analysis is also displayed if graph = TRUE.
#'
#' @importFrom ggpubr ggbarplot
#' @import methods
#' @export
#' @examples
#' # Perform MCA
#' # AI_MCA()
AI_MCA = function(data,main_variable_names,caracterize_variable_name,cosinus_limit = 0.1){
  output = list()
  data[,which(colnames(data) == caracterize_variable_name)] = recode_var(data[,which(colnames(data) == caracterize_variable_name)],unique(data$NIVTRANSECO),paste(unique(data$NIVTRANSECO),"#",sep = ""))
  acm = MODULE_MCA(
    data = data,
    main_variable_names = main_variable_names,
    caracterize_variable_name = caracterize_variable_name
  )

  output$MCA = acm

  #-------------------------TABLE EIGEN
  output$eigenvalues$table = acm$eig
  df1 = data.frame(Dim = paste("Dim",1:nrow(acm$eig)),Percentage = acm$eig[,2])
  output$eigenvalues$graph = ggbarplot(df1,x = "Dim",y = "Percentage",
                                       xlab = "Dimensions",
                                       ylab = "Percentages",
                                       title = "Eigens values (in %)",
                                       label = TRUE,
                                       lab.nb.digits = 4)

  number_main_variable = length(main_variable_names)
  variables_contributions = acm$var$contrib
  variables_cosinus2 = rbind(acm$var$cos2,acm$quali.sup$cos2)
  variables_coord = rbind(acm$var$coord,acm$quali.sup$coord,acm$quanti.sup$coord)

  concerned_cos2_index = which(search_r(rownames(variables_cosinus2),"#",full_case = FALSE) != "Not found!")

  output$AI$important_varlist = NULL

  # detection des axes representatif piur caracterize variable
  for(i in concerned_cos2_index){
    for(j in 1:ncol(variables_cosinus2)){
      if(variables_cosinus2[i,j] >= cosinus_limit){
        output$AI$important_varlist = c(unique(output$AI$important_varlist),rownames(variables_cosinus2)[i])
        output$AI$acm_index$row = sort(unique(c(output$AI$acm_index$row,i)))
        output$AI$acm_index$col = unique(c(output$AI$acm_index$col,j))
      }
    }
  }

  if(!is.null(output$AI$acm_index$row) & !is.null(output$AI$acm_index$col)){
    cat("\nNo dimension detected for thz caracterize variables.\n")
    if(length(output$AI$acm_index$row) >= 1 & length(output$AI$acm_index$col) >= 1){

      output$AI$Axes$Dist = list()
      output$AI$Plans$Dist = list()
      individual_axe = NULL
      individual_plan = NULL
      lengthconcernedindex = length(concerned_cos2_index)
      finallevellength = nrow(variables_coord)
      current_distance_axe = matrix(NA,ncol = lengthconcernedindex,nrow = finallevellength)
      current_distance_plan = matrix(NA,ncol = lengthconcernedindex,nrow = finallevellength)

      for(axe_index in output$AI$acm_index$col){

        current_distance_axe = matrix(NA,ncol = lengthconcernedindex,nrow = finallevellength)
        individual_axe = c(individual_axe,axe_index) #axe name

        for(second_axe_index in output$AI$acm_index$col){

          current_distance_plan = matrix(NA,ncol = lengthconcernedindex,nrow = finallevellength)

          if(axe_index == second_axe_index){
            for(i in 1:finallevellength){
              #cat(axe_index,"axe&",i,"et",j)
              col = 0
              for(j in output$AI$acm_index$row){
                col = col+1
                current_distance_axe[i,col] = distanceAB(variables_coord[i,axe_index],variables_coord[j,axe_index],method = "absolut")
              }
            }
          }
          #axe_index = 3
          #second_axe_index = 1
          if(axe_index > second_axe_index){
            individual_plan = c(individual_plan,paste(axe_index,second_axe_index,sep = "_")) #axe name
            for(i in 1:finallevellength){
              col = 0
              for(j in output$AI$acm_index$row){
                col = col + 1
                #cat("axe1=",axe_index,"axe2=",second_axe_index,"i=",i,"j=",j,"col=",col,"\n")
                current_distance_plan[i,col] = distanceAB(variables_coord[i,c(axe_index,second_axe_index)],variables_coord[j,c(axe_index,second_axe_index)])
              }
            }

            if(length(output$AI$acm_index$col)>=2 & !is.null(current_distance_plan)){
              rownames(current_distance_plan) = rownames(variables_coord)
              colnames(current_distance_plan) = rownames(variables_coord)[concerned_cos2_index]
              output$AI$Plans$Dist = c(output$AI$Plans$Dist,list(current_distance_plan))
            }
          }

          if(axe_index < second_axe_index){current_distance_plan = NULL}

        }


        rownames(current_distance_axe) = rownames(variables_coord)
        colnames(current_distance_axe) = rownames(variables_coord)[concerned_cos2_index]
        output$AI$Axes$Dist = c(output$AI$Axes$Dist,list(current_distance_axe))


      }

      names(output$AI$Axes$Dist) = paste0("Dim",individual_axe)
      names(output$AI$Plans$Dist) = paste0("Plan",individual_plan)

      output$AI$Contrib = correcttable(variables_contributions[,output$AI$acm_index$col],100/nrow(variables_contributions),method = "less")
      output$AI$Cos2 = correcttable(variables_cosinus2[,output$AI$acm_index$col],cosinus_limit,method = "less")
      output$AI$Coord = variables_coord[,output$AI$acm_index$col]
      for(i in 1:nrow(output$AI$Cos2)){
        for(j in 1:ncol(output$AI$Cos2)){
          if(is.na(output$AI$Cos2[i,j])){
            output$AI$Coord[i,j] = NA_real_
          }
        }
      }

      output$AI$General = rbind(
        matrix("Contributions",nrow = 1,ncol = length(output$AI$acm_index$col)),
        round(output$AI$Contrib,4),
        matrix("Cos2",nrow = 1,ncol = length(output$AI$acm_index$col)),
        round(output$AI$Cos2,4),
        matrix("Coord",nrow = 1,ncol = length(output$AI$acm_index$col)),
        round(output$AI$Coord,4)
      )
    }
  }

  return(output)
}



