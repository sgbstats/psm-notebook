library(tidyverse)


regression_imp=function(data, iter=10, remove_model_data=c("model", "y", "fitted")){
  
  #getting the mean/mode for each column to initialise the data
  
  calculate_summary <- function(col) {
    if (is.numeric(col)) {
      mean(col, na.rm = TRUE)
    } else if (is.factor(col)) {
      mode_val <- levels(col)[which.max(tabulate(col))]
      factor(mode_val, levels = levels(col))  # preserve factor structure
    } else {
      NA
    }
  }
  
  #initialised data
  init <- data %>%
    summarise(across(everything(), calculate_summary))
  
  #filling the data in
  df_filled <- data %>%
    mutate(across(everything(), ~ coalesce(.x, init[[cur_column()]])))
  
  #tracking the missing data
  na_data=data %>% mutate(across(everything(), ~ ifelse(is.na(.x), 0, 1)))
  rs=rowSums(na_data)
  #getting the class of the data
  class2=function(x)
  {
    return(class(x)[1])
  }
  t=sapply(df_filled, class2)
  
  #build each of the models
  mod=list()
  interim=list()
  #repeat for each iteration
  for(k in 1:iter){
    #build a model for each variable
    for(i in 1:ncol(data)){
      formula=as.formula(paste0(names(t)[i], "~."))
      # uses only the known vlaues as the outcome
      tempdata=df_filled[na_data[i]==1,] %>% droplevels()
      
      #currently can only cope with numeric and binary
      if(t[i]=="numeric"){
        mod[[names(data[i])]]=lm(formula = formula, data=tempdata)
      }else if(t[i] %in% c("factor", "logical")){
        mod[[names(data[i])]]=glm(formula = formula, data=tempdata, family="binomial")
      }
      
    }
    
    
    for(i in 1:dim(df_filled)[1]){
      # skips complete rows
      if(rs[i]==dim(df_filled)[1]){next}
      for(j in 1:dim(df_filled)[2]){
        if(na_data[i,j]==1){
          #skips the known values
          next
        }else{
          x=predict(mod[[names(data[j])]], newdata=df_filled[i,])
        }
        if(t[j]=="factor"){
          # if it is binary then it uses the most likely value
          df_filled[i,j]=if_else(x<0, levels(df_filled[,j])[1],levels(df_filled[,j])[2])
        }else{
          df_filled[i,j]=x
        }
      }
    }
    interim[[k]]=df_filled
  }
  #removes individual data from the output
  for(i in remove_model_data){
    mod[i][[i]]=NULL
  }
  return(list("data_imp"=df_filled, "mod"=mod, "init"=init))
}
