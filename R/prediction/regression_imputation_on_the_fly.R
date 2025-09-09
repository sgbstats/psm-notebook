library(tidyverse)


regression_imp_on_the_fly=function(imp_mod,newdata, init=imp_mod[["init"]], iter=10){
  
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
  
  #only it is told to reinitialize the data
  if(is.null(init)){
    init <- newdata %>%
      summarise(across(everything(), calculate_summary))
  }
  
  
  # filling the data in
  df_filled <- newdata %>%
    mutate(across(everything(), ~ coalesce(.x, init[[cur_column()]])))
  
  
  na_data=newdata %>% mutate(across(everything(), ~ ifelse(is.na(.x), 0, 1)))
  rs=rowSums(na_data)
  
  class2=function(x)
  {
    return(class(x)[1])
  }
  
  t=sapply(df_filled, class2)
  
  mod=imp_mod[["mod"]]
  
  #a reapet for each iteration
  for(k in 1:iter){
    for(i in 1:dim(df_filled)[1]){
      # cat(crayon::yellow(paste0(i, "\n")))
      for(j in 1:dim(df_filled)[2]){
        # cat(crayon::blue(paste0(j, "\n")))
        if(na_data[i,j]==1){
          next
        }else{
          x=predict(mod[[names(newdata[j])]], newdata=df_filled[i,])
        }
        if(t[j] %in% c("factor", "logical")){
          df_filled[i,j]=if_else(x<0, levels(df_filled[,j])[1],levels(df_filled[,j])[2])
        }else{
          df_filled[i,j]=x
        }
        
        
      }
    }
  }
  return(df_filled)
}
