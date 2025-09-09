
submodel.print=function(submodels.object, long_wide=c("long", "wide"), remove_formula=T){
  out=list()
  f <- function(x =c("long", "wide")) {
    x <- match.arg(x)
    return(x)
  }
  out[["meta"]]=submodels.object[["meta"]]
  coefficients=tidyr::tribble(~"pattern",~"formula",~"var", ~"coef" )
  for(i in 1:(length(submodels.object)-1)){
    # cat(paste(i, "\n"))
    coefficients1=data.frame("pattern"=submodels.object[["meta"]][["all.patterns"]][i] %>% as.vector(),
                             "formula"=paste0((submodels.object[[i]][["mod"]][["terms"]] %>% as.character())[2],
                             (submodels.object[[i]][["mod"]][["terms"]] %>% as.character())[1],
                               (submodels.object[[i]][["mod"]][["terms"]] %>% as.character())[3]),
                             "var"=names(submodels.object[[i]][["mod"]][["coefficients"]])%>% as.vector(),
                             "coef"=submodels.object[[i]][["mod"]][["coefficients"]])%>% as.vector()
    row.names(coefficients1)=NULL
    coefficients=rbind(coefficients, coefficients1)
    
  }
  
  long_wide=f(long_wide)
  if(long_wide=="long"){
    out[["coefficients"]]=coefficients
  }
  if(long_wide=="wide"){
    out[["coefficients"]]=coefficients %>% pivot_wider(names_from = "var", values_from="coef")
  }
  
  if(!remove_formula){
    out[["patterns"]]=out[["coefficients"]] %>% count(pattern,formula) %>% dplyr::select(-n)
    
    
  }
  out[["coefficients"]]=out[["coefficients"]] %>% dplyr::select(-"formula")
  
  return(out)
}
