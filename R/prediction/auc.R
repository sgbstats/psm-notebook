#' @data A dataframe to test
#' @actual @predicted column names of the actual and predicted values
#' @invert whether the predictor should be inverted (eg if the lower predictor value is associated with a higher actual score then force_inversion may be the right pick). invert_option takes the higher of the mean and 1-mean. No invert is the mean and force_invert is 1-mean
#' @boot whether the bootstrap estiamet should be used. 

auc=function(data, actual, predicted,invert=c("invert_optional", "no_inversion", "force_inversion"), boot=F,...){
  
  
  invert=invert[1]
  if(!invert %in% c("invert_optional", "no_inversion", "force_inversion")){
    stop("Choose invert option correctly")
  }
  x=cbind.data.frame(data[actual], data[predicted])
  names(x)=c("actual", "predicted")
  
  #cross join on the data to make every combination and then filter so that there is one row for each pair of data points, minus those that equal actual outcomes.
  y=cross_join(x,x) %>% filter(predicted.x<predicted.y, actual.x!=actual.y) %>% 
    mutate(correct=actual.x<actual.y)
  
  if(invert=="invert_optional"){
    auc=max(mean(y$correct),1-mean(y$correct))
  }else if(invert=="no_inversion"){
    auc=mean(y$correct)
  }else if(invert=="force_inversion"){
    auc=1-mean(y$correct)
  }  
  if(boot)
  {
    b=boot::boot(x, statistic=auc_boot, R=1000, actual="actual", predicted="predicted", invert=invert)
    b1=boot::boot.ci(b, type="norm")
    b2=b1$normal
    return(data.frame("t"=auc,
                      "lower"=b2[2],
                      "upper"=b2[3]))
  }
  if(!boot){
    return(auc)
  }
}
