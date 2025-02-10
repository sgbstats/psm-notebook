auc_boot=function(data, indices, actual, predicted)
{
  d=data[indices,]
  return(auc(d, actual,predicted, boot=F))
}