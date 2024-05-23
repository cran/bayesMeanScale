
print.bayes_mean_scale_pred <- function(x, ...){
  
  print(x$predTable, row.names=F, ...)
  
}

print.bayes_mean_scale_marg <- function(x, ...){
  
  print(x$diffTable, row.names=F, ...)
  
}

print.bayes_mean_scale_marg_compare <- function(x, ...){
  
  print(x$diffTable, row.names=F, ...)
  
}