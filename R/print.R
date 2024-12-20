
print.bayesmeanscale_pred <- function(x, ...){
  
  print(x$predTable, row.names=F, ...)
  
}

print.bayesmeanscale_marg <- function(x, ...){
  
  print(x$diffTable, row.names=F, ...)
  
}

print.bayesmeanscale_margcompare <- function(x, ...){
  
  print(x$diffTable, row.names=F, ...)
  
}