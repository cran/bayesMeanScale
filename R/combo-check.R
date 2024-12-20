
comboCheckF <- function(i, j, comboData, marg_list){
  
  if(attr(marg_list, 'response')=='mean'){
    if(comboData$comparison[i]==comboData$comparison[j] & comboData$marg_effect[i]==comboData$marg_effect[j]){
      return(T)
    } else{
        return(F)
      }
  }
  
  if(attr(marg_list, 'response')=='count_probability'){
    if(comboData$comparison[i]==comboData$comparison[j] & comboData$marg_effect[i]==comboData$marg_effect[j] & comboData$count[i]==comboData$count[j]){
      return(T)
    } else{
        return(F)
      }
  }
  
  if(attr(marg_list, 'response')=='outcome_probability'){
    if(comboData$comparison[i]==comboData$comparison[j] & comboData$marg_effect[i]==comboData$marg_effect[j] & comboData$outcome[i]==comboData$outcome[j]){
      return(T)
    } else{
        return(F)
      }
  }
  
}
