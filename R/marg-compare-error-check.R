
margCompareErrorCheckF <- function(marg_list, ci, hdi_interval, centrality){

  if(!inherits(marg_list, 'bayes_mean_scale_marg')){
    stop("The 'marg_list' argument must have class 'bayes_mean_scale_marg'!")
  }
  
  if(!(hdi_interval %in% c(T, F))){
    stop("This is a logical argument!")
  }
  
  if(ci > 1 | ci < 0){
    stop("The credible interval level must be between 0 and 1!")
  }

  if(nrow(unique(subset(marg_list$diffDraws, select=-diff)))==1){
    stop("There is only 1 marginal effect, so nothing to compare to!")
  }
  
  # check that the centrality measure is supported #
  
  if(!(centrality %in% c('mean', 'median'))){
    stop("Centrality options are 'mean' or 'median'!")
  }

}
