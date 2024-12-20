
meanDiffF <- function(pred_start, pred_end, model_data, marg_list, at, at_means, i){

  if(!is.null(at) & at_means==F){

    atVars <- names(expand.grid(at))

    predDiffOrg <- pred_start - pred_end %>%
      data.table::as.data.table()

    predDiff <- predDiffOrg %>%
      cbind(model_data[, ..atVars]) %>%
      .[, lapply(.SD, mean), by=atVars, .SDcols=!atVars] %>%
      data.table::melt(id.vars       = atVars,
           variable.name = 'which_diff',
           value.name    = 'diff') %>%
      .[, !"which_diff"] %>%
      .[, `:=`(comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
               marg_effect = marg_list$marg[[i]])]

  }
  
  if(is.null(at) & at_means==F){

    predDiff <- data.table::data.table(diff        = colMeans(pred_start - pred_end),
                           comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
                           marg_effect = marg_list$marg[[i]])

  }
  
  if(!is.null(at) & at_means==T){
    
    atVars   <- names(expand.grid(at))
    atValues <- expand.grid(at)
    
    predDiffOrg <- pred_start - pred_end %>%
      data.table::as.data.table()
    
    predDiff <- predDiffOrg %>%
      cbind(atValues) %>%
      data.table::melt(id.vars       = atVars,
           variable.name = 'which_diff',
           value.name    = 'diff') %>%
      .[, !"which_diff"] %>%
      .[, `:=`(comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
               marg_effect = marg_list$marg[[i]])]
    
  }
  
  if(is.null(at) & at_means==T){
    
    predDiff <- data.table(diff        = colMeans(pred_start - pred_end),
                           comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
                           marg_effect = marg_list$marg[[i]])
    
  }
  
  return(predDiff)

}
