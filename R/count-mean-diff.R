
countMeanDiffF <- function(pred_start, pred_end, model_data, marg_list, counts, at, at_means, i){

  if(!is.null(at) & at_means==F){

    atVars    <- names(expand.grid(at))
    groupVars <- c(atVars, "count")

    predDiffOrg <- subset(pred_start, select = -count) - subset(pred_end, select = -count) %>%
      as.data.frame()

    predDiff <- predDiffOrg %>%
      data.table::setDT() %>%
      cbind(model_data[, ..atVars]) %>%
      cbind(pred_start["count"]) %>%
      .[, lapply(.SD, mean), by=groupVars, .SDcols=!groupVars] %>%
      data.table::melt(id.vars       = groupVars,
           variable.name = 'which_diff',
           value.name    = 'diff') %>%
      .[, !"which_diff"] %>%
      .[, `:=`(comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
               marg_effect = marg_list$marg[[i]])]

  }
  
  if(is.null(at) & at_means==F){

    predDiffOrg <- subset(pred_start, select = -count) - subset(pred_end, select = -count) %>%
      as.data.frame()
    
    countVar <- "count"
    
    predDiff <- predDiffOrg %>%
      data.table::setDT() %>%
      cbind(pred_start["count"]) %>%
      .[, lapply(.SD, mean), by=countVar, .SDcols=!countVar] %>%
      data.table::melt(id.vars       = countVar,
           variable.name = 'which_diff',
           value.name    = 'diff') %>%
      .[, !"which_diff"] %>%
      .[, `:=`(comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
               marg_effect = marg_list$marg[[i]])]

  }
  
  if(!is.null(at) & at_means==T){
    
    atVars   <- names(expand.grid(at))
    atValues <- expand.grid(at)
    
    groupVars <- c(atVars, "count")
    
    predDiffOrg <- subset(pred_start, select = -count) - subset(pred_end, select = -count) %>%
      as.data.frame()
    
    predDiff <- predDiffOrg %>%
      data.table::setDT() %>%
      cbind(atValues) %>%
      cbind(pred_start["count"]) %>%
      data.table::melt(id.vars       = groupVars,
           variable.name = 'which_diff',
           value.name    = 'diff') %>%
      .[, !"which_diff"] %>%
      .[, `:=`(comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
               marg_effect = marg_list$marg[[i]])]
    
  }
  
  if(is.null(at) & at_means==T){
    
    predDiffOrg <- subset(pred_start, select = -count) - subset(pred_end, select = -count) %>%
      as.data.frame()
    
    countVar <- "count"
    
    predDiff <- predDiffOrg %>%
      data.table::setDT() %>%
      cbind(pred_start["count"]) %>%
      data.table::melt(id.vars       = countVar,
           variable.name = 'which_diff',
           value.name    = 'diff') %>%
      .[, !"which_diff"] %>%
      .[, `:=`(comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
               marg_effect = marg_list$marg[[i]])]
    
  }

  return(predDiff)

}
