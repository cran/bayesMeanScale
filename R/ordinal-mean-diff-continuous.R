
ordinalMeanDiffContinuousF <- function(pred_start, pred_end, model_data, marg_list, at, at_means, i, h){

  if(!is.null(at) & at_means==F){

    atVars    <- names(expand.grid(at))
    groupVars <- c(atVars, "draw")
    
    predDiff <- ((pred_start[, .SD, .SDcols=!groupVars] - pred_end[, .SD, .SDcols=!groupVars])/h) %>%
      cbind(subset(pred_start, select=groupVars)) %>%
      .[, lapply(.SD, mean), by=groupVars, .SDcols=!groupVars] %>%
      data.table::melt(id.vars       = groupVars,
                       variable.name = 'outcome',
                       value.name    = 'diff') %>%
      .[, `:=` (comparison  = "Instantaneous rate of change",
                marg_effect = marg_list$marg[[i]])] %>%
      .[, .SD, .SDcols=!"draw"]

  }
  
  if(is.null(at) & at_means==F){
    
    predDiff <- ((pred_start[, .SD, .SDcols=!"draw"] - pred_end[, .SD, .SDcols=!"draw"])/h) %>%
      cbind(subset(pred_start, select=draw)) %>%
      .[, lapply(.SD, mean), by="draw", .SDcols=!"draw"] %>%
      data.table::melt(id.vars       = "draw",
                       variable.name = 'outcome',
                       value.name    = 'diff') %>%
      .[, `:=` (comparison  = "Instantaneous rate of change",
                marg_effect = marg_list$marg[[i]])] %>%
      .[, .SD, .SDcols=!"draw"]

  }
  
  if(!is.null(at) & at_means==T){
    
    atVars    <- names(expand.grid(at))
    groupVars <- c(atVars, "draw")
    
    predDiff <- ((pred_start[, .SD, .SDcols=!groupVars] - pred_end[, .SD, .SDcols=!groupVars])/h) %>%
      cbind(subset(pred_start, select=groupVars)) %>%
      data.table::melt(id.vars       = groupVars,
                       variable.name = 'outcome',
                       value.name    = 'diff') %>%
      .[, `:=` (comparison  = "Instantaneous rate of change",
                marg_effect = marg_list$marg[[i]])] %>%
      .[, .SD, .SDcols=!"draw"]
    
  }
  
  if(is.null(at) & at_means==T){
    
    predDiff <- ((pred_start[, .SD, .SDcols=!"draw"] - pred_end[, .SD, .SDcols=!"draw"])/h) %>%
      cbind(subset(pred_start, select=draw)) %>%
      data.table::melt(id.vars       = "draw",
                       variable.name = 'outcome',
                       value.name    = 'diff') %>%
      .[, `:=` (comparison  = "Instantaneous rate of change",
                marg_effect = marg_list$marg[[i]])] %>%
      .[, .SD, .SDcols=!"draw"]
    
  }
  
  return(predDiff)

}
