
ordinalPredTableF <- function(preds, model_data, at_vars, at_values, y_outcomes, hdi_interval, centrality, digits, ci, at_means){
  
  centralityF <- eval(parse(text=centrality))
  tableNames  <- c(at_vars, 'outcome', centrality, 'lower', 'upper')
  
  groupVars <- c(at_vars, "draw")
  tableVars <- c(at_vars, "outcome")
  
  if(at_means==F){

    # get the means by posterior draw and shape to long format #

    predsNew <- preds %>%
      .[, lapply(.SD, mean), by=groupVars, .SDcols=!groupVars] %>%
      data.table::melt(id.vars       = groupVars,
                       variable.name = 'outcome',
                       value.name    = 'pred')

    # make the table #

    if(hdi_interval==TRUE){

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(bayestestR::hdi(pred, ci=ci)$CI_low, digits=digits),
              upper      = round(bayestestR::hdi(pred, ci=ci)$CI_high, digits=digits)), by=tableVars]

    } else{

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(quantile(pred, probs=(1-ci)/2), digits=digits),
              upper      = round(quantile(pred, probs=1-(1-ci)/2), digits=digits)), by=tableVars]

    }

  } else{

    # shape to long format #

    predsNew <- preds %>%
      data.table::melt(id.vars       = groupVars,
                       variable.name = 'outcome',
                       value.name    = 'pred')

    # make the table #

    if(hdi_interval==TRUE){

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(bayestestR::hdi(pred, ci=ci)$CI_low, digits=digits),
              upper      = round(bayestestR::hdi(pred, ci=ci)$CI_high, digits=digits)), by=tableVars]

    } else{

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(quantile(pred, probs=(1-ci)/2), digits=digits),
              upper      = round(quantile(pred, probs=1-(1-ci)/2), digits=digits)), by=tableVars]

    }

  }
  
  names(predTable) <- tableNames

  return(predTable)

}
