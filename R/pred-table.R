
predTableF <- function(preds, model_data, at_vars, at_values, hdi_interval, centrality, digits, ci, at_means){

  centralityF <- eval(parse(text=centrality))
  
  if(at_means==F){

    # tack on the grouping values, get the means by posterior draw, and shape to long format #

    predsNew <- preds %>%
      data.table::as.data.table() %>%
      cbind(model_data[, ..at_vars]) %>%
      .[, lapply(.SD, mean), by=at_vars, .SDcols=!at_vars] %>%
      data.table::melt(id.vars       = at_vars,
                       variable.name = 'which_pred',
                       value.name    = 'pred')

    # make the table #

    if(hdi_interval==TRUE){

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(bayestestR::hdi(pred, ci=ci)$CI_low, digits=digits),
              upper      = round(bayestestR::hdi(pred, ci=ci)$CI_high, digits=digits)), by=at_vars]

    } else{

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(quantile(pred, probs=(1-ci)/2), digits=digits),
              upper      = round(quantile(pred, probs=1-(1-ci)/2), digits=digits)), by=at_vars]

    }

  } else{

    # tack on the grouping values and shape to long format #

    predsNew <- preds %>%
      data.table::as.data.table() %>%
      cbind(at_values) %>%
      data.table::melt(id.vars       = at_vars,
                       variable.name = 'which_pred',
                       value.name    = 'pred')

    # make the table #

    if(hdi_interval==TRUE){

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(bayestestR::hdi(pred, ci=ci)$CI_low, digits=digits),
              upper      = round(bayestestR::hdi(pred, ci=ci)$CI_high, digits=digits)), by=at_vars]

    } else{

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(quantile(pred, probs=(1-ci)/2), digits=digits),
              upper      = round(quantile(pred, probs=1-(1-ci)/2), digits=digits)), by=at_vars]

    }

  }
  
  names(predTable)[names(predTable)=='centrality'] <- centrality

  return(predTable)

}
