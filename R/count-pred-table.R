
countPredTableF <- function(preds, model_data, counts, at_vars, at_values, hdi_interval, centrality, digits, ci, at_means){
  
  centralityF <- eval(parse(text=centrality))
  
  group_vars <- c(at_vars, "count")
  
  if(at_means==F){

    # tack on the grouping values and get the means #

    predsNew <- preds %>%
      data.table::as.data.table() %>%
      cbind(model_data[, ..at_vars]) %>%
      .[, lapply(.SD, mean), by=group_vars, .SDcols=!group_vars] %>%
      data.table::melt(id.vars       = group_vars,
           variable.name = 'which_pred',
           value.name    = 'pred')

    # make the table #

    if(hdi_interval==TRUE){

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(bayestestR::hdi(pred, ci=ci)$CI_low, digits=digits),
              upper      = round(bayestestR::hdi(pred, ci=ci)$CI_high, digits=digits)), by=group_vars]

    } else{

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(quantile(pred, probs=(1-ci)/2), digits=digits),
              upper      = round(quantile(pred, probs=1-(1-ci)/2), digits=digits)), by=group_vars]

    }

  } else{

    # tack on the grouping values and get the means #

    predsNew <- preds %>%
      data.table::as.data.table() %>%
      cbind(at_values) %>%
      data.table::melt(id.vars       = group_vars,
           variable.name = 'which_pred',
           value.name    = 'pred')

    # make the table #

    if(hdi_interval==TRUE){

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(bayestestR::hdi(pred, ci=ci)$CI_low, digits=digits),
              upper      = round(bayestestR::hdi(pred, ci=ci)$CI_high, digits=digits)), by=group_vars]

    } else{

      predTable <- predsNew %>%
        .[, .(centrality = round(centralityF(pred), digits=digits),
              lower      = round(quantile(pred, probs=(1-ci)/2), digits=digits),
              upper      = round(quantile(pred, probs=1-(1-ci)/2), digits=digits)), by=group_vars]

    }

  }
  
  names(predTable)[names(predTable)=='centrality'] <- centrality

  return(predTable)

}
