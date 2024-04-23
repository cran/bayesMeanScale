

countMargTableF <- function(pred_diff, marg_list, counts, at, digits, ci, hdi_interval, centrality, at_means, i){

  if(centrality=='map'){
    centrality <- "map_estimate"
  }
  
  centralityF <- eval(parse(text=centrality))
  
  if(!is.null(at)){
    
    atVars <- names(expand.grid(at))
    tableNames <- c(atVars, "count", 'marg_effect', 'start_val', 'end_val', centrality, 'lower', 'upper')
    
  } else{
    
    tableNames <- c("count", 'marg_effect', 'start_val', 'end_val', centrality, 'lower', 'upper')
    
  }
  
  if(!is.null(at)){

    atVars    <- names(expand.grid(at))
    groupVars <- c(atVars, "count")

    if(hdi_interval==T){

    diffTableTemp <- pred_diff %>%
      .[, .(marg_effect = marg_list$marg[[i]],
            start_val   = marg_list$start[[i]],
            end_val     = marg_list$end[[i]],
            centrality  = round(centralityF(diff), digits=digits),
            lower       = round(bayestestR::hdi(diff, ci=ci)$CI_low, digits=digits),
            upper       = round(bayestestR::hdi(diff, ci=ci)$CI_high, digits=digits)), by=groupVars]

    } else{

      diffTableTemp <- pred_diff %>%
        .[, .(marg_effect = marg_list$marg[[i]],
              start_val   = marg_list$start[[i]],
              end_val     = marg_list$end[[i]],
              centrality  = round(centralityF(diff), digits=digits),
              lower       = round(quantile(diff, probs=(1-ci)/2), digits=digits),
              upper       = round(quantile(diff, probs=1-(1-ci)/2), digits=digits)), by=groupVars]

    }

  } else{

    countVar <- "count"
    
    if(hdi_interval==T){

      diffTableTemp <- pred_diff %>%
        .[, .(marg_effect = marg_list$marg[[i]],
              start_val   = marg_list$start[[i]],
              end_val     = marg_list$end[[i]],
              centrality  = round(centralityF(diff), digits=digits),
              lower       = round(bayestestR::hdi(diff, ci=ci)$CI_low, digits=digits),
              upper       = round(bayestestR::hdi(diff, ci=ci)$CI_high, digits=digits)), by=countVar]

    } else{

      diffTableTemp <- pred_diff %>%
        .[, .(marg_effect = marg_list$marg[[i]],
              start_val   = marg_list$start[[i]],
              end_val     = marg_list$end[[i]],
              centrality  = round(centralityF(diff), digits=digits),
              lower       = round(quantile(diff, probs=(1-ci)/2), digits=digits),
              upper       = round(quantile(diff, probs=1-(1-ci)/2), digits=digits)), by=countVar]

    }

  }
  
  names(diffTableTemp) <- tableNames

  return(diffTableTemp)

}
