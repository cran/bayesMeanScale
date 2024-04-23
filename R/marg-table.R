

margTableF <- function(pred_diff, marg_list, at, digits, ci, hdi_interval, centrality, at_means, i){

  if(centrality=='map'){
    centrality <- "map_estimate"
  }
  
  centralityF <- eval(parse(text=centrality))
  
  if(!is.null(at)){
    
    atVars <- names(expand.grid(at))
    tableNames <- c(atVars, 'marg_effect', 'start_val', 'end_val', centrality, 'lower', 'upper')
    
  } else{
    
    tableNames <- c('marg_effect', 'start_val', 'end_val', centrality, 'lower', 'upper')
    
  }
  
  if(!is.null(at)){

    atVars <- names(expand.grid(at))

    if(hdi_interval==T){

    diffTableTemp <- pred_diff %>%
      .[, .(marg_effect = marg_list$marg[[i]],
            start_val   = marg_list$start[[i]],
            end_val     = marg_list$end[[i]],
            centrality  = round(centralityF(diff), digits=digits),
            lower       = round(bayestestR::hdi(diff, ci=ci)$CI_low, digits=digits),
            upper       = round(bayestestR::hdi(diff, ci=ci)$CI_high, digits=digits)), by=atVars]

    } else{

      diffTableTemp <- pred_diff %>%
        .[, .(marg_effect = marg_list$marg[[i]],
              start_val   = marg_list$start[[i]],
              end_val     = marg_list$end[[i]],
              centrality  = round(centralityF(diff), digits=digits),
              lower       = round(quantile(diff, probs=(1-ci)/2), digits=digits),
              upper       = round(quantile(diff, probs=1-(1-ci)/2), digits=digits)), by=atVars]

    }

  } else{

    if(hdi_interval==T){

      diffTableTemp <- data.table::data.table(marg_effect = marg_list$marg[[i]],
                                  start_val   = marg_list$start[[i]],
                                  end_val     = marg_list$end[[i]],
                                  centrality  = round(centralityF(pred_diff$diff), digits=digits),
                                  lower       = round(bayestestR::hdi(pred_diff$diff, ci=ci)$CI_low, digits=digits),
                                  upper       = round(bayestestR::hdi(pred_diff$diff, ci=ci)$CI_high, digits=digits))

    } else{

      diffTableTemp <- data.table::data.table(marg_effect = marg_list$marg[[i]],
                                  start_val   = marg_list$start[[i]],
                                  end_val     = marg_list$end[[i]],
                                  centrality  = round(centralityF(pred_diff$diff), digits=digits),
                                  lower       = round(quantile(pred_diff$diff, probs=(1-ci)/2), digits=digits),
                                  upper       = round(quantile(pred_diff$diff, probs=1-(1-ci)/2), digits=digits))

    }

  }
  
  names(diffTableTemp) <- tableNames

  return(diffTableTemp)

}
