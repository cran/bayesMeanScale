
bayesCountMargEffF <- function(model, counts, marginal_effect, start_value, end_value, n_draws=2000, ci=.95, hdi_interval=TRUE, centrality='mean', digits=4, at=NULL, at_means=FALSE, h=.0001){

  countMargErrorCheckF(model           = model,
                       counts          = counts,
                       marginal_effect = marginal_effect,
                       at              = at,
                       start_value     = start_value,
                       end_value       = end_value,
                       centrality      = centrality)

  # initialize the model table and diff matrices list #

  diffTable <- data.table()
  diffDraws <- data.table()

  # make the list of marginal effects, start values, and end values #

  margList <- list(marg  = marginal_effect,
                   start = start_value,
                   end   = end_value)

  # modify the model formula if there's an offset #

  formulaNoOffsets <- modifyFormulaF(model = model)
  
  # get the draws #
  
  draws <- sample(1:nrow(posterior::as_draws_df(model)), size=n_draws, replace=T)

  for(i in 1:length(marginal_effect)){

    # get the model data #

    if(start_value[i]=="instantaneous"){
      
      modData <- margModelDataContinuousF(model       = model,
                                          new_formula = formulaNoOffsets,
                                          at          = at,
                                          marg_list   = margList,
                                          i           = i,
                                          h           = h)
      
    } else{
      
      modData <- margModelDataF(model       = model,
                                new_formula = formulaNoOffsets,
                                at          = at,
                                marg_list   = margList,
                                i           = i)
      
    }

    # get the predictions #

    predStart <- meanCountPredF(model       = model,
                                counts      = counts,
                                new_data    = modData$startData,
                                draws       = draws,
                                new_formula = formulaNoOffsets,
                                at_means    = at_means,
                                at          = at)

    predEnd   <- meanCountPredF(model       = model,
                                counts      = counts,
                                new_data    = modData$endData,
                                draws       = draws,
                                new_formula = formulaNoOffsets,
                                at_means    = at_means,
                                at          = at)

    # get the difference in predicted means #

    if(start_value[i]=='instantaneous'){
      
      predDiff <- countMeanDiffContinuousF(pred_start = predStart,
                                           pred_end   = predEnd,
                                           model_data = modData$modelData,
                                           marg_list  = margList,
                                           counts     = counts,
                                           at         = at,
                                           at_means   = at_means,
                                           i          = i,
                                           h          = h)
      
    } else{
      
      predDiff <- countMeanDiffF(pred_start = predStart,
                                 pred_end   = predEnd,
                                 model_data = modData$modelData,
                                 marg_list  = margList,
                                 counts     = counts,
                                 at         = at,
                                 at_means   = at_means,
                                 i          = i)
      
    }

    # make the marginal effects tables #

    diffTableTemp <- countMargTableF(pred_diff    = predDiff,
                                     marg_list    = margList,
                                     counts       = counts,
                                     at           = at,
                                     digits       = digits,
                                     ci           = ci,
                                     hdi_interval = hdi_interval,
                                     centrality   = centrality,
                                     at_means     = at_means,
                                     i            = i)

    # bind the model tables and difference matrices #

    diffTable <- rbind(diffTable, diffTableTemp)
    diffDraws <- rbind(diffDraws, predDiff)

    }

    # output #

    diffList <- structure(list(diffDraws = diffDraws,
                               diffTable = as.data.frame(diffTable)),
                          class        = c("bayesmeanscale_marg", "list"), 
                          response     = "count_probability", 
                          at           = at, 
                          at_means     = at_means, 
                          n_draws      = n_draws, 
                          ci           = ci, 
                          hdi_interval = hdi_interval)


    return(diffList)


}



