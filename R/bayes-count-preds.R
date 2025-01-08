
bayesCountPredsF <- function(model, counts, at, n_draws=2000, ci=.95, hdi_interval=TRUE, centrality='mean', digits=4, at_means=FALSE, data_slice='full'){

  countPredsErrorCheckF(model      = model,
                        counts     = counts,
                        at         = at,
                        centrality = centrality)

  # modify the model formula if there's an offset #

  formulaNoOffsets <- modifyFormulaF(model=model)

  # get the model data #
  
  modelDataOrg <- model$data %>%
    .[, colnames(.) %in% all.vars(formulaNoOffsets), drop=F] %>%
    .[row.names(model$model),] %>%
    {if(!is.null(model$offset)) cbind(., offset=model$offset) else .}

  # prepare 'at' values and 'at' names #
  
  atValues  <- expand.grid(at)
  atVars    <- names(at)
  
  # check if there are any variables left to average over #
  
  if(ncol(modelDataOrg[, !(colnames(modelDataOrg) %in% c(atVars, "offset")), drop=F])==1){
    data_slice <- 1
  }
  
  # tack on the grouping variables #

  modelData <- modelDataOrg %>%
    .[, !(colnames(.) %in% atVars), drop=F] %>%
    {if(data_slice=="full") . else .[sample(1:nrow(modelDataOrg), size=data_slice, replace=T), colnames(.), drop=F]} %>%
    merge(atValues, all=T) %>%
    data.table::setDT()

  # do some checks for data integrity #

  newData <- levelsPrepF(data          = modelData,
                         at            = atValues,
                         original_data = modelDataOrg)

  dataCheckF(new_data    = newData,
             model_frame = modelDataOrg)

  # get the draws #
  
  draws <- sample(1:nrow(posterior::as_draws_df(model)), size=n_draws, replace=T)
  
  # get the predictions #

  preds <- meanCountPredF(model,
                          new_data    = newData,
                          counts      = counts,
                          at          = at,
                          draws       = draws,
                          new_formula = formulaNoOffsets,
                          at_means    = at_means)

  # get the results table #

  predTable <- countPredTableF(preds        = preds,
                               model_data   = modelData,
                               counts       = counts,
                               at_vars      = atVars,
                               at_values    = atValues,
                               hdi_interval = hdi_interval,
                               centrality   = centrality,
                               digits       = digits,
                               ci           = ci,
                               at_means     = at_means)

  # output #

  predList <- structure(list(predDraws = preds,
                             predTable = as.data.frame(predTable)),
                        class        = c("bayesmeanscale_pred", "list"), 
                        response     = "count_probability", 
                        at           = at, 
                        at_means     = at_means, 
                        n_draws      = n_draws, 
                        ci           = ci, 
                        hdi_interval = hdi_interval)
  
  
  return(predList)
  
}

