
bayesPredsF <- function(model, at, n_draws=2000, ci=.95, hdi_interval=TRUE, centrality='mean', digits=4, at_means=FALSE){

  predsErrorCheckF(model      = model,
                   at         = at,
                   centrality = centrality)

  # modify the model formula if there's an offset #

  formulaNoOffsets <- modifyFormulaF(model=model)

  # get the model data #
  
  if(!is.null(model$offset)){
    
    modelDataOrg <- model$data %>%
      cbind(offset=model$offset)
    
  } else{
    
    modelDataOrg <- model$data
    
  }

  # tack on the grouping variables #

  atValues  <- expand.grid(at)
  atVars    <- names(atValues)

  modelData <- modelDataOrg %>%
    na.omit() %>%
    .[, !(colnames(.) %in% atVars), drop=F] %>%
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
  
  preds <- meanPredF(model,
                     new_data    = newData,
                     at          = at,
                     draws       = draws,
                     new_formula = formulaNoOffsets,
                     at_means    = at_means)

  # get the results table #

  predTable <- predTableF(preds        = preds,
                          model_data   = modelData,
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
                        class = c("bayes_mean_scale_pred", "list"),
                        scale = "mean")
  
  
  return(predList)
  
}

